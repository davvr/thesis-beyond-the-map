# pipeline/map/map_last.R
#
# LAST BAROMETER MAP — reads only the most recent CIS barometer

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(mapSpain)
  library(readxl)
  library(zoo)
  library(colorspace)
  library(cowplot)
  library(haven)
})

source("R/dhondt.R")
dir.create("docs/images/maps", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# CSSLab constants (verbatim)
# ---------------------------------------------------------------------------
HEX_PP_OSCURO <- "#014163"
file_path <- "data-raw/infoelectoral/PROV_02_202307_1.xlsx"

clean_text <- function(x) {
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", "_")
  x <- str_replace_all(x, "[^a-z0-9_]", "")
  return(x)
}

# ---------------------------------------------------------------------------
# 1. Read the original 2023 Excel (only for province metadata + magnitudes)
# ---------------------------------------------------------------------------
raw_headers <- read_excel(file_path, skip = 4, n_max = 2, col_names = FALSE)
parties_raw <- as.character(raw_headers[1, ])
metrics_raw <- as.character(raw_headers[2, ])

parties_filled <- zoo::na.locf(parties_raw, na.rm = FALSE)
parties_clean  <- clean_text(parties_filled)
metrics_clean  <- clean_text(metrics_raw)

clean_names <- ifelse(metrics_clean %in% c("votos", "diputados"),
                      paste(metrics_clean, parties_clean, sep = "_"),
                      metrics_clean)

results_2023 <- read_excel(file_path, skip = 6, col_names = clean_names)

cols_metadata <- c("nombre_de_comunidad", "codigo_de_provincia",
                   "nombre_de_provincia", "poblacion", "numero_de_mesas",
                   "censo_electoral_sin_cera", "censo_cera",
                   "total_censo_electoral", "total_votantes_cer",
                   "total_votantes_cera", "total_votantes", "votos_validos",
                   "votos_a_candidaturas", "votos_en_blanco", "votos_nulos")

# Province magnitudes from the 2023 file (sum of diputados_* per row)
diputados_cols <- str_subset(names(results_2023), "^diputados_")
prov_mag <- results_2023 |>
  filter(nombre_de_provincia != "España", !is.na(codigo_de_provincia)) |>
  mutate(magnitude = rowSums(across(all_of(diputados_cols)), na.rm = TRUE)) |>
  select(codigo_de_provincia, magnitude)
stopifnot(sum(prov_mag$magnitude) == 350L)

# ---------------------------------------------------------------------------
# 2. Read only the most recent CIS barometer
# ---------------------------------------------------------------------------
cis_dir <- "data-raw/cis"
all_zip_files <- fs::dir_ls(cis_dir, glob = "*.zip")

# Extract study IDs and find the most recent
zip_ids <- stringr::str_extract(fs::path_file(all_zip_files), "\\d+") |> as.integer()
most_recent_idx <- which.max(zip_ids)
zip_files <- all_zip_files[most_recent_idx]

message("Found ", length(all_zip_files), " ZIP files total")
message("Using most recent: ", fs::path_file(zip_files))

# Read and stack only the most recent barometer
cis_list <- purrr::map(zip_files, function(zip_path) {
  message("Reading ", fs::path_file(zip_path))

  # Unzip to temp
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(zip_path, exdir = temp_dir)

  # Find the .sav file
  sav_files <- fs::dir_ls(temp_dir, regexp = "\\.sav$")
  if (length(sav_files) == 0) {
    warning("No .sav in ", zip_path)
    unlink(temp_dir, recursive = TRUE)
    return(NULL)
  }

  # Read
  raw <- haven::read_sav(sav_files[1])

  # Clean up
  unlink(temp_dir, recursive = TRUE)

  # Extract the 3 variables
  raw |>
    transmute(
      province_code = haven::zap_labels(PROV) |> as.integer(),
      intention     = haven::zap_labels(INTENCIONGR) |> as.integer(),
      weight        = as.numeric(PESO)
    )
})

# Stack (only one barometer in this case)
cis <- bind_rows(purrr::compact(cis_list))
message("Total weighted observations: ", nrow(cis))

# Drop DK/NA/blank/void/no-vote/other
drop_codes <- c(8995, 8996, 9977, 9997, 9998, 9999)
cis_clean <- cis |>
  filter(!is.na(intention), !intention %in% drop_codes)

# Map CIS codes to parties (identical to pilot)
party_lookup <- tribble(
  ~code, ~partido,
  1,     "psoe",
  2,     "pp",
  3,     "vox",
  21,    "sumar",
  503,   "cca",
  901,   "erc",
  902,   "jxcat__junts",
  1201,  "bng",
  1501,  "upn",
  1601,  "eajpnv",
  1602,  "eh_bildu"
)
all_parties <- party_lookup$partido

cis_mapped <- cis_clean |>
  inner_join(party_lookup, by = c("intention" = "code"))

# Weighted province × party vote share
prov_share <- cis_mapped |>
  group_by(province_code, partido) |>
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") |>
  group_by(province_code) |>
  mutate(p_votos = 100 * weighted_count / sum(weighted_count)) |>
  ungroup() |>
  select(codigo_de_provincia = province_code, partido, p_votos)

# Fill in missing (province, party) combinations with 0
prov_share_full <- expand_grid(
  codigo_de_provincia = unique(results_2023$codigo_de_provincia),
  partido = all_parties
) |>
  filter(!is.na(codigo_de_provincia)) |>
  left_join(prov_share, by = c("codigo_de_provincia", "partido")) |>
  mutate(p_votos = replace_na(p_votos, 0))

# ---------------------------------------------------------------------------
# 3. Project provincial seat allocation under D'Hondt
# ---------------------------------------------------------------------------
# We use the projected vote share directly as the input to D'Hondt (any
# positive scaling produces the same allocation; here % suffices).
prov_share_with_mag <- prov_share_full |>
  inner_join(prov_mag, by = "codigo_de_provincia")

allocate_one <- function(df_prov) {
  m <- unique(df_prov$magnitude)
  votes <- setNames(df_prov$p_votos, df_prov$partido)
  seats <- dhondt(votes, magnitude = m, threshold = 0.03)
  tibble(partido = names(seats), diputados = as.integer(seats))
}

prov_alloc <- prov_share_with_mag |>
  group_by(codigo_de_provincia) |>
  group_modify(~ allocate_one(.x)) |>
  ungroup()

# ---------------------------------------------------------------------------
# 4. Build a `results_enriched` tibble in the SAME shape the CSSLab Rmd uses
# ---------------------------------------------------------------------------
# Columns expected by the downstream visual code:
#   - all cols_metadata (we copy them from the 2023 Excel)
#   - votos_<partido>, p_votos_<partido>, diputados_<partido> for every party
# Plus a "España" summary row.

# For votos_<partido>, we use the projected percentage scaled by an arbitrary
# constant so that votos and p_votos are consistent. Since the CSSLab uses
# votos / votos_validos * 100 to derive p_votos, we mirror that: we set
# votos = p_votos and votos_validos = 100 inside our synthetic file.
# This way the CSSLab national_stats block recomputes exactly the same %.
prov_meta <- results_2023 |>
  filter(nombre_de_provincia != "España", !is.na(codigo_de_provincia)) |>
  select(all_of(cols_metadata)) |>
  mutate(votos_validos = 100)   # synthetic scale: % == votos

prov_long <- prov_share_full |>
  left_join(prov_alloc, by = c("codigo_de_provincia", "partido")) |>
  mutate(votos = p_votos,
         diputados = replace_na(diputados, 0L))

prov_wide <- prov_long |>
  pivot_wider(names_from = partido,
              values_from = c(votos, p_votos, diputados),
              names_glue = "{.value}_{partido}")

# Order columns: metadata first, then votos/p_votos/diputados grouped per party
orden_final <- cols_metadata
for (p in all_parties) {
  orden_final <- c(orden_final,
                   paste0("votos_", p),
                   paste0("p_votos_", p),
                   paste0("diputados_", p))
}

prov_enriched <- prov_meta |>
  left_join(prov_wide, by = "codigo_de_provincia") |>
  select(any_of(orden_final))

# Build the "España" summary row (national totals)
nat_seats <- prov_alloc |>
  group_by(partido) |>
  summarise(diputados_nacional = sum(diputados), .groups = "drop")

# National vote share from the survey: sum weighted counts over all provinces
nat_share <- cis_mapped |>
  group_by(partido) |>
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") |>
  mutate(p_votos = 100 * weighted_count / sum(weighted_count))

espana_row <- tibble(
  nombre_de_comunidad      = NA_character_,
  codigo_de_provincia      = NA_real_,
  nombre_de_provincia      = "España",
  poblacion                = NA_real_,
  numero_de_mesas          = NA_real_,
  censo_electoral_sin_cera = NA_real_,
  censo_cera               = NA_real_,
  total_censo_electoral    = NA_real_,
  total_votantes_cer       = NA_real_,
  total_votantes_cera      = NA_real_,
  total_votantes           = NA_real_,
  votos_validos            = 100,
  votos_a_candidaturas     = NA_real_,
  votos_en_blanco          = NA_real_,
  votos_nulos              = NA_real_
)

for (p in all_parties) {
  pct  <- nat_share$p_votos[match(p, nat_share$partido)]
  if (is.na(pct)) pct <- 0
  seats <- nat_seats$diputados_nacional[match(p, nat_seats$partido)]
  if (is.na(seats)) seats <- 0L
  espana_row[[paste0("votos_",     p)]] <- pct
  espana_row[[paste0("p_votos_",   p)]] <- pct
  espana_row[[paste0("diputados_", p)]] <- as.integer(seats)
}

results_enriched <- bind_rows(prov_enriched, espana_row) |>
  select(any_of(orden_final))

# ---------------------------------------------------------------------------
# 5. From here, the CSSLab Rmd code is reused verbatim
# ---------------------------------------------------------------------------
# Party color tables
party_base <- tribble(
  ~party,     ~base_hex,
  "PP",       "#15a6ef", "PSOE",     "#f11123", "Vox",      "#74cd30",
  "Sumar",    "#ef4a92", "ERC",      "#ffa503", "Junts",    "#00c8b0",
  "EH Bildu", "#00ae8f", "PNV",      "#48b049",
  "BNG",      "#aed0ef", "CC",       "#ffd800", "UPN",      "#00589c"
)

party_colours <- tribble(
  ~party,   ~bin,      ~hex,
  "PP", "0-25",   "#b1e0f9",
  "PP", "25-30",  "#7fcdf5",
  "PP", "30-35",  "#4cb9f2",
  "PP", "35-40",  "#15a6ef",
  "PP", "40-45",  "#0687c7",
  "PP", "45-50",  "#036596",
  "PP", "50plus", "#014163",
  "PSOE", "0-25",   "#fbb0b5",
  "PSOE", "25-30",  "#f87d87",
  "PSOE", "30-35",  "#f44956",
  "PSOE", "35-40",  "#f11123",
  "PSOE", "40-45",  "#ca0413",
  "PSOE", "45-50",  "#98020b",
  "PSOE", "50plus", "#640106",
  "EH Bildu", "0-25",   "#c0f8ef",
  "EH Bildu", "25-30",  "#76f0db",
  "EH Bildu", "30-35",  "#22e8c7",
  "EH Bildu", "35-40",  "#00b999",
  "EH Bildu", "40-45",  "#009076",
  "EH Bildu", "45-50",  "#006653",
  "EH Bildu", "50plus", "#003a2d",
  "PNV", "0-25",   "#c3e8c3",
  "PNV", "25-30",  "#9dd99d",
  "PNV", "30-35",  "#77ca77",
  "PNV", "35-40",  "#4fbb4f",
  "PNV", "40-45",  "#399a39",
  "PNV", "45-50",  "#297429",
  "PNV", "50plus", "#184c18"
)

# Add Vox bins (the CSSLab table only has gradients for PP/PSOE/EH Bildu/PNV
# because in 2023 only those parties won provinces. Here Vox may win some,
# so we add a bin table for Vox so the gradient works there too.)
vox_bins <- tribble(
  ~party, ~bin,      ~hex,
  "Vox", "0-25",   "#d3edae",
  "Vox", "25-30",  "#b9e286",
  "Vox", "30-35",  "#9ed75e",
  "Vox", "35-40",  "#74cd30",
  "Vox", "40-45",  "#5da821",
  "Vox", "45-50",  "#467d18",
  "Vox", "50plus", "#2f530f"
)
party_colours <- bind_rows(party_colours, vox_bins)

# Compute the winner per province by p_votos and assign the gradient hex
results_final_mapa <- results_enriched |>
  bind_cols(
    results_enriched |> select(starts_with("p_votos_")) |> mutate(id_row = row_number()) |>
      pivot_longer(cols = -id_row, names_to = "partido_temp", values_to = "pct_temp") |>
      group_by(id_row) |> slice_max(pct_temp, n = 1, with_ties = FALSE) |> ungroup() |>
      transmute(ganador_raw = gsub("p_votos_", "", partido_temp), ganador_pct = pct_temp)
  ) |>
  mutate(
    ganador = case_when(
      ganador_raw == "pp" ~ "PP", ganador_raw == "psoe" ~ "PSOE",
      ganador_raw == "vox" ~ "Vox",
      ganador_raw == "sumar" ~ "Sumar", ganador_raw == "jxcat__junts" ~ "Junts",
      ganador_raw == "eh_bildu" ~ "EH Bildu", ganador_raw == "eajpnv" ~ "PNV",
      ganador_raw == "erc" ~ "ERC", ganador_raw == "bng" ~ "BNG",
      ganador_raw == "cca" ~ "CC",
      ganador_raw == "upn" ~ "UPN", TRUE ~ toupper(ganador_raw)
    ),
    ganador_bin = cut(ganador_pct, breaks = c(0, 25, 30, 35, 40, 45, 50, 101),
                      labels = c("0-25", "25-30", "30-35", "35-40", "40-45",
                                 "45-50", "50plus"),
                      include.lowest = TRUE, right = FALSE)
  ) |>
  left_join(party_colours, by = c("ganador" = "party", "ganador_bin" = "bin")) |>
  left_join(party_base,    by = c("ganador" = "party")) |>
  mutate(final_hex = coalesce(hex, base_hex, "#d3d3d3")) |>
  select(-ganador_raw, -hex, -base_hex)

# ---------------------------------------------------------------------------
# 6. Spatial data and seat layout (CSSLab verbatim)
# ---------------------------------------------------------------------------
mapa_provincias_raw <- esp_get_prov(year = "2021", epsg = 3857)

melilla_main <- mapa_provincias_raw |>
  filter(cpro == "52") |> st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |> slice_max(area, n = 1) |> select(-area)

ceuta_main <- mapa_provincias_raw |>
  filter(cpro == "51") |> st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |> slice_max(area, n = 1) |> select(-area)

mapa_provincias <- mapa_provincias_raw |>
  filter(!cpro %in% c("51", "52")) |>
  bind_rows(melilla_main, ceuta_main)

data_mapa <- results_final_mapa |>
  filter(nombre_de_provincia != "España") |>
  mutate(cpro = str_pad(as.character(codigo_de_provincia), width = 2, pad = "0"))

mapa_completo <- mapa_provincias |>
  left_join(data_mapa, by = "cpro")

manual_offsets <- tibble(
  cpro   = c("08", "48", "20"),
  move_x = c(160000, -20000, 50000),
  move_y = c(-120000, 80000, 80000)
)

centroids_base <- mapa_provincias |>
  st_centroid() |>
  select(cpro, ine.prov.name) |>
  mutate(real_x = st_coordinates(geometry)[,1],
         real_y = st_coordinates(geometry)[,2]) |>
  st_drop_geometry()

centroids <- centroids_base |>
  left_join(manual_offsets, by = "cpro") |>
  mutate(
    cx = if_else(!is.na(move_x), real_x + move_x, real_x),
    cy = if_else(!is.na(move_y), real_y + move_y, real_y),
    is_displaced = !is.na(move_x)
  )

seats_data <- results_enriched |>
  filter(nombre_de_provincia != "España") |>
  distinct(codigo_de_provincia, .keep_all = TRUE) |>
  select(codigo_de_provincia, matches("diputados_")) |>
  pivot_longer(cols = starts_with("diputados_"),
               names_to = "party_raw", values_to = "seats") |>
  filter(seats > 0) |>
  mutate(
    party_clean = str_remove(party_raw, "diputados_"),
    party = case_when(
      party_clean == "pp" ~ "PP",
      party_clean == "psoe" ~ "PSOE",
      party_clean == "vox" ~ "Vox",
      party_clean == "sumar" ~ "Sumar",
      party_clean == "erc" ~ "ERC",
      party_clean %in% c("jxcat__junts", "junts") ~ "Junts",
      party_clean %in% c("eh_bildu", "bildu") ~ "EH Bildu",
      party_clean %in% c("eajpnv", "pnv") ~ "PNV",
      party_clean == "bng" ~ "BNG",
      party_clean %in% c("cca", "cc") ~ "CC",
      party_clean == "upn" ~ "UPN",
      TRUE ~ "Otros"
    ),
    cpro = str_pad(as.character(codigo_de_provincia), width = 2, pad = "0")
  ) |>
  left_join(party_base |> select(party, base_hex), by = "party")

province_stats <- seats_data |>
  group_by(cpro) |>
  summarise(total_seats = sum(seats)) |>
  mutate(
    cols_grid = case_when(
      cpro == "08" ~ 10,
      cpro == "28" ~ 10,
      cpro %in% c("48", "20") ~ 4,
      cpro == "46" ~ 6,
      cpro == "41" ~ 5,
      cpro == "03" ~ 5,
      cpro == "29" ~ 5,
      cpro == "11" ~ 4,
      cpro == "15" ~ 4,
      total_seats == 1 ~ 1,
      total_seats == 2 ~ 2,
      total_seats <= 4 ~ 2,
      total_seats <= 6 ~ 3,
      total_seats <= 9 ~ 3,
      TRUE ~ 4
    )
  )

seats_expanded <- seats_data |>
  left_join(province_stats, by = "cpro") |>
  arrange(cpro, desc(seats), party) |>
  uncount(seats) |>
  group_by(cpro) |>
  mutate(seat_id = row_number()) |>
  ungroup()

SPACING <- 22000

lines_data <- centroids |>
  filter(is_displaced) |>
  left_join(province_stats, by = "cpro") |>
  mutate(
    total_rows = ceiling(total_seats / cols_grid),
    approx_text_y = cy + ((total_rows - 1) * SPACING / 2) + 24000,
    end_y = case_when(
      cpro == "08" ~ approx_text_y + 5000,
      move_y > 0 ~ cy - 30000,
      TRUE ~ cy + 40000
    )
  )

seats_coords <- seats_expanded |>
  left_join(centroids, by = "cpro") |>
  mutate(row_idx = floor((seat_id - 1) / cols_grid),
         col_idx_internal = (seat_id - 1) %% cols_grid) |>
  group_by(cpro, row_idx) |>
  mutate(dots_in_this_row = n()) |>
  ungroup() |>
  mutate(
    total_rows = ceiling(total_seats / cols_grid),
    x_pos = cx + (col_idx_internal * SPACING) -
      ((dots_in_this_row - 1) * SPACING / 2),
    y_pos = cy - (row_idx * SPACING) +
      ((total_rows - 1) * SPACING / 2)
  )

label_data <- province_stats |>
  left_join(centroids, by = "cpro") |>
  left_join(mapa_completo |> st_drop_geometry() |> select(cpro, final_hex),
            by = "cpro") |>
  mutate(
    total_rows = ceiling(total_seats / cols_grid),
    y_label = cy + ((total_rows - 1) * SPACING / 2) + 24000,
    provincia_limpia = case_when(
      ine.prov.name == "Rioja, La" ~ "La Rioja",
      ine.prov.name == "Coruña, A" ~ "A Coruña",
      ine.prov.name == "Palmas, Las" ~ "Las Palmas",
      ine.prov.name == "Balears, Illes" ~ "Illes Balears",
      str_detect(ine.prov.name, "Alicante") ~ "Alicante",
      str_detect(ine.prov.name, "Valencia") ~ "Valencia",
      str_detect(ine.prov.name, "Castellón") ~ "Castellón",
      str_detect(ine.prov.name, "Araba") ~ "Álava",
      str_detect(ine.prov.name, "Bizkaia") ~ "Bizkaia",
      str_detect(ine.prov.name, "Gipuzkoa") ~ "Gipuzkoa",
      TRUE ~ ine.prov.name
    )
  )

# ---------------------------------------------------------------------------
# 7. Zoom function for Madrid / Ceuta / Melilla (CSSLab verbatim)
# ---------------------------------------------------------------------------
crear_zoom <- function(codigo_prov, nombre_prov, n_cols_grid, spacing_custom,
                       size_dots, radio_vista = NULL, draw_line_below = FALSE) {

  prov_sf    <- mapa_completo  |> filter(cpro == codigo_prov)
  prov_seats <- seats_expanded |> filter(cpro == codigo_prov)

  bbox <- st_bbox(prov_sf)
  alto_prov <- bbox$ymax - bbox$ymin
  centro <- st_centroid(prov_sf) |> st_coordinates()
  cx_local <- centro[1]; cy_local <- centro[2]

  offset_y_val <- 0
  if (!is.null(radio_vista) && codigo_prov %in% c("51", "52")) {
    offset_y_val <- -radio_vista * 0.15
  }

  # Guard: if a province has zero allocated seats in the projection, skip dots
  if (nrow(prov_seats) == 0) {
    coords_calc <- tibble(x_pos = numeric(0), y_pos = numeric(0),
                          base_hex = character(0))
  } else {
    coords_calc <- prov_seats |>
      mutate(seat_id_local = row_number(),
             row_idx = floor((seat_id_local - 1) / n_cols_grid),
             col_idx = (seat_id_local - 1) %% n_cols_grid) |>
      group_by(row_idx) |> mutate(dots_in_row = n()) |> ungroup() |>
      mutate(total_rows = ceiling(n() / n_cols_grid),
             x_pos = cx_local + (col_idx * spacing_custom) -
               ((dots_in_row - 1) * spacing_custom / 2),
             y_pos = cy_local - (row_idx * spacing_custom) +
               ((total_rows - 1) * spacing_custom / 2) + offset_y_val)
  }

  if (nrow(coords_calc) > 0) {
    min_x_content <- min(bbox$xmin, min(coords_calc$x_pos))
    max_x_content <- max(bbox$xmax, max(coords_calc$x_pos))
  } else {
    min_x_content <- bbox$xmin; max_x_content <- bbox$xmax
  }

  if (!is.null(radio_vista)) {
    x_line_start <- cx_local - (radio_vista * 0.9)
    x_line_end   <- cx_local + (radio_vista * 0.9)
    y_linea      <- cy_local - (radio_vista * 0.8)
    y_texto      <- cy_local + (radio_vista * 0.25)
  } else {
    margen_x      <- spacing_custom * 0.5
    x_line_start <- min_x_content - margen_x
    x_line_end   <- max_x_content + margen_x
    y_linea      <- bbox$ymin - (alto_prov * 0.15)
    y_texto      <- if (nrow(coords_calc) > 0)
      max(coords_calc$y_pos) + (spacing_custom * 0.9) else bbox$ymax + 20000
  }

  p <- ggplot() +
    geom_sf(data = prov_sf, aes(fill = final_hex),
            color = "transparent", size = 0.2)

  if (nrow(coords_calc) > 0) {
    p <- p + geom_point(data = coords_calc,
                        aes(x = x_pos, y = y_pos, fill = base_hex),
                        shape = 21, color = "black",
                        size = size_dots, stroke = 0.3)
  }

  p <- p + geom_text(aes(x = cx_local, y = y_texto, label = nombre_prov),
                     fontface = "bold", size = 3.5, vjust = 0) +
    scale_fill_identity() +
    theme_void() +
    theme(plot.background = element_blank())

  if (is.null(radio_vista)) {
    p <- p + expand_limits(y = c(y_linea - 500, y_texto + 500),
                           x = c(x_line_start, x_line_end)) +
      coord_sf(clip = "off")
  } else {
    p <- p + coord_sf(xlim = c(cx_local - radio_vista, cx_local + radio_vista),
                      ylim = c(cy_local - radio_vista, cy_local + radio_vista),
                      expand = FALSE, clip = "off")
  }

  if (draw_line_below) {
    p <- p + geom_segment(aes(x = x_line_start, xend = x_line_end,
                              y = y_linea, yend = y_linea),
                          color = "black", linewidth = 0.5)
  }
  return(p)
}

plot_madrid  <- crear_zoom("28", "Madrid", n_cols_grid = 10,
                           spacing_custom = 15000, size_dots = 4.8,
                           radio_vista = NULL, draw_line_below = TRUE)
plot_ceuta   <- crear_zoom("51", "Ceuta",  n_cols_grid = 1,
                           spacing_custom = 5000,  size_dots = 4.8,
                           radio_vista = 8000,  draw_line_below = TRUE)
plot_melilla <- crear_zoom("52", "Melilla", n_cols_grid = 1,
                           spacing_custom = 3000,  size_dots = 4.8,
                           radio_vista = 7500,  draw_line_below = FALSE)

# ---------------------------------------------------------------------------
# 8. National polar summary (CSSLab verbatim)
# ---------------------------------------------------------------------------
df_espana <- results_final_mapa |> filter(nombre_de_provincia == "España")

partidos_interes <- c("pp", "psoe", "vox", "sumar", "erc",
                      "jxcat__junts", "eh_bildu", "eajpnv",
                      "bng", "cca", "upn")

df_seats_nac <- df_espana |>
  select(starts_with("diputados_")) |>
  pivot_longer(everything(), names_to = "partido", values_to = "seats") |>
  mutate(partido = str_remove(partido, "^diputados_")) |>
  filter(partido %in% partidos_interes, seats > 0) |>
  arrange(desc(seats)) |>
  mutate(pct_seats = seats / sum(seats),
         pct_shift = pct_seats / 2,
         x = 1.0,
         partido = factor(partido, levels = partido))

orden_partidos <- levels(df_seats_nac$partido)
df_seats_nac <- df_seats_nac |> mutate(pct_seats = pct_seats + pct_shift)

df_votes_nac <- df_espana |>
  select(starts_with("p_votos_")) |>
  pivot_longer(everything(), names_to = "partido", values_to = "pct_votes") |>
  mutate(partido = str_remove(partido, "^p_votos_"),
         pct_votes = pct_votes / 100) |>
  filter(partido %in% orden_partidos) |>
  mutate(pct_votes = pct_votes / sum(pct_votes),
         pct_shift = pct_votes / 2,
         x = 2.5,
         partido = factor(partido, levels = orden_partidos))
df_votes_nac <- df_votes_nac |> mutate(pct_votes = pct_votes + pct_shift)

eps <- 1e-6
if (nrow(df_seats_nac) > 0) df_seats_nac$pct_seats[1] <- df_seats_nac$pct_seats[1] + eps
if (nrow(df_votes_nac) > 0) df_votes_nac$pct_votes[1] <- df_votes_nac$pct_votes[1] + eps

colores_nacional <- c(
  pp = "#15a6ef", psoe = "#f11123", vox = "#74cd30", sumar = "#ef4a92",
  erc = "#ffa503", jxcat__junts = "#00c8b0", eh_bildu = "#00ae8f",
  eajpnv = "#48b049", bng = "#aed0ef", cca = "#ffd800", upn = "#00589c"
)

plot_resumen_nacional <- ggplot() +
  geom_col(data = df_seats_nac, aes(x = x, y = pct_seats, fill = partido),
           width = 2, linewidth = 0, color = NA) +
  geom_col(data = df_votes_nac, aes(x = x, y = pct_votes, fill = partido),
           width = 1, linewidth = 0, color = NA) +
  geom_vline(xintercept = 2.0, color = "white", linewidth = 1.5) +
  scale_fill_manual(values = colores_nacional) +
  scale_x_continuous(limits = c(0, 3)) +
  coord_polar(theta = "y", start = pi / 2, direction = -1) +
  geom_text(aes(x = 1.0, y = 0.02), label = "Seats",
            color = "black", size = 3, fontface = "bold") +
  geom_text(aes(x = 2.9, y = 0.02), label = "Votes",
            color = "black", size = 3, fontface = "bold") +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.position  = "none")

# ---------------------------------------------------------------------------
# 9. Base map (CSSLab verbatim)
# ---------------------------------------------------------------------------
OFFSET_CANARIAS <- -200000
codigos_zoom_data <- c("28", "51", "52")
seats_coords_main <- seats_coords |> filter(!cpro %in% codigos_zoom_data)
label_data_main   <- label_data   |> filter(!cpro %in% codigos_zoom_data)
lines_data_main   <- lines_data   |> filter(!cpro %in% codigos_zoom_data)

mapa_shift <- mapa_completo
idx_can <- which(mapa_shift$cpro %in% c("35", "38"))
st_geometry(mapa_shift)[idx_can] <- st_geometry(mapa_shift)[idx_can] + c(0, OFFSET_CANARIAS)
st_crs(mapa_shift) <- st_crs(mapa_completo)

seats_shift <- seats_coords_main |>
  mutate(y_pos = if_else(cpro %in% c("35", "38"),
                         y_pos + OFFSET_CANARIAS, y_pos))

labels_shift <- label_data_main |>
  mutate(y_label = if_else(cpro %in% c("35", "38"),
                           y_label + OFFSET_CANARIAS, y_label))

sf_canarias_orig <- mapa_completo |> filter(cpro %in% c("35", "38"))
bbox_can_orig <- st_bbox(sf_canarias_orig)

rect_canarias <- tibble(
  xmin = bbox_can_orig$xmin - 40000,
  xmax = bbox_can_orig$xmax + 20000,
  ymin = bbox_can_orig$ymin - 20000 + OFFSET_CANARIAS,
  ymax = bbox_can_orig$ymax + 80000 + OFFSET_CANARIAS
)

bbox_38 <- st_bbox(mapa_completo |> filter(cpro == "38"))
bbox_35 <- st_bbox(mapa_completo |> filter(cpro == "35"))
mid_x_can <- (bbox_38$xmax + bbox_35$xmin) / 2
cy_can <- (bbox_can_orig$ymin + bbox_can_orig$ymax) / 2 + OFFSET_CANARIAS

dx <- 18000; dy <- 49000; tx <- 9000; ty <- 23000
linea_diagonal <- tibble(x    = mid_x_can - dx + tx,
                         xend = mid_x_can + dx + tx,
                         y    = cy_can - dy - ty,
                         yend = cy_can + dy - ty)

centroids_zoom <- mapa_provincias |>
  filter(cpro == "28") |> st_centroid() |>
  mutate(cx = st_coordinates(geometry)[,1],
         cy = st_coordinates(geometry)[,2])

mapa_base <- ggplot() +
  geom_sf(data = mapa_shift |> filter(!cpro %in% c("07", "35", "38")),
          aes(fill = final_hex), color = "white", size = 0.2) +
  geom_sf(data = mapa_shift |> filter(cpro %in% c("07", "35", "38")),
          aes(fill = final_hex), color = "transparent", size = 0.2) +
  geom_segment(data = rect_canarias,
               aes(x = xmin, xend = xmax, y = ymax, yend = ymax),
               color = "black", linewidth = 0.4) +
  geom_segment(data = rect_canarias,
               aes(x = xmax, xend = xmax, y = ymin, yend = ymax),
               color = "black", linewidth = 0.4) +
  geom_segment(data = linea_diagonal,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", linewidth = 0.4) +
  geom_segment(data = lines_data_main,
               aes(x = real_x, y = real_y, xend = cx, yend = end_y),
               color = "black", linewidth = 0.5) +
  geom_text(data = centroids_zoom,
            aes(x = cx, y = cy), label = "*", color = "white",
            size = 15, fontface = "bold") +
  geom_point(data = seats_shift,
             aes(x = x_pos, y = y_pos, fill = base_hex),
             shape = 21, color = "black",
             size = 4.8, stroke = 0.3) +
  geom_text(data = labels_shift,
            aes(x = cx, y = y_label, label = provincia_limpia,
                color = if_else(is_displaced, "black",
                                if_else(final_hex == HEX_PP_OSCURO,
                                        "white", "black",
                                        missing = "black"))),
            size = 3.4, fontface = "bold") +
  scale_fill_identity() + scale_color_identity() +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA))

# ---------------------------------------------------------------------------
# 10. Custom legends (CSSLab verbatim, with the same 11 leaders as 2023)
# ---------------------------------------------------------------------------
national_stats <- results_enriched |>
  filter(nombre_de_provincia == "España") |>
  mutate(total_validos = votos_validos) |>
  select(total_validos, matches("^(votos|diputados)_")) |>
  pivot_longer(cols = matches("^(votos|diputados)_"),
               names_to = c(".value", "party_raw"),
               names_pattern = "(votos|diputados)_(.*)") |>
  mutate(
    pct_nacional = votos / total_validos * 100,
    party = case_when(
      party_raw == "pp" ~ "PP", party_raw == "psoe" ~ "PSOE",
      party_raw == "vox" ~ "Vox", party_raw == "sumar" ~ "Sumar",
      party_raw == "erc" ~ "ERC", party_raw %in% c("jxcat__junts", "junts") ~ "Junts",
      party_raw %in% c("eh_bildu", "bildu") ~ "EH Bildu",
      party_raw %in% c("eajpnv", "pnv") ~ "PNV", party_raw == "bng" ~ "BNG",
      party_raw %in% c("cca", "cc") ~ "CC", party_raw == "upn" ~ "UPN",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(party))

layout_manual <- tribble(
  ~party,      ~candidate,              ~col, ~row,
  "PP",        "Alberto Núñez Feijóo",  1,    1,
  "PSOE",      "Pedro Sánchez",         1,    2,
  "Vox",       "Santiago Abascal",      1,    3,
  "Sumar",     "Yolanda Díaz",          1,    4,
  "ERC",       "Gabriel Rufián",        1,    5,
  "Junts",     "Miriam Nogueras",       2,    1,
  "EH Bildu",  "Mertxe Aizpurua",       2,    2,
  "PNV",       "Aitor Esteban",         2,    3,
  "BNG",       "Néstor Rego",           2,    4,
  "CC",        "Cristina Valido",       2,    5,
  "UPN",       "Alberto Catalán",       2,    6
)

# Determine winners FROM THE PROJECTION (not hard-coded from 2023)
projected_winners <- results_final_mapa |>
  filter(nombre_de_provincia != "España") |>
  distinct(ganador) |>
  pull(ganador)

legend_data <- layout_manual |>
  left_join(national_stats, by = "party") |>
  left_join(party_base, by = "party") |>
  mutate(
    label_pct   = sprintf("%.2f", pct_nacional),
    label_seats = as.character(diputados),
    base_hex    = replace_na(base_hex, "#999999"),
    is_winner   = party %in% projected_winners
  )

data_izq  <- legend_data |> filter(col == 1) |> arrange(row)
data_dcha <- legend_data |> filter(col == 2) |> arrange(row)

crear_bloque_leyenda <- function(datos, mostrar_headers = FALSE) {
  prep <- datos |> mutate(y_base = -(row - 1) * 1.5, x_base = 0)

  tiles <- prep |>
    filter(is_winner) |>
    pmap_dfr(function(base_hex, x_base, y_base, ...) {
      colors <- colorRampPalette(c(lighten(base_hex, 0.6),
                                   base_hex,
                                   darken(base_hex, 0.2)))(7)
      tibble(x_base = x_base, tile_id = 1:7, fill = colors,
             ymin = y_base + 0.8, ymax = y_base + 1.1)
    })

  min_y <- min(prep$y_base)

  p <- ggplot() +
    geom_rect(data = tiles,
              aes(xmin = x_base + tile_id - 0.48,
                  xmax = x_base + tile_id + 0.48,
                  ymin = ymin, ymax = ymax, fill = fill),
              color = NA) +
    geom_rect(data = prep,
              aes(xmin = 0.5, xmax = 7.5,
                  ymin = y_base - 0.1,
                  ymax = y_base + 0.75 + 0.1,
                  fill = base_hex),
              color = NA) +
    geom_text(data = prep,
              aes(x = 0.7, y = y_base + 0.375, label = party),
              color = "white", fontface = "bold", hjust = 0, size = 4.5) +
    geom_text(data = prep,
              aes(x = 7.3, y = y_base + 0.375, label = candidate),
              color = "white", fontface = "bold.italic", hjust = 1, size = 3.5) +
    geom_text(data = prep,
              aes(x = 8.5, y = y_base + 0.375, label = label_pct, color = base_hex),
              fontface = "bold", hjust = 0.5, size = 4.5) +
    geom_text(data = prep,
              aes(x = 10.5, y = y_base + 0.375, label = label_seats, color = base_hex),
              fontface = "bold", hjust = 0.5, size = 4.5) +
    scale_fill_identity() + scale_color_identity() +
    theme_void() +
    theme(panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA)) +
    coord_fixed(ratio = 1) +
    xlim(0, 11.5) +
    ylim(min_y - 0.5, 2)

  if (mostrar_headers) {
    headers <- tibble(
      label = c("-25%", "25%", "30%", "35%", "40%", "45%", "50%+", "Vote %", "Seats"),
      x = c(1, 2, 3, 4, 5, 6, 7, 8.5, 10.5),
      y = 1.6
    )
    p <- p + geom_text(data = headers, aes(x = x, y = y, label = label),
                       color = "black", fontface = "bold",
                       size = 3, hjust = 0.5)
  }
  return(p)
}

plot_leyenda_izq  <- crear_bloque_leyenda(data_izq,  mostrar_headers = TRUE)
plot_leyenda_dcha <- crear_bloque_leyenda(data_dcha, mostrar_headers = FALSE)

# ---------------------------------------------------------------------------
# 11. Final composition (CSSLab verbatim)
# ---------------------------------------------------------------------------
mapa_final <- ggdraw() +
  draw_plot(mapa_base,             x = 0,    y = 0,     width = 1,    height = 1) +
  draw_plot(plot_madrid,           x = 0.02, y = 0.60,  width = 0.24, height = 0.23) +
  draw_plot(plot_ceuta,            x = 0.02, y = 0.42,  width = 0.22, height = 0.19) +
  draw_plot(plot_melilla,          x = 0.02, y = 0.24,  width = 0.22, height = 0.19) +
  draw_plot(plot_resumen_nacional, x = 0.06, y = 0.83,  width = 0.18, height = 0.15) +
  draw_plot(plot_leyenda_izq,      x = 0.5,  y = 0.085, width = 0.25, height = 0.30) +
  draw_plot(plot_leyenda_dcha,     x = 0.73, y = 0.052, width = 0.25, height = 0.40)

# Save
out_path <- "docs/images/maps/map_last.png"
ggsave(out_path, mapa_final, width = 16, height = 13, dpi = 200, bg = "white")
cat("Map saved to:", out_path, "\n")
