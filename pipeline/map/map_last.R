# pipeline/map/map_last.R
# LAST BAROMETER MAP — reads only the most recent CIS barometer

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(zoo)
  library(haven)
  library(fs)
})

source("R/dhondt.R")
source("R/render_map.R")

file_path <- "data-raw/infoelectoral/PROV_02_202307_1.xlsx"

clean_text <- function(x) {
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", "_")
  x <- str_replace_all(x, "[^a-z0-9_]", "")
  return(x)
}

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

diputados_cols <- str_subset(names(results_2023), "^diputados_")
prov_mag <- results_2023 |>
  filter(nombre_de_provincia != "España", !is.na(codigo_de_provincia)) |>
  mutate(magnitude = rowSums(across(all_of(diputados_cols)), na.rm = TRUE)) |>
  select(codigo_de_provincia, magnitude)
stopifnot(sum(prov_mag$magnitude) == 350L)

cis_dir <- "data-raw/cis"
all_zip_files <- fs::dir_ls(cis_dir, glob = "*.zip")

zip_ids <- stringr::str_extract(fs::path_file(all_zip_files), "\\d+") |> as.integer()
most_recent_idx <- which.max(zip_ids)
zip_files <- all_zip_files[most_recent_idx]

message("Found ", length(all_zip_files), " ZIP files total")
message("Using most recent: ", fs::path_file(zip_files))

cis_list <- purrr::map(zip_files, function(zip_path) {
  message("Reading ", fs::path_file(zip_path))
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(zip_path, exdir = temp_dir)
  sav_files <- fs::dir_ls(temp_dir, regexp = "\\.sav$")
  
  if (length(sav_files) == 0) {
    warning("No .sav in ", zip_path)
    unlink(temp_dir, recursive = TRUE)
    return(NULL)
  }
  raw <- haven::read_sav(sav_files[1])
  unlink(temp_dir, recursive = TRUE)
  
  raw |> transmute(
    province_code = haven::zap_labels(PROV) |> as.integer(),
    intention     = haven::zap_labels(INTENCIONGR) |> as.integer(),
    weight        = as.numeric(PESO)
  )
})

cis <- bind_rows(purrr::compact(cis_list))
message("Total weighted observations: ", nrow(cis))

drop_codes <- c(8995, 8996, 9977, 9997, 9998, 9999)
cis_clean <- cis |> filter(!is.na(intention), !intention %in% drop_codes)

party_lookup <- tribble(
  ~code, ~partido,
  1,     "psoe", 2,     "pp", 3,     "vox", 21,    "sumar",
  503,   "cca", 901,   "erc", 902,   "jxcat__junts",
  1201,  "bng", 1501,  "upn", 1601,  "eajpnv", 1602,  "eh_bildu"
)
all_parties <- party_lookup$partido

cis_mapped <- cis_clean |> inner_join(party_lookup, by = c("intention" = "code"))

prov_share <- cis_mapped |>
  group_by(province_code, partido) |>
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") |>
  group_by(province_code) |>
  mutate(p_votos = 100 * weighted_count / sum(weighted_count)) |>
  ungroup() |>
  select(codigo_de_provincia = province_code, partido, p_votos)

prov_share_full <- expand_grid(
  codigo_de_provincia = unique(results_2023$codigo_de_provincia),
  partido = all_parties
) |>
  filter(!is.na(codigo_de_provincia)) |>
  left_join(prov_share, by = c("codigo_de_provincia", "partido")) |>
  mutate(p_votos = replace_na(p_votos, 0))

prov_share_with_mag <- prov_share_full |> inner_join(prov_mag, by = "codigo_de_provincia")

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

prov_meta <- results_2023 |> filter(nombre_de_provincia != "España", !is.na(codigo_de_provincia)) |>
  select(all_of(cols_metadata)) |> mutate(votos_validos = 100)

prov_long <- prov_share_full |> left_join(prov_alloc, by = c("codigo_de_provincia", "partido")) |>
  mutate(votos = p_votos, diputados = replace_na(diputados, 0L))

prov_wide <- prov_long |> pivot_wider(names_from = partido, values_from = c(votos, p_votos, diputados), names_glue = "{.value}_{partido}")

orden_final <- cols_metadata
for (p in all_parties) {
  orden_final <- c(orden_final, paste0("votos_", p), paste0("p_votos_", p), paste0("diputados_", p))
}

prov_enriched <- prov_meta |> left_join(prov_wide, by = "codigo_de_provincia") |> select(any_of(orden_final))

nat_seats <- prov_alloc |> group_by(partido) |> summarise(diputados_nacional = sum(diputados), .groups = "drop")
nat_share <- cis_mapped |> group_by(partido) |> summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") |> mutate(p_votos = 100 * weighted_count / sum(weighted_count))

espana_row <- tibble(
  nombre_de_comunidad = NA_character_, codigo_de_provincia = NA_real_, nombre_de_provincia = "España",
  poblacion = NA_real_, numero_de_mesas = NA_real_, censo_electoral_sin_cera = NA_real_, censo_cera = NA_real_,
  total_censo_electoral = NA_real_, total_votantes_cer = NA_real_, total_votantes_cera = NA_real_,
  total_votantes = NA_real_, votos_validos = 100, votos_a_candidaturas = NA_real_, votos_en_blanco = NA_real_, votos_nulos = NA_real_
)

for (p in all_parties) {
  pct  <- nat_share$p_votos[match(p, nat_share$partido)]
  if (is.na(pct)) pct <- 0
  seats <- nat_seats$diputados_nacional[match(p, nat_seats$partido)]
  if (is.na(seats)) seats <- 0L
  espana_row[[paste0("votos_", p)]] <- pct
  espana_row[[paste0("p_votos_", p)]] <- pct
  espana_row[[paste0("diputados_", p)]] <- as.integer(seats)
}

results_enriched <- bind_rows(prov_enriched, espana_row) |> select(any_of(orden_final))

# Delegate rendering
render_cis_map(results_enriched, out_path = "docs/images/maps/map_last.png")