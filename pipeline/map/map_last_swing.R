# pipeline/map/map_last_swing.R
# Latest CIS barometer projection adjusted with PROPORTIONAL National Swing.
#
# Method: reads the most recent CIS barometer, computes each party's national
# vote share from the (weighted) direct vote intention, and projects it onto
# each province by scaling the province's 2023 result by the national ratio
# (current share / 2023 share). Unlike additive swing, proportional swing
# preserves each party's territorial structure and does not inflate large
# parties in their strongholds, which is more appropriate for Spain's
# territorially heterogeneous system. Fully automated: the source is the
# latest barometer's microdata, so the map updates every month.

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

party_lookup <- tribble(
  ~code, ~partido,
  1,     "psoe", 2,     "pp", 3,     "vox", 21,    "sumar",
  503,   "cca", 901,   "erc", 902,   "jxcat__junts",
  1201,  "bng", 1501,  "upn", 1601,  "eajpnv", 1602,  "eh_bildu"
)
all_parties <- party_lookup$partido

# ---------------------------------------------------------------------------
# 2023 provincial and national baselines
# ---------------------------------------------------------------------------
prov_2023 <- results_2023 |>
  filter(nombre_de_provincia != "España", !is.na(codigo_de_provincia)) |>
  select(codigo_de_provincia, votos_validos, any_of(paste0("votos_", all_parties))) |>
  pivot_longer(any_of(paste0("votos_", all_parties)), names_to = "partido",
               values_to = "votos", names_prefix = "votos_") |>
  mutate(p_votos_2023 = 100 * votos / votos_validos) |>
  select(codigo_de_provincia, partido, p_votos_2023) |>
  replace_na(list(p_votos_2023 = 0))

nat_2023 <- results_2023 |>
  filter(nombre_de_provincia == "España") |>
  select(votos_validos, any_of(paste0("votos_", all_parties))) |>
  pivot_longer(any_of(paste0("votos_", all_parties)), names_to = "partido",
               values_to = "votos", names_prefix = "votos_") |>
  mutate(p_nat_2023 = 100 * votos / votos_validos) |>
  select(partido, p_nat_2023) |>
  replace_na(list(p_nat_2023 = 0))

# ---------------------------------------------------------------------------
# National vote share from the LATEST barometer (weighted direct intention)
# ---------------------------------------------------------------------------
cis_dir <- "data-raw/cis"
all_zip_files <- fs::dir_ls(cis_dir, glob = "*.zip")
zip_ids <- stringr::str_extract(fs::path_file(all_zip_files), "\\d+") |> as.integer()
latest_zip <- all_zip_files[which.max(zip_ids)]
message("Using most recent barometer: ", fs::path_file(latest_zip))

temp_dir <- tempfile(); dir.create(temp_dir)
unzip(latest_zip, exdir = temp_dir)
sav_files <- fs::dir_ls(temp_dir, regexp = "\\.sav$")
raw <- haven::read_sav(sav_files[1])
unlink(temp_dir, recursive = TRUE)

cis <- raw |> transmute(
  intention = haven::zap_labels(INTENCIONGR) |> as.integer(),
  weight    = as.numeric(PESO)
)

drop_codes <- c(8995, 8996, 9977, 9997, 9998, 9999)

nat_cis <- cis |>
  filter(!is.na(intention), !intention %in% drop_codes) |>
  inner_join(party_lookup, by = c("intention" = "code")) |>
  group_by(partido) |>
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") |>
  mutate(p_cis_nat = 100 * weighted_count / sum(weighted_count)) |>
  select(partido, p_cis_nat)

# ---------------------------------------------------------------------------
# Uniform National Swing: national swing applied over provincial 2023 baseline
# ---------------------------------------------------------------------------
# Proportional swing: scale each party by the national ratio current/2023.
# Parties absent in 2023 (p_nat_2023 == 0) have an undefined ratio; we leave
# their projected share at 0, since there is no 2023 territorial structure to
# scale (a documented limitation of proportional swing for new entrants).
swing <- nat_cis |>
  full_join(nat_2023, by = "partido") |>
  replace_na(list(p_cis_nat = 0, p_nat_2023 = 0)) |>
  mutate(ratio = if_else(p_nat_2023 > 0, p_cis_nat / p_nat_2023, 0))

prov_share_full <- prov_2023 |>
  left_join(swing, by = "partido") |>
  mutate(p_votos = pmax(0, p_votos_2023 * ratio)) |>
  select(codigo_de_provincia, partido, p_votos)

prov_share_with_mag <- prov_share_full |> inner_join(prov_mag, by = "codigo_de_provincia")

allocate_one <- function(df_prov) {
  m <- unique(df_prov$magnitude)
  votes <- setNames(df_prov$p_votos, df_prov$partido)
  seats <- dhondt(votes, magnitude = m, threshold = 0.03)
  tibble(partido = names(seats), diputados = as.integer(seats))
}

prov_alloc <- prov_share_with_mag |> group_by(codigo_de_provincia) |>
  group_modify(~ allocate_one(.x)) |> ungroup()

prov_meta <- results_2023 |> filter(nombre_de_provincia != "España", !is.na(codigo_de_provincia)) |>
  select(all_of(cols_metadata)) |> mutate(votos_validos = 100)

prov_long <- prov_share_full |> left_join(prov_alloc, by = c("codigo_de_provincia", "partido")) |>
  mutate(votos = p_votos, diputados = replace_na(diputados, 0L))

prov_wide <- prov_long |> pivot_wider(names_from = partido,
                                      values_from = c(votos, p_votos, diputados), names_glue = "{.value}_{partido}")

orden_final <- cols_metadata
for (p in all_parties) {
  orden_final <- c(orden_final, paste0("votos_", p), paste0("p_votos_", p), paste0("diputados_", p))
}

prov_enriched <- prov_meta |> left_join(prov_wide, by = "codigo_de_provincia") |> select(any_of(orden_final))

nat_seats <- prov_alloc |> group_by(partido) |> summarise(diputados_nacional = sum(diputados), .groups = "drop")

espana_row <- tibble(
  nombre_de_comunidad = NA_character_, codigo_de_provincia = NA_real_, nombre_de_provincia = "España",
  poblacion = NA_real_, numero_de_mesas = NA_real_, censo_electoral_sin_cera = NA_real_, censo_cera = NA_real_,
  total_censo_electoral = NA_real_, total_votantes_cer = NA_real_, total_votantes_cera = NA_real_,
  total_votantes = NA_real_, votos_validos = 100, votos_a_candidaturas = NA_real_, votos_en_blanco = NA_real_, votos_nulos = NA_real_
)

for (p in all_parties) {
  pct  <- nat_cis$p_cis_nat[match(p, nat_cis$partido)]
  if (is.na(pct)) pct <- 0
  seats <- nat_seats$diputados_nacional[match(p, nat_seats$partido)]
  if (is.na(seats)) seats <- 0L
  espana_row[[paste0("votos_", p)]] <- pct
  espana_row[[paste0("p_votos_", p)]] <- pct
  espana_row[[paste0("diputados_", p)]] <- as.integer(seats)
}

results_enriched <- bind_rows(prov_enriched, espana_row) |> select(any_of(orden_final))

render_cis_map(results_enriched, out_path = "docs/images/maps/map_last_swing.png")