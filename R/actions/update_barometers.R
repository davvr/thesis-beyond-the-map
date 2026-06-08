# R/actions/update_barometers.R
# Called by GitHub Actions: checks for a new barometer, downloads it,
# drops the oldest, and regenerates the map. Exits silently if no new data.

# Be polite with the CIS server
Sys.sleep(10)
source("R/scraping/list_barometers.R")
source("R/scraping/get_zip_url.R")
source("R/scraping/download_zip.R")

# 1. What study_ids do we currently have in the repo?
local_zips <- fs::dir_ls("data-raw/cis", glob = "*.zip")
local_ids  <- stringr::str_extract(fs::path_file(local_zips), "\\d+")

# 2. What does the CIS catalogue have right now?
remote     <- list_barometers(n = 6)
remote_ids <- remote$study_id

# 3. Is there anything new?
new_ids <- setdiff(remote_ids, local_ids)

if (length(new_ids) == 0) {
  message("No new barometers. Nothing to do.")
  quit(save = "no", status = 0)
}

message("New barometer(s) found: ", paste(new_ids, collapse = ", "))

# 4. Download new ZIPs
for (id in new_ids) {
  url <- get_zip_url(remote$url[remote$study_id == id])
  download_zip(url, id)
}

# 5. FIFO: drop oldest ZIP(s) to keep exactly 6
local_zips_updated <- fs::dir_ls("data-raw/cis", glob = "*.zip")
local_ids_updated  <- stringr::str_extract(fs::path_file(local_zips_updated), "\\d+")

if (length(local_zips_updated) > 6) {
  # Join with remote dates to sort; fall back to study_id order if not in remote
  all_known <- list_barometers(n = 20)
  id_dates  <- tibble::tibble(study_id = local_ids_updated) |>
    dplyr::left_join(dplyr::select(all_known, study_id, date), by = "study_id") |>
    dplyr::arrange(date)

  n_to_drop <- length(local_zips_updated) - 6
  ids_to_drop <- head(id_dates$study_id, n_to_drop)

  for (id in ids_to_drop) {
    path <- fs::path("data-raw/cis", paste0("MD", id, ".zip"))
    message("Dropping oldest barometer: ", path)
    fs::file_delete(path)
  }
}

# 6. Generate docs/barometers.json with metadata of the current 6 barometers

month_label <- function(date) {
  format(date, "%b")  # "Jan", "Feb", etc.
}

year_label <- function(date) {
  format(date, "%Y")
}

# Read current ZIPs and match with catalogue dates
current_zips <- fs::dir_ls("data-raw/cis", glob = "*.zip")
current_ids  <- stringr::str_extract(fs::path_file(current_zips), "\\d+")

catalogue    <- list_barometers(n = 20)

barometers_meta <- tibble::tibble(study_id = current_ids) |>
  dplyr::left_join(
    dplyr::select(catalogue, study_id, date),
    by = "study_id"
  ) |>
  dplyr::arrange(dplyr::desc(date)) |>
  dplyr::mutate(
    month = month_label(date),
    year  = year_label(date)
  )

json_out <- list(
  latest = list(
    id    = barometers_meta$study_id[1],
    month = barometers_meta$month[1],
    year  = barometers_meta$year[1]
  ),
  all = purrr::map(seq_len(nrow(barometers_meta)), function(i) {
    list(
      id    = barometers_meta$study_id[i],
      month = barometers_meta$month[i],
      year  = barometers_meta$year[i]
    )
  })
)

fs::dir_create("docs")
jsonlite::write_json(json_out, "docs/barometers.json", auto_unbox = TRUE, pretty = TRUE)
message("barometers.json updated.")

# 7. Regenerate the maps
message("Regenerating map...")
source("pipeline/map/map_agg.R")
source("pipeline/map/map_last.R")
source("pipeline/map/map_last_swing.R")

# 8. Regenerate problems and ideology
source("pipeline/problems/problems.R")
source("pipeline/ideology/ideology_evolution.R")

message("Done.")
