# R/read_catalogue.R
# Shared helper: read barometer catalogue from docs/barometers.json
# Returns a tibble with study_id, month_label, date

read_catalogue <- function(json_path = "docs/barometers.json") {
  raw <- jsonlite::read_json(json_path)$all
  purrr::map_dfr(raw, ~ tibble::tibble(
    study_id    = .x$id,
    month_label = paste(.x$month, .x$year),
    date        = as.Date(paste("01", .x$month, .x$year), format = "%d %b %Y")
  ))
}