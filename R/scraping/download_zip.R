# R/scraping/download_zip.R
# Download a CIS ZIP file and save it to data-raw/cis/.
# Validates that the response is actually a ZIP (not an HTML error page).

#' Download a CIS microdata ZIP
#'
#' @param zip_url Character. Direct URL to the ZIP file.
#' @param study_id Character. CIS study code (e.g. "3546").
#' @param dest_dir Character. Directory where to save the ZIP. Default "data-raw/cis".
#'
#' @return Character. Path to the downloaded file.
#'
#' @examples
#' \dontrun{
#' download_zip(
#'   "https://www.cis.es/documents/20117/13805127/MD3546.zip",
#'   "3546"
#' )
#' }
download_zip <- function(zip_url, study_id, dest_dir = "data-raw/cis") {
  stopifnot(
    is.character(zip_url), length(zip_url) == 1, nzchar(zip_url),
    is.character(study_id), length(study_id) == 1, nzchar(study_id),
    is.character(dest_dir), length(dest_dir) == 1
  )
  
  fs::dir_create(dest_dir)
  dest_path <- fs::path(dest_dir, paste0("MD", study_id, ".zip"))
  
  message("Downloading ", study_id, " -> ", dest_path)
  
  resp <- httr2::request(zip_url) |>
    httr2::req_user_agent(CIS_USER_AGENT) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2 ^ .x) |>
    httr2::req_perform()
  
  raw_bytes <- httr2::resp_body_raw(resp)
  
  # ZIP files start with magic bytes 0x504B0304 or 0x504B0506.
  # If the server returned an HTML error page, this check will catch it.
  if (length(raw_bytes) < 4 || !is_zip_magic(raw_bytes)) {
    stop(
      "Downloaded content from ", zip_url, " is not a ZIP file. ",
      "First 20 bytes: ", paste(as.character(head(raw_bytes, 20)), collapse = " ")
    )
  }
  
  writeBin(raw_bytes, dest_path)
  message("Saved ", fs::file_size(dest_path), " to ", dest_path)
  
  invisible(dest_path)
}

# -- internals ---------------------------------------------------------------

is_zip_magic <- function(raw_bytes) {
  # ZIP files start with PK\x03\x04 (0x504B0304) or PK\x05\x06 (empty archive).
  identical(raw_bytes[1:2], as.raw(c(0x50, 0x4B)))
}