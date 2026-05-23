# R/scraping/get_zip_url.R
# Given the URL of a CIS study page, return the direct URL of the
# "Fichero datos" ZIP (microdata bundle).
#
# Why this is safe and stable:
#   The CIS study pages embed a JSON-LD block (schema.org Dataset) with a
#   `distribution` array. One of its entries has `name == "Fichero datos"` and
#   a `contentUrl` pointing directly to the ZIP on the CIS document store.
#   The ZIP is publicly downloadable under CC BY 4.0 with no email form.
#
# Why we extract from JSON-LD instead of building a URL template:
#   The internal Liferay folder id (the digits between /documents/20117/ and
#   the filename) varies across studies. Examples:
#     - 3546  ->  /documents/20117/13805127/MD3546.zip
#     - 3544  ->  /documents/20117/13760224/MD3544.zip
#   so a template like /documents/20117/<X>/MD<study>.zip is wrong. We must
#   read the URL from each study page.

#' Get the direct ZIP URL of a CIS study
#'
#' @param study_url Character. Absolute URL of the study page,
#'   e.g. "https://www.cis.es/es/estudios/barometro-de-marzo-2026".
#'
#' @return Character of length 1: the absolute URL of the ZIP bundle.
#'
#' @examples
#' \dontrun{
#' get_zip_url("https://www.cis.es/es/estudios/barometro-de-marzo-2026")
#' # -> "https://www.cis.es/documents/20117/13805127/MD3546.zip"
#' }
get_zip_url <- function(study_url) {
  stopifnot(is.character(study_url), length(study_url) == 1, nzchar(study_url))
  
  resp <- httr2::request(study_url) |>
    httr2::req_user_agent(CIS_USER_AGENT) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2 ^ .x) |>
    httr2::req_perform()
  
  html <- httr2::resp_body_html(resp)
  
  extract_zip_url_from_html(html, study_url)
}

# -- internals ---------------------------------------------------------------

# Pulled out so we can unit-test it against a saved HTML fixture without HTTP.
extract_zip_url_from_html <- function(html, study_url) {
  # 1. Collect every <script type="application/ld+json"> block on the page.
  blocks <- rvest::html_elements(html, 'script[type="application/ld+json"]')
  if (length(blocks) == 0) {
    stop("No JSON-LD blocks found at ", study_url)
  }
  
  jsons <- purrr::map(rvest::html_text(blocks), safe_parse_json)
  jsons <- purrr::compact(jsons)
  
  # 2. Find the Dataset node (one of the blocks wraps it in a @graph array).
  dataset <- find_dataset_node(jsons)
  if (is.null(dataset)) {
    stop("No schema.org Dataset node found in the JSON-LD of ", study_url)
  }
  
  # 3. From the distribution array, pick the entry whose name is "Fichero datos".
  distributions <- dataset$distribution
  if (is.null(distributions)) {
    stop("Dataset node has no `distribution` field at ", study_url)
  }
  
  # `distributions` may be a list of lists (purrr-style) or a data.frame
  # (jsonlite simplification). Normalise to a list of lists.
  if (is.data.frame(distributions)) {
    distributions <- purrr::transpose(distributions) |>
      purrr::map(as.list)
  }
  
  names_vec  <- purrr::map_chr(distributions, ~ .x$name %||% NA_character_)
  match_idx  <- which(names_vec == "Fichero datos")
  
  if (length(match_idx) == 0) {
    stop(
      "No distribution entry named 'Fichero datos' at ", study_url,
      ". Available names: ", paste(names_vec, collapse = ", ")
    )
  }
  if (length(match_idx) > 1) {
    warning(
      "Multiple 'Fichero datos' entries at ", study_url,
      "; using the first one."
    )
    match_idx <- match_idx[1]
  }
  
  zip_url <- distributions[[match_idx]]$contentUrl
  if (is.null(zip_url) || !nzchar(zip_url)) {
    stop("'Fichero datos' entry has empty contentUrl at ", study_url)
  }
  
  zip_url
}

safe_parse_json <- function(txt) {
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Find the @type == "Dataset" node, walking @graph if present.
find_dataset_node <- function(jsons) {
  for (j in jsons) {
    # Case A: top-level is a Dataset.
    if (identical(j$`@type`, "Dataset")) {
      return(j)
    }
    # Case B: wrapped in @graph as observed on every CIS study page.
    if (!is.null(j$`@graph`)) {
      for (node in j$`@graph`) {
        if (identical(node$`@type`, "Dataset")) {
          return(node)
        }
      }
    }
  }
  NULL
}

# Null-coalescing operator (rlang style) so we don't need to import rlang.
`%||%` <- function(a, b) if (is.null(a)) b else a