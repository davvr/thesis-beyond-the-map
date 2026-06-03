# R/scraping/list_barometers.R
# List the N most recent monthly CIS barometers from the public catalogue.
#
# The catalogue page is rendered server-side, so an HTTP GET + HTML parsing
# suffices (no headless browser needed).
#
# Each barometer is rendered as an <article class="jdt-card ..."> element.
# The catalogue mixes three kinds of entries that we have to disambiguate:
#   (a) Regular monthly barometers  -> title matches  "Barómetro de <month> <year>"
#   (b) Aggregated "fusion" files    -> title starts  "Fusión de barómetros..."
#   (c) Post-electoral barometers   -> title contains "Postelectoral"
# This function returns only kind (a).
#
# Filter parameters of the catalogue URL:
#   catalogo=estudio      -> only studies (not questions/series)
#   sort=createDateBDE-   -> most recent first (BDE = base de datos de estudios)
#   c1=4675982            -> taxonomy id for "Barómetros de opinión"

CIS_CATALOGUE_URL <- paste0(
  "https://www.cis.es/es/estudios/catalogo?start=1&q=bar%C3%B3metro&fromDate=&from=%5B+now%5D&toDate=&to=%5Bnow-100y+%5D&sort=&catalogo=estudio"
)

CIS_USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36 (David Valero Regalón; dvalreg@gmail.com / 100561036@alumnos.uc3m.es)"

#' List the N most recent monthly CIS barometers
#'
#' @param n Integer. Number of barometers to return. Default 6.
#' @param catalogue_url Character. Catalogue URL with filters. Override only
#'   for testing.
#'
#' @return A tibble with one row per barometer and columns:
#'   - `study_id`  : character, CIS study code (e.g. "3546")
#'   - `title`     : character, e.g. "Barómetro de marzo 2026"
#'   - `date`      : Date, publication date
#'   - `url`       : character, absolute URL of the study page
#'
#' @examples
#' \dontrun{
#' list_barometers(n = 6)
#' }
list_barometers <- function(n = 6, catalogue_url = CIS_CATALOGUE_URL) {
  stopifnot(is.numeric(n), n >= 1, n == as.integer(n))
  
  # 1. Fetch the catalogue page.
  resp <- httr2::request(catalogue_url) |>
    httr2::req_user_agent(CIS_USER_AGENT) |>
    httr2::req_retry(max_tries = 5, backoff = ~ 30 * .x) |>
    httr2::req_perform()
  
  html <- httr2::resp_body_html(resp)
  
  # 2. Extract every <article class="jdt-card ..."> in the listing.
  cards <- rvest::html_elements(html, "article.jdt-card")
  if (length(cards) == 0) {
    stop(
      "No <article class='jdt-card'> elements found at ", catalogue_url, ". ",
      "The CIS site may have changed its markup; inspect manually."
    )
  }
  
  parsed <- purrr::map_dfr(cards, parse_card)
  
  # 3. Keep only regular monthly barometers.
  monthly_pattern <- "^Bar.{1,2}metro de [a-z]+ \\d{4}$"
  monthly <- dplyr::filter(
    parsed,
    stringr::str_detect(title, stringr::regex(monthly_pattern, ignore_case = TRUE))
  )
  
  if (nrow(monthly) < n) {
    warning(
      "Requested n = ", n, " but only ", nrow(monthly),
      " monthly barometers were parsed from the current page. ",
      "The catalogue is paginated; this function reads only the first page."
    )
  }
  
  # 4. Sort by date desc (the page is already sorted, but we don't trust HTML order).
  monthly <- dplyr::arrange(monthly, dplyr::desc(date))
  
  head(monthly, n)
}

# -- internals ---------------------------------------------------------------

# Parse a single <article class="jdt-card"> into a one-row tibble.
parse_card <- function(card) {
  link_node <- rvest::html_element(card, ".card-content__title")
  title     <- stringr::str_squish(rvest::html_text(link_node))
  url       <- rvest::html_attr(link_node, "href")
  
  info_lis  <- rvest::html_elements(card, ".card-info ul li")
  info_text <- stringr::str_squish(rvest::html_text(info_lis))
  
  # info_text typically holds two entries:
  #   [1] "DD/MM/YYYY"
  #   [2] "Estudio <code>"
  study_raw <- info_text[1]
  date_raw  <- info_text[2]
  
  date     <- as.Date(date_raw, format = "%d/%m/%Y")
  study_id <- stringr::str_extract(study_raw, "\\d+$")
  
  tibble::tibble(
    study_id = study_id,
    title    = title,
    date     = date,
    url      = url
  )
}