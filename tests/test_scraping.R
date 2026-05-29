# tests/test_scraping.R
# Unit tests for the CIS scraping module.
# The first three tests run without network (using inline HTML/JSON fixtures).
# The last test is an integration test against the live CIS site (skip if offline).

library(testthat)
library(httr2)
library(rvest)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

source("R/scraping/list_barometers.R")
source("R/scraping/get_zip_url.R")
source("R/scraping/download_zip.R")

# -- Unit test: parse_card ----------------------------------------------------

test_that("parse_card extracts study_id, title, date, url from an <article>", {
  html_str <- '
    <article class="jdt-card">
      <a class="card-content__title" href="/es/estudios/barometro-de-marzo-2026">
        Barómetro de marzo 2026
      </a>
      <ul class="card-info">
        <li>27/03/2026</li>
        <li>Estudio 3546</li>
      </ul>
    </article>
  '
  card <- rvest::read_html(html_str) |> rvest::html_element("article")
  result <- parse_card(card)
  
  expect_equal(result$study_id, "3546")
  expect_equal(result$title, "Barómetro de marzo 2026")
  expect_equal(result$date, as.Date("2026-03-27"))
  expect_match(result$url, "barometro-de-marzo-2026")
})

# -- Unit test: extract_zip_url_from_html -------------------------------------

test_that("extract_zip_url_from_html finds the Fichero datos URL", {
  # Minimal JSON-LD fixture (copied from the March 2026 study page).
  json_ld <- '{
    "@context": "https://schema.org",
    "@graph": [
      {
        "@type": "Dataset",
        "distribution": [
          {"name": "Ficha técnica", "contentUrl": "https://www.cis.es/.../FT3546.pdf"},
          {"name": "Fichero datos", "contentUrl": "https://www.cis.es/documents/20117/13805127/MD3546.zip"}
        ]
      }
    ]
  }'
  html_str <- paste0(
    '<html><head>',
    '<script type="application/ld+json">', json_ld, '</script>',
    '</head></html>'
  )
  html <- rvest::read_html(html_str)
  
  result <- extract_zip_url_from_html(html, "https://example.com")
  expect_equal(result, "https://www.cis.es/documents/20117/13805127/MD3546.zip")
})

test_that("extract_zip_url_from_html errors if no Fichero datos entry", {
  json_ld <- '{
    "@context": "https://schema.org",
    "@graph": [{"@type": "Dataset", "distribution": []}]
  }'
  html_str <- paste0(
    '<html><head>',
    '<script type="application/ld+json">', json_ld, '</script>',
    '</head></html>'
  )
  html <- rvest::read_html(html_str)
  
  expect_error(
    extract_zip_url_from_html(html, "https://example.com"),
    "No distribution entry named 'Fichero datos'"
  )
})

# -- Unit test: is_zip_magic --------------------------------------------------

test_that("is_zip_magic identifies valid ZIP magic bytes", {
  valid_zip <- as.raw(c(0x50, 0x4B, 0x03, 0x04))
  invalid_html <- charToRaw("<html>")
  
  expect_true(is_zip_magic(valid_zip))
  expect_false(is_zip_magic(invalid_html))
})

# -- Integration test (live CIS site) -----------------------------------------

test_that("INTEGRATION: list_barometers returns 6 recent monthly barometers", {
  skip_if_offline()
  
  result <- list_barometers(n = 6)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6)
  expect_true(all(c("study_id", "title", "date", "url") %in% names(result)))
  expect_true(all(grepl("^Barómetro de [a-záéíóú]+ \\d{4}$", result$title, ignore.case = TRUE)))
})

test_that("INTEGRATION: get_zip_url returns a valid ZIP URL for March 2026", {
  skip_if_offline()
  
  url <- get_zip_url("https://www.cis.es/es/estudios/barometro-de-marzo-2026")
  
  expect_match(url, "^https://www.cis.es/documents/")
  expect_match(url, "MD3546\\.zip$")
})