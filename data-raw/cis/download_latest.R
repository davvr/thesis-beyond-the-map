# data-raw/cis/download_latest.R
# Orchestrator: download the 6 most recent monthly CIS barometers.
# Run this script to refresh your local copy of the microdata.

library(httr2)
library(rvest)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(fs)

# Source the scraping functions.
source("R/scraping/list_barometers.R")
source("R/scraping/get_zip_url.R")
source("R/scraping/download_zip.R")

# 1. List the 6 most recent monthly barometers.
message("Fetching catalogue...")
barometers <- list_barometers(n = 6)
print(barometers)

# 2. For each barometer, get the ZIP URL and download it.
message("\nDownloading ZIPs...")
barometers <- barometers |>
  mutate(
    zip_url = map_chr(url, get_zip_url),
    local_path = map2_chr(zip_url, study_id, download_zip)
  )

message("\n--- Done ---")
message("Downloaded ", nrow(barometers), " barometers to data-raw/cis/")
print(select(barometers, study_id, title, local_path))