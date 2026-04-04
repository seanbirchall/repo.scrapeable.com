#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# alleghenycounty.us.R - Allegheny County / WPRDC open data client (CKAN API)
#
# Data source: data.wprdc.org (Western PA Regional Data Center, CKAN)
# Datasets: ~365 (property, air quality, dog licenses, jail census, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wprdc_base <- "https://data.wprdc.org"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  notes = character(), organization = character(),
  num_resources = integer(), metadata_modified = character()
)

