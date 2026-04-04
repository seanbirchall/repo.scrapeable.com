#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# townofcary.org.R - Town of Cary NC open data client (Opendatasoft v2 API)
#
# Data source: data.townofcary.org (Opendatasoft)
# Datasets: ~76 (police, developments, greenways, stream gages, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cary_base <- "https://data.townofcary.org/api/v2"

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
  dataset_id = character(), title = character(), description = character(),
  theme = character(), records_count = integer(), modified = character()
)

.schema_records <- tibble()

