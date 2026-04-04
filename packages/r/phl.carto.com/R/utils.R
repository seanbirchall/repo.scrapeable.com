#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# phila.gov.R - City of Philadelphia open data client (CARTO SQL API)
#
# Data source: phl.carto.com (CARTO SQL API)
# Datasets: ~267 (crime, permits, violations, licenses, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.phl_carto <- "https://phl.carto.com/api/v2/sql"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.phl_sql <- function(sql) {
  url <- paste0(.phl_carto, "?q=", utils::URLencode(sql))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$rows) || length(raw$rows) == 0) return(tibble())
  df <- as_tibble(raw$rows)
  # Remove CARTO internal geom columns
  df <- df |> select(-any_of(c("the_geom", "the_geom_webmercator")))
  df
}

# == Known tables ==============================================================

.phl_tables <- c(
  "incidents_part1_part2",
  "li_permits",
  "li_violations",
  "li_complaints",
  "li_case_inspections",
  "li_trade_licenses",
  "li_commercial_activity_licenses",
  "li_business_licenses",
  "complaints_against_police",
  "shootings"
)

