#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# tempe.gov.R - City of Tempe AZ open data client (ArcGIS Hub)
#
# Data source: data.tempe.gov (ArcGIS Hub)
# Datasets: ~599 (police, crime, addresses, wastewater, fire, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tempe_base <- "https://data.tempe.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.fetch_csv <- function(url) {
  tmp <- tryCatch(.fetch(url, ext = ".csv"), error = function(e) NULL)
  if (is.null(tmp)) return(tibble())
  tryCatch(as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE)),
           error = function(e) tibble())
}

# == Known datasets ============================================================

.tempe_datasets <- list(
  arrests_90d    = list(id = "8931f2aae8f44e9786d6398c4bdc6151", layer = 0, name = "Police Arrests - Last 90 Days"),
  arrests_all    = list(id = "aeb0328cb1d6412b9f7a1fc65b056148", layer = 0, name = "Police Arrests - All Data"),
  offenses       = list(id = "1563be5b343b4f78b1163e97a9a503ad", layer = 0, name = "General Offenses"),
  addresses      = list(id = "9250a8606ddb4b0c869b58ed1525cd9e", layer = 0, name = "Addresses"),
  zip_codes      = list(id = "9e56f81e8af8469eb89a9d464d6ce59f", layer = 0, name = "Zip Code Boundaries"),
  covid_ww       = list(id = "c921c8af868f4e17b857c390543c5250", layer = 0, name = "Wastewater COVID-19 Results"),
  fire_survey    = list(id = "744963c36de8447195a0461b8beb435a", layer = 0, name = "Fire Services Survey")
)

