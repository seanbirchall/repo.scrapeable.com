#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# hartford.gov.R - City of Hartford CT open data client (ArcGIS Hub)
#
# Data source: data.hartford.gov (ArcGIS Hub)
# Datasets: ~105 (food licenses, service requests, zoning, buildings, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.htfd_base <- "https://data.hartford.gov"

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

# == Known datasets (item_id, layer, name) =====================================

.htfd_datasets <- list(
  food_licenses    = list(id = "27139a92097e4e6a957646fb633e1e71", layer = 2, name = "Food Establishments Licenses"),
  service_requests = list(id = "2185af186dda46caa1e59323407d1daf", layer = 9, name = "Service Requests Current Year"),
  zoning           = list(id = "0b899d5a56bc4791a66688a366ce9ea6", layer = 4, name = "Zoning Districts"),
  city_property    = list(id = "8e4ef8c879594a6f8f01805e7d0b0235", layer = 0, name = "City Owned Property"),
  buildings        = list(id = "493ed7fa65414636bb497504d3db1c80", layer = 29, name = "Buildings"),
  parking_lots     = list(id = "5272d58a79c7452b94b0c27e4a50032d", layer = 27, name = "Parking Lots")
)

