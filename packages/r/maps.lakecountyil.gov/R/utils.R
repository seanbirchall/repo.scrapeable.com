#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# lakecountyil.gov.R - Lake County IL open data client (ArcGIS REST)
#
# Data source: maps.lakecountyil.gov (ArcGIS REST services)
# Also: data-lakecountyil.opendata.arcgis.com (ArcGIS Hub)
# Datasets: ~1217 (parcels, zoning, health, public works, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.lc_arcgis <- "https://maps.lakecountyil.gov/arcgis/rest/services"
.lc_hub <- "https://data-lakecountyil.opendata.arcgis.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.arcgis_query <- function(service_url, where = "1=1", out_fields = "*",
                          limit = 1000, offset = 0) {
  url <- sprintf("%s/query?where=%s&outFields=%s&f=json&resultRecordCount=%d&resultOffset=%d&returnGeometry=false",
                 service_url,
                 utils::URLencode(where),
                 utils::URLencode(out_fields),
                 limit, offset)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$features) || length(raw$features) == 0) return(tibble())
  attrs <- raw$features$attributes
  if (is.data.frame(attrs)) as_tibble(attrs) else tibble()
}

# == Known services ============================================================

.lc_services <- list(
  health    = "Health/HealthMapService/MapServer",
  dot       = "DOT/DOT/MapServer",
  sheriff   = "Sheriff/SheriffMapService/MapServer",
  utilities = "Utilities/UtilitiesMapService/MapServer",
  ccao      = "CCAO/CCAOMapService/MapServer",
  smc       = "SMC/SMCMapService/MapServer"
)

