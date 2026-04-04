#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# arkansas.gov.R - State of Arkansas GIS open data client (ArcGIS FeatureServer)
#
# Data source: gis.arkansas.gov (ArcGIS REST FeatureServer)
# Datasets: ~72 (boundaries, environment, transportation, health, farming, water)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ark_base <- "https://gis.arkansas.gov/arcgis/rest/services/FEATURESERVICES"

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

# == Known feature services ====================================================

.ark_services <- list(
  boundaries     = "Boundaries/FeatureServer",
  environment    = "Environment/FeatureServer",
  transportation = "Transportation/FeatureServer",
  health         = "Health/FeatureServer",
  farming        = "Farming/FeatureServer",
  water          = "Water/FeatureServer"
)

# Known layer IDs within services
.ark_layers <- list(
  senate_districts     = list(svc = "boundaries", layer = 34, name = "Senate Districts"),
  house_districts      = list(svc = "boundaries", layer = 14, name = "House Districts"),
  fire_districts       = list(svc = "boundaries", layer = 12, name = "Fire Districts"),
  hospitals            = list(svc = "health",     layer = 2,  name = "Hospital Related Services"),
  trout_streams        = list(svc = "environment", layer = 5, name = "Trout Streams"),
  erw_segments         = list(svc = "environment", layer = 2, name = "Extraordinary Resource Waters"),
  posted_bridges       = list(svc = "transportation", layer = 3, name = "Posted Highway Bridges"),
  intermodal_terminals = list(svc = "transportation", layer = 1, name = "Intermodal Terminals"),
  hydrologic_units     = list(svc = "water",      layer = 24, name = "8 Digit Hydrologic Units")
)

