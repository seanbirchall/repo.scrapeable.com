#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom httr2 req_timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# gisservicemt.gov.R - Self-contained Montana GIS (MSDI) ArcGIS client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public ArcGIS services)
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.mtgis_base <- "https://gisservicemt.gov/arcgis/rest/services"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_service <- tibble(
  name = character(), type = character()
)

.schema_layer <- tibble(
  id = integer(), name = character(), type = character(),
  geometryType = character(), minScale = numeric(), maxScale = numeric()
)

.schema_feature <- tibble(
  OBJECTID = integer()
)

