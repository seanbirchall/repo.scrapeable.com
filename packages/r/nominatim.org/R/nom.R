#' Forward geocode — search for places by name
#'
#' @param query Place name (e.g. "New York", "Eiffel Tower")
#' @param limit Max results (default 5, max 50)
#' @param countrycodes Limit to countries (e.g. "us,ca")
#' @return tibble: place_id, osm_id, display_name, latitude, longitude, type, importance
#' @export
nom_search <- function(query, limit = 5, countrycodes = NULL) {
  url <- sprintf("%s/search?q=%s&format=json&limit=%d",
                 .nom_base, utils::URLencode(query, reserved = TRUE), limit)
  if (!is.null(countrycodes)) url <- paste0(url, "&countrycodes=", countrycodes)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Nominatim error: ", e$message); NULL })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_geocode)
  tibble(place_id = as.integer(raw$place_id), osm_id = as.character(raw$osm_id),
         display_name = as.character(raw$display_name),
         latitude = as.numeric(raw$lat), longitude = as.numeric(raw$lon),
         type = as.character(raw$type), importance = as.numeric(raw$importance))
}

#' Reverse geocode — coordinates to address
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @return tibble: one row with place_id, osm_id, display_name, latitude, longitude, type, importance
#' @export
nom_reverse <- function(latitude, longitude) {
  url <- sprintf("%s/reverse?lat=%s&lon=%s&format=json", .nom_base, latitude, longitude)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Nominatim error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_geocode)
  tibble(place_id = as.integer(raw$place_id), osm_id = as.character(raw$osm_id),
         display_name = as.character(raw$display_name),
         latitude = as.numeric(raw$lat), longitude = as.numeric(raw$lon),
         type = as.character(raw$type %||% NA_character_), importance = as.numeric(raw$importance %||% NA_real_))
}

#' Generate context
#' @return Character string (invisibly)
#' @export
nom_context <- function() {
  .build_context("nominatim.org", header_lines = c(
    "# nominatim.org - OpenStreetMap Geocoding Client for R",
    "# Auth: none (User-Agent required per TOS)",
    "# Forward + reverse geocoding. Rate limit: 1 req/sec."
  ))
}
