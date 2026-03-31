#' Query USGS earthquake data
#'
#' Returns earthquake events from the USGS FDSN web service.
#'
#' @param starttime Start date (Date or "YYYY-MM-DD")
#' @param endtime End date (Date or "YYYY-MM-DD"). Default: today.
#' @param minmagnitude Minimum magnitude (default 2.5)
#' @param maxmagnitude Maximum magnitude
#' @param limit Max results (default 100, max 20000)
#' @param latitude Center latitude for radius search
#' @param longitude Center longitude for radius search
#' @param maxradiuskm Max radius in km (requires lat/lon)
#' @param orderby Sort: "time" (default), "time-asc", "magnitude", "magnitude-asc"
#' @return tibble: id, time (POSIXct), mag, place, longitude, latitude,
#'   depth, type, status, url
#' @export
usgs_quakes <- function(starttime = NULL, endtime = NULL,
                        minmagnitude = 2.5, maxmagnitude = NULL,
                        limit = 100, latitude = NULL, longitude = NULL,
                        maxradiuskm = NULL, orderby = "time") {
  params <- list(format = "geojson", limit = limit,
                 minmagnitude = minmagnitude, orderby = orderby)
  if (!is.null(starttime)) params$starttime <- as.character(starttime)
  if (!is.null(endtime))   params$endtime <- as.character(endtime)
  if (!is.null(maxmagnitude)) params$maxmagnitude <- maxmagnitude
  if (!is.null(latitude))   params$latitude <- latitude
  if (!is.null(longitude))  params$longitude <- longitude
  if (!is.null(maxradiuskm)) params$maxradiuskm <- maxradiuskm

  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.usgs_base, "/query?", query)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("USGS API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_quakes)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_quakes)

  tibble(
    id        = vapply(features, function(f) f$id %||% NA_character_, character(1)),
    time      = as.POSIXct(vapply(features, function(f) {
      (f$properties$time %||% NA_real_) / 1000
    }, numeric(1)), origin = "1970-01-01", tz = "UTC"),
    mag       = vapply(features, function(f) as.numeric(f$properties$mag %||% NA_real_), numeric(1)),
    place     = vapply(features, function(f) f$properties$place %||% NA_character_, character(1)),
    longitude = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    latitude  = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    depth     = vapply(features, function(f) as.numeric(f$geometry$coordinates[[3]] %||% NA_real_), numeric(1)),
    type      = vapply(features, function(f) f$properties$type %||% NA_character_, character(1)),
    status    = vapply(features, function(f) f$properties$status %||% NA_character_, character(1)),
    url       = vapply(features, function(f) f$properties$url %||% NA_character_, character(1))
  )
}


#' Get significant earthquakes in the last 30 days
#'
#' @return tibble: same columns as usgs_quakes
#' @export
usgs_significant <- function() {
  url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/significant_month.geojson"
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("USGS API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_quakes)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_quakes)

  tibble(
    id        = vapply(features, function(f) f$id %||% NA_character_, character(1)),
    time      = as.POSIXct(vapply(features, function(f) (f$properties$time %||% NA_real_) / 1000, numeric(1)),
                           origin = "1970-01-01", tz = "UTC"),
    mag       = vapply(features, function(f) as.numeric(f$properties$mag %||% NA_real_), numeric(1)),
    place     = vapply(features, function(f) f$properties$place %||% NA_character_, character(1)),
    longitude = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    latitude  = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    depth     = vapply(features, function(f) as.numeric(f$geometry$coordinates[[3]] %||% NA_real_), numeric(1)),
    type      = vapply(features, function(f) f$properties$type %||% NA_character_, character(1)),
    status    = vapply(features, function(f) f$properties$status %||% NA_character_, character(1)),
    url       = vapply(features, function(f) f$properties$url %||% NA_character_, character(1))
  )
}


#' Generate context
#' @return Character string (invisibly)
#' @export
usgs_context <- function() {
  .build_context("usgs.gov", header_lines = c(
    "# usgs.gov - USGS Earthquake Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# GeoJSON format, magnitude/location/time filters"
  ))
}
