#' Get weather forecast for a location
#'
#' Returns 7-day forecast in 12-hour periods (day/night).
#' US locations only (NWS coverage area).
#'
#' @param latitude Latitude (e.g. 40.7128 for NYC)
#' @param longitude Longitude (e.g. -74.0060 for NYC)
#' @return tibble: number, name (period name), temperature, temperature_unit,
#'   wind_speed, wind_direction, short_forecast, detailed_forecast, start_time
#' @export
nws_forecast <- function(latitude, longitude) {
  # Step 1: Get grid point
  point_url <- sprintf("%s/points/%.4f,%.4f", .nws_base, latitude, longitude)
  point <- tryCatch(.fetch_json(point_url), error = function(e) {
    warning("NWS API error: ", e$message); NULL
  })
  if (is.null(point)) return(.schema_forecast)

  forecast_url <- point$properties$forecast
  if (is.null(forecast_url)) return(.schema_forecast)

  # Step 2: Get forecast
  raw <- tryCatch(.fetch_json(forecast_url), error = function(e) {
    warning("NWS forecast error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_forecast)

  periods <- raw$properties$periods
  if (is.null(periods) || length(periods) == 0) return(.schema_forecast)

  tibble(
    number            = vapply(periods, function(p) as.integer(p$number %||% NA_integer_), integer(1)),
    name              = vapply(periods, function(p) p$name %||% NA_character_, character(1)),
    temperature       = vapply(periods, function(p) as.numeric(p$temperature %||% NA_real_), numeric(1)),
    temperature_unit  = vapply(periods, function(p) p$temperatureUnit %||% NA_character_, character(1)),
    wind_speed        = vapply(periods, function(p) p$windSpeed %||% NA_character_, character(1)),
    wind_direction    = vapply(periods, function(p) p$windDirection %||% NA_character_, character(1)),
    short_forecast    = vapply(periods, function(p) p$shortForecast %||% NA_character_, character(1)),
    detailed_forecast = vapply(periods, function(p) p$detailedForecast %||% NA_character_, character(1)),
    start_time        = as.POSIXct(vapply(periods, function(p) p$startTime %||% NA_character_, character(1)),
                                   format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )
}


# == Alerts ====================================================================

#' Get active weather alerts
#'
#' @param state Two-letter state code (e.g. "NY", "CA")
#' @param area Area code (NWS zone or county code)
#' @param severity Severity filter: "Extreme", "Severe", "Moderate", "Minor"
#' @return tibble: id, event, severity, headline, area, onset, expires
#' @export
nws_alerts <- function(state = NULL, area = NULL, severity = NULL) {
  url <- paste0(.nws_base, "/alerts/active?status=actual")
  if (!is.null(state))    url <- paste0(url, "&area=", state)
  if (!is.null(area))     url <- paste0(url, "&zone=", area)
  if (!is.null(severity)) url <- paste0(url, "&severity=", severity)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NWS alerts error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_alerts)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_alerts)

  tibble(
    id       = vapply(features, function(f) f$properties$id %||% NA_character_, character(1)),
    event    = vapply(features, function(f) f$properties$event %||% NA_character_, character(1)),
    severity = vapply(features, function(f) f$properties$severity %||% NA_character_, character(1)),
    headline = vapply(features, function(f) f$properties$headline %||% NA_character_, character(1)),
    area     = vapply(features, function(f) f$properties$areaDesc %||% NA_character_, character(1)),
    onset    = as.POSIXct(vapply(features, function(f) f$properties$onset %||% NA_character_, character(1)),
                          format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    expires  = as.POSIXct(vapply(features, function(f) f$properties$expires %||% NA_character_, character(1)),
                          format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )
}


# == Stations ==================================================================

#' Get observation stations near a location
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @return tibble: station_id, name, latitude, longitude, elevation_m
#' @export
nws_stations <- function(latitude, longitude) {
  point_url <- sprintf("%s/points/%.4f,%.4f", .nws_base, latitude, longitude)
  point <- tryCatch(.fetch_json(point_url), error = function(e) {
    warning("NWS API error: ", e$message); NULL
  })
  if (is.null(point)) return(.schema_stations)

  stations_url <- point$properties$observationStations
  if (is.null(stations_url)) return(.schema_stations)

  raw <- tryCatch(.fetch_json(stations_url), error = function(e) {
    warning("NWS stations error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_stations)

  features <- raw$features %||% raw$observationStations
  if (is.null(features) || length(features) == 0) return(.schema_stations)

  tibble(
    station_id  = vapply(features, function(f) f$properties$stationIdentifier %||% NA_character_, character(1)),
    name        = vapply(features, function(f) f$properties$name %||% NA_character_, character(1)),
    latitude    = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    longitude   = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    elevation_m = vapply(features, function(f) as.numeric(f$properties$elevation$value %||% NA_real_), numeric(1))
  )
}


#' Generate context
#' @return Character string (invisibly)
#' @export
nws_context <- function() {
  .build_context("weather.gov", header_lines = c(
    "# weather.gov - NWS Weather API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none (User-Agent recommended)",
    "# US locations only. 7-day forecast, active alerts, observation stations."
  ))
}
