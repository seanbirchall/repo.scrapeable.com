#' Get hourly weather forecast
#'
#' Returns hourly weather data for a location. Free, no API key needed.
#' Forecasts up to 16 days ahead, historical data back to 1940.
#'
#' @param latitude Latitude (e.g. 40.71 for New York)
#' @param longitude Longitude (e.g. -74.01 for New York)
#' @param hourly Variables to include. Default: "temperature_2m,relative_humidity_2m,wind_speed_10m".
#'   Options: temperature_2m, apparent_temperature, precipitation, rain, snowfall,
#'   cloud_cover, wind_speed_10m, wind_direction_10m, pressure_msl, etc.
#' @param forecast_days Days ahead (default 7, max 16)
#' @param start_date Start date for historical (YYYY-MM-DD)
#' @param end_date End date for historical
#' @param timezone Timezone (default "auto")
#' @return tibble with time (POSIXct) and requested weather variables
#' @export
meteo_hourly <- function(latitude, longitude,
                         hourly = "temperature_2m,relative_humidity_2m,wind_speed_10m",
                         forecast_days = 7, start_date = NULL, end_date = NULL,
                         timezone = "auto") {
  url <- sprintf("%s/forecast?latitude=%s&longitude=%s&hourly=%s&forecast_days=%d&timezone=%s",
                 .meteo_base, latitude, longitude, hourly, forecast_days, timezone)
  if (!is.null(start_date)) url <- paste0(url, "&start_date=", start_date)
  if (!is.null(end_date))   url <- paste0(url, "&end_date=", end_date)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Open-Meteo API error: ", e$message); NULL
  })
  if (is.null(raw) || is.null(raw$hourly)) return(.schema_hourly)

  h <- raw$hourly
  result <- tibble(time = as.POSIXct(h$time, format = "%Y-%m-%dT%H:%M", tz = raw$timezone %||% "UTC"))

  # Add all returned variables
  vars <- setdiff(names(h), "time")
  for (v in vars) {
    result[[v]] <- as.numeric(h[[v]])
  }
  result
}


#' Get daily weather forecast/history
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @param daily Variables. Default: "temperature_2m_max,temperature_2m_min,precipitation_sum".
#'   Options: temperature_2m_max/min, apparent_temperature_max/min, precipitation_sum,
#'   rain_sum, snowfall_sum, wind_speed_10m_max, sunrise, sunset, etc.
#' @param forecast_days Days ahead (default 7)
#' @param start_date Start date for historical
#' @param end_date End date for historical
#' @param timezone Timezone (default "auto")
#' @return tibble with date (Date) and requested variables
#' @export
meteo_daily <- function(latitude, longitude,
                        daily = "temperature_2m_max,temperature_2m_min,precipitation_sum",
                        forecast_days = 7, start_date = NULL, end_date = NULL,
                        timezone = "auto") {
  url <- sprintf("%s/forecast?latitude=%s&longitude=%s&daily=%s&forecast_days=%d&timezone=%s",
                 .meteo_base, latitude, longitude, daily, forecast_days, timezone)
  if (!is.null(start_date)) url <- paste0(url, "&start_date=", start_date)
  if (!is.null(end_date))   url <- paste0(url, "&end_date=", end_date)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Open-Meteo API error: ", e$message); NULL
  })
  if (is.null(raw) || is.null(raw$daily)) return(.schema_daily)

  d <- raw$daily
  result <- tibble(date = as.Date(d$time))
  vars <- setdiff(names(d), "time")
  for (v in vars) {
    result[[v]] <- as.numeric(d[[v]])
  }
  result
}


#' Get historical weather archive data
#'
#' Access historical weather data from 1940 to present.
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @param start_date Start date (required, YYYY-MM-DD)
#' @param end_date End date (required, YYYY-MM-DD)
#' @param daily Variables (same options as meteo_daily)
#' @return tibble with date and requested variables
#' @export
meteo_historical <- function(latitude, longitude, start_date, end_date,
                             daily = "temperature_2m_max,temperature_2m_min,precipitation_sum") {
  url <- sprintf(
    "https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=%s&end_date=%s&daily=%s&timezone=auto",
    latitude, longitude, start_date, end_date, daily
  )

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Open-Meteo Archive API error: ", e$message); NULL
  })
  if (is.null(raw) || is.null(raw$daily)) return(.schema_daily)

  d <- raw$daily
  result <- tibble(date = as.Date(d$time))
  vars <- setdiff(names(d), "time")
  for (v in vars) {
    result[[v]] <- as.numeric(d[[v]])
  }
  result
}


#' Generate context
#' @return Character string (invisibly)
#' @export
meteo_context <- function() {
  .build_context("open-meteo.com", header_lines = c(
    "# open-meteo.com - Open-Meteo Weather API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (free, open source)",
    "# Forecast + historical data back to 1940",
    "# Variables: temperature, humidity, wind, precipitation, cloud cover, etc."
  ))
}
