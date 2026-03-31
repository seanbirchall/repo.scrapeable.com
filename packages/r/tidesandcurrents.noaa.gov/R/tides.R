#' Fetch observed water level data
#'
#' Retrieves verified or preliminary water level observations from
#' NOAA tide gauge stations.
#'
#' @param station Station ID (e.g. "8454000" for Providence, RI;
#'   "9414290" for San Francisco, CA)
#' @param begin_date Start date (Date or "YYYY-MM-DD")
#' @param end_date End date (Date or "YYYY-MM-DD")
#' @param datum Vertical datum: "MLLW" (default), "MSL", "NAVD", "STND"
#' @param units "metric" (default) or "english"
#' @return tibble: datetime, value, sigma, flags, quality
#' @export
tides_water_level <- function(station, begin_date, end_date,
                              datum = "MLLW", units = "metric") {
  url <- sprintf(
    "%s?begin_date=%s&end_date=%s&station=%s&product=water_level&datum=%s&time_zone=gmt&units=%s&format=json&application=%s",
    .tides_base, .tides_date(begin_date), .tides_date(end_date),
    station, datum, units, .ua
  )
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_water_level)
  if (is.data.frame(d) && nrow(d) == 0) return(.schema_water_level)

  as_tibble(d) |>
    transmute(
      datetime = as.POSIXct(t, format = "%Y-%m-%d %H:%M", tz = "UTC"),
      value = as.numeric(v),
      sigma = as.numeric(if ("s" %in% names(d)) s else NA),
      flags = as.character(if ("f" %in% names(d)) f else NA),
      quality = as.character(if ("q" %in% names(d)) q else NA)
    )
}

# == Predictions ===============================================================

#' Fetch tide predictions
#'
#' Retrieves tide predictions (high/low or 6-minute intervals) for a station.
#'
#' @param station Station ID
#' @param begin_date Start date (Date or "YYYY-MM-DD")
#' @param end_date End date (Date or "YYYY-MM-DD")
#' @param datum Vertical datum: "MLLW" (default), "MSL"
#' @param interval "hilo" for high/low only, "6" for 6-minute, "h" for hourly
#' @param units "metric" (default) or "english"
#' @return tibble: datetime, value
#' @export
tides_predictions <- function(station, begin_date, end_date,
                              datum = "MLLW", interval = "hilo",
                              units = "metric") {
  url <- sprintf(
    "%s?begin_date=%s&end_date=%s&station=%s&product=predictions&datum=%s&time_zone=gmt&units=%s&interval=%s&format=json&application=%s",
    .tides_base, .tides_date(begin_date), .tides_date(end_date),
    station, datum, units, interval, .ua
  )
  raw <- .fetch_json(url)
  preds <- raw$predictions
  if (is.null(preds) || length(preds) == 0) return(.schema_predictions)
  if (is.data.frame(preds) && nrow(preds) == 0) return(.schema_predictions)

  as_tibble(preds) |>
    transmute(
      datetime = as.POSIXct(t, format = "%Y-%m-%d %H:%M", tz = "UTC"),
      value = as.numeric(v)
    )
}

# == Stations ==================================================================

#' Fetch list of NOAA tide stations
#'
#' Returns metadata for all active water level stations.
#'
#' @return tibble: id, name, state, lat, lng
#' @export
tides_stations <- function() {
  url <- "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.json?type=waterlevels"
  raw <- .fetch_json(url)
  stations <- raw$stations
  if (is.null(stations) || length(stations) == 0) return(.schema_stations)
  if (is.data.frame(stations) && nrow(stations) == 0) return(.schema_stations)

  as_tibble(stations) |>
    transmute(
      id = as.character(id),
      name = as.character(name),
      state = as.character(if ("state" %in% names(stations)) state else NA),
      lat = as.numeric(if ("lat" %in% names(stations)) lat else NA),
      lng = as.numeric(if ("lng" %in% names(stations)) lng else NA)
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the tidesandcurrents.noaa.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
tides_context <- function() {
  .build_context("tidesandcurrents.noaa.gov", header_lines = c(
    "# tidesandcurrents.noaa.gov - NOAA Tides and Currents API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://api.tidesandcurrents.noaa.gov/api/prod/datagetter",
    "# All functions return tibbles with typed columns.",
    "# Popular stations: 8454000 (Providence RI), 9414290 (San Francisco CA),",
    "#   8518750 (The Battery NYC), 9447130 (Seattle WA)",
    "# Datums: MLLW, MSL, NAVD, STND"
  ))
}
