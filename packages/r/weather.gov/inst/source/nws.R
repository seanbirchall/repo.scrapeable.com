# weather-gov.R
# Self-contained NWS Weather API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent header recommended)
# API: https://api.weather.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "scrapeable.com (support@scrapeable.com)"
.nws_base <- "https://api.weather.gov"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/geo+json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_forecast <- tibble(
  number = integer(), name = character(), temperature = numeric(),
  temperature_unit = character(), wind_speed = character(),
  wind_direction = character(), short_forecast = character(),
  detailed_forecast = character(), start_time = as.POSIXct(character())
)

.schema_alerts <- tibble(
  id = character(), event = character(), severity = character(),
  headline = character(), area = character(),
  onset = as.POSIXct(character()), expires = as.POSIXct(character())
)

.schema_stations <- tibble(
  station_id = character(), name = character(),
  latitude = numeric(), longitude = numeric(), elevation_m = numeric()
)

# == Forecast ==================================================================

#' Get weather forecast for a location
#'
#' Returns 7-day forecast in 12-hour periods (day/night).
#' US locations only (NWS coverage area).
#'
#' @param latitude Latitude (e.g. 40.7128 for NYC)
#' @param longitude Longitude (e.g. -74.0060 for NYC)
#' @return tibble: number, name (period name), temperature, temperature_unit,
#'   wind_speed, wind_direction, short_forecast, detailed_forecast, start_time
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
nws_context <- function() {
  .build_context("weather.gov", header_lines = c(
    "# weather.gov - NWS Weather API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none (User-Agent recommended)",
    "# US locations only. 7-day forecast, active alerts, observation stations."
  ))
}
