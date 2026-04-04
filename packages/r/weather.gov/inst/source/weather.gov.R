# weather.gov.R - Self-contained weather.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# weather-gov.R
# Self-contained NWS Weather API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent header recommended)
# API: https://api.weather.gov


# == Private utilities =========================================================

.ua <- "scrapeable.com (support@scrapeable.com)"
.nws_base <- "https://api.weather.gov"

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


#' Get 7-day weather forecast for a US location
#'
#' Returns the 7-day forecast in 12-hour periods (day/night) from the
#' National Weather Service API. Works only for US locations within
#' NWS coverage. Internally resolves coordinates to a forecast grid
#' point, then fetches the forecast.
#'
#' @param latitude Numeric latitude in decimal degrees
#'   (e.g., \code{40.7128} for New York City).
#' @param longitude Numeric longitude in decimal degrees
#'   (e.g., \code{-74.0060} for New York City). Use negative values
#'   for western hemisphere.
#' @return A tibble with up to 14 rows (7 days x 2 periods) and columns:
#'   \describe{
#'     \item{number}{Integer period number (1 = current)}
#'     \item{name}{Period label (e.g., "Tonight", "Saturday", "Saturday Night")}
#'     \item{temperature}{Numeric temperature value}
#'     \item{temperature_unit}{Unit: "F" (Fahrenheit) or "C" (Celsius)}
#'     \item{wind_speed}{Wind speed description (e.g., "8 to 13 mph")}
#'     \item{wind_direction}{Cardinal wind direction (e.g., "SW", "NE")}
#'     \item{short_forecast}{Brief forecast text (e.g., "Partly Cloudy")}
#'     \item{detailed_forecast}{Full narrative forecast}
#'     \item{start_time}{POSIXct start of the forecast period}
#'   }
#' @export
#' @family NWS functions
#' @seealso \code{\link{nws_alerts}} for active weather alerts,
#'   \code{\link{nws_stations}} for nearby observation stations
#' @examples
#' \dontrun{
#' # Forecast for New York City
#' nws_forecast(40.7128, -74.0060)
#'
#' # Forecast for San Francisco
#' nws_forecast(37.7749, -122.4194)
#' }
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

#' Get active weather alerts from the National Weather Service
#'
#' Returns currently active weather alerts (warnings, watches, advisories)
#' filtered by state, zone, or severity level. Data refreshes in near
#' real-time from the NWS alert system.
#'
#' @param state Optional two-letter state code (e.g., \code{"NY"},
#'   \code{"CA"}). Filters alerts to the given state.
#' @param area Optional NWS zone or county code for finer geographic
#'   filtering.
#' @param severity Optional severity filter. One of \code{"Extreme"},
#'   \code{"Severe"}, \code{"Moderate"}, or \code{"Minor"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Unique alert identifier (URN)}
#'     \item{event}{Event type (e.g., "Tornado Warning", "Flood Watch")}
#'     \item{severity}{Alert severity level}
#'     \item{headline}{Human-readable alert headline}
#'     \item{area}{Geographic area description}
#'     \item{onset}{POSIXct alert onset time}
#'     \item{expires}{POSIXct alert expiration time}
#'   }
#' @export
#' @family NWS functions
#' @seealso \code{\link{nws_forecast}} for location forecasts
#' @examples
#' \dontrun{
#' # All active alerts in California
#' nws_alerts(state = "CA")
#'
#' # Only severe alerts nationwide
#' nws_alerts(severity = "Severe")
#' }
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

#' Get NWS observation stations near a location
#'
#' Returns weather observation stations near the given coordinates.
#' Stations are part of the NWS cooperative and automated
#' observation networks. Use station IDs to fetch current conditions.
#'
#' @param latitude Numeric latitude in decimal degrees.
#' @param longitude Numeric longitude in decimal degrees.
#' @return A tibble with columns:
#'   \describe{
#'     \item{station_id}{Four-character station identifier (e.g., "KJFK")}
#'     \item{name}{Station name (e.g., "JFK International Airport")}
#'     \item{latitude}{Numeric station latitude}
#'     \item{longitude}{Numeric station longitude}
#'     \item{elevation_m}{Numeric station elevation in meters}
#'   }
#' @export
#' @family NWS functions
#' @seealso \code{\link{nws_forecast}} for location forecasts
#' @examples
#' \dontrun{
#' # Stations near Washington, DC
#' nws_stations(38.8951, -77.0364)
#' }
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


# == Context ===================================================================

#' Get weather.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nws_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nws_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/weather.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "weather.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# weather.gov context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# weather.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
