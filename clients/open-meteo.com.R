# open-meteo.com.R - Self-contained open-meteo.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# open-meteo-com.R
# Self-contained Open-Meteo weather API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://api.open-meteo.com/v1


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.meteo_base <- "https://api.open-meteo.com/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_hourly <- tibble(
  time = as.POSIXct(character()), temperature_2m = numeric(),
  relative_humidity_2m = numeric(), wind_speed_10m = numeric()
)

.schema_daily <- tibble(
  date = as.Date(character()), temperature_2m_max = numeric(),
  temperature_2m_min = numeric(), precipitation_sum = numeric()
)

# == Forecast ==================================================================


#' Get hourly weather forecast from Open-Meteo
#'
#' Returns hourly weather data for any location on Earth. The Open-Meteo
#' API is free, requires no API key, and provides forecasts up to 16 days
#' ahead. Use \code{\link{meteo_geocode}} to convert place names to
#' coordinates.
#'
#' @param latitude Numeric. Latitude in decimal degrees (e.g., \code{40.71}
#'   for New York, \code{51.51} for London, \code{-33.87} for Sydney).
#' @param longitude Numeric. Longitude in decimal degrees (e.g., \code{-74.01}
#'   for New York, \code{-0.13} for London, \code{151.21} for Sydney).
#' @param hourly Character. Comma-separated weather variables to retrieve.
#'   Default: \code{"temperature_2m,relative_humidity_2m,wind_speed_10m"}.
#'   Other options include \code{apparent_temperature}, \code{precipitation},
#'   \code{rain}, \code{snowfall}, \code{cloud_cover},
#'   \code{wind_direction_10m}, \code{pressure_msl}, \code{visibility}.
#' @param forecast_days Integer. Number of forecast days (default \code{7},
#'   maximum \code{16}).
#' @param start_date Character or \code{NULL}. Start date in
#'   \code{"YYYY-MM-DD"} format for a specific date range. If \code{NULL}
#'   (default), returns the current forecast.
#' @param end_date Character or \code{NULL}. End date in
#'   \code{"YYYY-MM-DD"} format.
#' @param timezone Character. Timezone for timestamps (default
#'   \code{"auto"} detects from coordinates). Examples: \code{"UTC"},
#'   \code{"America/New_York"}, \code{"Europe/London"}.
#' @return A tibble with a \code{time} column (POSIXct) and one numeric
#'   column per requested variable. Column names match the variable names
#'   (e.g., \code{temperature_2m}, \code{wind_speed_10m}).
#' @examples
#' meteo_hourly(40.71, -74.01)
#' meteo_hourly(51.51, -0.13, hourly = "temperature_2m,precipitation")
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


#' Get daily weather forecast or history from Open-Meteo
#'
#' Returns daily aggregated weather data (min/max temperatures,
#' precipitation totals, wind, sunrise/sunset) for any location.
#' Free, no API key needed.
#'
#' @param latitude Numeric. Latitude in decimal degrees.
#' @param longitude Numeric. Longitude in decimal degrees.
#' @param daily Character. Comma-separated daily variables. Default:
#'   \code{"temperature_2m_max,temperature_2m_min,precipitation_sum"}.
#'   Other options: \code{apparent_temperature_max},
#'   \code{apparent_temperature_min}, \code{rain_sum}, \code{snowfall_sum},
#'   \code{wind_speed_10m_max}, \code{sunrise}, \code{sunset},
#'   \code{uv_index_max}.
#' @param forecast_days Integer. Number of forecast days (default \code{7}).
#' @param start_date Character or \code{NULL}. Start date (\code{"YYYY-MM-DD"}).
#' @param end_date Character or \code{NULL}. End date (\code{"YYYY-MM-DD"}).
#' @param timezone Character. Timezone (default \code{"auto"}).
#' @return A tibble with a \code{date} column (Date) and one numeric column
#'   per requested variable.
#' @examples
#' meteo_daily(40.71, -74.01)
#' meteo_daily(35.68, 139.69, daily = "temperature_2m_max,precipitation_sum")
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


#' Get historical weather archive data from Open-Meteo
#'
#' Accesses the Open-Meteo historical archive covering weather data from
#' 1940 to approximately 5 days ago. Uses a separate archive endpoint
#' that is optimised for long time ranges. Free, no API key needed.
#'
#' @param latitude Numeric. Latitude in decimal degrees.
#' @param longitude Numeric. Longitude in decimal degrees.
#' @param start_date Character. Start date in \code{"YYYY-MM-DD"} format
#'   (required). Earliest available: \code{"1940-01-01"}.
#' @param end_date Character. End date in \code{"YYYY-MM-DD"} format
#'   (required).
#' @param daily Character. Comma-separated daily variables (same options
#'   as \code{\link{meteo_daily}}). Default:
#'   \code{"temperature_2m_max,temperature_2m_min,precipitation_sum"}.
#' @return A tibble with a \code{date} column (Date) and one numeric column
#'   per requested variable.
#' @examples
#' meteo_historical(40.71, -74.01, "2023-01-01", "2023-01-31")
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


#' Geocode a location name to coordinates via Open-Meteo
#'
#' Converts a place name to latitude and longitude using the Open-Meteo
#' geocoding API. Returns multiple matches ranked by population, which
#' is useful for disambiguating common city names. The returned
#' coordinates can be passed directly to \code{\link{meteo_hourly}} or
#' \code{\link{meteo_daily}}.
#'
#' @param name Character. Place name to look up (e.g., \code{"New York"},
#'   \code{"Tokyo"}, \code{"London"}, \code{"Paris"}).
#' @param count Integer. Maximum number of results to return (default
#'   \code{5}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Matched place name.}
#'     \item{latitude}{Numeric. Latitude in decimal degrees.}
#'     \item{longitude}{Numeric. Longitude in decimal degrees.}
#'     \item{country}{Character. Country name.}
#'     \item{timezone}{Character. IANA timezone identifier.}
#'     \item{elevation}{Numeric. Elevation in metres above sea level.}
#'     \item{population}{Integer. Estimated population.}
#'   }
#' @examples
#' meteo_geocode("Tokyo")
#' meteo_geocode("Springfield", count = 10)
meteo_geocode <- function(name, count = 5) {
  schema <- tibble(name = character(), latitude = numeric(), longitude = numeric(),
                   country = character(), timezone = character(),
                   elevation = numeric(), population = integer())
  url <- sprintf("https://geocoding-api.open-meteo.com/v1/search?name=%s&count=%d&language=en&format=json",
                 utils::URLencode(name, reserved = TRUE), as.integer(count))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results)) return(schema)

  r <- raw$results
  if (is.data.frame(r)) {
    nms <- names(r)
    as_tibble(r) |>
      transmute(
        name = as.character(if ("name" %in% nms) name else NA_character_),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        country = as.character(if ("country" %in% nms) country else NA_character_),
        timezone = as.character(if ("timezone" %in% nms) timezone else NA_character_),
        elevation = as.numeric(if ("elevation" %in% nms) elevation else NA_real_),
        population = as.integer(if ("population" %in% nms) population else NA_integer_)
      )
  } else {
    schema
  }
}

#' Compare weather forecasts across multiple locations
#'
#' Fetches daily forecasts for several locations and combines the results
#' into a single tibble with a \code{location} label column. Convenient
#' for side-by-side weather comparisons.
#'
#' @param locations Named list of coordinate vectors. Each element should
#'   be \code{c(latitude, longitude)}. Example:
#'   \code{list(NYC = c(40.71, -74.01), LA = c(34.05, -118.24))}.
#' @param daily Character. Comma-separated daily variables (default
#'   \code{"temperature_2m_max,temperature_2m_min,precipitation_sum"}).
#' @param forecast_days Integer. Number of forecast days (default \code{7}).
#' @return A tibble with a \code{location} column (Character, from the
#'   list names), a \code{date} column (Date), and one numeric column per
#'   requested variable.
#' @examples
#' meteo_compare(list(NYC = c(40.71, -74.01), London = c(51.51, -0.13)))
meteo_compare <- function(locations,
                          daily = "temperature_2m_max,temperature_2m_min,precipitation_sum",
                          forecast_days = 7) {
  rows <- lapply(names(locations), function(loc_name) {
    coords <- locations[[loc_name]]
    tryCatch({
      result <- meteo_daily(coords[1], coords[2], daily = daily,
                           forecast_days = forecast_days)
      if (nrow(result) > 0) {
        result$location <- loc_name
        result
      } else NULL
    }, error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(tibble(location = character(), date = as.Date(character())))
  bind_rows(rows) |> select(location, everything())
}

# == Context ===================================================================

#' Get open-meteo.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
meteo_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(meteo_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/open-meteo.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "open-meteo.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# open-meteo.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# open-meteo.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
