# open-meteo-com.R
# Self-contained Open-Meteo weather API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://api.open-meteo.com/v1

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.meteo_base <- "https://api.open-meteo.com/v1"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

#' Get open-meteo-com client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/open-meteo-com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "open-meteo-com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# open-meteo-com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# open-meteo-com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
