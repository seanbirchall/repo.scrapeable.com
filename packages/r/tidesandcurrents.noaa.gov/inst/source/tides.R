# tidesandcurrents-noaa-gov.R
# Self-contained NOAA Tides and Currents API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tides_base <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Date formatting -----------------------------------------------------------

.tides_date <- function(d) {
  if (is.null(d)) return(NULL)
  format(as.Date(d), "%Y%m%d")
}

# == Schemas ===================================================================

.schema_water_level <- tibble(
  datetime = as.POSIXct(character()), value = numeric(),
  sigma = numeric(), flags = character(), quality = character()
)

.schema_predictions <- tibble(
  datetime = as.POSIXct(character()), value = numeric()
)

.schema_stations <- tibble(
  id = character(), name = character(), state = character(),
  lat = numeric(), lng = numeric()
)

# == Water Level ===============================================================

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
