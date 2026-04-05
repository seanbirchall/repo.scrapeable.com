# tidesandcurrents-noaa-gov.R
# Self-contained NOAA Tides and Currents API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' @return A tibble with columns:
#'   \describe{
#'     \item{datetime}{Observation timestamp in UTC (POSIXct)}
#'     \item{value}{Water level in meters or feet depending on units (numeric)}
#'     \item{sigma}{Standard deviation of 1-second samples (numeric)}
#'     \item{flags}{Data quality flags (character)}
#'     \item{quality}{Quality assurance level: "v" verified, "p" preliminary (character)}
#'   }
#' @examples
#' tides_water_level("8454000", "2025-01-01", "2025-01-02")
#' @seealso [tides_predictions()], [tides_stations()], [tides_context()]
#' @source <https://api.tidesandcurrents.noaa.gov/api/prod/datagetter>
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
#' @return A tibble with columns:
#'   \describe{
#'     \item{datetime}{Prediction timestamp in UTC (POSIXct)}
#'     \item{value}{Predicted water level in meters or feet (numeric)}
#'   }
#' @examples
#' tides_predictions("9414290", "2025-06-01", "2025-06-02")
#' tides_predictions("8454000", "2025-06-01", "2025-06-02", interval = "6")
#' @seealso [tides_water_level()], [tides_stations()], [tides_context()]
#' @source <https://api.tidesandcurrents.noaa.gov/api/prod/datagetter>
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
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Station identifier, e.g. "8454000" (character)}
#'     \item{name}{Station name (character)}
#'     \item{state}{US state abbreviation or NA for non-US (character)}
#'     \item{lat}{Latitude in decimal degrees (numeric)}
#'     \item{lng}{Longitude in decimal degrees (numeric)}
#'   }
#' @examples
#' tides_stations()
#' @seealso [tides_water_level()], [tides_predictions()], [tides_context()]
#' @source <https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.json>
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

#' Get tidesandcurrents-noaa-gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
tides_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(tides_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/tidesandcurrents-noaa-gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "tidesandcurrents-noaa-gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# tidesandcurrents-noaa-gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# tidesandcurrents-noaa-gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
