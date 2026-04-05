# api.tidesandcurrents.noaa.gov.R - Self-contained api.tidesandcurrents.noaa.gov client




.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

#' Get tide predictions for a station
#'
#' Returns predicted tide water levels for a NOAA tide station at 6-minute
#' intervals. Predictions are astronomically computed (not observed). Use
#' \code{\link{tides_stations}} to find valid station IDs, and
#' \code{\link{tides_water_levels}} for actual observed water levels.
#'
#' @param station Character. NOAA station ID. Examples: \code{"9414290"} (San
#'   Francisco), \code{"8518750"} (The Battery, NYC), \code{"9447130"}
#'   (Seattle). Find IDs with \code{\link{tides_stations}}.
#' @param begin_date Character. Start date in \code{YYYYMMDD} format
#'   (default: today). Example: \code{"20260101"}.
#' @param end_date Character. End date in \code{YYYYMMDD} format
#'   (default: tomorrow). Maximum range is 31 days. Example: \code{"20260102"}.
#'
#' @return A tibble with one row per 6-minute interval and the following columns:
#' \describe{
#'   \item{time}{Character. Timestamp in \code{"YYYY-MM-DD HH:MM"} format (GMT).}
#'   \item{value}{Numeric. Predicted water level in meters relative to MLLW (Mean Lower Low Water) datum.}
#' }
#'
#' @examples
#' \dontrun{
#' # Get tide predictions for San Francisco today
#' tides_predictions("9414290")
#'
#' # Get predictions for a specific date range
#' tides_predictions("8518750", begin_date = "20260101", end_date = "20260103")
#' }
tides_predictions <- function(station, begin_date = format(Sys.Date(), "%Y%m%d"),
                              end_date = format(Sys.Date() + 1, "%Y%m%d")) {
  url <- sprintf("%s?station=%s&begin_date=%s&end_date=%s&product=predictions&datum=MLLW&units=metric&time_zone=gmt&application=scrapeable&format=json",
                 .base, station, begin_date, end_date)
  raw <- .fetch_json(url)
  preds <- raw$predictions
  if (length(preds) == 0) return(tibble::tibble(time = character(), value = numeric()))
  tibble::tibble(
    time = vapply(preds, function(x) x$t %||% NA_character_, character(1)),
    value = vapply(preds, function(x) as.numeric(x$v %||% NA), numeric(1))
  )
}

#' Get observed water levels for a station
#'
#' Returns actual observed (measured) water levels for a NOAA tide station at
#' 6-minute intervals. Compare with \code{\link{tides_predictions}} to see
#' how predictions differ from observations. Use \code{\link{tides_stations}}
#' to find valid station IDs.
#'
#' @param station Character. NOAA station ID. Examples: \code{"9414290"} (San
#'   Francisco), \code{"8518750"} (The Battery, NYC). Find IDs with
#'   \code{\link{tides_stations}}.
#' @param begin_date Character. Start date in \code{YYYYMMDD} format
#'   (default: yesterday). Example: \code{"20260101"}.
#' @param end_date Character. End date in \code{YYYYMMDD} format
#'   (default: today). Maximum range is 31 days. Example: \code{"20260102"}.
#'
#' @return A tibble with one row per 6-minute interval and the following columns:
#' \describe{
#'   \item{time}{Character. Timestamp in \code{"YYYY-MM-DD HH:MM"} format (GMT).}
#'   \item{value}{Numeric. Observed water level in meters relative to MLLW datum.}
#'   \item{quality}{Character. Data quality flag: \code{"p"} (preliminary),
#'     \code{"v"} (verified).}
#' }
#'
#' @examples
#' \dontrun{
#' # Get yesterday's water levels for San Francisco
#' tides_water_levels("9414290")
#'
#' # Get water levels for a specific date range
#' tides_water_levels("8518750", begin_date = "20260301", end_date = "20260302")
#' }
tides_water_levels <- function(station, begin_date = format(Sys.Date() - 1, "%Y%m%d"),
                               end_date = format(Sys.Date(), "%Y%m%d")) {
  url <- sprintf("%s?station=%s&begin_date=%s&end_date=%s&product=water_level&datum=MLLW&units=metric&time_zone=gmt&application=scrapeable&format=json",
                 .base, station, begin_date, end_date)
  raw <- .fetch_json(url)
  data <- raw$data
  if (length(data) == 0) return(tibble::tibble(time = character(), value = numeric()))
  tibble::tibble(
    time = vapply(data, function(x) x$t %||% NA_character_, character(1)),
    value = vapply(data, function(x) as.numeric(x$v %||% NA), numeric(1)),
    quality = vapply(data, function(x) x$q %||% NA_character_, character(1))
  )
}

#' List NOAA tide stations
#'
#' Returns all NOAA CO-OPS tide monitoring stations in the United States and
#' territories. Use this to discover station IDs for use with
#' \code{\link{tides_predictions}} and \code{\link{tides_water_levels}}.
#' Currently returns approximately 301 stations.
#'
#' @return A tibble with one row per station and the following columns:
#' \describe{
#'   \item{id}{Character. Station ID for use with other functions (e.g.
#'     \code{"9414290"} for San Francisco, \code{"8518750"} for The Battery NYC).}
#'   \item{name}{Character. Station name (e.g. \code{"San Francisco"},
#'     \code{"Honolulu"}).}
#'   \item{state}{Character. State abbreviation or territory name (e.g.
#'     \code{"CA"}, \code{"HI"}, \code{"United States of America"}).}
#'   \item{latitude}{Numeric. Station latitude in decimal degrees.}
#'   \item{longitude}{Numeric. Station longitude in decimal degrees.}
#' }
#'
#' @examples
#' \dontrun{
#' # List all stations
#' stations <- tides_stations()
#'
#' # Find California stations
#' library(dplyr)
#' stations |> filter(state == "CA")
#'
#' # Find station by name
#' stations |> filter(grepl("San Francisco", name))
#' }
tides_stations <- function() {
  url <- "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.json"
  raw <- .fetch_json(url)
  stns <- raw$stations
  if (length(stns) == 0) return(tibble::tibble(id = character(), name = character()))
  tibble::tibble(
    id = vapply(stns, function(x) x$id %||% NA_character_, character(1)),
    name = vapply(stns, function(x) x$name %||% NA_character_, character(1)),
    state = vapply(stns, function(x) x$state %||% NA_character_, character(1)),
    latitude = vapply(stns, function(x) as.numeric(x$lat %||% NA), numeric(1)),
    longitude = vapply(stns, function(x) as.numeric(x$lng %||% NA), numeric(1))
  )
}

#' Get api.tidesandcurrents.noaa.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.tidesandcurrents.noaa.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.tidesandcurrents.noaa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.tidesandcurrents.noaa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.tidesandcurrents.noaa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
