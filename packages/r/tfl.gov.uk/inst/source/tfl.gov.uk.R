# api.tfl.gov.uk.R - Self-contained api.tfl.gov.uk client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# tfl.R
# Self-contained Transport for London API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (open data)
# Rate limits: 500 requests/minute


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tfl_base <- "https://api.tfl.gov.uk"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_lines <- tibble(
  id = character(), name = character(), mode = character(),
  status = character(), reason = character()
)

.schema_arrivals <- tibble(
  id = character(), line_name = character(), station_name = character(),
  destination = character(), towards = character(),
  expected_arrival = as.POSIXct(character()), time_to_station = integer(),
  platform_name = character(), direction = character()
)

.schema_stops <- tibble(
  id = character(), name = character(), lat = numeric(),
  lon = numeric(), modes = character()
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Get Transport for London lines and their live status
#'
#' Returns all TfL lines for a given transport mode with their current
#' service status. Useful for checking disruptions, closures, and delays.
#'
#' @param mode Character. Transport mode to query. One of \code{"tube"},
#'   \code{"bus"}, \code{"dlr"}, \code{"overground"}, \code{"elizabeth-line"},
#'   \code{"tram"}, or \code{"cable-car"} (default \code{"tube"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Line identifier (e.g. \code{"victoria"}, \code{"central"}).}
#'     \item{name}{Character. Human-readable line name.}
#'     \item{mode}{Character. Transport mode (e.g. \code{"tube"}).}
#'     \item{status}{Character. Current status such as \code{"Good Service"},
#'       \code{"Minor Delays"}, \code{"Part Closure"}, or \code{"Planned Closure"}.}
#'     \item{reason}{Character. Disruption reason text, or \code{NA} when service is good.}
#'   }
#' @examples
#' tfl_lines()
#' tfl_lines("dlr")
#' tfl_lines("elizabeth-line")
#' @export
tfl_lines <- function(mode = "tube") {
  url <- sprintf("%s/Line/Mode/%s/Status", .tfl_base, mode)
  raw <- jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
  if (length(raw) == 0) return(.schema_lines)

  rows <- lapply(raw, function(line) {
    st <- if (length(line$lineStatuses) > 0) line$lineStatuses[[1]] else list()
    tibble(
      id = as.character(line$id),
      name = as.character(line$name),
      mode = as.character(line$modeName),
      status = as.character(st$statusSeverityDescription %||% NA_character_),
      reason = as.character(st$reason %||% NA_character_)
    )
  })
  bind_rows(rows)
}

#' Get live arrival predictions for a TfL line or stop
#'
#' Returns real-time arrival predictions for all stations on a line, or
#' for a specific stop if \code{stop_id} is given. Data refreshes every
#' 30 seconds from TfL's prediction system.
#'
#' @param line_id Character. Line identifier (e.g. \code{"victoria"},
#'   \code{"central"}, \code{"northern"}). Use \code{tfl_lines()} to
#'   discover valid IDs.
#' @param stop_id Character or \code{NULL}. Optional NAPTAN stop/station ID
#'   to restrict arrivals to a single station (e.g. \code{"940GZZLUKSX"}).
#'   Use \code{tfl_stops()} to find IDs.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Unique arrival prediction ID.}
#'     \item{line_name}{Character. Line name (e.g. \code{"Victoria"}).}
#'     \item{station_name}{Character. Station name.}
#'     \item{destination}{Character. Destination station name.}
#'     \item{towards}{Character. Direction of travel description.}
#'     \item{expected_arrival}{POSIXct. Predicted arrival time (UTC).}
#'     \item{time_to_station}{Integer. Seconds until arrival at station.}
#'     \item{platform_name}{Character. Platform name/number.}
#'     \item{direction}{Character. Direction (e.g. \code{"inbound"}, \code{"outbound"}).}
#'   }
#' @examples
#' tfl_arrivals("victoria")
#' tfl_arrivals("northern", stop_id = "940GZZLUKSX")
#' @export
tfl_arrivals <- function(line_id, stop_id = NULL) {
  if (!is.null(stop_id)) {
    url <- sprintf("%s/Line/%s/Arrivals/%s", .tfl_base, line_id, stop_id)
  } else {
    url <- sprintf("%s/Line/%s/Arrivals", .tfl_base, line_id)
  }
  raw <- .fetch_json(url)
  if (length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) return(.schema_arrivals)

  tibble(
    id = as.character(raw$id),
    line_name = as.character(raw$lineName),
    station_name = as.character(raw$stationName),
    destination = as.character(raw$destinationName),
    towards = as.character(raw$towards %||% NA_character_),
    expected_arrival = as.POSIXct(raw$expectedArrival, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    time_to_station = as.integer(raw$timeToStation),
    platform_name = as.character(raw$platformName %||% NA_character_),
    direction = as.character(raw$direction %||% NA_character_)
  )
}

#' Search for TfL stops and stations by name
#'
#' Performs a free-text search across all TfL stop points including Tube
#' stations, bus stops, DLR stations, and rail interchanges. Returns
#' geographic coordinates and supported transport modes for each match.
#'
#' @param query Character. Search term (e.g. \code{"Kings Cross"},
#'   \code{"Victoria"}, \code{"Paddington"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. NAPTAN stop ID (use with \code{tfl_arrivals()}).}
#'     \item{name}{Character. Stop/station name.}
#'     \item{lat}{Numeric. Latitude (WGS84).}
#'     \item{lon}{Numeric. Longitude (WGS84).}
#'     \item{modes}{Character. Comma-separated transport modes available
#'       (e.g. \code{"bus, tube, national-rail"}).}
#'   }
#' @examples
#' tfl_stops("Kings Cross")
#' tfl_stops("Canary Wharf")
#' @export
tfl_stops <- function(query) {
  url <- sprintf("%s/StopPoint/Search/%s", .tfl_base,
                 utils::URLencode(query, reserved = TRUE))
  raw <- .fetch_json(url)
  matches <- raw$matches
  if (is.null(matches) || length(matches) == 0 ||
      (is.data.frame(matches) && nrow(matches) == 0)) return(.schema_stops)

  tibble(
    id = as.character(matches$id),
    name = as.character(matches$name),
    lat = as.numeric(matches$lat),
    lon = as.numeric(matches$lon),
    modes = vapply(matches$modes, function(m) paste(m, collapse = ", "), character(1))
  )
}

# == Context ===================================================================

#' Get tfl.gov.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
tfl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(tfl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/tfl.gov.uk.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "tfl.gov.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# tfl.gov.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# tfl.gov.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
