# tfl.R
# Self-contained Transport for London API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (open data)
# Rate limits: 500 requests/minute

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tfl_base <- "https://api.tfl.gov.uk"

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

# == Public functions ==========================================================

#' Get Transport for London lines with live status
#'
#' Retrieve all TfL lines for a given transport mode along with their current
#' service status (good service, delays, closures, etc.). Provides real-time
#' disruption information.
#'
#' @param mode Transport mode string: \code{"tube"}, \code{"bus"}, \code{"dlr"},
#'   \code{"overground"}, \code{"elizabeth-line"}, \code{"tram"}, or
#'   \code{"cable-car"} (default \code{"tube"}).
#' @return A tibble with one row per line:
#'   \describe{
#'     \item{id}{\code{character} -- Line identifier (e.g. \code{"victoria"}, \code{"central"}).}
#'     \item{name}{\code{character} -- Display name.}
#'     \item{mode}{\code{character} -- Transport mode.}
#'     \item{status}{\code{character} -- Service status (e.g. \code{"Good Service"}, \code{"Minor Delays"}).}
#'     \item{reason}{\code{character} -- Disruption reason (NA when service is good).}
#'   }
#' @examples
#' \dontrun{
#' tfl_lines("tube")
#' tfl_lines("dlr")
#' }
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

#' Get live arrivals for a TfL line or stop
#'
#' Retrieve real-time arrival predictions for a specific Tube/DLR/rail line
#' or a specific station. Returns upcoming arrivals sorted by expected time.
#'
#' @param line_id Line identifier (e.g. \code{"victoria"}, \code{"central"},
#'   \code{"northern"}). Obtain from the \code{id} column of \code{tfl_lines()}.
#' @param stop_id Optional NAPTAN stop/station ID to filter arrivals to a
#'   single station. Obtain from the \code{id} column of \code{tfl_stops()}.
#' @return A tibble with one row per predicted arrival:
#'   \describe{
#'     \item{id}{\code{character} -- Arrival prediction ID.}
#'     \item{line_name}{\code{character} -- Line display name.}
#'     \item{station_name}{\code{character} -- Station name.}
#'     \item{destination}{\code{character} -- Destination station name.}
#'     \item{towards}{\code{character} -- Direction of travel description.}
#'     \item{expected_arrival}{\code{POSIXct} -- Predicted arrival time (UTC).}
#'     \item{time_to_station}{\code{integer} -- Seconds until arrival.}
#'     \item{platform_name}{\code{character} -- Platform name.}
#'     \item{direction}{\code{character} -- Direction (e.g. \code{"inbound"}, \code{"outbound"}).}
#'   }
#' @examples
#' \dontrun{
#' tfl_arrivals("victoria")
#' tfl_arrivals("northern", stop_id = "940GZZLUKSX")
#' }
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

#' Search TfL stops and stations
#'
#' Search for Transport for London stops and stations by name. Returns
#' matching locations with their coordinates and supported transport modes.
#'
#' @param query Search term (e.g. \code{"Kings Cross"}, \code{"Victoria"},
#'   \code{"Paddington"}).
#' @return A tibble with one row per matching stop:
#'   \describe{
#'     \item{id}{\code{character} -- NAPTAN stop ID (pass to \code{tfl_arrivals()}).}
#'     \item{name}{\code{character} -- Stop/station name.}
#'     \item{lat}{\code{numeric} -- Latitude.}
#'     \item{lon}{\code{numeric} -- Longitude.}
#'     \item{modes}{\code{character} -- Transport modes, comma-separated.}
#'   }
#' @examples
#' \dontrun{
#' tfl_stops("Kings Cross")
#' tfl_stops("Victoria")
#' }
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

#' Get TfL client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' tfl_context()
#' }
#' @export
tfl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(tfl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/tfl.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "tfl")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# tfl context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# tfl", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
