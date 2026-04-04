# opensky.R
# Self-contained OpenSky Network API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (anonymous access has lower rate limits)
# Rate limits: ~100 req/day anonymous, higher with account

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.opensky_base <- "https://opensky-network.org/api"

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

.schema_states <- tibble(
  icao24 = character(), callsign = character(), origin_country = character(),
  longitude = numeric(), latitude = numeric(), baro_altitude = numeric(),
  velocity = numeric(), true_track = numeric(), on_ground = logical(),
  geo_altitude = numeric()
)

.schema_flights <- tibble(
  icao24 = character(), callsign = character(),
  departure_airport = character(), arrival_airport = character(),
  first_seen = as.POSIXct(character()), last_seen = as.POSIXct(character())
)

# == Public functions ==========================================================

#' Get current aircraft states from OpenSky Network
#'
#' Fetch real-time positions and velocities of all aircraft currently tracked
#' by the OpenSky Network. Optionally restrict to a geographic bounding box
#' to reduce result size. Anonymous access is limited to ~100 requests/day.
#'
#' @param bbox Optional geographic bounding box as a named list with keys
#'   \code{lamin}, \code{lomin}, \code{lamax}, \code{lomax} (latitude/longitude
#'   in decimal degrees). Example: \code{list(lamin = 47, lomin = 8, lamax = 48,
#'   lomax = 9)} for a region in Switzerland.
#' @return A tibble with one row per aircraft:
#'   \describe{
#'     \item{icao24}{\code{character} -- ICAO 24-bit transponder address (hex).}
#'     \item{callsign}{\code{character} -- Flight callsign (may be blank).}
#'     \item{origin_country}{\code{character} -- Country of aircraft registration.}
#'     \item{longitude}{\code{numeric} -- Longitude in decimal degrees.}
#'     \item{latitude}{\code{numeric} -- Latitude in decimal degrees.}
#'     \item{baro_altitude}{\code{numeric} -- Barometric altitude in meters.}
#'     \item{velocity}{\code{numeric} -- Ground speed in m/s.}
#'     \item{true_track}{\code{numeric} -- True track angle in degrees (clockwise from north).}
#'     \item{on_ground}{\code{logical} -- Whether the aircraft is on the ground.}
#'     \item{geo_altitude}{\code{numeric} -- Geometric altitude in meters.}
#'   }
#' @examples
#' \dontrun{
#' # All aircraft (large result)
#' opensky_states()
#'
#' # Aircraft over Switzerland
#' opensky_states(bbox = list(lamin = 45.8, lomin = 5.9, lamax = 47.8, lomax = 10.5))
#' }
#' @export
opensky_states <- function(bbox = NULL) {
  url <- paste0(.opensky_base, "/states/all")
  if (!is.null(bbox)) {
    qstr <- paste(names(bbox), bbox, sep = "=", collapse = "&")
    url <- paste0(url, "?", qstr)
  }
  raw <- .fetch_json(url)
  states <- raw$states
  if (is.null(states) || length(states) == 0) return(.schema_states)

  # states is a matrix — each row is a state vector
  if (is.matrix(states)) {
    df <- as.data.frame(states, stringsAsFactors = FALSE)
  } else if (is.list(states) && !is.data.frame(states)) {
    df <- as.data.frame(do.call(rbind, states), stringsAsFactors = FALSE)
  } else {
    return(.schema_states)
  }

  tibble(
    icao24 = as.character(df$V1),
    callsign = trimws(as.character(df$V2)),
    origin_country = as.character(df$V3),
    longitude = as.numeric(df$V6),
    latitude = as.numeric(df$V7),
    baro_altitude = as.numeric(df$V8),
    velocity = as.numeric(df$V10),
    true_track = as.numeric(df$V11),
    on_ground = as.logical(df$V9),
    geo_altitude = as.numeric(df$V14)
  )
}

#' Get flights within a time interval
#'
#' Retrieve all flights seen by the OpenSky Network in a given time window.
#' The interval must be at most 2 hours. Timestamps are Unix epoch seconds.
#'
#' @param begin Start of time interval as a Unix timestamp (integer seconds
#'   since 1970-01-01). Example: \code{as.integer(Sys.time()) - 7200}.
#' @param end End of time interval as a Unix timestamp (integer). The
#'   difference \code{end - begin} must be <= 7200 (2 hours).
#' @return A tibble with one row per flight:
#'   \describe{
#'     \item{icao24}{\code{character} -- ICAO 24-bit transponder address (hex).}
#'     \item{callsign}{\code{character} -- Flight callsign.}
#'     \item{departure_airport}{\code{character} -- Estimated departure airport ICAO code.}
#'     \item{arrival_airport}{\code{character} -- Estimated arrival airport ICAO code.}
#'     \item{first_seen}{\code{POSIXct} -- First ADS-B message timestamp (UTC).}
#'     \item{last_seen}{\code{POSIXct} -- Last ADS-B message timestamp (UTC).}
#'   }
#' @examples
#' \dontrun{
#' now <- as.integer(Sys.time())
#' opensky_flights(begin = now - 7200, end = now)
#' }
#' @export
opensky_flights <- function(begin, end) {
  url <- sprintf("%s/flights/all?begin=%d&end=%d",
                 .opensky_base, as.integer(begin), as.integer(end))
  raw <- .fetch_json(url)
  if (length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) return(.schema_flights)

  tibble(
    icao24 = as.character(raw$icao24),
    callsign = trimws(as.character(raw$callsign %||% NA_character_)),
    departure_airport = as.character(raw$estDepartureAirport %||% NA_character_),
    arrival_airport = as.character(raw$estArrivalAirport %||% NA_character_),
    first_seen = as.POSIXct(as.numeric(raw$firstSeen), origin = "1970-01-01"),
    last_seen = as.POSIXct(as.numeric(raw$lastSeen), origin = "1970-01-01")
  )
}

#' Get OpenSky client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' opensky_context()
#' }
#' @export
opensky_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(opensky_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/opensky.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "opensky")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# opensky context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# opensky", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
