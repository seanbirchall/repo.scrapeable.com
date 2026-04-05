# opensky.network.org.R - Self-contained opensky.network.org client



# opensky.R
# Self-contained OpenSky Network API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (anonymous access has lower rate limits)
# Rate limits: ~100 req/day anonymous, higher with account


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.opensky_base <- "https://opensky-network.org/api"

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


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Get current aircraft states from OpenSky Network
#'
#' Retrieves real-time position and velocity data for all aircraft currently
#' tracked by the OpenSky Network. Use the bounding box to limit results to
#' a geographic region and reduce response size.
#'
#' @param bbox Named list. Optional geographic bounding box with elements
#'   \code{lamin}, \code{lomin}, \code{lamax}, \code{lomax} (latitude/longitude
#'   in degrees). Example: \code{list(lamin = 45, lomin = 5, lamax = 55, lomax = 15)}
#'   for central Europe. Default NULL returns all aircraft worldwide.
#' @return A tibble with columns:
#'   \describe{
#'     \item{icao24}{character -- ICAO 24-bit transponder address (hex)}
#'     \item{callsign}{character -- aircraft callsign (may be blank)}
#'     \item{origin_country}{character -- country of registration}
#'     \item{longitude}{numeric -- WGS84 longitude in degrees}
#'     \item{latitude}{numeric -- WGS84 latitude in degrees}
#'     \item{baro_altitude}{numeric -- barometric altitude in meters}
#'     \item{velocity}{numeric -- ground speed in m/s}
#'     \item{true_track}{numeric -- heading in degrees clockwise from north}
#'     \item{on_ground}{logical -- TRUE if aircraft is on the ground}
#'     \item{geo_altitude}{numeric -- geometric (GPS) altitude in meters}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # All aircraft over Switzerland
#' opensky_states(bbox = list(lamin = 45.8, lomin = 5.9, lamax = 47.8, lomax = 10.5))
#' }
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
#' Retrieves all flights observed by the OpenSky Network during a time window.
#' The interval must be at most 2 hours. Timestamps are Unix epoch seconds.
#'
#' @param begin Integer. Start of the time interval as a Unix timestamp
#'   (seconds since 1970-01-01). Use \code{as.integer(Sys.time()) - 7200} for
#'   2 hours ago.
#' @param end Integer. End of the time interval as a Unix timestamp. Must
#'   satisfy \code{end - begin <= 7200} (2 hours).
#' @return A tibble with columns:
#'   \describe{
#'     \item{icao24}{character -- ICAO 24-bit transponder address (hex)}
#'     \item{callsign}{character -- aircraft callsign}
#'     \item{departure_airport}{character -- estimated ICAO departure airport code}
#'     \item{arrival_airport}{character -- estimated ICAO arrival airport code}
#'     \item{first_seen}{POSIXct -- first ADS-B message timestamp}
#'     \item{last_seen}{POSIXct -- last ADS-B message timestamp}
#'   }
#' @export
#' @examples
#' \dontrun{
#' t2 <- as.integer(Sys.time())
#' t1 <- t2 - 3600
#' opensky_flights(t1, t2)
#' }
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

#' Get flights for a specific aircraft by ICAO24 address
#'
#' Retrieves the flight history for a single aircraft identified by its
#' ICAO24 transponder address. The time interval can span up to 30 days.
#'
#' @param icao24 Character. ICAO 24-bit transponder address as a lowercase
#'   hex string (e.g., \code{"3c6444"}, \code{"a1b2c3"}).
#' @param begin Integer. Start of the time interval as a Unix timestamp.
#' @param end Integer. End of the time interval as a Unix timestamp.
#'   Must satisfy \code{end - begin <= 2592000} (30 days).
#' @return A tibble with columns:
#'   \describe{
#'     \item{icao24}{character -- ICAO 24-bit transponder address (hex)}
#'     \item{callsign}{character -- aircraft callsign}
#'     \item{departure_airport}{character -- estimated ICAO departure airport code}
#'     \item{arrival_airport}{character -- estimated ICAO arrival airport code}
#'     \item{first_seen}{POSIXct -- first ADS-B message timestamp}
#'     \item{last_seen}{POSIXct -- last ADS-B message timestamp}
#'   }
#' @export
#' @examples
#' \dontrun{
#' t2 <- as.integer(Sys.time())
#' t1 <- t2 - 86400
#' opensky_aircraft("3c6444", t1, t2)
#' }
opensky_aircraft <- function(icao24, begin, end) {
  url <- sprintf("%s/flights/aircraft?icao24=%s&begin=%d&end=%d",
                 .opensky_base, icao24, as.integer(begin), as.integer(end))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 ||
      (is.data.frame(raw) && nrow(raw) == 0)) return(.schema_flights)

  tibble(
    icao24 = as.character(raw$icao24),
    callsign = trimws(as.character(raw$callsign %||% NA_character_)),
    departure_airport = as.character(raw$estDepartureAirport %||% NA_character_),
    arrival_airport = as.character(raw$estArrivalAirport %||% NA_character_),
    first_seen = as.POSIXct(as.numeric(raw$firstSeen), origin = "1970-01-01"),
    last_seen = as.POSIXct(as.numeric(raw$lastSeen), origin = "1970-01-01")
  )
}

# == Context ===================================================================

#' Get opensky.network.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
opensky_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(opensky_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/opensky.network.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "opensky.network.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# opensky.network.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# opensky.network.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
