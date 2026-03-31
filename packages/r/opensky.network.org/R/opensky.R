# == Public functions ==========================================================

#' Get current aircraft states from OpenSky Network
#'
#' @param bbox Optional bounding box as named list: list(lamin, lomin, lamax, lomax).
#'   Example: list(lamin=45, lomin=5, lamax=55, lomax=15) for central Europe.
#' @return tibble: icao24, callsign, origin_country, longitude, latitude,
#'   baro_altitude, velocity, true_track, on_ground, geo_altitude
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
#' @param begin Start of time interval as Unix timestamp (integer)
#' @param end End of time interval as Unix timestamp (integer).
#'   Interval must be <= 2 hours.
#' @return tibble: icao24, callsign, departure_airport, arrival_airport,
#'   first_seen, last_seen
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

#' Show OpenSky client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
opensky_context <- function() {
  .build_context(
    pkg_name = "opensky.network.org",
    header_lines = c(
      "# opensky.network.org -- OpenSky Network Flight Data Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none for anonymous (limited); account gives higher limits",
      "# Bounding boxes: list(lamin=45, lomin=5, lamax=55, lomax=15) for Europe",
      "# Time intervals for flights: max 2 hours, Unix timestamps"
    )
  )
}

