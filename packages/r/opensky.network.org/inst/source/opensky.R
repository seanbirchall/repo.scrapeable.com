


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

# == Context ===================================================================

#' Generate LLM-friendly context for opensky.network.org
#'
#' @return Character string with full function signatures and bodies
#' @export
opensky_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/opensky.network.org.R"
  if (!file.exists(src_file)) {
    cat("# opensky.network.org context - source not found\n")
    return(invisible("# opensky.network.org context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

