# == Public functions ==========================================================

#' Get real-time departures from a BART station
#'
#' @param station Station abbreviation (e.g. "MONT", "EMBR", "12TH").
#'   Use bart_stations() to see all abbreviations.
#' @return tibble: station, destination, minutes, platform, direction,
#'   length, color, delay
#' @export
bart_departures <- function(station) {
  url <- sprintf(
    "%s/etd.aspx?cmd=etd&orig=%s&json=y&key=%s",
    .bart_base, toupper(station), .bart_key
  )
  raw <- .fetch_json(url)
  root <- raw$root
  if (is.null(root)) return(.schema_departures)
  stations <- root$station
  if (is.null(stations) || length(stations) == 0) return(.schema_departures)

  # stations is typically a list/df with one entry
  sta <- if (is.data.frame(stations)) stations else stations[[1]]
  etd <- sta$etd
  if (is.null(etd)) return(.schema_departures)

  # etd may be a list of destinations
  if (is.data.frame(etd)) {
    rows <- lapply(seq_len(nrow(etd)), function(i) {
      est <- etd$estimate[[i]]
      if (!is.data.frame(est)) return(NULL)
      tibble(
        station = toupper(station),
        destination = as.character(etd$destination[i]),
        minutes = as.character(est$minutes),
        platform = as.integer(est$platform),
        direction = as.character(est$direction),
        length = as.integer(est$length),
        color = as.character(est$color),
        delay = as.character(est$delay %||% NA_character_)
      )
    })
    bind_rows(rows)
  } else {
    .schema_departures
  }
}

#' List all BART stations
#'
#' @return tibble: abbr, name, city, county, state, latitude, longitude,
#'   address, zipcode
#' @export
bart_stations <- function() {
  url <- sprintf("%s/stn.aspx?cmd=stns&json=y&key=%s", .bart_base, .bart_key)
  raw <- .fetch_json(url)
  stations <- raw$root$stations$station
  if (is.null(stations) || length(stations) == 0 ||
      (is.data.frame(stations) && nrow(stations) == 0)) return(.schema_stations)

  tibble(
    abbr = as.character(stations$abbr),
    name = as.character(stations$name),
    city = as.character(stations$city),
    county = as.character(stations$county),
    state = as.character(stations$state),
    latitude = as.numeric(stations$gtfs_latitude),
    longitude = as.numeric(stations$gtfs_longitude),
    address = as.character(stations$address),
    zipcode = as.character(stations$zipcode)
  )
}

#' List all BART routes
#'
#' @return tibble: number, name, abbr, color, holidays, num_stations
#' @export
bart_routes <- function() {
  url <- sprintf("%s/route.aspx?cmd=routes&json=y&key=%s", .bart_base, .bart_key)
  raw <- .fetch_json(url)
  routes <- raw$root$routes$route
  if (is.null(routes) || length(routes) == 0 ||
      (is.data.frame(routes) && nrow(routes) == 0)) return(.schema_routes)

  tibble(
    number = as.character(routes$number),
    name = as.character(routes$name),
    abbr = as.character(routes$abbr),
    color = as.character(routes$color),
    holidays = as.character(routes$holidays %||% NA_character_),
    num_stations = as.integer(routes$num_stns %||% NA_integer_)
  )
}

#' Show BART client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
bart_context <- function() {
  .build_context(
    pkg_name = "api.bart.gov",
    header_lines = c(
      "# api.bart.gov -- BART Transit API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: Public demo key included (MW9S-E7SL-26DU-VV8V)",
      "# Popular stations: MONT (Montgomery), EMBR (Embarcadero), 12TH (12th St Oakland)",
      "#   CIVC (Civic Center), POWL (Powell), DALY (Daly City), FRMT (Fremont)"
    )
  )
}

