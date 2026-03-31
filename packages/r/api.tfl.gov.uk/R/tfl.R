# == Public functions ==========================================================

#' Get TfL lines by mode
#'
#' @param mode Transport mode: "tube", "bus", "dlr", "overground",
#'   "elizabeth-line", "tram", "cable-car" (default "tube")
#' @return tibble: id, name, mode, status, reason
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
#' @param line_id Line ID (e.g. "victoria", "central", "northern")
#' @param stop_id Optional stop/station NAPTAN ID to filter arrivals
#' @return tibble: id, line_name, station_name, destination, towards,
#'   expected_arrival, time_to_station, platform_name, direction
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

#' Search TfL stops/stations
#'
#' @param query Search term (e.g. "Kings Cross", "Victoria")
#' @return tibble: id, name, lat, lon, modes
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

#' Show TfL client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
tfl_context <- function() {
  .build_context(
    pkg_name = "api.tfl.gov.uk",
    header_lines = c(
      "# api.tfl.gov.uk -- Transport for London API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Modes: tube, bus, dlr, overground, elizabeth-line, tram, cable-car",
      "# Popular tube lines: victoria, central, northern, jubilee, piccadilly, district"
    )
  )
}

