#' Fetch GDACS disaster events
#'
#' Query the GDACS event list API for natural disasters including
#' earthquakes, tropical cyclones, floods, volcanoes, droughts, and wildfires.
#'
#' @param type Event type code: "EQ" (earthquake), "TC" (tropical cyclone),
#'   "FL" (flood), "VO" (volcano), "DR" (drought), "WF" (wildfire).
#'   Multiple types can be comma-separated (e.g. "EQ,TC").
#' @param from_date Start date (Date or "YYYY-MM-DD")
#' @param to_date End date (Date or "YYYY-MM-DD")
#' @param alert_level Filter by alert level: "Green", "Orange", "Red"
#' @param limit Max results (default 100)
#' @return tibble: event_id, event_type, name, country, alert_level,
#'   severity, date, lat, lng, url
#' @export
gdacs_events <- function(type = "EQ", from_date = NULL, to_date = NULL,
                         alert_level = NULL, limit = 100) {
  if (is.null(from_date)) from_date <- Sys.Date() - 30
  if (is.null(to_date)) to_date <- Sys.Date()
  url <- sprintf(
    "%s/geteventlist/SEARCH?eventlist=%s&fromDate=%s&toDate=%s&alertlevel=%s&limit=%d",
    .gdacs_base, type,
    format(as.Date(from_date), "%Y-%m-%d"),
    format(as.Date(to_date), "%Y-%m-%d"),
    alert_level %||% "",
    as.integer(limit)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_events)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_events)

  rows <- lapply(features, function(f) {
    p <- f$properties %||% list()
    g <- f$geometry %||% list()
    coords <- g$coordinates %||% list(NA, NA)
    tibble(
      event_id = as.character(p$eventid %||% p$eventId %||% NA),
      event_type = as.character(p$eventtype %||% p$eventType %||% NA),
      name = as.character(p$name %||% p$eventname %||% NA),
      country = as.character(p$country %||% NA),
      alert_level = as.character(p$alertlevel %||% NA),
      severity = as.character(p$severitydata %||% p$severity %||% NA),
      date = tryCatch(
        as.POSIXct(p$fromdate %||% p$eventDate %||% NA, tz = "UTC"),
        error = function(e) as.POSIXct(NA)
      ),
      lat = as.numeric(if (length(coords) >= 2) coords[[2]] else NA),
      lng = as.numeric(if (length(coords) >= 1) coords[[1]] else NA),
      url = as.character(p$url %||% p$link %||% NA)
    )
  })
  result <- bind_rows(rows)
  if (nrow(result) > limit) result <- result[seq_len(limit), ]
  result
}

# == Context ===================================================================

#' Generate LLM-friendly context for the gdacs.org package
#'
#' @return Character string (invisibly), also printed
#' @export
gdacs_context <- function() {
  .build_context("gdacs.org", header_lines = c(
    "# gdacs.org - GDACS Disaster Alert API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://www.gdacs.org/gdacsapi/api/events",
    "# All functions return tibbles with typed columns.",
    "# Event types: EQ (earthquake), TC (tropical cyclone), FL (flood),",
    "#   VO (volcano), DR (drought), WF (wildfire)",
    "# Alert levels: Green, Orange, Red"
  ))
}
