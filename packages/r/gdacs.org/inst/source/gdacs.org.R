# gdacs.org.R - Self-contained gdacs.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)


# gdacs-org.R
# Self-contained GDACS (Global Disaster Alert and Coordination System) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gdacs_base <- "https://www.gdacs.org/gdacsapi/api/events"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_events <- tibble(
  event_id = character(), event_type = character(), name = character(),
  country = character(), alert_level = character(),
  severity = character(), date = as.POSIXct(character()),
  lat = numeric(), lng = numeric(), url = character()
)

# == Events ====================================================================


#' Fetch GDACS disaster events
#'
#' Query the GDACS (Global Disaster Alert and Coordination System) event list
#' API for natural disasters including earthquakes, tropical cyclones, floods,
#' volcanoes, droughts, and wildfires. Returns a tibble with one row per event,
#' including geographic coordinates, alert levels, and severity information.
#'
#' @param type Character. Event type code: \code{"EQ"} (earthquake),
#'   \code{"TC"} (tropical cyclone), \code{"FL"} (flood), \code{"VO"} (volcano),
#'   \code{"DR"} (drought), \code{"WF"} (wildfire). Multiple types can be
#'   comma-separated (e.g. \code{"EQ,TC"}). Default \code{"EQ"}.
#' @param from_date Start date as Date object or \code{"YYYY-MM-DD"} string.
#'   Defaults to 30 days ago.
#' @param to_date End date as Date object or \code{"YYYY-MM-DD"} string.
#'   Defaults to today.
#' @param alert_level Character. Filter by alert level: \code{"Green"},
#'   \code{"Orange"}, or \code{"Red"}. \code{NULL} for all levels (default).
#' @param limit Integer. Maximum number of results to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{event_id}{Character. Unique GDACS event identifier.}
#'     \item{event_type}{Character. Event type code (EQ, TC, FL, etc.).}
#'     \item{name}{Character. Descriptive event name.}
#'     \item{country}{Character. Affected country.}
#'     \item{alert_level}{Character. Green, Orange, or Red.}
#'     \item{severity}{Character. Severity data string (e.g. magnitude).}
#'     \item{date}{POSIXct. Event date/time in UTC.}
#'     \item{lat}{Numeric. Latitude of the event.}
#'     \item{lng}{Numeric. Longitude of the event.}
#'     \item{url}{Character. URL to the event detail page.}
#'   }
#' @export
#' @family gdacs functions
#' @seealso [gdacs_event()] for a single event, [gdacs_summary()] for counts
#' @examples
#' \dontrun{
#' # Recent earthquakes
#' gdacs_events(type = "EQ", limit = 10)
#'
#' # Floods in the last 7 days
#' gdacs_events(type = "FL", from_date = Sys.Date() - 7)
#'
#' # Red-alert events of all types
#' gdacs_events(type = "EQ,TC,FL,VO,DR,WF", alert_level = "Red")
#' }
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

#' Get a single GDACS event by event ID and type
#'
#' Retrieve detailed information for a specific GDACS disaster event using
#' its unique event ID and type code. Returns a single-row tibble with the
#' same schema as \code{\link{gdacs_events}}.
#'
#' @param event_id Character or numeric. The GDACS event ID (e.g. \code{"1000001"}).
#' @param event_type Character. Event type code: \code{"EQ"}, \code{"TC"},
#'   \code{"FL"}, \code{"VO"}, \code{"DR"}, or \code{"WF"}. Default \code{"EQ"}.
#' @return A single-row tibble with the same columns as \code{\link{gdacs_events}}:
#'   event_id, event_type, name, country, alert_level, severity, date, lat, lng, url.
#' @export
#' @family gdacs functions
#' @seealso [gdacs_events()] to search for events
#' @examples
#' \dontrun{
#' gdacs_event("1000001", event_type = "EQ")
#' }
gdacs_event <- function(event_id, event_type = "EQ") {
  url <- sprintf(
    "%s/geteventdata?eventtype=%s&eventid=%s",
    .gdacs_base, event_type, as.character(event_id)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_events)

  # Try GeoJSON feature format
  p <- raw$properties %||% raw$Properties %||% list()
  g <- raw$geometry %||% raw$Geometry %||% list()

  if (length(p) == 0 && !is.null(raw$features) && length(raw$features) > 0) {
    p <- raw$features[[1]]$properties %||% list()
    g <- raw$features[[1]]$geometry %||% list()
  }

  if (length(p) == 0) return(.schema_events)

  coords <- g$coordinates %||% list(NA, NA)
  tibble(
    event_id = as.character(p$eventid %||% p$eventId %||% event_id),
    event_type = as.character(p$eventtype %||% p$eventType %||% event_type),
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
}

#' Get GDACS events summary by type
#'
#' Returns a summary count of recent disaster events grouped by event type
#' and alert level. Internally queries all six event types (EQ, TC, FL, VO,
#' DR, WF) and aggregates the results. Useful for a quick situational
#' overview of global disaster activity.
#'
#' @param days Integer. Number of days to look back from today (default 30).
#' @return A tibble with columns:
#'   \describe{
#'     \item{event_type}{Character. Event type code (EQ, TC, FL, etc.).}
#'     \item{alert_level}{Character. Alert level (Green, Orange, Red).}
#'     \item{count}{Integer. Number of events in this group.}
#'   }
#' @export
#' @family gdacs functions
#' @seealso [gdacs_events()] for full event listings
#' @examples
#' \dontrun{
#' # Last 30 days summary
#' gdacs_summary()
#'
#' # Last 7 days summary
#' gdacs_summary(days = 7)
#' }
gdacs_summary <- function(days = 30) {
  from_date <- Sys.Date() - days
  to_date <- Sys.Date()
  types <- c("EQ", "TC", "FL", "VO", "DR", "WF")

  rows <- lapply(types, function(t) {
    events <- tryCatch(
      gdacs_events(type = t, from_date = from_date, to_date = to_date, limit = 500),
      error = function(e) .schema_events
    )
    if (nrow(events) == 0) return(NULL)
    events |>
      group_by(event_type, alert_level) |>
      summarise(count = n(), .groups = "drop")
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(tibble(event_type = character(), alert_level = character(), count = integer()))
  }
  bind_rows(rows)
}

# == Context ===================================================================

#' Get gdacs.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gdacs_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gdacs_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gdacs.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gdacs.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gdacs.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gdacs.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
