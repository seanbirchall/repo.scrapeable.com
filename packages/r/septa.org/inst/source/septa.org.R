# septa.org.R
# Self-contained SEPTA (Southeastern Pennsylvania Transportation Authority) client.
# Real-time transit data for Philadelphia region.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www3.septa.org/

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.septa_base <- "https://www3.septa.org/api"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

# == Schemas ===================================================================

.schema_trains <- tibble(
  train_number = character(), service = character(), destination = character(),
  line = character(), source = character(), current_stop = character(),
  next_stop = character(), latitude = numeric(), longitude = numeric(),
  late = integer(), track = character()
)

.schema_buses <- tibble(
  vehicle_id = character(), route = character(), direction = character(),
  destination = character(), latitude = numeric(), longitude = numeric(),
  late = integer(), next_stop_name = character(), trip = character(),
  seat_availability = character()
)

.schema_alerts <- tibble(
  route = character(), route_name = character(), mode = character(),
  is_advisory = character(), is_detour = character(), is_alert = character(),
  is_suspended = character(), is_delays = character(), last_updated = character()
)

# == Public functions ==========================================================

#' Get real-time regional rail train positions
#'
#' Returns current GPS location and status of every active SEPTA regional rail
#' train in the Philadelphia network. Data refreshes every few seconds from the
#' SEPTA TrainView API. Includes lateness in minutes, current and next stops,
#' and track assignment at terminals.
#'
#' @return A tibble with one row per active train:
#' \describe{
#'   \item{train_number}{Character. Train run number (e.g. "2365").}
#'   \item{service}{Character. Service type ("LOCAL" or "EXPRESS").}
#'   \item{destination}{Character. Final destination station name.}
#'   \item{line}{Character. Regional rail line name (e.g. "Paoli/Thorndale").}
#'   \item{source}{Character. Originating station.}
#'   \item{current_stop}{Character. Station the train is currently at or near.}
#'   \item{next_stop}{Character. Next scheduled stop.}
#'   \item{latitude}{Numeric. Current GPS latitude.}
#'   \item{longitude}{Numeric. Current GPS longitude.}
#'   \item{late}{Integer. Minutes behind schedule (0 = on time).}
#'   \item{track}{Character. Track number at terminal station (may be NA en route).}
#' }
#' @export
#' @examples
#' \dontrun{
#' trains <- septa_trains()
#' # Find late trains
#' trains |> dplyr::filter(late > 5)
#' }
septa_trains <- function() {
  url <- paste0(.septa_base, "/TrainView/index.php")
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_trains)
  if (!is.data.frame(raw)) return(.schema_trains)

  tibble(
    train_number = as.character(raw$trainno),
    service = as.character(raw$service),
    destination = as.character(raw$dest),
    line = as.character(raw$line),
    source = as.character(raw$SOURCE),
    current_stop = as.character(raw$currentstop),
    next_stop = as.character(raw$nextstop),
    latitude = as.numeric(raw$lat),
    longitude = as.numeric(raw$lon),
    late = as.integer(raw$late),
    track = as.character(raw$TRACK)
  )
}

#' Get real-time bus/trolley positions for a route
#'
#' Returns the current GPS position and status of all active vehicles on a
#' specific SEPTA bus or trolley route. Data comes from the TransitView API
#' and refreshes in near real-time.
#'
#' @param route Character. Route number or ID (e.g. \code{"33"}, \code{"44"},
#'   \code{"LUCYGR"} for the Lucy Gold Route). Use \code{septa_list()} to
#'   see all routes with active vehicles.
#' @return A tibble with one row per active vehicle:
#' \describe{
#'   \item{vehicle_id}{Character. Unique vehicle identifier.}
#'   \item{route}{Character. Route number.}
#'   \item{direction}{Character. Direction of travel (e.g. "NorthBound").}
#'   \item{destination}{Character. Displayed destination on the vehicle.}
#'   \item{latitude}{Numeric. Current GPS latitude.}
#'   \item{longitude}{Numeric. Current GPS longitude.}
#'   \item{late}{Integer. Minutes behind schedule (0 = on time).}
#'   \item{next_stop_name}{Character. Name of the next scheduled stop.}
#'   \item{trip}{Character. Internal trip identifier.}
#'   \item{seat_availability}{Character. Estimated seat availability level.}
#' }
#' @export
#' @examples
#' \dontrun{
#' buses <- septa_buses("44")
#' }
septa_buses <- function(route) {
  url <- sprintf("%s/TransitView/index.php?route=%s", .septa_base, route)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_buses)

  # Response is a dict with the route as key containing a list of buses
  buses <- raw$bus
  if (is.null(buses)) {
    # Try first element
    if (is.list(raw) && length(raw) > 0) {
      first <- raw[[1]]
      if (is.data.frame(first)) buses <- first
    }
  }
  if (is.null(buses) || !is.data.frame(buses) || nrow(buses) == 0) return(.schema_buses)

  tibble(
    vehicle_id = as.character(buses$VehicleID %||% buses$label),
    route = as.character(buses$route_id %||% route),
    direction = as.character(buses$Direction),
    destination = as.character(buses$destination),
    latitude = as.numeric(buses$lat),
    longitude = as.numeric(buses$lng),
    late = as.integer(buses$late),
    next_stop_name = as.character(buses$next_stop_name %||% NA_character_),
    trip = as.character(buses$trip %||% NA_character_),
    seat_availability = as.character(buses$estimated_seat_availability %||% NA_character_)
  )
}

#' List all SEPTA transit alerts
#'
#' Returns the current alert/advisory/detour/suspension status for every SEPTA
#' route and rail line. Each row indicates whether a route has an active
#' advisory, detour, alert, suspension, or delay. Useful for monitoring
#' system-wide service disruptions.
#'
#' @return A tibble with one row per route/line:
#' \describe{
#'   \item{route}{Character. Route or line identifier.}
#'   \item{route_name}{Character. Human-readable route name.}
#'   \item{mode}{Character. Transit mode ("Bus", "Trolley", "Rail", etc.).}
#'   \item{is_advisory}{Character. "Yes" or "No" for active advisory.}
#'   \item{is_detour}{Character. "Y" or "N" for active detour.}
#'   \item{is_alert}{Character. "Y" or "N" for active alert.}
#'   \item{is_suspended}{Character. "Y" or "N" for suspended service.}
#'   \item{is_delays}{Character. "Y" or "N" for current delays.}
#'   \item{last_updated}{Character. Timestamp of last update.}
#' }
#' @export
#' @examples
#' \dontrun{
#' alerts <- septa_alerts()
#' # Routes with active alerts
#' alerts |> dplyr::filter(is_alert == "Y")
#' }
septa_alerts <- function() {
  url <- paste0(.septa_base, "/Alerts/index.php")
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_alerts)
  if (!is.data.frame(raw)) return(.schema_alerts)

  tibble(
    route = as.character(raw$route),
    route_name = as.character(raw$route_name),
    mode = as.character(raw$mode),
    is_advisory = as.character(raw$isadvisory),
    is_detour = as.character(raw$isdetour),
    is_alert = as.character(raw$isalert),
    is_suspended = as.character(raw$issuspended),
    is_delays = as.character(raw$isdelays),
    last_updated = as.character(raw$last_updated)
  )
}

#' Search SEPTA alerts by route
#'
#' Filters the full alert list to find routes matching a keyword. Searches
#' against both the route identifier and the route name using case-insensitive
#' matching.
#'
#' @param query Character. Route name or number to search for
#'   (case-insensitive, e.g. \code{"Broad"}, \code{"44"}, \code{"Paoli"}).
#' @return A tibble of matching alerts with the same columns as
#'   \code{\link{septa_alerts}}.
#' @export
#' @examples
#' \dontrun{
#' septa_search("Paoli")
#' septa_search("Broad")
#' }
septa_search <- function(query) {
  df <- septa_alerts()
  pattern <- query
  df |> filter(
    grepl(pattern, route, ignore.case = TRUE) |
    grepl(pattern, route_name, ignore.case = TRUE)
  )
}

#' Get current elevator outages
#'
#' Returns information about elevator outages across the SEPTA rail system
#' (subway, trolley, and regional rail stations). Useful for accessibility
#' planning and ADA compliance monitoring.
#'
#' @return A tibble with one row per outage:
#' \describe{
#'   \item{line}{Character. Transit line name (e.g. "Broad Street Subway").}
#'   \item{station}{Character. Station name.}
#'   \item{elevator}{Character. Elevator identifier and direction.}
#'   \item{message}{Character. Description of the outage and alternatives.}
#' }
#' @export
#' @examples
#' \dontrun{
#' septa_elevators()
#' }
septa_elevators <- function() {
  url <- paste0(.septa_base, "/elevator/index.php")
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) {
    return(tibble(line = character(), station = character(),
                  elevator = character(), message = character()))
  }
  if (!is.data.frame(results)) return(tibble(line = character(), station = character(),
                                              elevator = character(), message = character()))
  tibble(
    line = as.character(results$line),
    station = as.character(results$station),
    elevator = as.character(results$elevator),
    message = as.character(results$message)
  )
}

#' Get bus detours
#'
#' Returns active bus detours across the SEPTA network. When a route is
#' specified, returns detours for just that route; otherwise returns all
#' current detours system-wide.
#'
#' @param route Character or NULL. Optional route number (e.g. \code{"44"}).
#'   If \code{NULL} (default), returns all active detours.
#' @return A tibble with one row per detour:
#' \describe{
#'   \item{route}{Character. Route number affected.}
#'   \item{reason}{Character. Reason for the detour.}
#'   \item{current_message}{Character. Current detour instructions.}
#'   \item{start_date}{Character. When the detour began.}
#' }
#' @export
#' @examples
#' \dontrun{
#' # All detours
#' septa_detours()
#' # Detours for route 44
#' septa_detours("44")
#' }
septa_detours <- function(route = NULL) {
  if (is.null(route)) {
    url <- paste0(.septa_base, "/BusDetours/index.php")
  } else {
    url <- sprintf("%s/BusDetours/index.php?route=%s", .septa_base, route)
  }
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) {
    return(tibble(route = character(), reason = character(),
                  current_message = character(), start_date = character()))
  }
  if (!is.data.frame(raw) && is.list(raw)) {
    # May be nested by route
    parts <- lapply(raw, function(x) {
      if (is.data.frame(x) && nrow(x) > 0) x else NULL
    })
    raw <- bind_rows(parts)
  }
  if (!is.data.frame(raw) || nrow(raw) == 0) {
    return(tibble(route = character(), reason = character(),
                  current_message = character(), start_date = character()))
  }
  tibble(
    route = as.character(raw$route_id %||% raw$Route %||% NA_character_),
    reason = as.character(raw$reason %||% raw$Reason %||% NA_character_),
    current_message = as.character(raw$current_message %||% raw$CurrentMessage %||% NA_character_),
    start_date = as.character(raw$start_date %||% raw$StartDate %||% NA_character_)
  )
}

#' List SEPTA routes with active vehicles
#'
#' Returns all bus and trolley routes that currently have vehicles in
#' service. Derived from the alerts endpoint, filtered to Bus and Trolley
#' modes. Use the \code{route} column with \code{\link{septa_buses}} to
#' get real-time vehicle positions.
#'
#' @return A tibble with one row per active route:
#' \describe{
#'   \item{route}{Character. Route identifier.}
#'   \item{route_name}{Character. Human-readable route name.}
#'   \item{mode}{Character. Transit mode ("Bus" or "Trolley").}
#' }
#' @export
#' @examples
#' \dontrun{
#' routes <- septa_list()
#' # Get buses for the first route
#' septa_buses(routes$route[1])
#' }
septa_list <- function() {
  df <- septa_alerts()
  df |> filter(mode %in% c("Bus", "Trolley")) |>
    select(route, route_name, mode) |>
    arrange(route)
}

# == Context ===================================================================

#' Get septa.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
septa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(septa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/septa.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "septa.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# septa.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# septa.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
