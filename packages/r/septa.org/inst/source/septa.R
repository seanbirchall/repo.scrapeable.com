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
#' Returns current location and status of all active regional rail trains.
#'
#' @return tibble: train_number, service, destination, line, source,
#'   current_stop, next_stop, latitude, longitude, late, track
#' @export
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
#' @param route Route number or ID (e.g. "33", "44", "LUCYGR").
#' @return tibble: vehicle_id, route, direction, destination, latitude, longitude,
#'   late, next_stop_name, trip, seat_availability
#' @export
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
#' Returns current alert status for all routes and lines.
#'
#' @return tibble: route, route_name, mode, is_advisory, is_detour, is_alert,
#'   is_suspended, is_delays, last_updated
#' @export
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
#' @param query Route name/number to search for (case-insensitive).
#' @return tibble of matching alerts
#' @export
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
#' @return tibble: line, station, elevator, message
#' @export
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
#' @param route Optional route number; if NULL returns all detours.
#' @return tibble: route, reason, current_message, start_date
#' @export
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
#' Returns all routes that currently have buses/trolleys running.
#'
#' @return tibble: route, route_name, mode
#' @export
septa_list <- function() {
  df <- septa_alerts()
  df |> filter(mode %in% c("Bus", "Trolley")) |>
    select(route, route_name, mode) |>
    arrange(route)
}

# == Context ===================================================================

#' Show SEPTA client context for LLM use
#'
#' @return Invisibly returns context string
#' @export
septa_context <- function() {
  .build_context("septa.org")
}

.build_context <- function(pkg_name) {
  src_dir <- system.file("source", package = pkg_name)
  if (src_dir != "" && length(list.files(src_dir, pattern = "[.]R$")) > 0) {
    src_file <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)[1]
  } else {
    src_file <- NULL
    tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
    if (is.null(src_file)) src_file <- paste0("clients/", pkg_name, ".R")
  }
  if (is.null(src_file) || !file.exists(src_file)) {
    cat("# ", pkg_name, " context - source not found\n")
    return(invisible(paste0("# ", pkg_name, " context - source not found")))
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
