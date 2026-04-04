# api.tfl.gov.uk.R - Self-contained api.tfl.gov.uk client



# tfl.R
# Self-contained Transport for London API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (open data)
# Rate limits: 500 requests/minute


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tfl_base <- "https://api.tfl.gov.uk"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_lines <- tibble(
  id = character(), name = character(), mode = character(),
  status = character(), reason = character()
)

.schema_arrivals <- tibble(
  id = character(), line_name = character(), station_name = character(),
  destination = character(), towards = character(),
  expected_arrival = as.POSIXct(character()), time_to_station = integer(),
  platform_name = character(), direction = character()
)

.schema_stops <- tibble(
  id = character(), name = character(), lat = numeric(),
  lon = numeric(), modes = character()
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Get TfL lines by mode with live status
#'
#' Returns all Transport for London lines/routes for a given transport mode,
#' including their current service status. Use this to check for disruptions
#' or to discover valid line IDs for use with \code{\link{tfl_arrivals}}.
#'
#' @param mode Character. Transport mode to query. Valid values:
#'   \code{"tube"} (default), \code{"bus"}, \code{"dlr"},
#'   \code{"overground"}, \code{"elizabeth-line"}, \code{"tram"},
#'   \code{"cable-car"}.
#'
#' @return A tibble with one row per line/route and the following columns:
#' \describe{
#'   \item{id}{Character. Line identifier for use with other functions (e.g.
#'     \code{"victoria"}, \code{"central"}, \code{"northern"}).}
#'   \item{name}{Character. Human-readable line name (e.g.
#'     \code{"Victoria"}, \code{"Hammersmith & City"}).}
#'   \item{mode}{Character. Transport mode (e.g. \code{"tube"}).}
#'   \item{status}{Character. Current service status: \code{"Good Service"},
#'     \code{"Part Closure"}, \code{"Planned Closure"}, \code{"Minor Delays"},
#'     \code{"Severe Delays"}, \code{"Suspended"}, etc.}
#'   \item{reason}{Character. Disruption details when status is not
#'     \code{"Good Service"} (\code{NA} otherwise).}
#' }
#'
#' @examples
#' \dontrun{
#' # Get all Tube lines with current status
#' tfl_lines("tube")
#'
#' # Check DLR status
#' tfl_lines("dlr")
#'
#' # Get bus routes (many rows)
#' tfl_lines("bus")
#' }
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
#' Returns real-time arrival predictions for a given Tube/DLR/rail line. When
#' called without a stop ID, returns arrivals for all stations on the line.
#' When a stop ID is provided, filters to that specific station. Data refreshes
#' every 30 seconds. Use \code{\link{tfl_stops}} to find NAPTAN stop IDs.
#'
#' @param line_id Character. Line identifier from \code{\link{tfl_lines}}.
#'   Examples: \code{"victoria"}, \code{"central"}, \code{"northern"},
#'   \code{"jubilee"}, \code{"piccadilly"}, \code{"bakerloo"}.
#' @param stop_id Character or \code{NULL}. Optional NAPTAN stop/station ID to
#'   filter arrivals to a single station. Find IDs using
#'   \code{\link{tfl_stops}}. Example: \code{"940GZZLUVIC"} (Victoria station).
#'   Default \code{NULL} returns arrivals for all stations on the line.
#'
#' @return A tibble with one row per predicted arrival and the following columns:
#' \describe{
#'   \item{id}{Character. Unique arrival prediction ID.}
#'   \item{line_name}{Character. Line name (e.g. \code{"Victoria"}).}
#'   \item{station_name}{Character. Station name (e.g. \code{"Blackhorse Road Underground Station"}).}
#'   \item{destination}{Character. Destination station (e.g. \code{"Walthamstow Central Underground Station"}).}
#'   \item{towards}{Character. Direction description (e.g. \code{"Walthamstow Central"}).}
#'   \item{expected_arrival}{POSIXct. Predicted arrival time (UTC).}
#'   \item{time_to_station}{Integer. Seconds until arrival at the station.}
#'   \item{platform_name}{Character. Platform name/number.}
#'   \item{direction}{Character. Direction of travel (e.g. \code{"inbound"}, \code{"outbound"}).}
#' }
#'
#' @examples
#' \dontrun{
#' # Get all arrivals on the Victoria line
#' tfl_arrivals("victoria")
#'
#' # Get arrivals at a specific station
#' tfl_arrivals("victoria", stop_id = "940GZZLUVIC")
#'
#' # Sort by next arriving train
#' library(dplyr)
#' tfl_arrivals("northern") |> arrange(expected_arrival)
#' }
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

#' Search TfL stops and stations
#'
#' Searches for Transport for London stops and stations by name. Returns
#' matching locations with coordinates and supported transport modes. Use the
#' returned \code{id} values as the \code{stop_id} parameter in
#' \code{\link{tfl_arrivals}}.
#'
#' @param query Character. Search term for station/stop names. Examples:
#'   \code{"Kings Cross"}, \code{"Victoria"}, \code{"Paddington"},
#'   \code{"Heathrow"}.
#'
#' @return A tibble with one row per matching stop/station and the following columns:
#' \describe{
#'   \item{id}{Character. NAPTAN stop/station ID (e.g. \code{"HUBVIC"},
#'     \code{"910GMNCRVIC"}). Use with \code{tfl_arrivals()}.}
#'   \item{name}{Character. Stop/station name (e.g. \code{"Victoria"}).}
#'   \item{lat}{Numeric. Latitude.}
#'   \item{lon}{Numeric. Longitude.}
#'   \item{modes}{Character. Comma-separated transport modes available at this
#'     stop (e.g. \code{"bus, tube, national-rail"}).}
#' }
#'
#' @examples
#' \dontrun{
#' # Search for Victoria stations
#' tfl_stops("Victoria")
#'
#' # Find King's Cross
#' tfl_stops("Kings Cross")
#'
#' # Find stations near an area
#' tfl_stops("Canary Wharf")
#' }
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

# == Context ===================================================================

#' Get api.tfl.gov.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
tfl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(tfl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.tfl.gov.uk.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.tfl.gov.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.tfl.gov.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.tfl.gov.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
