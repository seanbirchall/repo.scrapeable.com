# api.bart.gov.R - Self-contained api.bart.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# bart.R
# Self-contained BART (Bay Area Rapid Transit) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: Public demo key included (MW9S-E7SL-26DU-VV8V)
# Rate limits: generous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bart_base <- "https://api.bart.gov/api"
.bart_key <- "MW9S-E7SL-26DU-VV8V"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_departures <- tibble(
  station = character(), destination = character(), minutes = character(),
  platform = integer(), direction = character(), length = integer(),
  color = character(), delay = character()
)

.schema_stations <- tibble(
  abbr = character(), name = character(), city = character(),
  county = character(), state = character(),
  latitude = numeric(), longitude = numeric(), address = character(),
  zipcode = character()
)

.schema_routes <- tibble(
  number = character(), name = character(), abbr = character(),
  color = character(), holidays = character(), num_stations = integer()
)


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Get real-time departures from a BART station
#'
#' Returns live estimated departure times for all trains leaving a given
#' station, grouped by destination. Each row is one upcoming train.
#' Use \code{bart_stations()} to discover valid station abbreviations.
#' Pair with \code{bart_routes()} to understand route colors and lines.
#'
#' @param station Character. Station abbreviation, e.g. \code{"MONT"}
#'   (Montgomery St.), \code{"EMBR"} (Embarcadero), \code{"12TH"}
#'   (12th St. Oakland City Center). Case-insensitive. Use
#'   \code{bart_stations()$abbr} for the full list of 50 station codes.
#' @return A tibble with one row per upcoming train:
#'   \describe{
#'     \item{station}{\code{character} -- Station abbreviation queried (uppercased)}
#'     \item{destination}{\code{character} -- Final destination station name}
#'     \item{minutes}{\code{character} -- Minutes until departure ("Leaving", "1", "5", etc.)}
#'     \item{platform}{\code{integer} -- Platform number (1 or 2)}
#'     \item{direction}{\code{character} -- "North" or "South"}
#'     \item{length}{\code{integer} -- Number of cars (e.g. 6, 8, 10)}
#'     \item{color}{\code{character} -- Route color ("BLUE", "RED", "GREEN", "YELLOW", "ORANGE")}
#'     \item{delay}{\code{character} -- Delay in seconds, or NA if on time}
#'   }
#'   Returns an empty tibble with correct column types when no departures
#'   are scheduled (e.g. late night / early morning).
#' @examples
#' \dontrun{
#' # Real-time departures from Montgomery St.
#' bart_departures("MONT")
#'
#' # Embarcadero departures heading north
#' bart_departures("EMBR") |> dplyr::filter(direction == "North")
#'
#' # Find next departure to SFO Airport from any station
#' bart_departures("MLBR") |> dplyr::filter(grepl("Airport", destination))
#' }
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

#' List all BART stations with location metadata
#'
#' Returns a tibble of all 50 BART stations with names, geographic
#' coordinates, and address information. Useful for discovering station
#' abbreviations needed by \code{bart_departures()}, or for mapping
#' and spatial analysis of the BART network.
#'
#' @return A tibble with one row per station (currently 50 stations):
#'   \describe{
#'     \item{abbr}{\code{character} -- 4-letter station abbreviation (e.g. "MONT", "EMBR")}
#'     \item{name}{\code{character} -- Full station name (e.g. "Montgomery St.")}
#'     \item{city}{\code{character} -- City name (e.g. "San Francisco", "Oakland")}
#'     \item{county}{\code{character} -- County name, lowercase (e.g. "sanfrancisco", "alameda")}
#'     \item{state}{\code{character} -- State abbreviation ("CA")}
#'     \item{latitude}{\code{numeric} -- GTFS latitude in decimal degrees}
#'     \item{longitude}{\code{numeric} -- GTFS longitude in decimal degrees (negative for west)}
#'     \item{address}{\code{character} -- Street address of the station}
#'     \item{zipcode}{\code{character} -- ZIP code (stored as character to preserve leading zeros)}
#'   }
#' @examples
#' \dontrun{
#' # Browse all stations
#' bart_stations()
#'
#' # Find stations in San Francisco
#' bart_stations() |> dplyr::filter(city == "San Francisco")
#'
#' # Get station abbreviations for use with bart_departures()
#' bart_stations() |> dplyr::select(abbr, name)
#' }
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
#' Returns metadata for all active BART routes (lines). Each route is a
#' directional service identified by color (Blue, Red, Green, Yellow,
#' Orange, Grey). Pair with \code{bart_departures()} to understand which
#' color lines serve a given station.
#'
#' @return A tibble with one row per route (currently 12 directional routes):
#'   \describe{
#'     \item{number}{\code{character} -- Route number (e.g. "1", "12")}
#'     \item{name}{\code{character} -- Full route name with direction (e.g. "Daly City to Dublin/Pleasanton")}
#'     \item{abbr}{\code{character} -- Route abbreviation (e.g. "DALY-DUBL", "RICH-BERY")}
#'     \item{color}{\code{character} -- Line color ("BLUE", "GREEN", "GREY", "ORANGE", "RED", "YELLOW")}
#'     \item{holidays}{\code{character} -- Holiday schedule indicator (currently NA for all routes)}
#'     \item{num_stations}{\code{integer} -- Number of stations on the route (may be NA)}
#'   }
#' @examples
#' \dontrun{
#' # See all routes
#' bart_routes()
#'
#' # Filter to just the Blue line
#' bart_routes() |> dplyr::filter(color == "BLUE")
#'
#' # Unique line colors
#' unique(bart_routes()$color)
#' }
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

# == Context ===================================================================

#' Get api.bart.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bart_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bart_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.bart.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.bart.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.bart.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.bart.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
