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
#' Queries the BART real-time estimated departure API for a single station,
#' returning one row per upcoming train. The \code{minutes} column shows
#' estimated minutes until departure ("Leaving" when the train is boarding).
#' Results update roughly every 15 seconds at the API level.
#'
#' @param station Character. Station abbreviation (e.g. \code{"MONT"},
#'   \code{"EMBR"}, \code{"12TH"}). Case-insensitive. Call
#'   \code{\link{bart_stations}} to list every valid abbreviation.
#' @return A tibble with one row per upcoming departure and columns:
#' \describe{
#'   \item{station}{Character. Uppercase station abbreviation queried.}
#'   \item{destination}{Character. Final destination of the train.}
#'   \item{minutes}{Character. Minutes until departure or \code{"Leaving"}.}
#'   \item{platform}{Integer. Platform number (1--4).}
#'   \item{direction}{Character. Compass direction (\code{"North"} / \code{"South"}).}
#'   \item{length}{Integer. Number of cars on the train.}
#'   \item{color}{Character. Route color (e.g. \code{"BLUE"}, \code{"RED"}).}
#'   \item{delay}{Character. Delay in seconds, or \code{NA} if on time.}
#' }
#' @examples
#' \dontrun{
#' bart_departures("MONT")
#' bart_departures("embr")
#' }
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
#' Returns every station in the Bay Area Rapid Transit system with its
#' geographic coordinates, address, and administrative area. Useful as a
#' lookup table for \code{\link{bart_departures}}.
#'
#' @return A tibble with one row per station and columns:
#' \describe{
#'   \item{abbr}{Character. Four-letter station abbreviation used by the API.}
#'   \item{name}{Character. Full human-readable station name.}
#'   \item{city}{Character. City where the station is located.}
#'   \item{county}{Character. County name (lowercase).}
#'   \item{state}{Character. Two-letter state code (always \code{"CA"}).}
#'   \item{latitude}{Numeric. GTFS latitude (WGS 84).}
#'   \item{longitude}{Numeric. GTFS longitude (WGS 84).}
#'   \item{address}{Character. Street address.}
#'   \item{zipcode}{Character. Five-digit ZIP code.}
#' }
#' @examples
#' \dontrun{
#' bart_stations()
#' }
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
#' Returns every currently-defined route in the BART system including its
#' colour designation and the number of stations served. Route colours
#' correspond to the familiar map colours (Blue, Red, Green, Yellow, Orange,
#' Beige).
#'
#' @return A tibble with one row per route and columns:
#' \describe{
#'   \item{number}{Character. Internal route number.}
#'   \item{name}{Character. Human-readable route description (origin to destination).}
#'   \item{abbr}{Character. Short route abbreviation.}
#'   \item{color}{Character. Route colour name (e.g. \code{"BLUE"}).}
#'   \item{holidays}{Character. Holiday schedule flag, or \code{NA}.}
#'   \item{num_stations}{Integer. Number of stations on the route, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' bart_routes()
#' }
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

# == Context ===================================================================

#' Get bart.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bart.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bart.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bart.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bart.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
