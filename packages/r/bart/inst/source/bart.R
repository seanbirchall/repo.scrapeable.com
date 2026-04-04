# bart.R
# Self-contained BART (Bay Area Rapid Transit) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: Public demo key included (MW9S-E7SL-26DU-VV8V)
# Rate limits: generous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bart_base <- "https://api.bart.gov/api"
.bart_key <- "MW9S-E7SL-26DU-VV8V"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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

# == Public functions ==========================================================

#' Get real-time departures from a BART station
#'
#' Fetches live estimated departure times for all trains leaving a given
#' BART station. Returns an empty tibble when the station is closed or
#' no trains are scheduled (e.g. overnight).
#'
#' @param station Character. Station abbreviation, e.g. \code{"MONT"},
#'   \code{"EMBR"}, \code{"12TH"}, \code{"SFIA"}. Run \code{bart_stations()}
#'   to list all 50 valid abbreviations.
#' @return A tibble with one row per train departure and 8 columns:
#' \describe{
#'   \item{station}{Character. Uppercased origin station abbreviation.}
#'   \item{destination}{Character. Destination station name (e.g. \code{"Antioch"}).}
#'   \item{minutes}{Character. Minutes until departure (\code{"Leaving"} if imminent, or numeric string).}
#'   \item{platform}{Integer. Platform number (1--4).}
#'   \item{direction}{Character. Compass direction: \code{"North"} or \code{"South"}.}
#'   \item{length}{Integer. Number of cars in the train (typically 5--10).}
#'   \item{color}{Character. Route color: \code{"BLUE"}, \code{"GREEN"}, \code{"ORANGE"}, \code{"RED"}, \code{"YELLOW"}, \code{"BEIGE"}.}
#'   \item{delay}{Character. Delay in seconds, or \code{NA} if on time.}
#' }
#' @examples
#' bart_departures("MONT")
#' bart_departures("EMBR")
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
#' Returns metadata for all 50 stations in the BART system, including
#' geographic coordinates and addresses. Useful for looking up station
#' abbreviations needed by \code{bart_departures()}.
#'
#' @return A tibble with one row per station and 9 columns:
#' \describe{
#'   \item{abbr}{Character. Station abbreviation (e.g. \code{"12TH"}, \code{"MONT"}).}
#'   \item{name}{Character. Full station name (e.g. \code{"12th St. Oakland City Center"}).}
#'   \item{city}{Character. City name.}
#'   \item{county}{Character. County name (lowercase).}
#'   \item{state}{Character. State abbreviation (always \code{"CA"}).}
#'   \item{latitude}{Numeric. GTFS latitude in decimal degrees.}
#'   \item{longitude}{Numeric. GTFS longitude in decimal degrees.}
#'   \item{address}{Character. Street address.}
#'   \item{zipcode}{Character. ZIP code.}
#' }
#' @examples
#' bart_stations()
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
#' Returns metadata for every active BART route, including route color,
#' name, and abbreviation. There are typically 12 routes (6 lines, each
#' with two directions).
#'
#' @return A tibble with one row per route and 6 columns:
#' \describe{
#'   \item{number}{Character. Route number (e.g. \code{"11"}, \code{"12"}).}
#'   \item{name}{Character. Descriptive route name (e.g. \code{"Daly City to Dublin/Pleasanton"}).}
#'   \item{abbr}{Character. Route abbreviation (e.g. \code{"DALY-DUBL"}).}
#'   \item{color}{Character. Route color: \code{"BLUE"}, \code{"GREEN"}, \code{"ORANGE"}, \code{"RED"}, \code{"YELLOW"}, \code{"BEIGE"}.}
#'   \item{holidays}{Character. Holiday schedule flag, or \code{NA} if not applicable.}
#'   \item{num_stations}{Integer. Number of stations on the route, or \code{NA} if unavailable.}
#' }
#' @examples
#' bart_routes()
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

#' Get bart client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bart.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bart")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bart context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bart", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
