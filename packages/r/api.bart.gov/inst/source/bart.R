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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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

`%||%` <- function(x, y) if (is.null(x)) y else x
