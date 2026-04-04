# energy.gov.R
# Self-contained Department of Energy data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key for AFDC endpoints (DEMO_KEY works for testing).
#   Register at https://developer.nrel.gov/signup/
# Docs: https://developer.nrel.gov/docs/transportation/alt-fuel-stations-v1/
#
# Data sources:
#   - Alternative Fuels Data Center (AFDC): 100K+ fuel stations nationwide
#   - FITARA IT milestones (DOE IT modernization)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.afdc_base <- "https://developer.nrel.gov/api/alt-fuel-stations/v1"

# -- Core fetch engine ---------------------------------------------------------

.doe_get_json <- function(url, params = list(), api_key = NULL) {
  key <- api_key %||% Sys.getenv("NREL_API_KEY", "DEMO_KEY")
  params$api_key <- key

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 2, max_seconds = 15) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# Parse AFDC station records into tibble
.doe_parse_stations <- function(stations) {
  if (length(stations) == 0) return(tibble())
  bind_rows(lapply(stations, function(s) {
    tibble(
      id                 = as.integer(s$id %||% NA),
      station_name       = s$station_name %||% NA_character_,
      fuel_type_code     = s$fuel_type_code %||% NA_character_,
      status_code        = s$status_code %||% NA_character_,
      street_address     = s$street_address %||% NA_character_,
      city               = s$city %||% NA_character_,
      state              = s$state %||% NA_character_,
      zip                = s$zip %||% NA_character_,
      latitude           = as.numeric(s$latitude %||% NA),
      longitude          = as.numeric(s$longitude %||% NA),
      access_code        = s$access_code %||% NA_character_,
      ev_network         = s$ev_network %||% NA_character_,
      ev_level1_evse_num = as.integer(s$ev_level1_evse_num %||% NA),
      ev_level2_evse_num = as.integer(s$ev_level2_evse_num %||% NA),
      ev_dc_fast_num     = as.integer(s$ev_dc_fast_num %||% NA),
      station_phone      = s$station_phone %||% NA_character_,
      open_date          = as.Date(s$open_date %||% NA_character_),
      date_last_confirmed = as.Date(s$date_last_confirmed %||% NA_character_),
      owner_type_code    = s$owner_type_code %||% NA_character_,
      facility_type      = s$facility_type %||% NA_character_
    )
  }))
}

# -- Context generator ---------------------------------------------------------

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

# == Public functions ==========================================================

#' List available DOE data sources
#'
#' Returns a catalog of datasets available through this client, including
#' the AFDC alternative fuel stations API and FITARA IT milestones.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{dataset}{character -- dataset identifier (\code{"afdc_stations"},
#'       \code{"fitara_milestones"})}
#'     \item{description}{character -- human-readable description}
#'     \item{source}{character -- data source name}
#'     \item{records}{character -- approximate number of records}
#'     \item{auth}{character -- authentication requirements}
#'   }
#' @examples
#' doe_list()
#' @export
doe_list <- function() {
  tibble(
    dataset = c("afdc_stations", "fitara_milestones"),
    description = c(
      "Alternative fuel stations nationwide (EV, CNG, LPG, H2, etc.)",
      "DOE FITARA IT modernization milestones"
    ),
    source = c(
      "NREL Alternative Fuels Data Center API",
      "energy.gov/digitalstrategy"
    ),
    records = c("~100,000+", "~2"),
    auth = c("API key (DEMO_KEY for testing)", "none")
  )
}

#' Search alternative fuel stations
#'
#' Search the DOE/NREL Alternative Fuels Data Center (AFDC) for fuel stations
#' including EV chargers, CNG, LPG, hydrogen, and more. Covers 100,000+
#' stations nationwide.
#'
#' @param state Character. Two-letter state abbreviation (e.g. \code{"CA"},
#'   \code{"TX"}). Default \code{NULL} returns all states.
#' @param fuel_type Character. Fuel type code: \code{"ELEC"} (electric, default),
#'   \code{"CNG"} (compressed natural gas), \code{"LPG"} (propane),
#'   \code{"BD"} (biodiesel), \code{"E85"} (ethanol), \code{"HY"} (hydrogen),
#'   \code{"LNG"} (liquefied natural gas), \code{"RD"} (renewable diesel).
#' @param city Character. City name to filter by. Default \code{NULL}.
#' @param zip Character. ZIP code to filter by. Default \code{NULL}.
#' @param status Character. Station status: \code{"E"} (open, default),
#'   \code{"P"} (planned), \code{"T"} (temporarily unavailable).
#' @param limit Integer. Maximum results (default 200, max 200 per request).
#' @param api_key Character. NREL API key. Uses \code{NREL_API_KEY} env var
#'   or \code{DEMO_KEY}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- station ID}
#'     \item{station_name}{character -- station name}
#'     \item{fuel_type_code}{character -- fuel type code}
#'     \item{status_code}{character -- status code (\code{"E"}/\code{"P"}/\code{"T"})}
#'     \item{street_address}{character -- street address}
#'     \item{city}{character -- city}
#'     \item{state}{character -- 2-letter state}
#'     \item{zip}{character -- ZIP code}
#'     \item{latitude}{numeric -- latitude}
#'     \item{longitude}{numeric -- longitude}
#'     \item{access_code}{character -- \code{"public"} or \code{"private"}}
#'     \item{ev_network}{character -- EV network name (e.g. \code{"ChargePoint Network"})}
#'     \item{ev_level1_evse_num}{integer -- number of Level 1 EVSE ports}
#'     \item{ev_level2_evse_num}{integer -- number of Level 2 EVSE ports}
#'     \item{ev_dc_fast_num}{integer -- number of DC fast charger ports}
#'     \item{station_phone}{character -- contact phone}
#'     \item{open_date}{Date -- date station opened}
#'     \item{date_last_confirmed}{Date -- date of last data confirmation}
#'     \item{owner_type_code}{character -- owner type (\code{"P"} private, \code{"LG"} local gov, \code{"FG"} federal)}
#'     \item{facility_type}{character -- facility type (e.g. \code{"PARKING_GARAGE"}, \code{"UTILITY"})}
#'   }
#' @examples
#' doe_search(state = "CA", fuel_type = "ELEC", limit = 5)
#' doe_search(state = "TX", fuel_type = "HY")
#' @export
doe_search <- function(state = NULL, fuel_type = "ELEC", city = NULL,
                       zip = NULL, status = "E", limit = 200,
                       api_key = NULL) {
  params <- list(
    fuel_type = fuel_type,
    status = status,
    limit = min(limit, 200)
  )
  if (!is.null(state)) params$state <- toupper(state)
  if (!is.null(city))  params$city <- city
  if (!is.null(zip))   params$zip <- zip

  raw <- .doe_get_json(paste0(.afdc_base, ".json"), params, api_key)
  .doe_parse_stations(raw$fuel_stations)
}

#' Get fuel station by ID
#'
#' Retrieves full details for a single AFDC fuel station by its ID.
#'
#' @param station_id Integer. Station ID (from \code{doe_search()} results).
#' @param api_key Character. NREL API key.
#' @return A tibble with one row and the same columns as \code{doe_search()}.
#' @examples
#' doe_station(1517)
#' @export
doe_station <- function(station_id, api_key = NULL) {
  url <- paste0(.afdc_base, "/", station_id, ".json")
  raw <- .doe_get_json(url, api_key = api_key)
  .doe_parse_stations(list(raw$alt_fuel_station))
}

#' Get fuel station counts by state
#'
#' Queries the AFDC API for each U.S. state to count the number of
#' open stations for a given fuel type. Note: this makes 51 API calls
#' (one per state + DC) and may be slow with \code{DEMO_KEY}.
#'
#' @param fuel_type Character. Fuel type code (default \code{"ELEC"}).
#'   See \code{doe_search()} for valid codes.
#' @param api_key Character. NREL API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{state}{character -- 2-letter state abbreviation}
#'     \item{total_stations}{integer -- number of open stations}
#'   }
#'   Sorted descending by \code{total_stations}. States with zero are excluded.
#' @examples
#' \dontrun{
#' doe_station_counts("ELEC")
#' }
#' @export
doe_station_counts <- function(fuel_type = "ELEC", api_key = NULL) {
  key <- api_key %||% Sys.getenv("NREL_API_KEY", "DEMO_KEY")

  # Use the count endpoint grouped by state
  states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
              "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
              "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
              "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
              "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")

  # Fetch total for all states at once
  raw <- .doe_get_json(paste0(.afdc_base, ".json"),
                       list(fuel_type = fuel_type, status = "E", limit = 1),
                       api_key)
  total <- raw$total_results %||% 0L

  # Get state-level counts using the count endpoint
  counts <- list()
  for (st in states) {
    raw <- .doe_get_json(paste0(.afdc_base, ".json"),
                         list(fuel_type = fuel_type, status = "E",
                              state = st, limit = 1),
                         api_key)
    counts[[st]] <- raw$total_results %||% 0L
  }

  tibble(
    state = names(counts),
    total_stations = as.integer(unlist(counts))
  ) |> filter(.data$total_stations > 0) |>
    arrange(desc(.data$total_stations))
}

#' Get nearest fuel stations to coordinates
#'
#' Finds the nearest alternative fuel stations to a given latitude/longitude
#' point, sorted by distance.
#'
#' @param latitude Numeric. Latitude (e.g. 37.7749 for San Francisco).
#' @param longitude Numeric. Longitude (e.g. -122.4194 for San Francisco).
#' @param radius Numeric. Search radius in miles (default 10).
#' @param fuel_type Character. Fuel type code (default \code{"ELEC"}).
#' @param limit Integer. Maximum results (default 20, max 200).
#' @param api_key Character. NREL API key.
#' @return A tibble with the same columns as \code{doe_search()}, sorted
#'   by proximity to the given coordinates.
#' @examples
#' doe_nearest(37.7749, -122.4194, radius = 5, limit = 5)
#' @export
doe_nearest <- function(latitude, longitude, radius = 10,
                        fuel_type = "ELEC", limit = 20, api_key = NULL) {
  url <- paste0(.afdc_base, "/nearest.json")
  params <- list(
    latitude = latitude,
    longitude = longitude,
    radius = radius,
    fuel_type = fuel_type,
    limit = min(limit, 200)
  )
  raw <- .doe_get_json(url, params, api_key)
  .doe_parse_stations(raw$fuel_stations)
}

#' Get DOE FITARA IT milestones
#'
#' Retrieves DOE's FITARA (Federal IT Acquisition Reform Act) IT
#' modernization milestones from energy.gov.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{milestone_id}{integer -- milestone identifier}
#'     \item{description}{character -- milestone description}
#'     \item{target_date}{character -- target completion date}
#'     \item{status}{character -- current status}
#'     \item{status_desc}{character -- status description}
#'     \item{baseline_area}{character -- common baseline area}
#'     \item{dcoi_area}{character -- DCOI area}
#'   }
#' @examples
#' doe_fitara()
#' @export
doe_fitara <- function() {
  url <- "https://www.energy.gov/sites/default/files/2023-08/fitaramilestones8152023.json"
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)

  # The URL redirects; read the downloaded file
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  milestones <- raw$milestones %||% list()
  if (length(milestones) == 0) return(tibble())

  bind_rows(lapply(milestones, function(m) {
    tibble(
      milestone_id   = as.integer(m$milestoneID %||% NA),
      description    = m$milestoneDesc %||% NA_character_,
      target_date    = m$`milestoneTargetC ompletionDate` %||%
                       m$milestoneTargetCompletionDate %||% NA_character_,
      status         = m$milestoneStatus %||% NA_character_,
      status_desc    = m$milestoneStatusDesc %||% NA_character_,
      baseline_area  = m$commonBaselineArea %||% NA_character_,
      dcoi_area      = m$dcoiArea %||% NA_character_
    )
  }))
}

#' Get energy.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
doe_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(doe_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/energy.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "energy.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# energy.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# energy.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
