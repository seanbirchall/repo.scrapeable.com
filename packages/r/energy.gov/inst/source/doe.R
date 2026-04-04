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

# == Public functions ==========================================================

#' List available DOE data sources
#'
#' @return A tibble describing available endpoints.
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
#' Search the DOE/NREL Alternative Fuels Data Center for fuel stations
#' including EV chargers, CNG, LPG, hydrogen, and more.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "CA").
#' @param fuel_type Character. Fuel type code: "ELEC", "CNG", "LPG", "BD",
#'   "E85", "HY", "LNG", "RD". Default "ELEC" (electric).
#' @param city Character. City name to filter by.
#' @param zip Character. ZIP code to filter by.
#' @param status Character. Station status: "E" (open), "P" (planned),
#'   "T" (temporarily unavailable). Default "E".
#' @param limit Integer. Maximum results (default 200, max 200 per request).
#' @param api_key Character. NREL API key.
#' @return A tibble of fuel stations.
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
#' @param station_id Integer. Station ID.
#' @param api_key Character. NREL API key.
#' @return A tibble (1 row) with station details.
#' @export
doe_station <- function(station_id, api_key = NULL) {
  url <- paste0(.afdc_base, "/", station_id, ".json")
  raw <- .doe_get_json(url, api_key = api_key)
  .doe_parse_stations(list(raw$alt_fuel_station))
}

#' Get fuel station counts by state
#'
#' @param fuel_type Character. Fuel type code (default "ELEC").
#' @param api_key Character. NREL API key.
#' @return A tibble with columns: state, total_stations.
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
#' @param latitude Numeric. Latitude.
#' @param longitude Numeric. Longitude.
#' @param radius Numeric. Search radius in miles (default 10).
#' @param fuel_type Character. Fuel type code (default "ELEC").
#' @param limit Integer. Maximum results (default 20).
#' @param api_key Character. NREL API key.
#' @return A tibble of nearby stations.
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
#' @return A tibble of FITARA milestones.
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

#' Print energy.gov client context
#'
#' @return Character string of function signatures (invisibly).
#' @export
doe_context <- function() {
  src <- system.file("source", "doe.R", package = "energy.gov")
  if (src == "") {
    f <- sys.frame(sys.nframe())$ofile %||%
      attr(body(doe_context), "srcfile")$filename %||% ""
    if (nzchar(f)) src <- f
  }
  .build_context("energy.gov", src_file = if (nzchar(src)) src else NULL,
                 header_lines = c(
                   "# energy.gov -- Department of Energy R client",
                   "# AFDC: https://developer.nrel.gov/api/alt-fuel-stations/v1",
                   "# Auth: API key (DEMO_KEY for testing, register at developer.nrel.gov)"
                 ))
}
