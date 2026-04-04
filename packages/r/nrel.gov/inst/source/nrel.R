# nrel.gov.R - National Renewable Energy Laboratory API Client
# Self-contained client for developer.nrel.gov
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key via NREL_API_KEY env var (DEMO_KEY used as fallback)
# Base: https://developer.nrel.gov/api

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nrel_base <- "https://developer.nrel.gov/api"

`%||%` <- function(x, y) if (is.null(x)) y else x

.nrel_key <- function() {
  key <- Sys.getenv("NREL_API_KEY", "")
  if (nchar(key) == 0) key <- "DEMO_KEY"
  key
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

.schema_stations <- tibble(
  id = integer(), station_name = character(), street_address = character(),
  city = character(), state = character(), zip = character(),
  fuel_type_code = character(), status_code = character(),
  latitude = numeric(), longitude = numeric()
)

.schema_solar <- tibble(
  parameter = character(), annual = numeric(),
  jan = numeric(), feb = numeric(), mar = numeric(), apr = numeric(),
  may = numeric(), jun = numeric(), jul = numeric(), aug = numeric(),
  sep = numeric(), oct = numeric(), nov = numeric(), dec = numeric()
)

# == Alt Fuel Stations =========================================================

#' Search alternative fuel stations
#'
#' Query the NREL Alternative Fuel Stations API. Returns EV chargers,
#' CNG, LPG, biodiesel, E85, hydrogen, and renewable diesel stations.
#'
#' @param state Two-letter state code (e.g. "CA", "NY")
#' @param fuel_type Fuel type code: ELEC, CNG, LPG, BD, E85, HY, LNG, RD (optional)
#' @param city City name (optional)
#' @param zip ZIP code (optional)
#' @param status Station status: E (open), P (planned), T (temporarily unavailable) (optional)
#' @param limit Max results (default 100, max 200)
#' @return tibble: id, station_name, street_address, city, state, zip, fuel_type_code, status_code, latitude, longitude, plus more
nrel_stations <- function(state = NULL, fuel_type = NULL, city = NULL,
                          zip = NULL, status = NULL, limit = 100) {
  url <- sprintf("%s/alt-fuel-stations/v1.json?api_key=%s&limit=%d",
                 .nrel_base, .nrel_key(), as.integer(limit))
  if (!is.null(state))     url <- paste0(url, "&state=", state)
  if (!is.null(fuel_type)) url <- paste0(url, "&fuel_type=", fuel_type)
  if (!is.null(city))      url <- paste0(url, "&city=", utils::URLencode(city))
  if (!is.null(zip))       url <- paste0(url, "&zip=", zip)
  if (!is.null(status))    url <- paste0(url, "&status_code=", status)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$fuel_stations)) return(.schema_stations)
  fs <- raw$fuel_stations
  if (!is.data.frame(fs) || nrow(fs) == 0) return(.schema_stations)
  as_tibble(fs) |>
    select(any_of(c("id", "station_name", "street_address", "city", "state",
                     "zip", "fuel_type_code", "status_code", "latitude",
                     "longitude", "access_code", "owner_type_code",
                     "open_date", "ev_network", "ev_connector_types",
                     "ev_level2_evse_num", "ev_dc_fast_num")))
}

#' Get details for a specific fuel station
#'
#' @param station_id Numeric station ID
#' @return tibble with full station details (1 row)
nrel_station <- function(station_id) {
  url <- sprintf("%s/alt-fuel-stations/v1/%d.json?api_key=%s",
                 .nrel_base, as.integer(station_id), .nrel_key())
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$alt_fuel_station)) return(.schema_stations)
  s <- raw$alt_fuel_station
  flat <- lapply(s, function(v) {
    if (is.list(v) || length(v) > 1) paste(unlist(v), collapse = ", ")
    else as.character(v %||% NA)
  })
  as_tibble(flat)
}

# == Solar Resource ============================================================

#' Get solar resource data for a location
#'
#' Returns annual and monthly average solar irradiance data (DNI, GHI, tilt).
#'
#' @param lat Latitude
#' @param lon Longitude
#' @return tibble: parameter, annual, jan, feb, ... dec
nrel_solar <- function(lat, lon) {
  url <- sprintf("%s/solar/solar_resource/v1.json?api_key=%s&lat=%f&lon=%f",
                 .nrel_base, .nrel_key(), lat, lon)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$outputs)) return(.schema_solar)
  out <- raw$outputs
  params <- names(out)
  rows <- lapply(params, function(p) {
    v <- out[[p]]
    if (!is.list(v)) return(NULL)
    monthly <- v$monthly
    if (is.null(monthly)) return(NULL)
    tibble(
      parameter = p,
      annual = as.numeric(v$annual %||% NA),
      jan = as.numeric(monthly$jan %||% NA),
      feb = as.numeric(monthly$feb %||% NA),
      mar = as.numeric(monthly$mar %||% NA),
      apr = as.numeric(monthly$apr %||% NA),
      may = as.numeric(monthly$may %||% NA),
      jun = as.numeric(monthly$jun %||% NA),
      jul = as.numeric(monthly$jul %||% NA),
      aug = as.numeric(monthly$aug %||% NA),
      sep = as.numeric(monthly$sep %||% NA),
      oct = as.numeric(monthly$oct %||% NA),
      nov = as.numeric(monthly$nov %||% NA),
      dec = as.numeric(monthly$dec %||% NA)
    )
  })
  bind_rows(rows[!vapply(rows, is.null, logical(1))])
}

# == Utility Rates =============================================================

#' Get utility rates for a location
#'
#' Returns commercial, industrial, and residential electricity rates.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @return tibble: utility_name, residential, commercial, industrial (rates in $/kWh)
nrel_utility_rates <- function(lat, lon) {
  url <- sprintf("%s/utility_rates/v3.json?api_key=%s&lat=%f&lon=%f",
                 .nrel_base, .nrel_key(), lat, lon)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$outputs)) return(tibble())
  o <- raw$outputs
  tibble(
    utility_name = as.character(o$utility_name %||% NA),
    residential  = as.numeric(o$residential %||% NA),
    commercial   = as.numeric(o$commercial %||% NA),
    industrial   = as.numeric(o$industrial %||% NA)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for nrel.gov
#'
#' @return Character string (invisibly), also printed
nrel_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/nrel.gov.R"
  if (!file.exists(src_file)) { cat("# nrel.gov context - source not found\n"); return(invisible(NULL)) }
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
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
