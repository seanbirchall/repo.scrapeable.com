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
#' CNG, LPG, biodiesel, E85, hydrogen, and renewable diesel stations
#' across the U.S. Requires an API key via the \code{NREL_API_KEY}
#' environment variable (\code{DEMO_KEY} used as fallback with lower
#' rate limits).
#'
#' @param state Character or NULL. Two-letter state code
#'   (e.g. \code{"CA"}, \code{"NY"}, \code{"TX"}).
#' @param fuel_type Character or NULL. Fuel type code:
#'   \code{"ELEC"} (electric), \code{"CNG"} (compressed natural gas),
#'   \code{"LPG"} (propane), \code{"BD"} (biodiesel),
#'   \code{"E85"} (ethanol), \code{"HY"} (hydrogen),
#'   \code{"LNG"} (liquefied natural gas),
#'   \code{"RD"} (renewable diesel).
#' @param city Character or NULL. City name (e.g. \code{"Los Angeles"}).
#' @param zip Character or NULL. ZIP code (e.g. \code{"90210"}).
#' @param status Character or NULL. Station status:
#'   \code{"E"} (open), \code{"P"} (planned),
#'   \code{"T"} (temporarily unavailable).
#' @param limit Integer. Max results (default 100, API max 200).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{id}{integer -- station ID}
#'     \item{station_name}{character -- station name}
#'     \item{street_address}{character -- street address}
#'     \item{city}{character -- city}
#'     \item{state}{character -- two-letter state code}
#'     \item{zip}{character -- ZIP code}
#'     \item{fuel_type_code}{character -- fuel type code}
#'     \item{status_code}{character -- status code}
#'     \item{latitude}{numeric -- decimal degrees}
#'     \item{longitude}{numeric -- decimal degrees}
#'     \item{ev_network}{character -- EV network name (if electric)}
#'     \item{ev_connector_types}{character -- connector types}
#'   }
#' @examples
#' nrel_stations(state = "CA", fuel_type = "ELEC", limit = 10)
#' nrel_stations(state = "NY", fuel_type = "HY")
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
#' Returns full metadata for a single alternative fuel station
#' identified by its numeric ID.
#'
#' @param station_id Integer. Station ID (from \code{nrel_stations()}).
#' @return A tibble with 1 row and all available station fields
#'   (varies by fuel type; typically 30+ columns including address,
#'   hours, network, connector types, access details).
#' @examples
#' nrel_station(65087)
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
#' Returns annual and monthly average solar irradiance data including
#' Direct Normal Irradiance (DNI), Global Horizontal Irradiance (GHI),
#' and latitude-tilt irradiance for a given geographic coordinate.
#'
#' @param lat Numeric. Latitude in decimal degrees (e.g. \code{40.0}
#'   for Boulder, CO).
#' @param lon Numeric. Longitude in decimal degrees (e.g. \code{-105.0}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{parameter}{character -- irradiance parameter name (e.g.
#'       \code{"avg_dni"}, \code{"avg_ghi"}, \code{"avg_lat_tilt"})}
#'     \item{annual}{numeric -- annual average (kWh/m^2/day)}
#'     \item{jan--dec}{numeric -- monthly averages (kWh/m^2/day)}
#'   }
#' @examples
#' nrel_solar(40, -105)    # Boulder, CO
#' nrel_solar(33.45, -112) # Phoenix, AZ
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
#' Returns average commercial, industrial, and residential electricity
#' rates for the utility serving a given geographic coordinate.
#'
#' @param lat Numeric. Latitude in decimal degrees.
#' @param lon Numeric. Longitude in decimal degrees.
#' @return A tibble with columns:
#'   \describe{
#'     \item{utility_name}{character -- name of the electric utility}
#'     \item{residential}{numeric -- residential rate in USD per kWh}
#'     \item{commercial}{numeric -- commercial rate in USD per kWh}
#'     \item{industrial}{numeric -- industrial rate in USD per kWh}
#'   }
#' @examples
#' nrel_utility_rates(40, -105)  # Boulder, CO
#' nrel_utility_rates(40.7, -74) # NYC area
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

#' Get nrel.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nrel_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nrel_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nrel.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nrel.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nrel.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nrel.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
