# noaa.gov.R
# Self-contained NOAA client.
# Covers: Tides & Currents, Global Monitoring Lab (CO2/CH4), and Space Weather.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required



# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.base <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
.gml_base <- "https://gml.noaa.gov/webdata/ccgg/trends"
.swpc_base <- "https://services.swpc.noaa.gov/products"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
.fetch_csv <- function(url) .fetch(url, ext = ".csv")

.parse_swpc_array <- function(raw) {
  if (length(raw) < 2) return(tibble())
  headers <- unlist(raw[[1]])
  rows <- raw[-1]
  mat <- do.call(rbind, lapply(rows, function(r) unlist(r)))
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- headers
  as_tibble(df)
}


# == Schemas ===================================================================

.schema_trend <- tibble(
  date = as.Date(character()), smoothed = numeric(), trend = numeric()
)

.schema_annual <- tibble(
  year = integer(), mean = numeric(), unc = numeric()
)

.schema_monthly <- tibble(
  year = integer(), month = integer(), decimal = numeric(),
  average = numeric(), average_unc = numeric(),
  trend = numeric(), trend_unc = numeric()
)

.schema_plasma <- tibble(
  time_tag = as.POSIXct(character()), density = numeric(),
  speed = numeric(), temperature = numeric()
)

.schema_mag <- tibble(
  time_tag = as.POSIXct(character()), bx_gsm = numeric(),
  by_gsm = numeric(), bz_gsm = numeric(),
  lon_gsm = numeric(), lat_gsm = numeric(), bt = numeric()
)

.schema_kp <- tibble(
  time_tag = as.POSIXct(character()), kp = numeric(),
  a_running = numeric(), station_count = integer()
)

.schema_alerts <- tibble(
  product_id = character(), issue_datetime = as.POSIXct(character()),
  message = character()
)



#' Get tide predictions for a NOAA station
#'
#' Returns predicted tide heights (in meters relative to MLLW datum) at
#' 6-minute intervals for a given station and date range. Uses the NOAA
#' CO-OPS Tides & Currents API.
#'
#' @param station Character. NOAA station ID. Use \code{tides_stations()} to
#'   discover valid IDs. Examples: \code{"9414290"} (San Francisco),
#'   \code{"8518750"} (The Battery, NYC), \code{"8723214"} (Virginia Key, FL).
#' @param begin_date Character. Start date in \code{"YYYYMMDD"} format
#'   (default: today).
#' @param end_date Character. End date in \code{"YYYYMMDD"} format
#'   (default: tomorrow). Maximum range is 31 days.
#' @return A tibble with columns:
#'   \describe{
#'     \item{time}{character -- timestamp in \code{"YYYY-MM-DD HH:MM"} format (GMT)}
#'     \item{value}{numeric -- predicted water level in meters (MLLW datum)}
#'   }
#' @examples
#' # Predictions for San Francisco, today through tomorrow
#' tides_predictions("9414290")
#'
#' # Predictions for NYC Battery, specific date range
#' tides_predictions("8518750", begin_date = "20260101", end_date = "20260102")
tides_predictions <- function(station, begin_date = format(Sys.Date(), "%Y%m%d"),
                              end_date = format(Sys.Date() + 1, "%Y%m%d")) {
  url <- sprintf("%s?station=%s&begin_date=%s&end_date=%s&product=predictions&datum=MLLW&units=metric&time_zone=gmt&application=scrapeable&format=json",
                 .base, station, begin_date, end_date)
  raw <- .fetch_json(url)
  preds <- raw$predictions
  if (is.null(preds) || length(preds) == 0) {
    return(tibble::tibble(time = character(), value = numeric()))
  }
  tibble::tibble(
    time  = as.character(preds$t),
    value = as.numeric(preds$v)
  )
}


#' Get observed water levels for a NOAA station
#'
#' Returns verified or preliminary 6-minute water level observations
#' (in meters relative to MLLW datum) for a given station and date range.
#'
#' @param station Character. NOAA station ID. Use \code{tides_stations()} to
#'   discover valid IDs. Examples: \code{"9414290"} (San Francisco),
#'   \code{"8518750"} (The Battery, NYC).
#' @param begin_date Character. Start date in \code{"YYYYMMDD"} format
#'   (default: yesterday).
#' @param end_date Character. End date in \code{"YYYYMMDD"} format
#'   (default: today). Maximum range is 31 days.
#' @return A tibble with columns:
#'   \describe{
#'     \item{time}{character -- timestamp in \code{"YYYY-MM-DD HH:MM"} format (GMT)}
#'     \item{value}{numeric -- observed water level in meters (MLLW datum)}
#'     \item{quality}{character -- data quality flag: \code{"v"} (verified),
#'       \code{"p"} (preliminary)}
#'   }
#' @examples
#' # Recent observations for San Francisco
#' tides_water_levels("9414290")
#'
#' # Observations for a specific date range
#' tides_water_levels("8518750", begin_date = "20260101", end_date = "20260102")
tides_water_levels <- function(station, begin_date = format(Sys.Date() - 1, "%Y%m%d"),
                               end_date = format(Sys.Date(), "%Y%m%d")) {
  url <- sprintf("%s?station=%s&begin_date=%s&end_date=%s&product=water_level&datum=MLLW&units=metric&time_zone=gmt&application=scrapeable&format=json",
                 .base, station, begin_date, end_date)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) {
    return(tibble::tibble(time = character(), value = numeric(), quality = character()))
  }
  tibble::tibble(
    time    = as.character(data$t),
    value   = as.numeric(data$v),
    quality = as.character(data$q)
  )
}


#' List all NOAA Tides & Currents stations
#'
#' Returns metadata for all ~301 active NOAA water level stations
#' across the U.S. and territories. Use a station's \code{id} with
#' \code{tides_predictions()} or \code{tides_water_levels()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- station ID (e.g. \code{"9414290"})}
#'     \item{name}{character -- station name (e.g. \code{"San Francisco"})}
#'     \item{state}{character -- two-letter state code (e.g. \code{"CA"})}
#'     \item{latitude}{numeric -- decimal degrees north}
#'     \item{longitude}{numeric -- decimal degrees east}
#'   }
#' @examples
#' tides_stations()
tides_stations <- function() {
  url <- "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.json"
  raw <- .fetch_json(url)
  stns <- raw$stations
  if (is.null(stns) || length(stns) == 0) {
    return(tibble::tibble(id = character(), name = character(),
                          state = character(), latitude = numeric(),
                          longitude = numeric()))
  }
  tibble::tibble(
    id        = as.character(stns$id),
    name      = as.character(stns$name),
    state     = as.character(stns$state),
    latitude  = as.numeric(stns$lat),
    longitude = as.numeric(stns$lng)
  )
}



#' Fetch daily global CO2 trend data
#'
#' Returns smoothed and long-term trend values of globally-averaged
#' atmospheric CO2 concentration from the NOAA Global Monitoring
#' Laboratory (GML). Updated daily from flask and in-situ measurements.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- observation date}
#'     \item{smoothed}{numeric -- smoothed CO2 concentration in ppm}
#'     \item{trend}{numeric -- long-term trend CO2 concentration in ppm}
#'   }
#' @examples
#' co2_global_trend()
co2_global_trend <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_trend_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_trend)
  raw |>
    as_tibble() |>
    transmute(
      date     = as.Date(sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))),
      smoothed = as.numeric(smoothed),
      trend    = as.numeric(trend)
    )
}


#' Fetch annual global mean CO2
#'
#' Returns yearly globally-averaged atmospheric CO2 concentrations
#' from 1979 to the most recent complete year. Source: NOAA GML.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{integer -- calendar year (1979--present)}
#'     \item{mean}{numeric -- annual mean CO2 concentration in ppm}
#'     \item{unc}{numeric -- uncertainty of the annual mean in ppm}
#'   }
#' @examples
#' co2_annual()
co2_annual <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_annmean_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_annual)
  raw |>
    as_tibble() |>
    transmute(
      year = as.integer(year),
      mean = as.numeric(mean),
      unc  = as.numeric(unc)
    )
}


#' Fetch monthly global CO2 data
#'
#' Returns monthly globally-averaged atmospheric CO2 concentrations
#' with trend decomposition. Source: NOAA GML.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{integer -- calendar year}
#'     \item{month}{integer -- month (1--12)}
#'     \item{decimal}{numeric -- decimal date (e.g. 2025.042)}
#'     \item{average}{numeric -- monthly mean CO2 in ppm}
#'     \item{average_unc}{numeric -- uncertainty of monthly mean in ppm}
#'     \item{trend}{numeric -- de-seasonalized trend value in ppm}
#'     \item{trend_unc}{numeric -- uncertainty of trend in ppm}
#'   }
#' @examples
#' co2_monthly()
co2_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}


#' Fetch monthly global CH4 (methane) data
#'
#' Returns monthly globally-averaged atmospheric methane (CH4)
#' concentrations with trend decomposition. Source: NOAA GML.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{integer -- calendar year}
#'     \item{month}{integer -- month (1--12)}
#'     \item{decimal}{numeric -- decimal date}
#'     \item{average}{numeric -- monthly mean CH4 in ppb}
#'     \item{average_unc}{numeric -- uncertainty of monthly mean in ppb}
#'     \item{trend}{numeric -- de-seasonalized trend value in ppb}
#'     \item{trend_unc}{numeric -- uncertainty of trend in ppb}
#'   }
#' @examples
#' co2_ch4_monthly()
co2_ch4_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/ch4/ch4_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  # ch4 csv may have blank rows
  raw <- raw[!is.na(raw$year) & raw$year != "", ]
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}


#' Fetch NOAA SWPC solar wind plasma data
#'
#' Returns solar wind plasma measurements (density, speed, temperature)
#' from the DSCOVR satellite at L1 Lagrange point. Data is provided
#' at 1-minute resolution within the selected time window.
#'
#' @param days Numeric. Time window to retrieve. Valid values:
#'   \code{0.08} (2 hours), \code{0.25} (6 hours), \code{1} (1 day),
#'   \code{3} (3 days), \code{7} (7 days, default).
#' @return A tibble with columns:
#'   \describe{
#'     \item{time_tag}{POSIXct -- measurement timestamp (UTC)}
#'     \item{density}{numeric -- proton density in particles/cm^3}
#'     \item{speed}{numeric -- bulk solar wind speed in km/s}
#'     \item{temperature}{numeric -- proton temperature in Kelvin}
#'   }
#' @examples
#' swpc_plasma(1)   # last 24 hours
#' swpc_plasma(0.08) # last 2 hours
swpc_plasma <- function(days = 7) {
  slug <- switch(as.character(days),
    "0.08" = "2-hour", "0.25" = "6-hour",
    "1" = "1-day", "3" = "3-day", "7" = "7-day",
    stop("days must be one of: 0.08, 0.25, 1, 3, 7")
  )
  url <- sprintf("%s/solar-wind/plasma-%s.json", .swpc_base, slug)
  raw <- .fetch_json_nested(url)
  df <- .parse_swpc_array(raw)
  if (nrow(df) == 0) return(.schema_plasma)

  df |>
    transmute(
      time_tag    = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      density     = as.numeric(density),
      speed       = as.numeric(speed),
      temperature = as.numeric(temperature)
    )
}


#' Fetch NOAA SWPC solar wind magnetic field data
#'
#' Returns interplanetary magnetic field (IMF) measurements in
#' Geocentric Solar Magnetospheric (GSM) coordinates from the
#' DSCOVR satellite at L1.
#'
#' @param days Numeric. Time window to retrieve. Valid values:
#'   \code{0.08} (2 hours), \code{0.25} (6 hours), \code{1} (1 day),
#'   \code{3} (3 days), \code{7} (7 days, default).
#' @return A tibble with columns:
#'   \describe{
#'     \item{time_tag}{POSIXct -- measurement timestamp (UTC)}
#'     \item{bx_gsm}{numeric -- IMF Bx component in nT (GSM coords)}
#'     \item{by_gsm}{numeric -- IMF By component in nT (GSM coords)}
#'     \item{bz_gsm}{numeric -- IMF Bz component in nT (GSM coords);
#'       negative values indicate southward field}
#'     \item{lon_gsm}{numeric -- GSM longitude in degrees}
#'     \item{lat_gsm}{numeric -- GSM latitude in degrees}
#'     \item{bt}{numeric -- total magnetic field magnitude in nT}
#'   }
#' @examples
#' swpc_mag(1)   # last 24 hours
#' swpc_mag(0.25) # last 6 hours
swpc_mag <- function(days = 7) {
  slug <- switch(as.character(days),
    "0.08" = "2-hour", "0.25" = "6-hour",
    "1" = "1-day", "3" = "3-day", "7" = "7-day",
    stop("days must be one of: 0.08, 0.25, 1, 3, 7")
  )
  url <- sprintf("%s/solar-wind/mag-%s.json", .swpc_base, slug)
  raw <- .fetch_json_nested(url)
  df <- .parse_swpc_array(raw)
  if (nrow(df) == 0) return(.schema_mag)

  df |>
    transmute(
      time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      bx_gsm   = as.numeric(bx_gsm),
      by_gsm   = as.numeric(by_gsm),
      bz_gsm   = as.numeric(bz_gsm),
      lon_gsm  = as.numeric(lon_gsm),
      lat_gsm  = as.numeric(lat_gsm),
      bt       = as.numeric(bt)
    )
}


#' Fetch NOAA planetary Kp index
#'
#' Returns the planetary K-index (Kp), a 3-hour geomagnetic activity
#' index on a 0--9 scale. Values >= 5 indicate geomagnetic storms
#' (G1+). Covers the most recent ~7 days of data.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{time_tag}{POSIXct -- 3-hour period start (UTC)}
#'     \item{kp}{numeric -- Kp index value (0--9)}
#'     \item{a_running}{numeric -- running A-index value}
#'     \item{station_count}{integer -- number of reporting stations}
#'   }
#' @examples
#' swpc_kp_index()
swpc_kp_index <- function() {
  url <- sprintf("%s/noaa-planetary-k-index.json", .swpc_base)
  raw <- .fetch_json_nested(url)
  if (length(raw) == 0) return(.schema_kp)

  mat <- do.call(rbind, lapply(raw, unlist))
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- c("time_tag", "Kp", "a_running", "station_count")

  tibble(
    time_tag      = as.POSIXct(df$time_tag, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    kp            = as.numeric(df$Kp),
    a_running     = as.numeric(df$a_running),
    station_count = as.integer(df$station_count)
  )
}


#' Fetch NOAA space weather alerts
#'
#' Returns recent space weather alerts, watches, and warnings issued
#' by the NOAA Space Weather Prediction Center (SWPC). Includes solar
#' flare warnings, geomagnetic storm watches, and radiation alerts.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{product_id}{character -- unique alert identifier}
#'     \item{issue_datetime}{POSIXct -- alert issue timestamp (UTC)}
#'     \item{message}{character -- full alert text}
#'   }
#' @examples
#' swpc_alerts()
swpc_alerts <- function() {
  url <- sprintf("%s/alerts.json", .swpc_base)
  raw <- .fetch_json_nested(url)
  if (length(raw) == 0) return(.schema_alerts)

  rows <- lapply(raw, function(x) {
    tibble(
      product_id     = as.character(x$product_id %||% NA),
      issue_datetime = as.POSIXct(x$issue_datetime %||% NA,
                                  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      message        = as.character(x$message %||% NA)
    )
  })
  bind_rows(rows)
}


# == Context ===================================================================

#' Get noaa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
noaa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(noaa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/noaa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "noaa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# noaa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# noaa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
