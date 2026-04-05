# usgs.gov.R
# Self-contained USGS client.
# Covers: earthquake data and water data (NWIS).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required



# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.usgs_base <- "https://earthquake.usgs.gov/fdsnws/event/1"
.water_base <- "https://waterservices.usgs.gov/nwis"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)


# == Schemas ===================================================================

.schema_quakes <- tibble(
  id = character(), time = as.POSIXct(character()), mag = numeric(),
  place = character(), longitude = numeric(), latitude = numeric(),
  depth = numeric(), type = character(), status = character(),
  url = character()
)

.schema_realtime <- tibble(
  site = character(), parameter = character(), datetime = as.POSIXct(character()),
  value = numeric(), qualifier = character(), site_name = character()
)

.schema_daily <- tibble(
  site = character(), parameter = character(), date = as.Date(character()),
  value = numeric(), qualifier = character(), site_name = character()
)

.schema_sites <- tibble(
  site_no = character(), site_name = character(), site_type = character(),
  lat = numeric(), lon = numeric(), state_cd = character(),
  county_cd = character(), huc_cd = character()
)

# == Private response parsers ==================================================

.parse_iv_response <- function(raw) {
  ts <- raw$value$timeSeries
  if (is.null(ts) || length(ts) == 0) return(.schema_realtime)

  rows <- lapply(ts, function(entry) {
    site_code <- entry$sourceInfo$siteCode[[1]]$value
    site_name <- entry$sourceInfo$siteName
    param_code <- entry$variable$variableCode[[1]]$value
    vals <- entry$values[[1]]$value
    if (is.null(vals) || length(vals) == 0) return(NULL)
    datetimes <- vapply(vals, function(v) v$dateTime, character(1))
    values <- vapply(vals, function(v) v$value, character(1))
    qualifiers <- vapply(vals, function(v) {
      q <- v$qualifiers
      if (is.null(q)) NA_character_ else paste(q, collapse = ",")
    }, character(1))
    tibble(
      site = site_code,
      parameter = param_code,
      datetime = as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      value = suppressWarnings(as.numeric(values)),
      qualifier = qualifiers,
      site_name = site_name
    )
  })
  bind_rows(rows)
}

.parse_dv_response <- function(raw) {
  ts <- raw$value$timeSeries
  if (is.null(ts) || length(ts) == 0) return(.schema_daily)

  rows <- lapply(ts, function(entry) {
    site_code <- entry$sourceInfo$siteCode[[1]]$value
    site_name <- entry$sourceInfo$siteName
    param_code <- entry$variable$variableCode[[1]]$value
    vals <- entry$values[[1]]$value
    if (is.null(vals) || length(vals) == 0) return(NULL)
    datetimes <- vapply(vals, function(v) v$dateTime, character(1))
    values <- vapply(vals, function(v) v$value, character(1))
    qualifiers <- vapply(vals, function(v) {
      q <- v$qualifiers
      if (is.null(q)) NA_character_ else paste(q, collapse = ",")
    }, character(1))
    tibble(
      site = site_code,
      parameter = param_code,
      date = as.Date(datetimes),
      value = suppressWarnings(as.numeric(values)),
      qualifier = qualifiers,
      site_name = site_name
    )
  })
  bind_rows(rows)
}



#' Query USGS earthquake data
#'
#' Searches the USGS Earthquake Hazards Program FDSN Event Web Service
#' for earthquake events. Supports filtering by date range, magnitude,
#' and geographic radius. Data includes location, magnitude, depth,
#' and review status.
#'
#' @param starttime Date or character. Start date ("YYYY-MM-DD"). Optional.
#' @param endtime Date or character. End date ("YYYY-MM-DD"). Default: today.
#' @param minmagnitude Numeric. Minimum magnitude threshold (default 2.5).
#' @param maxmagnitude Numeric. Maximum magnitude threshold. Optional.
#' @param limit Integer. Maximum results to return (default 100, max 20000).
#' @param latitude Numeric. Center latitude for radius search (-90 to 90).
#'   Requires \code{longitude} and \code{maxradiuskm}.
#' @param longitude Numeric. Center longitude for radius search (-180 to 180).
#' @param maxradiuskm Numeric. Maximum search radius in kilometers.
#' @param orderby Character. Sort order: "time" (default, newest first),
#'   "time-asc", "magnitude" (largest first), "magnitude-asc".
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. USGS event identifier.}
#'     \item{time}{POSIXct. Event time in UTC.}
#'     \item{mag}{Numeric. Magnitude (e.g. Richter scale).}
#'     \item{place}{Character. Textual description of location.}
#'     \item{longitude}{Numeric. Epicenter longitude in degrees.}
#'     \item{latitude}{Numeric. Epicenter latitude in degrees.}
#'     \item{depth}{Numeric. Depth of the event in kilometers.}
#'     \item{type}{Character. Event type (usually "earthquake").}
#'     \item{status}{Character. Review status ("automatic" or "reviewed").}
#'     \item{url}{Character. URL to the USGS event detail page.}
#'   }
#'
#' @examples
#' \dontrun{
#' usgs_quakes(starttime = "2024-01-01", endtime = "2024-01-31", limit = 10)
#' usgs_quakes(minmagnitude = 5.0, limit = 20)
#'
#' # Earthquakes within 100km of San Francisco
#' usgs_quakes(latitude = 37.77, longitude = -122.42, maxradiuskm = 100)
#' }
#'
#' @seealso [usgs_significant()]
#' @export
usgs_quakes <- function(starttime = NULL, endtime = NULL,
                        minmagnitude = 2.5, maxmagnitude = NULL,
                        limit = 100, latitude = NULL, longitude = NULL,
                        maxradiuskm = NULL, orderby = "time") {
  params <- list(format = "geojson", limit = limit,
                 minmagnitude = minmagnitude, orderby = orderby)
  if (!is.null(starttime)) params$starttime <- as.character(starttime)
  if (!is.null(endtime))   params$endtime <- as.character(endtime)
  if (!is.null(maxmagnitude)) params$maxmagnitude <- maxmagnitude
  if (!is.null(latitude))   params$latitude <- latitude
  if (!is.null(longitude))  params$longitude <- longitude
  if (!is.null(maxradiuskm)) params$maxradiuskm <- maxradiuskm

  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.usgs_base, "/query?", query)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("USGS API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_quakes)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_quakes)

  tibble(
    id        = vapply(features, function(f) f$id %||% NA_character_, character(1)),
    time      = as.POSIXct(vapply(features, function(f) {
      (f$properties$time %||% NA_real_) / 1000
    }, numeric(1)), origin = "1970-01-01", tz = "UTC"),
    mag       = vapply(features, function(f) as.numeric(f$properties$mag %||% NA_real_), numeric(1)),
    place     = vapply(features, function(f) f$properties$place %||% NA_character_, character(1)),
    longitude = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    latitude  = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    depth     = vapply(features, function(f) as.numeric(f$geometry$coordinates[[3]] %||% NA_real_), numeric(1)),
    type      = vapply(features, function(f) f$properties$type %||% NA_character_, character(1)),
    status    = vapply(features, function(f) f$properties$status %||% NA_character_, character(1)),
    url       = vapply(features, function(f) f$properties$url %||% NA_character_, character(1))
  )
}



#' Get significant earthquakes in the last 30 days
#'
#' Fetches the USGS "significant earthquakes" GeoJSON feed, which
#' includes events deemed significant based on magnitude, PAGER
#' alert level, or human impact. Updated every few minutes.
#'
#' @return A tibble with the same columns as [usgs_quakes()]: id, time,
#'   mag, place, longitude, latitude, depth, type, status, url.
#'
#' @examples
#' \dontrun{
#' usgs_significant()
#' }
#'
#' @seealso [usgs_quakes()]
#' @export
usgs_significant <- function() {
  url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/significant_month.geojson"
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("USGS API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_quakes)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_quakes)

  tibble(
    id        = vapply(features, function(f) f$id %||% NA_character_, character(1)),
    time      = as.POSIXct(vapply(features, function(f) (f$properties$time %||% NA_real_) / 1000, numeric(1)),
                           origin = "1970-01-01", tz = "UTC"),
    mag       = vapply(features, function(f) as.numeric(f$properties$mag %||% NA_real_), numeric(1)),
    place     = vapply(features, function(f) f$properties$place %||% NA_character_, character(1)),
    longitude = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    latitude  = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    depth     = vapply(features, function(f) as.numeric(f$geometry$coordinates[[3]] %||% NA_real_), numeric(1)),
    type      = vapply(features, function(f) f$properties$type %||% NA_character_, character(1)),
    status    = vapply(features, function(f) f$properties$status %||% NA_character_, character(1)),
    url       = vapply(features, function(f) f$properties$url %||% NA_character_, character(1))
  )
}



#' Fetch real-time (instantaneous) water data from USGS
#'
#' Retrieves instantaneous values (typically 15-minute intervals) from
#' the USGS National Water Information System (NWIS). Data includes
#' streamflow discharge, gage height, water temperature, and other
#' parameters from USGS monitoring stations.
#'
#' @param sites Character. One or more USGS site numbers
#'   (e.g., "01646500" for Potomac River near Washington DC).
#' @param params Character. Parameter codes to retrieve. Common codes:
#'   "00060" = discharge (cfs), "00065" = gage height (ft),
#'   "00010" = water temperature (C). Default "00060".
#' @param period Character. ISO 8601 duration for the lookback window.
#'   "P1D" = past 1 day (default), "P7D" = past week.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{site}{Character. USGS site number.}
#'     \item{parameter}{Character. Parameter code.}
#'     \item{datetime}{POSIXct. Observation timestamp in UTC.}
#'     \item{value}{Numeric. Measured value.}
#'     \item{qualifier}{Character. Data quality flags (e.g. "P" = provisional).}
#'     \item{site_name}{Character. Station name.}
#'   }
#'
#' @examples
#' \dontrun{
#' water_realtime("01646500")
#' water_realtime(c("01646500", "01594440"), params = c("00060", "00065"))
#' }
#'
#' @seealso [water_daily()], [water_sites()]
#' @export
water_realtime <- function(sites, params = "00060", period = "P1D") {
  sites_str <- paste(sites, collapse = ",")
  params_str <- paste(params, collapse = ",")
  url <- sprintf("%s/iv/?format=json&sites=%s&parameterCd=%s&period=%s",
                 .water_base, sites_str, params_str, period)
  raw <- .fetch_json(url)
  .parse_iv_response(raw)
}


#' Fetch daily values from USGS water data
#'
#' Retrieves daily mean values from USGS NWIS for one or more monitoring
#' sites. Daily values are computed from the instantaneous data and are
#' suitable for time-series analysis over longer periods.
#'
#' @param sites Character. One or more USGS site numbers.
#' @param params Character. Parameter codes (default "00060" = discharge).
#' @param start Character. Start date as "YYYY-MM-DD". Optional.
#' @param end Character. End date as "YYYY-MM-DD". Optional.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{site}{Character. USGS site number.}
#'     \item{parameter}{Character. Parameter code.}
#'     \item{date}{Date. Observation date.}
#'     \item{value}{Numeric. Daily mean value.}
#'     \item{qualifier}{Character. Data quality flags.}
#'     \item{site_name}{Character. Station name.}
#'   }
#'
#' @examples
#' \dontrun{
#' water_daily("01646500", start = "2024-01-01", end = "2024-01-31")
#' }
#'
#' @seealso [water_realtime()], [water_sites()]
#' @export
water_daily <- function(sites, params = "00060", start = NULL, end = NULL) {
  sites_str <- paste(sites, collapse = ",")
  params_str <- paste(params, collapse = ",")
  url <- sprintf("%s/dv/?format=json&sites=%s&parameterCd=%s",
                 .water_base, sites_str, params_str)
  if (!is.null(start)) url <- paste0(url, "&startDT=", start)
  if (!is.null(end)) url <- paste0(url, "&endDT=", end)
  raw <- .fetch_json(url)
  .parse_dv_response(raw)
}


#' Search for USGS water monitoring sites by state
#'
#' Returns active USGS water monitoring sites in a given state,
#' optionally filtered by site type and parameter. Useful for
#' discovering site numbers to pass to [water_realtime()] and
#' [water_daily()].
#'
#' @param state Character. Two-letter state code (e.g., "MD", "CA").
#' @param site_type Character. Site type code. Default "ST" (stream).
#'   Other values: "GW" (groundwater), "LK" (lake), "SP" (spring),
#'   "AT" (atmosphere).
#' @param param_code Character. Optional parameter code to filter sites
#'   that measure a specific parameter (e.g., "00060" for discharge,
#'   "00010" for water temperature). Optional.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{site_no}{Character. USGS site number.}
#'     \item{site_name}{Character. Station name.}
#'     \item{site_type}{Character. Site type code.}
#'     \item{lat}{Numeric. Latitude in decimal degrees.}
#'     \item{lon}{Numeric. Longitude in decimal degrees.}
#'     \item{state_cd}{Character. State FIPS code.}
#'     \item{county_cd}{Character. County FIPS code.}
#'     \item{huc_cd}{Character. Hydrologic Unit Code.}
#'   }
#'
#' @examples
#' \dontrun{
#' water_sites("MD")
#' water_sites("CA", site_type = "GW")
#' water_sites("TX", param_code = "00060")
#' }
#'
#' @seealso [water_site_info()], [water_realtime()]
#' @export
water_sites <- function(state, site_type = "ST", param_code = NULL) {
  url <- sprintf("%s/site/?format=rdb&stateCD=%s&siteType=%s&siteStatus=active",
                 .water_base, state, site_type)
  if (!is.null(param_code)) url <- paste0(url, "&parameterCd=", param_code)

  tmp <- .fetch(url, ext = ".tsv")
  raw_lines <- readLines(tmp, warn = FALSE)

  # Skip comment lines (start with #)
  data_lines <- raw_lines[!grepl("^#", raw_lines)]
  if (length(data_lines) < 3) return(.schema_sites)

  # First line is header, second is format spec, rest is data
  header <- strsplit(data_lines[1], "\t")[[1]]
  data_lines <- data_lines[3:length(data_lines)]

  con <- textConnection(paste(c(data_lines), collapse = "\n"))
  df <- tryCatch(
    read.delim(con, header = FALSE, stringsAsFactors = FALSE,
               col.names = header, sep = "\t"),
    error = function(e) NULL
  )
  close(con)

  if (is.null(df) || nrow(df) == 0) return(.schema_sites)

  nms <- names(df)
  as_tibble(df) |>
    transmute(
      site_no = as.character(site_no),
      site_name = as.character(station_nm),
      site_type = as.character(site_tp_cd),
      lat = suppressWarnings(as.numeric(dec_lat_va)),
      lon = suppressWarnings(as.numeric(dec_long_va)),
      state_cd = as.character(if ("state_cd" %in% nms) .data[["state_cd"]] else NA_character_),
      county_cd = as.character(if ("county_cd" %in% nms) .data[["county_cd"]] else NA_character_),
      huc_cd = as.character(if ("huc_cd" %in% nms) .data[["huc_cd"]] else NA_character_)
    )
}


#' Get site info for a specific USGS monitoring site
#'
#' Fetches expanded metadata for a single USGS monitoring site,
#' including geographic coordinates, altitude, and drainage area.
#'
#' @param site Character. USGS site number (e.g., "01646500" for
#'   Potomac River near Washington DC).
#'
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{site_no}{Character. USGS site number.}
#'     \item{site_name}{Character. Station name.}
#'     \item{site_type}{Character. Site type code (e.g. "ST" = stream).}
#'     \item{lat}{Numeric. Latitude in decimal degrees.}
#'     \item{lon}{Numeric. Longitude in decimal degrees.}
#'     \item{state_cd}{Character. State FIPS code.}
#'     \item{county_cd}{Character. County FIPS code.}
#'     \item{huc_cd}{Character. Hydrologic Unit Code.}
#'     \item{altitude}{Numeric. Altitude of the gage in feet.}
#'     \item{drain_area_sq_mi}{Numeric. Drainage area in square miles.}
#'   }
#'
#' @examples
#' \dontrun{
#' water_site_info("01646500")
#' }
#'
#' @seealso [water_sites()]
#' @export
water_site_info <- function(site) {
  url <- sprintf("%s/site/?format=rdb&sites=%s&siteOutput=expanded",
                 .water_base, site)
  tmp <- .fetch(url, ext = ".tsv")
  raw_lines <- readLines(tmp, warn = FALSE)
  data_lines <- raw_lines[!grepl("^#", raw_lines)]
  if (length(data_lines) < 3) return(.schema_sites)

  header <- strsplit(data_lines[1], "\t")[[1]]
  data_lines <- data_lines[3:length(data_lines)]
  con <- textConnection(paste(data_lines, collapse = "\n"))
  df <- tryCatch(
    read.delim(con, header = FALSE, stringsAsFactors = FALSE,
               col.names = header, sep = "\t"),
    error = function(e) NULL
  )
  close(con)
  if (is.null(df) || nrow(df) == 0) return(.schema_sites)

  nms <- names(df)
  as_tibble(df) |>
    transmute(
      site_no = as.character(site_no),
      site_name = as.character(station_nm),
      site_type = as.character(site_tp_cd),
      lat = suppressWarnings(as.numeric(dec_lat_va)),
      lon = suppressWarnings(as.numeric(dec_long_va)),
      state_cd = as.character(if ("state_cd" %in% nms) .data[["state_cd"]] else NA_character_),
      county_cd = as.character(if ("county_cd" %in% nms) .data[["county_cd"]] else NA_character_),
      huc_cd = as.character(if ("huc_cd" %in% nms) .data[["huc_cd"]] else NA_character_),
      altitude = suppressWarnings(as.numeric(if ("alt_va" %in% nms) .data[["alt_va"]] else NA_real_)),
      drain_area_sq_mi = suppressWarnings(as.numeric(if ("drain_area_va" %in% nms) .data[["drain_area_va"]] else NA_real_))
    )
}


#' Get statistical summaries for a USGS site
#'
#' Returns statistical summaries (mean, count of years with data) based on
#' the full historical record for a site. Daily statistics show the
#' historical mean for each day of the year, useful for comparing current
#' conditions to historical norms.
#'
#' @param site Character. USGS site number (e.g., "01646500").
#' @param params Character. Parameter codes (default "00060" = discharge).
#' @param stat_type Character. Statistic type: "daily" (default) returns
#'   one row per day-of-year, "monthly" returns one per month, "annual"
#'   returns one per year.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{site}{Character. USGS site number.}
#'     \item{parameter}{Character. Parameter code.}
#'     \item{month_day}{Character. Period identifier: "MM-DD" for daily,
#'       "MM" for monthly, or NA for annual.}
#'     \item{mean}{Numeric. Historical mean value for this period.}
#'     \item{count_years}{Integer. Number of years with data for this period.}
#'   }
#'
#' @examples
#' \dontrun{
#' water_stats("01646500")
#' water_stats("01646500", stat_type = "monthly")
#' }
#'
#' @seealso [water_daily()], [water_site_info()]
#' @export
water_stats <- function(site, params = "00060", stat_type = "daily") {
  stat_cd <- switch(stat_type,
    daily = "daily",
    monthly = "monthly",
    annual = "annual",
    "daily"
  )
  url <- sprintf("%s/stat/?format=rdb&sites=%s&parameterCd=%s&statReportType=%s&statTypeCd=all",
                 .water_base, site, paste(params, collapse = ","), stat_cd)

  tmp <- .fetch(url, ext = ".tsv")
  raw_lines <- readLines(tmp, warn = FALSE)
  data_lines <- raw_lines[!grepl("^#", raw_lines)]
  if (length(data_lines) < 3) return(tibble(site = character(), parameter = character(),
                                             month_day = character(), mean = numeric(),
                                             count_years = integer()))

  header <- strsplit(data_lines[1], "\t")[[1]]
  data_lines <- data_lines[3:length(data_lines)]
  con <- textConnection(paste(data_lines, collapse = "\n"))
  df <- tryCatch(
    read.delim(con, header = FALSE, stringsAsFactors = FALSE,
               col.names = header, sep = "\t"),
    error = function(e) NULL
  )
  close(con)
  if (is.null(df) || nrow(df) == 0) return(tibble(site = character(), parameter = character(),
                                                    month_day = character(), mean = numeric(),
                                                    count_years = integer()))

  nms <- names(df)
  result <- as_tibble(df)
  out <- tibble(
    site = as.character(result[[if ("site_no" %in% nms) "site_no" else nms[1]]]),
    parameter = as.character(result[[if ("parameter_cd" %in% nms) "parameter_cd" else nms[2]]])
  )
  if ("month_nu" %in% nms && "day_nu" %in% nms) {
    out$month_day <- sprintf("%02d-%02d",
      suppressWarnings(as.integer(result$month_nu)),
      suppressWarnings(as.integer(result$day_nu)))
  } else if ("month_nu" %in% nms) {
    out$month_day <- sprintf("%02d", suppressWarnings(as.integer(result$month_nu)))
  } else {
    out$month_day <- NA_character_
  }
  out$mean <- suppressWarnings(as.numeric(if ("mean_va" %in% nms) result$mean_va else NA_real_))
  out$count_years <- suppressWarnings(as.integer(if ("count_nu" %in% nms) result$count_nu else NA_integer_))
  out
}


# == Context ===================================================================

#' Get usgs.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
usgs_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(usgs_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/usgs.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "usgs.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# usgs.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# usgs.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
