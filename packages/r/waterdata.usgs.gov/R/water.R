# waterdata.usgs.gov.R - Self-contained waterdata.usgs.gov client



# waterdata-usgs-gov.R
# Self-contained USGS Water Services client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.water_base <- "https://waterservices.usgs.gov/nwis"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

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

# == Private helpers ===========================================================

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

# == Public functions ==========================================================


#' Fetch real-time (instantaneous) water data from USGS
#'
#' Returns instantaneous values (typically 15-minute intervals) for
#' one or more USGS water monitoring sites. Commonly used for real-time
#' streamflow, gage height, and water temperature monitoring.
#'
#' @param sites Character vector of USGS site numbers
#'   (e.g., \code{"01646500"} for Potomac River near Washington, DC).
#' @param params Character vector of USGS parameter codes:
#'   \code{"00060"} = discharge (cfs), \code{"00065"} = gage height (ft),
#'   \code{"00010"} = water temperature (C). Default \code{"00060"}.
#' @param period ISO 8601 duration string for the lookback window:
#'   \code{"P1D"} = past 1 day (default), \code{"P7D"} = past week,
#'   \code{"PT6H"} = past 6 hours.
#' @return A tibble with columns:
#'   \describe{
#'     \item{site}{USGS site number}
#'     \item{parameter}{Parameter code}
#'     \item{datetime}{POSIXct observation timestamp (UTC)}
#'     \item{value}{Numeric measured value}
#'     \item{qualifier}{Data quality flags (e.g., "P" = provisional)}
#'     \item{site_name}{Human-readable site name}
#'   }
#' @export
#' @family USGS Water functions
#' @seealso \code{\link{water_daily}} for daily values,
#'   \code{\link{water_sites}} to find site numbers by state
#' @examples
#' \dontrun{
#' # Current discharge at Potomac River near DC
#' water_realtime("01646500")
#'
#' # Past week of water temperature
#' water_realtime("01646500", params = "00010", period = "P7D")
#' }
water_realtime <- function(sites, params = "00060", period = "P1D") {
  sites_str <- paste(sites, collapse = ",")
  params_str <- paste(params, collapse = ",")
  url <- sprintf("%s/iv/?format=json&sites=%s&parameterCd=%s&period=%s",
                 .water_base, sites_str, params_str, period)
  raw <- .fetch_json_nested(url)
  .parse_iv_response(raw)
}

#' Fetch daily statistical values from USGS water data
#'
#' Returns daily mean values for one or more USGS sites. Daily values
#' are computed from instantaneous measurements and are the standard
#' product for long-term hydrologic analysis.
#'
#' @param sites Character vector of USGS site numbers
#'   (e.g., \code{"01646500"}).
#' @param params Character vector of parameter codes.
#'   Default \code{"00060"} (discharge in cfs).
#' @param start Optional start date as \code{"YYYY-MM-DD"}.
#' @param end Optional end date as \code{"YYYY-MM-DD"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{site}{USGS site number}
#'     \item{parameter}{Parameter code}
#'     \item{date}{Date of the daily value}
#'     \item{value}{Numeric daily mean value}
#'     \item{qualifier}{Data quality flags}
#'     \item{site_name}{Human-readable site name}
#'   }
#' @export
#' @family USGS Water functions
#' @seealso \code{\link{water_realtime}} for instantaneous values,
#'   \code{\link{water_stats}} for historical statistics
#' @examples
#' \dontrun{
#' # Daily discharge for 2023
#' water_daily("01646500", start = "2023-01-01", end = "2023-12-31")
#' }
water_daily <- function(sites, params = "00060", start = NULL, end = NULL) {
  sites_str <- paste(sites, collapse = ",")
  params_str <- paste(params, collapse = ",")
  url <- sprintf("%s/dv/?format=json&sites=%s&parameterCd=%s",
                 .water_base, sites_str, params_str)
  if (!is.null(start)) url <- paste0(url, "&startDT=", start)
  if (!is.null(end)) url <- paste0(url, "&endDT=", end)
  raw <- .fetch_json_nested(url)
  .parse_dv_response(raw)
}

#' Search for USGS water monitoring sites by state
#'
#' Returns active monitoring sites in a state, optionally filtered by
#' site type and measured parameter. Use the returned site numbers
#' with \code{\link{water_realtime}} or \code{\link{water_daily}}.
#'
#' @param state Character two-letter state code (e.g., \code{"MD"},
#'   \code{"CA"}).
#' @param site_type Character site type code. Default \code{"ST"} (stream).
#'   Other options: \code{"GW"} (groundwater well), \code{"LK"} (lake),
#'   \code{"SP"} (spring), \code{"AT"} (atmosphere).
#' @param param_code Optional character parameter code to filter to sites
#'   that measure a specific parameter (e.g., \code{"00060"} for discharge).
#' @return A tibble with columns:
#'   \describe{
#'     \item{site_no}{USGS site number (use with other water_* functions)}
#'     \item{site_name}{Station name}
#'     \item{site_type}{Site type code}
#'     \item{lat}{Numeric latitude in decimal degrees}
#'     \item{lon}{Numeric longitude in decimal degrees}
#'     \item{state_cd}{FIPS state code}
#'     \item{county_cd}{FIPS county code}
#'     \item{huc_cd}{Hydrologic Unit Code}
#'   }
#' @export
#' @family USGS Water functions
#' @seealso \code{\link{water_site_info}} for detailed info on a single site
#' @examples
#' \dontrun{
#' # All stream sites in Maryland
#' water_sites("MD")
#'
#' # Groundwater wells in California that measure water level
#' water_sites("CA", site_type = "GW", param_code = "72019")
#' }
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

#' Get detailed info for a specific USGS monitoring site
#'
#' Returns expanded metadata for a single USGS site including
#' altitude and drainage area. More detailed than \code{\link{water_sites}}.
#'
#' @param site Character USGS site number (e.g., \code{"01646500"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{site_no}{USGS site number}
#'     \item{site_name}{Station name}
#'     \item{site_type}{Site type code}
#'     \item{lat}{Numeric latitude}
#'     \item{lon}{Numeric longitude}
#'     \item{state_cd}{FIPS state code}
#'     \item{county_cd}{FIPS county code}
#'     \item{huc_cd}{Hydrologic Unit Code}
#'     \item{altitude}{Numeric altitude in feet above sea level}
#'     \item{drain_area_sq_mi}{Numeric drainage area in square miles}
#'   }
#' @export
#' @family USGS Water functions
#' @seealso \code{\link{water_sites}} to find sites by state
#' @examples
#' \dontrun{
#' water_site_info("01646500")
#' }
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

#' Get historical statistical summaries for a USGS site
#'
#' Returns statistical summaries (mean, count of years) computed from
#' the full historical record for a site. Useful for comparing current
#' conditions to historical norms.
#'
#' @param site Character USGS site number (e.g., \code{"01646500"}).
#' @param params Character vector of parameter codes.
#'   Default \code{"00060"} (discharge).
#' @param stat_type Character statistic type: \code{"daily"} (default,
#'   stats for each day of year), \code{"monthly"} (monthly stats),
#'   or \code{"annual"} (annual stats).
#' @return A tibble with columns:
#'   \describe{
#'     \item{site}{USGS site number}
#'     \item{parameter}{Parameter code}
#'     \item{month_day}{Month-day string (e.g., "01-15") or month (e.g., "06")}
#'     \item{mean}{Numeric historical mean value}
#'     \item{count_years}{Integer number of years in the record}
#'   }
#' @export
#' @family USGS Water functions
#' @seealso \code{\link{water_daily}} for actual daily values
#' @examples
#' \dontrun{
#' # Daily flow statistics for Potomac River
#' water_stats("01646500")
#'
#' # Monthly statistics
#' water_stats("01646500", stat_type = "monthly")
#' }
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

#' Get waterdata.usgs.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
water_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(water_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/waterdata.usgs.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "waterdata.usgs.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# waterdata.usgs.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# waterdata.usgs.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
