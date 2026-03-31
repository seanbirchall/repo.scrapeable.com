#' Fetch real-time (instantaneous) water data from USGS
#'
#' @param sites Character vector of USGS site numbers (e.g., "01646500")
#' @param params Parameter codes: "00060" = discharge, "00065" = gage height,
#'   "00010" = water temp. Default "00060".
#' @param period ISO 8601 duration (e.g., "P1D" = past 1 day, "P7D" = past week).
#'   Default "P1D".
#' @return tibble: site, parameter, datetime, value, qualifier, site_name
#' @export
water_realtime <- function(sites, params = "00060", period = "P1D") {
  sites_str <- paste(sites, collapse = ",")
  params_str <- paste(params, collapse = ",")
  url <- sprintf("%s/iv/?format=json&sites=%s&parameterCd=%s&period=%s",
                 .water_base, sites_str, params_str, period)
  raw <- .fetch_json_nested(url)
  .parse_iv_response(raw)
}

#' Fetch daily values from USGS water data
#'
#' @param sites Character vector of USGS site numbers
#' @param params Parameter codes. Default "00060" (discharge).
#' @param start Start date as "YYYY-MM-DD"
#' @param end End date as "YYYY-MM-DD"
#' @return tibble: site, parameter, date, value, qualifier, site_name
#' @export
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
#' @param state Two-letter state code (e.g., "MD", "CA")
#' @param site_type Site type code. Default "ST" (stream). Use "GW" for
#'   groundwater, "LK" for lake, "SP" for spring.
#' @param param_code Optional parameter code to filter sites that measure
#'   a specific parameter (e.g., "00060" for discharge)
#' @return tibble: site_no, site_name, site_type, lat, lon, state_cd,
#'   county_cd, huc_cd
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

  as_tibble(df) |>
    transmute(
      site_no = as.character(site_no),
      site_name = station_nm,
      site_type = site_tp_cd,
      lat = suppressWarnings(as.numeric(dec_lat_va)),
      lon = suppressWarnings(as.numeric(dec_long_va)),
      state_cd = state_cd,
      county_cd = county_cd,
      huc_cd = huc_cd
    )
}

#' Print USGS Water Services context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
water_context <- function() {
  .build_context(
    pkg_name = "waterdata.usgs.gov",
    header_lines = c(
      "# Package: waterdata.usgs.gov",
      "# USGS Water Services API - real-time and daily water data",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# Common parameter codes:",
      "#   00060 = discharge (cfs), 00065 = gage height (ft),",
      "#   00010 = water temp (C), 00400 = pH",
      "#",
      "# Common sites: 01646500 (Potomac at Little Falls),",
      "#   09380000 (Colorado at Lees Ferry), 07010000 (Mississippi at St Louis)"
    )
  )
}
