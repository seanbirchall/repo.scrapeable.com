# waterdata-usgs-gov.R
# Self-contained USGS Water Services client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.water_base <- "https://waterservices.usgs.gov/nwis"

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
