# == Core data fetching ========================================================

#' Fetch BLS time series data
#'
#' Retrieves observations for one or more BLS series IDs.
#' Without an API key: max 25 series, 10-year range, 25 queries/day.
#' With key: max 50 series, 20-year range, 500 queries/day.
#'
#' @param series Character vector of BLS series IDs (e.g. "CUUR0000SA0" for CPI-U)
#' @param start_year Start year (integer, default: current year - 9)
#' @param end_year End year (integer, default: current year)
#' @param key Optional BLS API registration key
#' @param catalog If TRUE and key provided, include series metadata
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_series <- function(series, start_year = NULL, end_year = NULL,
                       key = NULL, catalog = FALSE) {
  if (is.null(end_year)) end_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(start_year)) start_year <- end_year - 9L

  body <- list(
    seriesid = as.list(series),
    startyear = as.character(start_year),
    endyear = as.character(end_year)
  )
  if (catalog && !is.null(key)) body$catalog <- TRUE

  raw <- .bls_post(body, key)
  results <- raw$Results$series
  if (is.null(results) || length(results) == 0) return(.schema_series)

  bind_rows(lapply(results, function(s) {
    data <- s$data
    if (is.null(data) || length(data) == 0) return(NULL)

    bind_rows(lapply(data, function(d) {
      tibble(
        series_id   = s$seriesID,
        year        = as.integer(d$year),
        period      = d$period,
        period_name = d$periodName,
        value       = as.numeric(d$value)
      )
    })) |>
      mutate(
        # Convert period to date: M01-M12 = monthly, Q01-Q05 = quarterly, A01 = annual
        date = case_when(
          grepl("^M\\d{2}$", period) ~
            as.Date(sprintf("%d-%s-01", year, sub("^M", "", period))),
          grepl("^Q01$", period) ~ as.Date(sprintf("%d-01-01", year)),
          grepl("^Q02$", period) ~ as.Date(sprintf("%d-04-01", year)),
          grepl("^Q03$", period) ~ as.Date(sprintf("%d-07-01", year)),
          grepl("^Q04$", period) ~ as.Date(sprintf("%d-10-01", year)),
          grepl("^Q05$", period) ~ as.Date(sprintf("%d-01-01", year)),
          grepl("^A01$", period) ~ as.Date(sprintf("%d-01-01", year)),
          TRUE ~ as.Date(NA)
        )
      )
  })) |> arrange(series_id, date)
}

#' Fetch multiple series with automatic chunking
#'
#' Handles the BLS limit of 25 (no key) or 50 (with key) series per request
#' by splitting into chunks automatically.
#'
#' @param series Character vector of BLS series IDs (any length)
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @param sleep Seconds between chunk requests (default 1)
#' @return tibble: same schema as bls_series
#' @export
bls_series_bulk <- function(series, start_year = NULL, end_year = NULL,
                            key = NULL, sleep = 1) {
  chunk_size <- if (!is.null(key)) 50L else 25L
  chunks <- split(series, ceiling(seq_along(series) / chunk_size))
  n <- length(chunks)

  results <- lapply(seq_along(chunks), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] Fetching %d series...", i, n, length(chunks[[i]])))
    tryCatch(
      bls_series(chunks[[i]], start_year, end_year, key),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
  })
  bind_rows(results)
}


# == Series catalog/metadata ===================================================

#' Fetch series catalog metadata
#'
#' Returns descriptive information about series (title, survey, area, item).
#' Requires an API key.
#'
#' @param series Character vector of series IDs
#' @param key BLS API registration key (required for catalog)
#' @return tibble: series_id, series_title, survey_name, survey_abbreviation,
#'   seasonal, area_code, area_name, item_code, item_name
#' @export
bls_catalog <- function(series, key) {
  body <- list(
    seriesid = as.list(series),
    startyear = format(Sys.Date(), "%Y"),
    endyear = format(Sys.Date(), "%Y"),
    catalog = TRUE,
    registrationKey = key
  )
  raw <- .bls_post(body, key)
  results <- raw$Results$series
  if (is.null(results) || length(results) == 0) return(.schema_catalog)

  bind_rows(lapply(results, function(s) {
    cat <- s$catalog
    if (is.null(cat)) return(NULL)
    tibble(
      series_id            = s$seriesID,
      series_title         = cat$series_title %||% NA_character_,
      survey_name          = cat$survey_name %||% NA_character_,
      survey_abbreviation  = cat$survey_abbreviation %||% NA_character_,
      seasonal             = cat$seasonal %||% NA_character_,
      area_code            = cat$area_code %||% NA_character_,
      area_name            = cat$area_name %||% NA_character_,
      item_code            = cat$item_code %||% NA_character_,
      item_name            = cat$item_name %||% NA_character_
    )
  }))
}


# == Convenience wrappers (popular series) =====================================

#' Consumer Price Index (CPI-U, All Items, US City Average)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @param seasonal "S" for seasonally adjusted (default), "U" for unadjusted
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_cpi <- function(start_year = NULL, end_year = NULL, key = NULL,
                    seasonal = "S") {
  sid <- if (seasonal == "S") "CUSR0000SA0" else "CUUR0000SA0"
  bls_series(sid, start_year, end_year, key)
}

#' Unemployment Rate (seasonally adjusted)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_unemployment <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("LNS14000000", start_year, end_year, key)
}

#' Total Nonfarm Payrolls (seasonally adjusted)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_payrolls <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("CES0000000001", start_year, end_year, key)
}

#' Average Hourly Earnings (private sector, seasonally adjusted)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_earnings <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("CES0500000003", start_year, end_year, key)
}

#' Producer Price Index (all commodities)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_ppi <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("WPUFD4", start_year, end_year, key)
}

#' Employment Cost Index (total compensation, private)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_eci <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("CIS1010000000000A", start_year, end_year, key)
}

#' Job Openings (JOLTS, total nonfarm)
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key
#' @return tibble: series_id, year, period, period_name, value, date
#' @export
bls_jolts <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("JTS000000000000000JOL", start_year, end_year, key)
}


# == Key economic indicators bundle ============================================

#' Fetch all major economic indicators in one call
#'
#' Returns CPI, unemployment, payrolls, earnings, PPI, and JOLTS
#' in a single tibble.
#'
#' @param start_year Start year
#' @param end_year End year
#' @param key Optional API key (recommended to avoid rate limits)
#' @return tibble: series_id, year, period, period_name, value, date,
#'   plus indicator (human-readable name)
#' @export
bls_indicators <- function(start_year = NULL, end_year = NULL, key = NULL) {
  ids <- c(
    "CUSR0000SA0",              # CPI-U (seasonally adjusted)
    "LNS14000000",              # Unemployment rate
    "CES0000000001",            # Total nonfarm payrolls
    "CES0500000003",            # Average hourly earnings
    "WPUFD4",                   # PPI all commodities
    "JTS000000000000000JOL"     # JOLTS job openings
  )
  labels <- c("CPI", "Unemployment Rate", "Nonfarm Payrolls",
              "Avg Hourly Earnings", "PPI", "Job Openings")

  df <- bls_series(ids, start_year, end_year, key)
  if (nrow(df) == 0) return(df)

  lookup <- tibble(series_id = ids, indicator = labels)
  df |> left_join(lookup, by = "series_id")
}


# == Context ===================================================================

#' Generate LLM-friendly context for the data.bls.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
bls_context <- function() {
  .build_context("data.bls.gov", header_lines = c(
    "# data.bls.gov - Bureau of Labor Statistics API v2 Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: optional registrationKey (25 req/day without, 500/day with)",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Popular series IDs:",
    "#   CUUR0000SA0    = CPI-U All Items (unadjusted)",
    "#   CUSR0000SA0    = CPI-U All Items (seasonally adjusted)",
    "#   LNS14000000    = Unemployment Rate",
    "#   CES0000000001  = Total Nonfarm Payrolls",
    "#   CES0500000003  = Avg Hourly Earnings (private)",
    "#   WPUFD4         = PPI All Commodities",
    "#   JTS000000000000000JOL = JOLTS Job Openings",
    "#",
    "# Series ID format: {survey_prefix}{seasonal}{area}{item}",
    "#   CU = CPI, LN = Labor Force, CE = Employment, WP = PPI, JT = JOLTS"
  ))
}
