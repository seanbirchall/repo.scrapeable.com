#' Fetch SOFR (Secured Overnight Financing Rate) data
#'
#' @param start Start date (Date or "YYYY-MM-DD"), default last 30 days
#' @param end End date (Date or "YYYY-MM-DD"), default today
#' @return tibble: date, rate, percentile_1, percentile_25,
#'   percentile_75, percentile_99, volume
#' @export
nyfed_sofr <- function(start = NULL, end = NULL) {
  if (is.null(start)) start <- Sys.Date() - 30
  if (is.null(end)) end <- Sys.Date()
  url <- sprintf(
    "%s/rates/secured/all/search.json?startDate=%s&endDate=%s&type=rate",
    .nyfed_base, .nyfed_date(start), .nyfed_date(end)
  )
  raw <- .fetch_json(url)
  rates <- raw$refRates
  if (is.null(rates) || length(rates) == 0) return(.schema_sofr)
  if (is.data.frame(rates) && nrow(rates) == 0) return(.schema_sofr)

  # Filter to SOFR only
  if (is.data.frame(rates)) {
    sofr <- rates[rates$type == "SOFR", , drop = FALSE]
    if (nrow(sofr) == 0) return(.schema_sofr)
    as_tibble(sofr) |>
      transmute(
        date = as.Date(effectiveDate),
        rate = as.numeric(percentRate %||% NA),
        percentile_1 = as.numeric(if ("percentPercentile1" %in% names(sofr)) percentPercentile1 else NA),
        percentile_25 = as.numeric(if ("percentPercentile25" %in% names(sofr)) percentPercentile25 else NA),
        percentile_75 = as.numeric(if ("percentPercentile75" %in% names(sofr)) percentPercentile75 else NA),
        percentile_99 = as.numeric(if ("percentPercentile99" %in% names(sofr)) percentPercentile99 else NA),
        volume = as.numeric(if ("volumeInBillions" %in% names(sofr)) volumeInBillions else NA)
      )
  } else {
    .schema_sofr
  }
}

# == EFFR (Effective Federal Funds Rate) =======================================

#' Fetch EFFR (Effective Federal Funds Rate) data
#'
#' @param start Start date (Date or "YYYY-MM-DD"), default last 30 days
#' @param end End date (Date or "YYYY-MM-DD"), default today
#' @return tibble: date, rate, percentile_1, percentile_25,
#'   percentile_75, percentile_99, volume
#' @export
nyfed_effr <- function(start = NULL, end = NULL) {
  if (is.null(start)) start <- Sys.Date() - 30
  if (is.null(end)) end <- Sys.Date()
  url <- sprintf(
    "%s/rates/unsecured/all/search.json?startDate=%s&endDate=%s&type=rate",
    .nyfed_base, .nyfed_date(start), .nyfed_date(end)
  )
  raw <- .fetch_json(url)
  rates <- raw$refRates
  if (is.null(rates) || length(rates) == 0) return(.schema_effr)
  if (is.data.frame(rates) && nrow(rates) == 0) return(.schema_effr)

  if (is.data.frame(rates)) {
    effr <- rates[rates$type == "EFFR", , drop = FALSE]
    if (nrow(effr) == 0) return(.schema_effr)
    as_tibble(effr) |>
      transmute(
        date = as.Date(effectiveDate),
        rate = as.numeric(percentRate %||% NA),
        percentile_1 = as.numeric(if ("percentPercentile1" %in% names(effr)) percentPercentile1 else NA),
        percentile_25 = as.numeric(if ("percentPercentile25" %in% names(effr)) percentPercentile25 else NA),
        percentile_75 = as.numeric(if ("percentPercentile75" %in% names(effr)) percentPercentile75 else NA),
        percentile_99 = as.numeric(if ("percentPercentile99" %in% names(effr)) percentPercentile99 else NA),
        volume = as.numeric(if ("volumeInBillions" %in% names(effr)) volumeInBillions else NA)
      )
  } else {
    .schema_effr
  }
}

# == Treasury securities =======================================================

#' Fetch Treasury securities operations data
#'
#' Retrieves results from the NY Fed's Treasury securities operations
#' (e.g., outright purchases, sales).
#'
#' @param start Start date (Date or "YYYY-MM-DD"), default last 30 days
#' @param end End date (Date or "YYYY-MM-DD"), default today
#' @param operation Operation type: "all", "purchases", "sales" (default "all")
#' @return tibble: date, maturity, rate
#' @export
nyfed_treasury <- function(start = NULL, end = NULL, operation = "all") {
  if (is.null(start)) start <- Sys.Date() - 30
  if (is.null(end)) end <- Sys.Date()
  url <- sprintf(
    "%s/tsy/%s/results/search.json?startDate=%s&endDate=%s",
    .nyfed_base, operation, .nyfed_date(start), .nyfed_date(end)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_treasury)

  # The treasury ops response has nested structure
  auctions <- raw$treasury$auctions %||% raw$auctions %||% NULL
  if (is.null(auctions) || length(auctions) == 0) return(.schema_treasury)
  if (is.data.frame(auctions) && nrow(auctions) == 0) return(.schema_treasury)

  if (is.data.frame(auctions)) {
    as_tibble(auctions) |>
      transmute(
        date = tryCatch(as.Date(operationDate %||% auctionDate %||% NA), error = function(e) NA_real_),
        maturity = as.character(if ("securityType" %in% names(auctions)) securityType else NA),
        rate = as.numeric(if ("highRate" %in% names(auctions)) highRate else if ("rate" %in% names(auctions)) rate else NA)
      )
  } else {
    .schema_treasury
  }
}

# == Context ===================================================================

#' Generate LLM-friendly context for the newyorkfed.org package
#'
#' @return Character string (invisibly), also printed
#' @export
nyfed_context <- function() {
  .build_context("newyorkfed.org", header_lines = c(
    "# newyorkfed.org - New York Fed Markets Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://markets.newyorkfed.org/api",
    "# All functions return tibbles with typed columns.",
    "# Key rates: SOFR (secured), EFFR (unsecured/fed funds)"
  ))
}
