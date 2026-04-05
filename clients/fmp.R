# fmp.R
# Financial Modeling Prep (financialmodelingprep.com) client.
# All functions return data.frames. No database dependencies.
#
# Requires an FMP API key: https://financialmodelingprep.com/developer

# == Setup =====================================================================



# -- HTTP helpers (inlined) ----------------------------------------------------
http_get <- function(url, ext = ".html", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

http_post <- function(url, body, ext = ".zip", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fmp_base_v3 <- "https://financialmodelingprep.com/api/v3"
.fmp_base_v4 <- "https://financialmodelingprep.com/api/v4"

.fmp_get <- function(url, ext = ".json") http_get(url, ext)

.fmp_json <- function(url) {
  path <- .fmp_get(url)
  df <- jsonlite::fromJSON(path)
  if (is.null(df) || length(df) == 0) return(data.frame())
  as.data.frame(df)
}

.fmp_csv <- function(url) {
  path <- .fmp_get(url, ".csv")
  df <- read.csv(path, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(data.frame())
  df
}



# == Stock list ================================================================

#' Fetch list of all FMP-supported symbols
#'
#' @param api_key FMP API key
#' @return data.frame with symbol, name, exchange, type
fmp_tickers <- function(api_key) {
  url <- paste0(.fmp_base_v3, "/stock/list?apikey=", api_key)
  .fmp_json(url)
}


# == Financial statements (single ticker) ======================================

#' Fetch a financial statement for a single ticker
#'
#' @param ticker Stock ticker
#' @param statement One of: "income-statement", "balance-sheet-statement",
#'   "cash-flow-statement"
#' @param api_key FMP API key
#' @param period "annual" or "quarter" (default: "annual")
#' @param limit Number of periods to return
#' @return data.frame of financial statement data
fmp_statement <- function(ticker, statement, api_key, period = "annual", limit = 40) {
  url <- paste0(.fmp_base_v3, "/", statement, "/", ticker,
                "?period=", period, "&limit=", limit, "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch income statement
#' @inheritParams fmp_statement
fmp_income <- function(ticker, api_key, period = "annual", limit = 40) {
  fmp_statement(ticker, "income-statement", api_key, period, limit)
}

#' Fetch balance sheet
#' @inheritParams fmp_statement
fmp_balance <- function(ticker, api_key, period = "annual", limit = 40) {
  fmp_statement(ticker, "balance-sheet-statement", api_key, period, limit)
}

#' Fetch cash flow statement
#' @inheritParams fmp_statement
fmp_cashflow <- function(ticker, api_key, period = "annual", limit = 40) {
  fmp_statement(ticker, "cash-flow-statement", api_key, period, limit)
}


# == Financial statement growth (single ticker) ================================

#' Fetch financial statement growth for a single ticker
#'
#' @param ticker Stock ticker
#' @param statement One of: "income-statement-growth", "balance-sheet-statement-growth",
#'   "cash-flow-statement-growth"
#' @param api_key FMP API key
#' @param period "annual" or "quarter"
#' @param limit Number of periods
#' @return data.frame of growth metrics
fmp_growth <- function(ticker, statement, api_key, period = "annual", limit = 40) {
  url <- paste0(.fmp_base_v3, "/", statement, "/", ticker,
                "?period=", period, "&limit=", limit, "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch income statement growth
#' @inheritParams fmp_growth
fmp_income_growth <- function(ticker, api_key, period = "annual", limit = 40) {
  fmp_growth(ticker, "income-statement-growth", api_key, period, limit)
}

#' Fetch balance sheet growth
#' @inheritParams fmp_growth
fmp_balance_growth <- function(ticker, api_key, period = "annual", limit = 40) {
  fmp_growth(ticker, "balance-sheet-statement-growth", api_key, period, limit)
}

#' Fetch cash flow growth
#' @inheritParams fmp_growth
fmp_cashflow_growth <- function(ticker, api_key, period = "annual", limit = 40) {
  fmp_growth(ticker, "cash-flow-statement-growth", api_key, period, limit)
}


# == Bulk financial statements =================================================

#' Fetch bulk financial statement data (all companies for a year/period)
#'
#' @param statement One of: "income-statement", "balance-sheet-statement",
#'   "cash-flow-statement"
#' @param year Calendar year
#' @param period "annual" or "quarter1", "quarter2", "quarter3", "quarter4"
#' @param api_key FMP API key
#' @return data.frame of all companies' statements
fmp_statement_bulk <- function(statement, year, period, api_key) {
  url <- paste0(.fmp_base_v4, "/", statement, "-bulk?year=", year,
                "&period=", period, "&apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch bulk income statements
#' @inheritParams fmp_statement_bulk
fmp_income_bulk <- function(year, period, api_key) {
  fmp_statement_bulk("income-statement", year, period, api_key)
}

#' Fetch bulk balance sheets
#' @inheritParams fmp_statement_bulk
fmp_balance_bulk <- function(year, period, api_key) {
  fmp_statement_bulk("balance-sheet-statement", year, period, api_key)
}

#' Fetch bulk cash flow statements
#' @inheritParams fmp_statement_bulk
fmp_cashflow_bulk <- function(year, period, api_key) {
  fmp_statement_bulk("cash-flow-statement", year, period, api_key)
}


# == Bulk growth statements ====================================================

#' Fetch bulk growth data
#'
#' @param statement One of: "income-statement-growth", "balance-sheet-statement-growth",
#'   "cash-flow-statement-growth"
#' @param year Calendar year
#' @param period "annual" or "quarter1"-"quarter4"
#' @param api_key FMP API key
#' @return data.frame
fmp_growth_bulk <- function(statement, year, period, api_key) {
  url <- paste0(.fmp_base_v4, "/", statement, "-bulk?year=", year,
                "&period=", period, "&apikey=", api_key)
  .fmp_csv(url)
}

fmp_income_growth_bulk <- function(year, period, api_key) {
  fmp_growth_bulk("income-statement-growth", year, period, api_key)
}

fmp_balance_growth_bulk <- function(year, period, api_key) {
  fmp_growth_bulk("balance-sheet-statement-growth", year, period, api_key)
}

fmp_cashflow_growth_bulk <- function(year, period, api_key) {
  fmp_growth_bulk("cash-flow-statement-growth", year, period, api_key)
}


# == Key metrics & ratios ======================================================

#' Fetch key metrics (bulk, all companies)
#' @param year Calendar year
#' @param period "annual" or "quarter1"-"quarter4"
#' @param api_key FMP API key
#' @return data.frame
fmp_keymetrics_bulk <- function(year, period, api_key) {
  url <- paste0(.fmp_base_v4, "/key-metrics-bulk?year=", year,
                "&period=", period, "&apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch TTM key metrics (bulk)
#' @param api_key FMP API key
#' @return data.frame
fmp_keymetrics_ttm_bulk <- function(api_key) {
  url <- paste0(.fmp_base_v4, "/key-metrics-ttm-bulk?apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch ratios (bulk)
#' @param year Calendar year
#' @param period "annual" or "quarter1"-"quarter4"
#' @param api_key FMP API key
#' @return data.frame
fmp_ratios_bulk <- function(year, period, api_key) {
  url <- paste0(.fmp_base_v4, "/ratios-bulk?year=", year,
                "&period=", period, "&apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch TTM ratios (bulk)
#' @param api_key FMP API key
#' @return data.frame
fmp_ratios_ttm_bulk <- function(api_key) {
  url <- paste0(.fmp_base_v4, "/ratios-ttm-bulk?apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch key metrics for a single ticker
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @param period "annual" or "quarter"
#' @return data.frame
fmp_keymetrics <- function(ticker, api_key, period = "annual") {
  url <- paste0(.fmp_base_v3, "/key-metrics/", ticker,
                "?period=", period, "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch TTM key metrics for a single ticker
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_keymetrics_ttm <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v3, "/key-metrics-ttm/", ticker, "?apikey=", api_key)
  .fmp_json(url)
}

#' Fetch ratios for a single ticker
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @param period "annual" or "quarter"
#' @return data.frame
fmp_ratios <- function(ticker, api_key, period = "annual") {
  url <- paste0(.fmp_base_v3, "/ratios/", ticker,
                "?period=", period, "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch TTM ratios for a single ticker
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_ratios_ttm <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v3, "/ratios-ttm/", ticker, "?apikey=", api_key)
  .fmp_json(url)
}


# == Company profiles ==========================================================

#' Fetch bulk company profiles
#' @param api_key FMP API key
#' @return data.frame of all company profiles
fmp_profiles_bulk <- function(api_key) {
  url <- paste0(.fmp_base_v4, "/profile/all?apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch stock peers
#' @param api_key FMP API key
#' @return data.frame
fmp_peers_bulk <- function(api_key) {
  url <- paste0(.fmp_base_v4, "/stock_peers_bulk?apikey=", api_key)
  .fmp_csv(url)
}

#' Fetch bulk analyst ratings
#' @param api_key FMP API key
#' @return data.frame
fmp_ratings_bulk <- function(api_key) {
  url <- paste0(.fmp_base_v4, "/rating-bulk?apikey=", api_key)
  .fmp_csv(url)
}


# == Insider trading ===========================================================

#' Fetch insider trading RSS feed
#' @param api_key FMP API key
#' @param page Page number (0-indexed)
#' @return data.frame of recent insider trades
fmp_insider_feed <- function(api_key, page = 0) {
  url <- paste0(.fmp_base_v4, "/insider-trading-rss-feed?page=", page,
                "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch insider trading for a specific company
#' @param ticker Stock ticker (or company CIK)
#' @param api_key FMP API key
#' @param tx_type Transaction type filter (e.g. "P-Purchase", "S-Sale"). NULL for all.
#' @param page Page number (0-indexed)
#' @return data.frame of insider transactions
fmp_insider <- function(ticker, api_key, tx_type = NULL, page = 0) {
  url <- paste0(.fmp_base_v4, "/insider-trading?symbol=", ticker,
                "&page=", page, "&apikey=", api_key)
  if (!is.null(tx_type)) url <- paste0(url, "&transactionType=", utils::URLencode(tx_type))
  .fmp_json(url)
}

#' Fetch insider trading by company CIK with pagination
#' @param cik Company CIK
#' @param api_key FMP API key
#' @param tx_type "P-Purchase" or "S-Sale"
#' @param max_pages Maximum pages to fetch
#' @return data.frame of all insider transactions
fmp_insider_all <- function(cik, api_key, tx_type = NULL, max_pages = 100) {
  results <- list()
  for (pg in seq_len(max_pages) - 1) {
    url <- paste0(.fmp_base_v4, "/insider-trading?companyCik=", cik,
                  "&page=", pg, "&apikey=", api_key)
    if (!is.null(tx_type)) url <- paste0(url, "&transactionType=", utils::URLencode(tx_type))
    df <- tryCatch(.fmp_json(url), error = function(e) data.frame())
    if (nrow(df) == 0) break
    results[[length(results) + 1]] <- df
  }
  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}


# == SEC filing feed ===========================================================

#' Fetch SEC filing RSS feed from FMP
#' @param api_key FMP API key
#' @param page Page number (0-indexed)
#' @return data.frame of recent SEC filings
fmp_filing_feed <- function(api_key, page = 0) {
  url <- paste0(.fmp_base_v3, "/rss_feed?page=", page, "&apikey=", api_key)
  .fmp_json(url)
}


# == Institutional ownership ===================================================

#' Fetch institutional ownership portfolio dates for a CIK
#' @param cik Institutional CIK
#' @param api_key FMP API key
#' @return data.frame with available report dates
fmp_institution_dates <- function(cik, api_key) {
  url <- paste0(.fmp_base_v4, "/institutional-ownership/portfolio-date?cik=", cik,
                "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch institutional holdings for a CIK on a specific date
#' @param cik Institutional CIK
#' @param date Report date
#' @param api_key FMP API key
#' @param page Page number (0-indexed)
#' @return data.frame of holdings
fmp_institution_holdings <- function(cik, date, api_key, page = 0) {
  url <- paste0(.fmp_base_v4, "/institutional-ownership/portfolio-holdings?cik=", cik,
                "&date=", date, "&page=", page, "&apikey=", api_key)
  .fmp_json(url)
}

#' List all institutions tracked by FMP
#' @param api_key FMP API key
#' @return data.frame
fmp_institution_list <- function(api_key) {
  url <- paste0(.fmp_base_v4, "/institutional-ownership/list?apikey=", api_key)
  .fmp_json(url)
}

#' Fetch institutional ownership performance summary
#' @param cik Institutional CIK
#' @param api_key FMP API key
#' @return data.frame
fmp_institution_performance <- function(cik, api_key) {
  url <- paste0(.fmp_base_v4, "/institutional-ownership/portfolio-holdings-summary?cik=", cik,
                "&apikey=", api_key)
  .fmp_json(url)
}


# == Segment data ==============================================================

#' Fetch revenue segment data (product or geographic)
#'
#' @param ticker Stock ticker
#' @param segment "product" or "geographic"
#' @param api_key FMP API key
#' @return data.frame with segment revenue breakdown
fmp_segment <- function(ticker, segment = "product", api_key) {
  type <- match.arg(segment, c("product", "geographic"))
  url <- paste0(.fmp_base_v4, "/revenue-", type, "-segmentation?symbol=", ticker,
                "&structure=flat&apikey=", api_key)
  .fmp_json(url)
}


# == Fund holdings =============================================================

#' Fetch mutual fund holdings
#' @param ticker Fund ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_fund <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v3, "/etf-holder/", ticker, "?apikey=", api_key)
  .fmp_json(url)
}


# == ESG, employees, executives ================================================

#' Fetch ESG data
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_esg <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v4, "/esg-environmental-social-governance-data?symbol=", ticker,
                "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch ESG risk ratings
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_esg_risk <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v4, "/esg-environmental-social-governance-data-ratings?symbol=", ticker,
                "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch historical employee count
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_employees <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v4, "/historical/employee_count?symbol=", ticker,
                "&apikey=", api_key)
  .fmp_json(url)
}

#' Fetch executive compensation
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_executives <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v4, "/governance/executive_compensation?symbol=", ticker,
                "&apikey=", api_key)
  .fmp_json(url)
}


# == Calendars =================================================================

#' Fetch earnings calendar
#' @param api_key FMP API key
#' @return data.frame
fmp_earnings_calendar <- function(api_key) {
  url <- paste0(.fmp_base_v3, "/earning_calendar?apikey=", api_key)
  .fmp_json(url)
}

#' Fetch IPO calendar
#' @param api_key FMP API key
#' @param from Start date (default: 90 days ago)
#' @param to End date (default: today)
#' @return data.frame
fmp_ipo_calendar <- function(api_key, from = Sys.Date() - 90, to = Sys.Date()) {
  url <- paste0(.fmp_base_v3, "/ipo_calendar?from=", format(from, "%Y-%m-%d"),
                "&to=", format(to, "%Y-%m-%d"), "&apikey=", api_key)
  .fmp_json(url)
}


# == Company notes =============================================================

#' Fetch company notes
#' @param ticker Stock ticker
#' @param api_key FMP API key
#' @return data.frame
fmp_notes <- function(ticker, api_key) {
  url <- paste0(.fmp_base_v4, "/company-notes?symbol=", ticker, "&apikey=", api_key)
  .fmp_json(url)
}
