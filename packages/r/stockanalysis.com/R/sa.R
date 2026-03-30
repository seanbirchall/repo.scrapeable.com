# == Financial statements ======================================================

#' Fetch income statement data
#'
#' Scrapes the income statement page. Returns metrics like Revenue,
#' Cost of Revenue, Gross Profit, Operating Income, Net Income, EPS, etc.
#'
#' @param ticker Stock ticker (e.g. "AAPL", "MSFT")
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_income <- function(ticker) {
  .sa_fetch_table(ticker, "financials")
}

#' Fetch quarterly income statement
#'
#' @param ticker Stock ticker
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_income_quarterly <- function(ticker) {
  .sa_fetch_table(ticker, "financials/?p=quarterly")
}

#' Fetch balance sheet data
#'
#' @param ticker Stock ticker
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_balance <- function(ticker) {
  .sa_fetch_table(ticker, "financials/balance-sheet")
}

#' Fetch quarterly balance sheet
#'
#' @param ticker Stock ticker
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_balance_quarterly <- function(ticker) {
  .sa_fetch_table(ticker, "financials/balance-sheet/?p=quarterly")
}

#' Fetch cash flow statement
#'
#' @param ticker Stock ticker
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_cashflow <- function(ticker) {
  .sa_fetch_table(ticker, "financials/cash-flow-statement")
}

#' Fetch quarterly cash flow statement
#'
#' @param ticker Stock ticker
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_cashflow_quarterly <- function(ticker) {
  .sa_fetch_table(ticker, "financials/cash-flow-statement/?p=quarterly")
}

#' Fetch financial ratios
#'
#' PE, PB, PS, dividend yield, margins, returns, etc.
#'
#' @param ticker Stock ticker
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value
#' @export
sa_ratios <- function(ticker) {
  .sa_fetch_table(ticker, "financials/ratios")
}


# == Convenience: all financials ===============================================

#' Fetch all financial statements for a ticker
#'
#' Combines income, balance sheet, and cash flow into one tibble.
#' Makes 3 HTTP requests.
#'
#' @param ticker Stock ticker
#' @param quarterly If TRUE, fetch quarterly data instead of annual
#' @return tibble: ticker, metric, period, fiscal_year, is_ttm, value, statement
#' @export
sa_financials <- function(ticker, quarterly = FALSE) {
  if (quarterly) {
    inc <- tryCatch(sa_income_quarterly(ticker), error = function(e) NULL)
    bal <- tryCatch(sa_balance_quarterly(ticker), error = function(e) NULL)
    cf  <- tryCatch(sa_cashflow_quarterly(ticker), error = function(e) NULL)
  } else {
    inc <- tryCatch(sa_income(ticker), error = function(e) NULL)
    bal <- tryCatch(sa_balance(ticker), error = function(e) NULL)
    cf  <- tryCatch(sa_cashflow(ticker), error = function(e) NULL)
  }

  bind_rows(
    if (!is.null(inc) && nrow(inc) > 0) mutate(inc, statement = "income") else NULL,
    if (!is.null(bal) && nrow(bal) > 0) mutate(bal, statement = "balance") else NULL,
    if (!is.null(cf) && nrow(cf) > 0)  mutate(cf, statement = "cashflow") else NULL
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the stockanalysis.com package
#'
#' @return Character string (invisibly), also printed
#' @export
sa_context <- function() {
  .build_context("stockanalysis.com", header_lines = c(
    "# stockanalysis.com - Financial Statement Data Client for R",
    "# Dependencies: httr2, xml2, dplyr, tidyr, tibble",
    "# Auth: none (HTML scraping)",
    "# Returns long-format tibbles: ticker, metric, period, fiscal_year, is_ttm, value",
    "#",
    "# Statements: income, balance sheet, cash flow, ratios",
    "# Periods: annual (default) or quarterly",
    "# Data: ~35 metrics per statement, 10+ years of history"
  ))
}
