# == Core metric fetching ======================================================

#' Fetch a financial metric from Macrotrends
#'
#' Scrapes the Macrotrends page for any available metric. Returns both
#' annual and quarterly data.
#'
#' @param ticker Stock ticker (e.g. "AAPL", "MSFT")
#' @param slug Company name slug (e.g. "apple", "microsoft"). If NULL,
#'   auto-resolved via a redirect.
#' @param metric Metric URL path. See mt_metrics() for available metrics.
#' @param period "annual" (default), "quarterly", or "both"
#' @return tibble: date, value, ticker, metric, period
#' @export
mt_get <- function(ticker, slug = NULL, metric = "revenue",
                   period = "annual") {
  if (is.null(slug)) slug <- tolower(ticker)

  data <- .mt_fetch(ticker, slug, metric)

  result <- if (period == "both") {
    bind_rows(
      if (nrow(data$annual) > 0) mutate(data$annual, period = "annual") else NULL,
      if (nrow(data$quarterly) > 0) mutate(data$quarterly, period = "quarterly") else NULL
    )
  } else if (period == "quarterly" && nrow(data$quarterly) > 0) {
    mutate(data$quarterly, period = "quarterly")
  } else {
    mutate(data$annual, period = "annual")
  }

  if (nrow(result) == 0) return(.schema_metric)
  result$ticker <- toupper(ticker)
  result$metric <- metric
  result |> select(date, value, ticker, metric, period) |> arrange(date)
}

#' Fetch multiple metrics for a ticker
#'
#' @param ticker Stock ticker
#' @param slug Company name slug (NULL = auto)
#' @param metrics Character vector of metric names
#' @param period "annual", "quarterly", or "both"
#' @param sleep Seconds between requests (default 0.5)
#' @return tibble: date, value, ticker, metric, period (stacked)
#' @export
mt_get_bulk <- function(ticker, slug = NULL, metrics, period = "annual",
                        sleep = 0.5) {
  n <- length(metrics)
  results <- lapply(seq_along(metrics), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] %s: %s", i, n, ticker, metrics[i]))
    tryCatch(
      mt_get(ticker, slug, metrics[i], period),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
  })
  bind_rows(results)
}


# == Available metrics =========================================================

#' List available Macrotrends metrics
#'
#' Returns all known metric URL slugs with descriptions.
#'
#' @return tibble: metric (URL slug), category, description
#' @export
mt_metrics <- function() {
  tibble(
    metric = c(
      # Income statement
      "revenue", "cost-goods-sold", "gross-profit", "operating-income",
      "net-income", "ebitda", "eps-earnings-per-share-diluted",
      # Balance sheet
      "total-assets", "total-liabilities", "total-shareholder-equity",
      "long-term-debt", "current-ratio", "book-value-per-share",
      "cash-on-hand", "total-current-assets", "total-current-liabilities",
      # Cash flow
      "free-cash-flow", "operating-cash-flow", "capital-expenditures",
      # Margins
      "gross-profit-margin", "operating-profit-margin", "net-profit-margin",
      "ebitda-margin",
      # Returns
      "roe", "roa", "roi", "roic",
      # Valuation
      "pe-ratio", "price-book", "price-fcf", "ev-ebitda",
      # Per share
      "revenue-per-share", "cash-flow-per-share", "dividend-per-share",
      # Other
      "shares-outstanding", "number-of-employees", "dividend-yield-history"
    ),
    category = c(
      rep("income", 7), rep("balance", 9), rep("cashflow", 3),
      rep("margin", 4), rep("return", 4), rep("valuation", 4),
      rep("per_share", 3), rep("other", 3)
    ),
    description = c(
      "Total revenue", "Cost of goods sold", "Gross profit", "Operating income",
      "Net income", "EBITDA", "Diluted EPS",
      "Total assets", "Total liabilities", "Shareholder equity",
      "Long-term debt", "Current ratio", "Book value per share",
      "Cash and equivalents", "Total current assets", "Total current liabilities",
      "Free cash flow", "Operating cash flow", "Capital expenditures",
      "Gross margin %", "Operating margin %", "Net margin %", "EBITDA margin %",
      "Return on equity %", "Return on assets %", "Return on investment %",
      "Return on invested capital %",
      "Price to earnings", "Price to book", "Price to FCF", "EV to EBITDA",
      "Revenue per share", "Cash flow per share", "Dividend per share",
      "Shares outstanding", "Employee count", "Dividend yield %"
    )
  )
}


# == Convenience: common financial data ========================================

#' Income statement metrics (revenue, net income, EPS, etc.)
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked income statement metrics
#' @export
mt_income <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("revenue", "gross-profit", "operating-income",
               "net-income", "ebitda", "eps-earnings-per-share-diluted")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Balance sheet metrics
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked balance sheet metrics
#' @export
mt_balance <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("total-assets", "total-liabilities", "total-shareholder-equity",
               "long-term-debt", "cash-on-hand", "current-ratio")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Cash flow metrics
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked cash flow metrics
#' @export
mt_cashflow <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("free-cash-flow", "operating-cash-flow", "capital-expenditures")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Profitability margins
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked margin metrics
#' @export
mt_margins <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("gross-profit-margin", "operating-profit-margin",
               "net-profit-margin", "ebitda-margin")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Valuation ratios (PE, PB, EV/EBITDA)
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual" or "quarterly"
#' @return tibble: stacked valuation metrics
#' @export
mt_valuation <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("pe-ratio", "price-book", "price-fcf", "ev-ebitda")
  mt_get_bulk(ticker, slug, metrics, period)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the macrotrends.net package
#'
#' @return Character string (invisibly), also printed
#' @export
mt_context <- function() {
  .build_context("macrotrends.net", header_lines = c(
    "# macrotrends.net - Financial Fundamental Data Client for R",
    "# Dependencies: httr2, xml2, dplyr, tibble",
    "# Auth: none (HTML scraping)",
    "# All functions return tibbles: date, value, ticker, metric, period",
    "#",
    "# Metrics available via mt_metrics():",
    "#   Income: revenue, net-income, ebitda, eps-earnings-per-share-diluted",
    "#   Balance: total-assets, long-term-debt, cash-on-hand",
    "#   Cash flow: free-cash-flow, operating-cash-flow",
    "#   Margins: gross-profit-margin, net-profit-margin",
    "#   Returns: roe, roa, roic",
    "#   Valuation: pe-ratio, price-book, ev-ebitda",
    "#",
    "# Note: slug param can usually be omitted (defaults to lowercase ticker)"
  ))
}
