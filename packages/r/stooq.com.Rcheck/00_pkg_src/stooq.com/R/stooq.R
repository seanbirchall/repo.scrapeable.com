# == Price history =============================================================

#' Fetch historical OHLCV price data
#'
#' Downloads CSV data from Stooq for any supported instrument.
#'
#' @param symbol Stooq ticker symbol. Examples:
#'   US stocks: "aapl.us", "msft.us", "googl.us"
#'   US indices: "^spx" (S&P 500), "^dji" (Dow), "^ndq" (Nasdaq)
#'   Forex: "eurusd", "gbpusd", "usdjpy"
#'   Commodities: "gc.f" (gold), "cl.f" (crude oil), "si.f" (silver)
#'   Crypto: "btc.v" (Bitcoin), "eth.v" (Ethereum)
#'   Bonds: "10usy.b" (US 10Y yield), "2usy.b" (US 2Y yield)
#' @param start Start date (Date or "YYYY-MM-DD"). Default: 1 year ago.
#' @param end End date. Default: today.
#' @param interval Bar size: "d" (daily, default), "w" (weekly), "m" (monthly)
#' @return tibble: date, open, high, low, close, volume, ticker
#' @export
stooq_history <- function(symbol, start = Sys.Date() - 365,
                          end = Sys.Date(), interval = "d") {
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch multiple symbols at once
#'
#' @param symbols Character vector of Stooq ticker symbols
#' @param start Start date
#' @param end End date
#' @param interval Bar size: "d", "w", "m"
#' @param sleep Seconds between requests (default 0.5)
#' @return tibble: stacked OHLCV for all symbols
#' @export
stooq_history_bulk <- function(symbols, start = Sys.Date() - 365,
                               end = Sys.Date(), interval = "d",
                               sleep = 0.5) {
  n <- length(symbols)
  results <- lapply(seq_along(symbols), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] %s", i, n, symbols[i]))
    tryCatch(
      .stooq_fetch(symbols[i], start, end, interval),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
  })
  bind_rows(results)
}


# == Convenience: US stocks ====================================================

#' Fetch US stock history (auto-appends .us suffix)
#'
#' @param ticker US stock ticker (e.g. "AAPL", "MSFT") — ".us" is added automatically
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
#' @export
stooq_stock <- function(ticker, start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0(tolower(ticker), ".us")
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch US index history
#'
#' @param index Index name: "spx" (S&P 500), "dji" (Dow Jones),
#'   "ndq" (Nasdaq Composite), "rut" (Russell 2000), "vix" (VIX)
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
#' @export
stooq_index <- function(index = "spx", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0("^", tolower(index))
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch forex pair history
#'
#' @param pair Forex pair (e.g. "eurusd", "gbpusd", "usdjpy")
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
#' @export
stooq_forex <- function(pair = "eurusd", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  .stooq_fetch(tolower(pair), start, end, interval)
}

#' Fetch US Treasury yield history
#'
#' @param maturity Maturity: "2" (2Y), "5" (5Y), "10" (10Y), "30" (30Y)
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
#' @export
stooq_yield <- function(maturity = "10", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0(maturity, "usy.b")
  .stooq_fetch(symbol, start, end, interval)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the stooq.com package
#'
#' @return Character string (invisibly), also printed
#' @export
stooq_context <- function() {
  .build_context("stooq.com", header_lines = c(
    "# stooq.com - Historical Price Data Client for R",
    "# Dependencies: httr2, dplyr, tibble",
    "# Auth: none (public CSV downloads)",
    "# All functions return tibbles: date, open, high, low, close, volume, ticker",
    "#",
    "# Ticker formats:",
    "#   US stocks: aapl.us, msft.us, googl.us",
    "#   Indices: ^spx (S&P 500), ^dji (Dow), ^ndq (Nasdaq), ^rut (Russell)",
    "#   Forex: eurusd, gbpusd, usdjpy",
    "#   Commodities: gc.f (gold), cl.f (crude), si.f (silver)",
    "#   Crypto: btc.v (Bitcoin), eth.v (Ethereum)",
    "#   Bonds: 10usy.b (10Y yield), 2usy.b (2Y), 30usy.b (30Y)",
    "#",
    "# Intervals: d (daily), w (weekly), m (monthly)"
  ))
}
