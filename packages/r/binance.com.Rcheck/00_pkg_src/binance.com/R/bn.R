# == Price data ================================================================

#' Current price for one or all symbols
#'
#' @param symbol Trading pair (e.g. "BTCUSD", "ETHUSD"). NULL = all symbols.
#' @param base API region: "us" (default, Binance.US) or "global" (Binance.com)
#' @return tibble: symbol, price
#' @export
bn_price <- function(symbol = NULL, base = "us") {
  params <- list()
  if (!is.null(symbol)) params$symbol <- toupper(symbol)
  raw <- .bn_get("ticker/price", params, base)

  if (is.data.frame(raw)) {
    as_tibble(raw) |> mutate(price = as.numeric(price))
  } else {
    tibble(symbol = raw$symbol, price = as.numeric(raw$price))
  }
}

#' 24-hour ticker statistics
#'
#' @param symbol Trading pair (e.g. "BTCUSD"). NULL = all.
#' @param base "us" or "global"
#' @return tibble: symbol, price_change, price_change_pct, weighted_avg_price,
#'   open, high, low, close, volume, quote_volume, trades, ...
#' @export
bn_ticker_24h <- function(symbol = NULL, base = "us") {
  params <- list()
  if (!is.null(symbol)) params$symbol <- toupper(symbol)
  raw <- .bn_get("ticker/24hr", params, base)

  df <- if (is.data.frame(raw)) as_tibble(raw) else as_tibble(as.list(raw))
  df |> mutate(
    across(any_of(c("priceChange", "priceChangePercent", "weightedAvgPrice",
                     "openPrice", "highPrice", "lowPrice", "lastPrice",
                     "volume", "quoteVolume")), as.numeric),
    across(any_of(c("count")), as.integer),
    across(any_of(c("openTime", "closeTime")),
           ~ as.POSIXct(as.numeric(.) / 1000, origin = "1970-01-01", tz = "UTC"))
  )
}


# == Candlestick / Kline data ==================================================

#' Historical OHLCV candlestick data (klines)
#'
#' @param symbol Trading pair (e.g. "BTCUSD", "ETHUSD")
#' @param interval Candle interval: "1m", "3m", "5m", "15m", "30m",
#'   "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M"
#' @param start Start time (Date, POSIXct, or millisecond timestamp)
#' @param end End time
#' @param limit Max candles (default 500, max 1000)
#' @param base "us" or "global"
#' @return tibble: open_time, open, high, low, close, volume, close_time,
#'   quote_volume, trades, symbol
#' @export
bn_klines <- function(symbol, interval = "1d", start = NULL, end = NULL,
                      limit = 500, base = "us") {
  params <- list(symbol = toupper(symbol), interval = interval, limit = min(limit, 1000))
  if (!is.null(start)) {
    if (inherits(start, "Date")) start <- as.numeric(as.POSIXct(start)) * 1000
    if (inherits(start, "POSIXct")) start <- as.numeric(start) * 1000
    params$startTime <- as.character(as.integer(start))
  }
  if (!is.null(end)) {
    if (inherits(end, "Date")) end <- as.numeric(as.POSIXct(end)) * 1000
    if (inherits(end, "POSIXct")) end <- as.numeric(end) * 1000
    params$endTime <- as.character(as.integer(end))
  }

  raw <- .bn_get("klines", params, base)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_klines)

  # Klines return array of arrays: [open_time, open, high, low, close, volume,
  #   close_time, quote_volume, trades, taker_buy_base, taker_buy_quote, ignore]
  tibble(
    open_time    = as.POSIXct(as.numeric(raw[, 1]) / 1000, origin = "1970-01-01", tz = "UTC"),
    open         = as.numeric(raw[, 2]),
    high         = as.numeric(raw[, 3]),
    low          = as.numeric(raw[, 4]),
    close        = as.numeric(raw[, 5]),
    volume       = as.numeric(raw[, 6]),
    close_time   = as.POSIXct(as.numeric(raw[, 7]) / 1000, origin = "1970-01-01", tz = "UTC"),
    quote_volume = as.numeric(raw[, 8]),
    trades       = as.integer(raw[, 9]),
    symbol       = toupper(symbol)
  )
}


# == Order book ================================================================

#' Order book depth (bids and asks)
#'
#' @param symbol Trading pair
#' @param limit Depth levels: 5, 10, 20, 50, 100, 500, 1000, 5000 (default 20)
#' @param base "us" or "global"
#' @return tibble: side (bid/ask), price, quantity, symbol
#' @export
bn_depth <- function(symbol, limit = 20, base = "us") {
  raw <- .bn_get("depth", list(symbol = toupper(symbol), limit = limit), base)

  bids <- if (!is.null(raw$bids) && nrow(raw$bids) > 0) {
    tibble(side = "bid", price = as.numeric(raw$bids[, 1]),
           quantity = as.numeric(raw$bids[, 2]))
  } else NULL

  asks <- if (!is.null(raw$asks) && nrow(raw$asks) > 0) {
    tibble(side = "ask", price = as.numeric(raw$asks[, 1]),
           quantity = as.numeric(raw$asks[, 2]))
  } else NULL

  result <- bind_rows(bids, asks)
  if (nrow(result) > 0) result$symbol <- toupper(symbol)
  result
}


# == Recent trades =============================================================

#' Recent trades
#'
#' @param symbol Trading pair
#' @param limit Number of trades (default 100, max 1000)
#' @param base "us" or "global"
#' @return tibble: id, price, qty, quote_qty, time, is_buyer_maker, symbol
#' @export
bn_trades <- function(symbol, limit = 100, base = "us") {
  raw <- .bn_get("trades", list(symbol = toupper(symbol), limit = min(limit, 1000)), base)
  if (is.null(raw) || nrow(raw) == 0) return(tibble())

  as_tibble(raw) |>
    mutate(
      price = as.numeric(price),
      qty = as.numeric(qty),
      quoteQty = as.numeric(quoteQty),
      time = as.POSIXct(as.numeric(time) / 1000, origin = "1970-01-01", tz = "UTC"),
      symbol = toupper(symbol)
    )
}


# == Exchange info =============================================================

#' List all trading pairs
#'
#' @param base "us" or "global"
#' @return tibble: symbol, status, baseAsset, quoteAsset, ...
#' @export
bn_symbols <- function(base = "us") {
  raw <- .bn_get("exchangeInfo", base = base)
  symbols <- raw$symbols
  if (is.null(symbols) || nrow(symbols) == 0) return(tibble())

  as_tibble(symbols) |>
    select(any_of(c("symbol", "status", "baseAsset", "quoteAsset",
                     "baseAssetPrecision", "quoteAssetPrecision")))
}

#' Average price (5-min VWAP)
#'
#' @param symbol Trading pair
#' @param base "us" or "global"
#' @return tibble: symbol, mins, price
#' @export
bn_avg_price <- function(symbol, base = "us") {
  raw <- .bn_get("avgPrice", list(symbol = toupper(symbol)), base)
  tibble(symbol = toupper(symbol), mins = as.integer(raw$mins),
         price = as.numeric(raw$price))
}


# == Context ===================================================================

#' Generate LLM-friendly context for the binance.com package
#'
#' @return Character string (invisibly), also printed
#' @export
bn_context <- function() {
  .build_context("binance.com", header_lines = c(
    "# binance.com - Binance/Binance.US Crypto Exchange Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none for public market data",
    "# Default: Binance.US (base='us'). Set base='global' for Binance.com (blocked in US).",
    "# All functions return tibbles.",
    "#",
    "# Common pairs (Binance.US): BTCUSD, ETHUSD, SOLUSD, BNBUSD",
    "# Common pairs (global): BTCUSDT, ETHUSDT, SOLUSDT, BNBUSDT",
    "#",
    "# Kline intervals: 1m, 5m, 15m, 30m, 1h, 4h, 1d, 1w, 1M"
  ))
}
