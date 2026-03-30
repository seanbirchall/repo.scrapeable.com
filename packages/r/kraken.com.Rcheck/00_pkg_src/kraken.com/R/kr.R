# == Ticker / Prices ===========================================================

#' Current ticker data for one or more pairs
#'
#' @param pair Trading pair(s). Single string or comma-separated.
#'   Examples: "XBTUSD" (BTC/USD), "ETHUSD", "SOLUSD", "XBTUSD,ETHUSD"
#' @return tibble: pair, ask, bid, last, volume_24h, vwap_24h, trades_24h,
#'   low_24h, high_24h
#' @export
kr_ticker <- function(pair) {
  result <- .kr_get("Ticker", list(pair = pair))
  if (is.null(result) || length(result) == 0) return(.schema_ticker)

  bind_rows(lapply(names(result), function(p) {
    t <- result[[p]]
    tibble(
      pair       = p,
      ask        = as.numeric(t$a[[1]]),
      bid        = as.numeric(t$b[[1]]),
      last       = as.numeric(t$c[[1]]),
      volume_24h = as.numeric(t$v[[2]]),
      vwap_24h   = as.numeric(t$p[[2]]),
      trades_24h = as.integer(t$t[[2]]),
      low_24h    = as.numeric(t$l[[2]]),
      high_24h   = as.numeric(t$h[[2]])
    )
  }))
}


# == OHLC Candlesticks ========================================================

#' Historical OHLC candlestick data
#'
#' @param pair Trading pair (e.g. "XBTUSD", "ETHUSD")
#' @param interval Minutes per candle: 1, 5, 15, 30, 60, 240, 1440 (1d),
#'   10080 (1w), 21600 (15d). Default: 1440 (daily).
#' @param since Unix timestamp to start from (optional)
#' @return tibble: timestamp, open, high, low, close, vwap, volume, count, pair
#' @export
kr_ohlc <- function(pair, interval = 1440, since = NULL) {
  params <- list(pair = pair, interval = interval)
  if (!is.null(since)) params$since <- since
  result <- .kr_get("OHLC", params)

  # Result has pair data + "last" timestamp
  pair_key <- setdiff(names(result), "last")
  if (length(pair_key) == 0) return(.schema_ohlc)

  raw <- result[[pair_key[1]]]
  if (is.null(raw) || length(raw) == 0) return(.schema_ohlc)

  # Convert list-of-lists to matrix
  mat <- do.call(rbind, lapply(raw, unlist))

  tibble(
    timestamp = as.POSIXct(as.numeric(mat[, 1]), origin = "1970-01-01", tz = "UTC"),
    open      = as.numeric(mat[, 2]),
    high      = as.numeric(mat[, 3]),
    low       = as.numeric(mat[, 4]),
    close     = as.numeric(mat[, 5]),
    vwap      = as.numeric(mat[, 6]),
    volume    = as.numeric(mat[, 7]),
    count     = as.integer(mat[, 8]),
    pair      = pair
  )
}


# == Order Book ================================================================

#' Order book depth
#'
#' @param pair Trading pair
#' @param count Depth levels (default 20, max 500)
#' @return tibble: side (bid/ask), price, volume, timestamp, pair
#' @export
kr_depth <- function(pair, count = 20) {
  result <- .kr_get("Depth", list(pair = pair, count = count))
  pair_key <- names(result)[1]
  if (is.null(pair_key)) return(tibble())

  book <- result[[pair_key]]

  parse_side <- function(data, side) {
    if (is.null(data) || length(data) == 0) return(NULL)
    mat <- do.call(rbind, lapply(data, unlist))
    tibble(
      side      = side,
      price     = as.numeric(mat[, 1]),
      volume    = as.numeric(mat[, 2]),
      timestamp = as.POSIXct(as.numeric(mat[, 3]), origin = "1970-01-01", tz = "UTC"),
      pair      = pair
    )
  }

  bind_rows(
    parse_side(book$bids, "bid"),
    parse_side(book$asks, "ask")
  )
}


# == Recent Trades =============================================================

#' Recent trades
#'
#' @param pair Trading pair
#' @param count Number of trades (default 100)
#' @param since Trade ID to start from (optional)
#' @return tibble: price, volume, timestamp, side (buy/sell), type (market/limit),
#'   misc, pair
#' @export
kr_trades <- function(pair, count = 100, since = NULL) {
  params <- list(pair = pair, count = count)
  if (!is.null(since)) params$since <- since
  result <- .kr_get("Trades", params)

  pair_key <- setdiff(names(result), "last")
  if (length(pair_key) == 0) return(tibble())

  raw <- result[[pair_key[1]]]
  if (is.null(raw) || length(raw) == 0) return(tibble())

  mat <- do.call(rbind, lapply(raw, unlist))

  tibble(
    price     = as.numeric(mat[, 1]),
    volume    = as.numeric(mat[, 2]),
    timestamp = as.POSIXct(as.numeric(mat[, 3]), origin = "1970-01-01", tz = "UTC"),
    side      = ifelse(mat[, 4] == "b", "buy", "sell"),
    type      = ifelse(mat[, 5] == "m", "market", "limit"),
    pair      = pair
  )
}


# == Spread ====================================================================

#' Recent spread data
#'
#' @param pair Trading pair
#' @param since Unix timestamp to start from (optional)
#' @return tibble: timestamp, bid, ask, pair
#' @export
kr_spread <- function(pair, since = NULL) {
  params <- list(pair = pair)
  if (!is.null(since)) params$since <- since
  result <- .kr_get("Spread", params)

  pair_key <- setdiff(names(result), "last")
  if (length(pair_key) == 0) return(tibble())

  raw <- result[[pair_key[1]]]
  if (is.null(raw) || length(raw) == 0) return(tibble())

  mat <- do.call(rbind, lapply(raw, unlist))
  tibble(
    timestamp = as.POSIXct(as.numeric(mat[, 1]), origin = "1970-01-01", tz = "UTC"),
    bid       = as.numeric(mat[, 2]),
    ask       = as.numeric(mat[, 3]),
    pair      = pair
  )
}


# == Exchange Info =============================================================

#' List all trading pairs
#'
#' @return tibble: pair, altname, base, quote, status, ...
#' @export
kr_pairs <- function() {
  result <- .kr_get("AssetPairs")
  if (is.null(result) || length(result) == 0) return(tibble())

  bind_rows(lapply(names(result), function(p) {
    r <- result[[p]]
    tibble(
      pair     = p,
      altname  = r$altname %||% NA_character_,
      base     = r$base %||% NA_character_,
      quote    = r$quote %||% NA_character_,
      status   = r$status %||% NA_character_,
      lot_decimals  = as.integer(r$lot_decimals %||% NA),
      pair_decimals = as.integer(r$pair_decimals %||% NA)
    )
  }))
}

#' List all assets
#'
#' @return tibble: asset, altname, decimals, display_decimals, status
#' @export
kr_assets <- function() {
  result <- .kr_get("Assets")
  if (is.null(result) || length(result) == 0) return(tibble())

  bind_rows(lapply(names(result), function(a) {
    r <- result[[a]]
    tibble(
      asset            = a,
      altname          = r$altname %||% NA_character_,
      decimals         = as.integer(r$decimals %||% NA),
      display_decimals = as.integer(r$display_decimals %||% NA),
      status           = r$status %||% NA_character_
    )
  }))
}


# == Context ===================================================================

#' Generate LLM-friendly context for the kraken.com package
#'
#' @return Character string (invisibly), also printed
#' @export
kr_context <- function() {
  .build_context("kraken.com", header_lines = c(
    "# kraken.com - Kraken Crypto Exchange Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none for public market data",
    "# Rate limit: ~15 req/sec",
    "# All functions return tibbles.",
    "#",
    "# Common pairs: XBTUSD (BTC/USD), ETHUSD, SOLUSD, XRPUSD, ADAUSD",
    "# Kraken uses XBT for Bitcoin (not BTC)",
    "#",
    "# OHLC intervals (minutes): 1, 5, 15, 30, 60, 240, 1440 (1d), 10080 (1w)"
  ))
}
