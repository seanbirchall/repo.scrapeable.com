# binance-com.R
# Self-contained Binance/Binance.US crypto exchange client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none for public market data endpoints.
# Rate limits: 1200 req/min (general), 10 req/sec (orders).
# Docs: https://binance-docs.github.io/apidocs/spot/en/
#
# Note: Binance.com blocks US IPs (HTTP 451). Use base = "us" for US access.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"

.binance_base <- function(region = "us") {
  switch(region,
    us      = "https://api.binance.us/api/v3",
    global  = "https://api.binance.com/api/v3",
    "https://api.binance.us/api/v3"
  )
}

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# -- Fetch helper --------------------------------------------------------------

.bn_get <- function(endpoint, params = list(), base = "us") {
  query <- if (length(params) > 0)
    paste(names(params), params, sep = "=", collapse = "&") else ""
  url <- if (query != "") paste0(.binance_base(base), "/", endpoint, "?", query)
         else paste0(.binance_base(base), "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}


# == Schemas ===================================================================

.schema_klines <- tibble(
  open_time = as.POSIXct(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), volume = numeric(),
  close_time = as.POSIXct(character()), quote_volume = numeric(),
  trades = integer(), symbol = character()
)

.schema_ticker <- tibble(
  symbol = character(), price = numeric()
)


# == Price data ================================================================

#' Current price for one or all symbols
#'
#' @param symbol Trading pair (e.g. "BTCUSD", "ETHUSD"). NULL = all symbols.
#' @param base API region: "us" (default, Binance.US) or "global" (Binance.com)
#' @return tibble: symbol, price
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
bn_avg_price <- function(symbol, base = "us") {
  raw <- .bn_get("avgPrice", list(symbol = toupper(symbol)), base)
  tibble(symbol = toupper(symbol), mins = as.integer(raw$mins),
         price = as.numeric(raw$price))
}


# == Context ===================================================================

#' Generate LLM-friendly context for the binance.com package
#'
#' @return Character string (invisibly), also printed
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
