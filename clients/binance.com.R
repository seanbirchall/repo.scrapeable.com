# binance.com.R - Self-contained binance.com client

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)



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

#' Get current price for one or all trading pairs
#'
#' Returns the latest price for a single symbol or all symbols on the
#' exchange. No authentication required.
#'
#' @param symbol Character. Trading pair symbol (e.g. \code{"BTCUSD"},
#'   \code{"ETHUSD"}). \code{NULL} returns all available symbols (600+).
#' @param base Character. API region: \code{"us"} (default, Binance.US) or
#'   \code{"global"} (Binance.com, blocked from US IPs).
#' @return A tibble with columns:
#'   \describe{
#'     \item{symbol}{Character. Trading pair (e.g. \code{"BTCUSD"}).}
#'     \item{price}{Numeric. Current price in quote currency.}
#'   }
#' @examples
#' bn_price("BTCUSD")
#' bn_price()  # all symbols
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

#' Get 24-hour rolling ticker statistics
#'
#' Returns price change, volume, and trade count for the trailing 24-hour
#' window. When called without a symbol, returns stats for all pairs.
#'
#' @param symbol Character. Trading pair (e.g. \code{"BTCUSD"}). \code{NULL}
#'   returns all pairs.
#' @param base Character. \code{"us"} (default) or \code{"global"}.
#' @return A tibble with 21 columns including:
#'   \describe{
#'     \item{symbol}{Character. Trading pair.}
#'     \item{priceChange}{Numeric. Absolute price change over 24h.}
#'     \item{priceChangePercent}{Numeric. Percentage price change.}
#'     \item{weightedAvgPrice}{Numeric. Volume-weighted average price.}
#'     \item{openPrice}{Numeric. Opening price 24h ago.}
#'     \item{highPrice}{Numeric. 24h high.}
#'     \item{lowPrice}{Numeric. 24h low.}
#'     \item{lastPrice}{Numeric. Most recent trade price.}
#'     \item{volume}{Numeric. Base asset volume.}
#'     \item{quoteVolume}{Numeric. Quote asset volume.}
#'     \item{openTime}{POSIXct. Window open time (UTC).}
#'     \item{closeTime}{POSIXct. Window close time (UTC).}
#'     \item{count}{Integer. Number of trades.}
#'   }
#' @examples
#' bn_ticker_24h("BTCUSD")
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

#' Fetch historical OHLCV candlestick data (klines)
#'
#' Returns open-high-low-close-volume bars for a trading pair at the
#' specified interval. Useful for charting and technical analysis.
#'
#' @param symbol Character. Trading pair (e.g. \code{"BTCUSD"}, \code{"ETHUSD"}).
#' @param interval Character. Candle interval. One of: \code{"1m"}, \code{"3m"},
#'   \code{"5m"}, \code{"15m"}, \code{"30m"}, \code{"1h"}, \code{"2h"},
#'   \code{"4h"}, \code{"6h"}, \code{"8h"}, \code{"12h"}, \code{"1d"},
#'   \code{"3d"}, \code{"1w"}, \code{"1M"}. Default \code{"1d"}.
#' @param start Start time as \code{Date}, \code{POSIXct}, or millisecond
#'   Unix timestamp. \code{NULL} lets the API choose.
#' @param end End time (same types as \code{start}).
#' @param limit Integer. Maximum number of candles to return (default 500,
#'   API max 1000).
#' @param base Character. \code{"us"} (default) or \code{"global"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{open_time}{POSIXct. Candle open time (UTC).}
#'     \item{open}{Numeric. Opening price.}
#'     \item{high}{Numeric. Highest price during interval.}
#'     \item{low}{Numeric. Lowest price during interval.}
#'     \item{close}{Numeric. Closing price.}
#'     \item{volume}{Numeric. Base asset volume traded.}
#'     \item{close_time}{POSIXct. Candle close time (UTC).}
#'     \item{quote_volume}{Numeric. Quote asset volume traded.}
#'     \item{trades}{Integer. Number of trades during interval.}
#'     \item{symbol}{Character. The queried trading pair.}
#'   }
#' @examples
#' bn_klines("BTCUSD", interval = "1d", limit = 7)
#' bn_klines("ETHUSD", interval = "1h", limit = 24)
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

#' Get order book depth (bids and asks)
#'
#' Returns current bid and ask levels for a trading pair, sorted by
#' price. Useful for market microstructure analysis and liquidity assessment.
#'
#' @param symbol Character. Trading pair (e.g. \code{"BTCUSD"}).
#' @param limit Integer. Number of price levels per side. Valid values:
#'   5, 10, 20, 50, 100, 500, 1000, 5000. Default 20.
#' @param base Character. \code{"us"} (default) or \code{"global"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{side}{Character. \code{"bid"} or \code{"ask"}.}
#'     \item{price}{Numeric. Price level in quote currency.}
#'     \item{quantity}{Numeric. Quantity available at that price.}
#'     \item{symbol}{Character. The queried trading pair.}
#'   }
#' @examples
#' bn_depth("BTCUSD", limit = 5)
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

#' Get recent trades for a trading pair
#'
#' Returns the most recent individual trades executed on the exchange.
#'
#' @param symbol Character. Trading pair (e.g. \code{"BTCUSD"}).
#' @param limit Integer. Number of trades to return (default 100, API max 1000).
#' @param base Character. \code{"us"} (default) or \code{"global"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Trade ID.}
#'     \item{price}{Numeric. Execution price.}
#'     \item{qty}{Numeric. Base asset quantity.}
#'     \item{quoteQty}{Numeric. Quote asset quantity (price * qty).}
#'     \item{time}{POSIXct. Trade timestamp (UTC).}
#'     \item{isBuyerMaker}{Logical. TRUE if the buyer was the maker.}
#'     \item{isBestMatch}{Logical. TRUE if this was the best price match.}
#'     \item{symbol}{Character. The queried trading pair.}
#'   }
#' @examples
#' bn_trades("BTCUSD", limit = 10)
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

#' List all available trading pairs on the exchange
#'
#' Returns metadata for every trading pair including status, base/quote
#' assets, and precision settings. Typically returns 600+ pairs on
#' Binance.US.
#'
#' @param base Character. \code{"us"} (default) or \code{"global"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{symbol}{Character. Trading pair symbol (e.g. \code{"BTCUSD"}).}
#'     \item{status}{Character. Trading status (\code{"TRADING"} or \code{"BREAK"}).}
#'     \item{baseAsset}{Character. Base asset (e.g. \code{"BTC"}).}
#'     \item{quoteAsset}{Character. Quote asset (e.g. \code{"USD"}).}
#'     \item{baseAssetPrecision}{Integer. Decimal precision for base asset.}
#'     \item{quoteAssetPrecision}{Integer. Decimal precision for quote asset.}
#'   }
#' @examples
#' syms <- bn_symbols()
#' syms |> dplyr::filter(status == "TRADING", quoteAsset == "USD")
#' @export
bn_symbols <- function(base = "us") {
  raw <- .bn_get("exchangeInfo", base = base)
  symbols <- raw$symbols
  if (is.null(symbols) || nrow(symbols) == 0) return(tibble())

  as_tibble(symbols) |>
    select(any_of(c("symbol", "status", "baseAsset", "quoteAsset",
                     "baseAssetPrecision", "quoteAssetPrecision")))
}

#' Get 5-minute volume-weighted average price (VWAP)
#'
#' Returns the average price over the last 5 minutes, weighted by
#' trade volume.
#'
#' @param symbol Character. Trading pair (e.g. \code{"BTCUSD"}).
#' @param base Character. \code{"us"} (default) or \code{"global"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{symbol}{Character. The queried trading pair.}
#'     \item{mins}{Integer. VWAP window in minutes (always 5).}
#'     \item{price}{Numeric. Volume-weighted average price.}
#'   }
#' @examples
#' bn_avg_price("BTCUSD")
#' bn_avg_price("ETHUSD")
#' @export
bn_avg_price <- function(symbol, base = "us") {
  raw <- .bn_get("avgPrice", list(symbol = toupper(symbol)), base)
  tibble(symbol = toupper(symbol), mins = as.integer(raw$mins),
         price = as.numeric(raw$price))
}


# == Context ===================================================================

#' Get binance.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bn_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bn_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/binance.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "binance.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# binance.com context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# binance.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
