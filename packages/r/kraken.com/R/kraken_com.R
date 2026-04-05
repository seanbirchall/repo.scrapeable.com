# kraken.com.R - Self-contained kraken.com client



# kraken-com.R
# Self-contained Kraken crypto exchange client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none for public market data endpoints.
# Rate limits: ~15 req/sec for public endpoints.
# Docs: https://docs.kraken.com/rest/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.kr_base <- "https://api.kraken.com/0/public"
# -- Fetch helper --------------------------------------------------------------

.kr_get <- function(endpoint, params = list()) {
  query <- if (length(params) > 0) paste(names(params), params, sep = "=", collapse = "&") else ""
  url <- if (query != "") paste0(.kr_base, "/", endpoint, "?", query)
         else paste0(.kr_base, "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  if (length(raw$error) > 0 && any(nchar(raw$error) > 0))
    warning("Kraken API: ", paste(raw$error, collapse = "; "), call. = FALSE)
  raw$result
}


# == Schemas ===================================================================

.schema_ohlc <- tibble(
  timestamp = as.POSIXct(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), vwap = numeric(),
  volume = numeric(), count = integer(), pair = character()
)

.schema_ticker <- tibble(
  pair = character(), ask = numeric(), bid = numeric(), last = numeric(),
  volume_24h = numeric(), vwap_24h = numeric(), trades_24h = integer(),
  low_24h = numeric(), high_24h = numeric()
)



# == Ticker / Prices ===========================================================

#' Fetch current ticker data for one or more Kraken trading pairs
#'
#' Retrieves real-time market summary data from the Kraken public API
#' including best ask/bid prices, last trade price, 24-hour volume,
#' and 24-hour price range. Multiple pairs can be queried in a single
#' request by comma-separating them.
#'
#' @param pair Character. Trading pair(s) in Kraken format. Pass a single
#'   pair or comma-separated pairs. Common examples:
#'   \itemize{
#'     \item \code{"XBTUSD"} -- Bitcoin / US Dollar
#'     \item \code{"ETHUSD"} -- Ethereum / US Dollar
#'     \item \code{"SOLUSD"} -- Solana / US Dollar
#'     \item \code{"XBTUSD,ETHUSD"} -- multiple pairs in one call
#'   }
#'   Use \code{\link{kr_pairs}} for a full list of valid pair names.
#' @return A tibble with one row per pair and columns:
#'   \describe{
#'     \item{pair}{Character. Kraken pair identifier (e.g., \code{"XXBTZUSD"}).}
#'     \item{ask}{Numeric. Best ask (lowest sell) price.}
#'     \item{bid}{Numeric. Best bid (highest buy) price.}
#'     \item{last}{Numeric. Last trade price.}
#'     \item{volume_24h}{Numeric. Total volume traded in the last 24 hours.}
#'     \item{vwap_24h}{Numeric. Volume-weighted average price over 24 hours.}
#'     \item{trades_24h}{Integer. Number of trades in the last 24 hours.}
#'     \item{low_24h}{Numeric. Lowest price in the last 24 hours.}
#'     \item{high_24h}{Numeric. Highest price in the last 24 hours.}
#'   }
#' @examples
#' kr_ticker("XBTUSD")
#' kr_ticker("XBTUSD,ETHUSD")
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

#' Fetch historical OHLC candlestick data from Kraken
#'
#' Retrieves Open-High-Low-Close-Volume candlestick data for a trading
#' pair. Kraken returns up to 720 candles per request. For longer
#' histories, use the \code{since} parameter to paginate.
#'
#' @param pair Character. Trading pair (e.g., \code{"XBTUSD"},
#'   \code{"ETHUSD"}). Use \code{\link{kr_pairs}} for valid names.
#' @param interval Integer. Candle width in minutes. Accepted values:
#'   \code{1}, \code{5}, \code{15}, \code{30}, \code{60}, \code{240},
#'   \code{1440} (1 day, default), \code{10080} (1 week),
#'   \code{21600} (15 days).
#' @param since Numeric or \code{NULL}. Unix timestamp to start from.
#'   If \code{NULL} (default), the API returns the most recent candles.
#' @return A tibble with columns:
#'   \describe{
#'     \item{timestamp}{POSIXct. Candle open time (UTC).}
#'     \item{open}{Numeric. Opening price.}
#'     \item{high}{Numeric. Highest price during the interval.}
#'     \item{low}{Numeric. Lowest price during the interval.}
#'     \item{close}{Numeric. Closing price.}
#'     \item{vwap}{Numeric. Volume-weighted average price.}
#'     \item{volume}{Numeric. Total volume traded.}
#'     \item{count}{Integer. Number of trades in the interval.}
#'     \item{pair}{Character. The requested pair identifier.}
#'   }
#' @examples
#' kr_ohlc("XBTUSD")
#' kr_ohlc("ETHUSD", interval = 60)
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

#' Fetch order book depth for a Kraken trading pair
#'
#' Returns current bid and ask orders from the Kraken order book,
#' sorted by price. Useful for assessing liquidity and spread at
#' different price levels.
#'
#' @param pair Character. Trading pair (e.g., \code{"XBTUSD"}).
#' @param count Integer. Number of price levels per side (default
#'   \code{20}, maximum \code{500}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{side}{Character. \code{"bid"} or \code{"ask"}.}
#'     \item{price}{Numeric. Order price.}
#'     \item{volume}{Numeric. Order volume at this price.}
#'     \item{timestamp}{POSIXct. Time the order was placed (UTC).}
#'     \item{pair}{Character. The requested pair identifier.}
#'   }
#' @examples
#' kr_depth("XBTUSD")
#' kr_depth("ETHUSD", count = 50)
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

#' Fetch recent trades for a Kraken trading pair
#'
#' Returns the most recent public trades executed on Kraken for the
#' given pair. Each row represents a single filled trade with price,
#' volume, direction, and execution type.
#'
#' @param pair Character. Trading pair (e.g., \code{"XBTUSD"}).
#' @param count Integer. Number of trades to return (default \code{100}).
#' @param since Character or \code{NULL}. Trade ID (nanosecond timestamp)
#'   to paginate from. If \code{NULL} (default), returns the most recent
#'   trades.
#' @return A tibble with columns:
#'   \describe{
#'     \item{price}{Numeric. Trade execution price.}
#'     \item{volume}{Numeric. Trade volume.}
#'     \item{timestamp}{POSIXct. Trade execution time (UTC).}
#'     \item{side}{Character. \code{"buy"} or \code{"sell"}.}
#'     \item{type}{Character. \code{"market"} or \code{"limit"}.}
#'     \item{pair}{Character. The requested pair identifier.}
#'   }
#' @examples
#' kr_trades("XBTUSD")
#' kr_trades("ETHUSD", count = 50)
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

#' Fetch recent bid-ask spread data for a Kraken trading pair
#'
#' Returns time-series spread snapshots showing the best bid and ask
#' prices over recent intervals. Useful for analysing market liquidity
#' and spread dynamics.
#'
#' @param pair Character. Trading pair (e.g., \code{"XBTUSD"}).
#' @param since Numeric or \code{NULL}. Unix timestamp to start from.
#'   If \code{NULL} (default), returns the most recent spread data.
#' @return A tibble with columns:
#'   \describe{
#'     \item{timestamp}{POSIXct. Spread snapshot time (UTC).}
#'     \item{bid}{Numeric. Best bid price at that time.}
#'     \item{ask}{Numeric. Best ask price at that time.}
#'     \item{pair}{Character. The requested pair identifier.}
#'   }
#' @examples
#' kr_spread("XBTUSD")
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

#' List all Kraken trading pairs
#'
#' Returns metadata for every trading pair available on the Kraken
#' exchange, including the base and quote assets, pair status, and
#' decimal precision for prices and lot sizes.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{pair}{Character. Kraken pair identifier (e.g., \code{"XXBTZUSD"}).}
#'     \item{altname}{Character. Shorter alternative name (e.g., \code{"XBTUSD"}).}
#'     \item{base}{Character. Base asset code.}
#'     \item{quote}{Character. Quote asset code.}
#'     \item{status}{Character. Trading status (\code{"online"}, \code{"cancel_only"}, etc.).}
#'     \item{lot_decimals}{Integer. Number of decimal places for order volume.}
#'     \item{pair_decimals}{Integer. Number of decimal places for price.}
#'   }
#' @examples
#' kr_pairs()
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

#' List all assets available on Kraken
#'
#' Returns metadata for every asset (cryptocurrency and fiat currency)
#' listed on the Kraken exchange.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{asset}{Character. Kraken asset code (e.g., \code{"XXBT"}, \code{"ZUSD"}).}
#'     \item{altname}{Character. Common name (e.g., \code{"XBT"}, \code{"USD"}).}
#'     \item{decimals}{Integer. Internal decimal precision.}
#'     \item{display_decimals}{Integer. Display decimal precision.}
#'     \item{status}{Character. Asset status (\code{"enabled"}, \code{"disabled"}).}
#'   }
#' @examples
#' kr_assets()
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

#' Get kraken.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
kr_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(kr_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/kraken.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "kraken.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# kraken.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# kraken.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
