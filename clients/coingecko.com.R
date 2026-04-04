# coingecko.com.R - Self-contained coingecko.com client

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)



# coingecko-com.R
# Self-contained CoinGecko cryptocurrency market data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none for free tier. Pro key via x-cg-pro-api-key header.
# Rate limits: Free = 10-30 req/min. Pro = higher.
# Docs: https://www.coingecko.com/en/api/documentation


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cg_base <- "https://api.coingecko.com/api/v3"
# -- Fetch helpers -------------------------------------------------------------

.cg_get <- function(endpoint, params = list()) {
  query <- if (length(params) > 0) {
    paste(names(params), params, sep = "=", collapse = "&")
  } else ""
  url <- if (query != "") paste0(.cg_base, "/", endpoint, "?", query)
         else paste0(.cg_base, "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

.cg_get_list <- function(endpoint, params = list()) {
  query <- if (length(params) > 0) {
    paste(names(params), params, sep = "=", collapse = "&")
  } else ""
  url <- if (query != "") paste0(.cg_base, "/", endpoint, "?", query)
         else paste0(.cg_base, "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


# == Schemas ===================================================================

.schema_markets <- tibble(
  id = character(), symbol = character(), name = character(),
  current_price = numeric(), market_cap = numeric(),
  market_cap_rank = integer(), total_volume = numeric(),
  price_change_24h = numeric(), price_change_pct_24h = numeric()
)

.schema_history <- tibble(
  timestamp = as.POSIXct(character()), price = numeric(),
  market_cap = numeric(), volume = numeric(), coin = character()
)

.schema_search <- tibble(
  id = character(), name = character(), symbol = character(),
  market_cap_rank = integer()
)



# == Market data ===============================================================

#' Top coins by market cap
#'
#' Returns current market data for top cryptocurrencies ranked by market
#' capitalization. Data refreshes every few minutes.
#'
#' @param vs_currency Character. Quote currency (default \code{"usd"}).
#'   Use \code{\link{cg_currencies}} to list valid values.
#'   Common: \code{"usd"}, \code{"eur"}, \code{"btc"}, \code{"eth"}.
#' @param per_page Integer. Results per page (default 100, max 250).
#' @param page Integer. Page number (default 1). Use with per_page for pagination.
#' @param order Character. Sort order (default \code{"market_cap_desc"}).
#'   Valid: \code{"market_cap_desc"}, \code{"market_cap_asc"},
#'   \code{"volume_desc"}, \code{"volume_asc"}, \code{"id_asc"}, \code{"id_desc"}.
#' @return A tibble with one row per coin and 27 columns including:
#'   \describe{
#'     \item{id}{Character. CoinGecko coin ID (e.g. "bitcoin").}
#'     \item{symbol}{Character. Ticker (e.g. "btc").}
#'     \item{name}{Character. Full name (e.g. "Bitcoin").}
#'     \item{current_price}{Numeric. Current price in vs_currency.}
#'     \item{market_cap}{Numeric. Total market cap.}
#'     \item{market_cap_rank}{Integer. Rank by market cap (1 = largest).}
#'     \item{total_volume}{Numeric. 24-hour trading volume.}
#'     \item{price_change_24h}{Numeric. Absolute price change (24h).}
#'     \item{price_change_pct_24h}{Numeric. Percent price change (24h).}
#'     \item{high_24h, low_24h}{Numeric. 24h high/low prices.}
#'     \item{ath}{Numeric. All-time high price.}
#'     \item{circulating_supply}{Numeric. Coins in circulation.}
#'   }
#' @examples
#' cg_markets()
#' cg_markets(vs_currency = "eur", per_page = 10)
cg_markets <- function(vs_currency = "usd", per_page = 100, page = 1,
                       order = "market_cap_desc") {
  raw <- .cg_get("coins/markets", list(
    vs_currency = vs_currency, order = order,
    per_page = per_page, page = page, sparkline = "false"
  ))
  if (is.null(raw) || nrow(raw) == 0) return(.schema_markets)
  as_tibble(raw) |>
    mutate(
      current_price = as.numeric(current_price),
      market_cap = as.numeric(market_cap),
      market_cap_rank = as.integer(market_cap_rank),
      total_volume = as.numeric(total_volume),
      price_change_24h = as.numeric(price_change_24h),
      price_change_pct_24h = as.numeric(price_change_percentage_24h)
    )
}


# == Price history =============================================================

#' Historical price/market cap/volume chart data
#'
#' Returns time series data at varying granularity based on the days parameter:
#' 1 day = 5-minute intervals, 2-90 days = hourly, 90+ days = daily.
#'
#' @param coin Character. CoinGecko coin ID. Use \code{\link{cg_search}} to find IDs.
#'   Example: \code{"bitcoin"}, \code{"ethereum"}, \code{"solana"}
#' @param vs_currency Character. Quote currency (default \code{"usd"}).
#' @param days Integer or character. Number of days back.
#'   Valid: \code{1}, \code{7}, \code{14}, \code{30}, \code{90}, \code{180},
#'   \code{365}, \code{"max"}.
#' @return A tibble with 5 columns:
#'   \describe{
#'     \item{timestamp}{POSIXct. UTC timestamp of the data point.}
#'     \item{price}{Numeric. Price in vs_currency.}
#'     \item{market_cap}{Numeric. Market cap at that time.}
#'     \item{volume}{Numeric. Trading volume at that time.}
#'     \item{coin}{Character. Coin ID (echoed back for joins).}
#'   }
#' @examples
#' cg_history("bitcoin", days = 7)
#' cg_history("ethereum", vs_currency = "eur", days = 30)
cg_history <- function(coin, vs_currency = "usd", days = 30) {
  raw <- .cg_get(paste0("coins/", coin, "/market_chart"), list(
    vs_currency = vs_currency, days = days
  ))
  if (is.null(raw$prices) || nrow(raw$prices) == 0) return(.schema_history)

  tibble(
    timestamp  = as.POSIXct(raw$prices[, 1] / 1000, origin = "1970-01-01", tz = "UTC"),
    price      = as.numeric(raw$prices[, 2]),
    market_cap = as.numeric(raw$market_caps[, 2]),
    volume     = as.numeric(raw$total_volumes[, 2]),
    coin       = coin
  )
}

#' Historical OHLC candlestick data
#'
#' Returns open-high-low-close candlestick data at varying granularity.
#' Candle intervals: 1-2 days = 30min, 3-30 days = 4h, 31+ days = daily.
#'
#' @param coin Character. CoinGecko coin ID.
#'   Example: \code{"bitcoin"}, \code{"ethereum"}
#' @param vs_currency Character. Quote currency (default \code{"usd"}).
#' @param days Integer or character. Days back.
#'   Valid: \code{1}, \code{7}, \code{14}, \code{30}, \code{90}, \code{180},
#'   \code{365}, \code{"max"}.
#' @return A tibble with 6 columns:
#'   \describe{
#'     \item{timestamp}{POSIXct. UTC candle open time.}
#'     \item{open}{Numeric. Opening price.}
#'     \item{high}{Numeric. Highest price in the candle.}
#'     \item{low}{Numeric. Lowest price in the candle.}
#'     \item{close}{Numeric. Closing price.}
#'     \item{coin}{Character. Coin ID.}
#'   }
#' @examples
#' cg_ohlc("bitcoin", days = 30)
#' cg_ohlc("ethereum", days = 7)
cg_ohlc <- function(coin, vs_currency = "usd", days = 30) {
  raw <- .cg_get(paste0("coins/", coin, "/ohlc"), list(
    vs_currency = vs_currency, days = days
  ))
  if (is.null(raw) || nrow(raw) == 0)
    return(tibble(timestamp = as.POSIXct(character()), open = numeric(),
                  high = numeric(), low = numeric(), close = numeric(),
                  coin = character()))

  tibble(
    timestamp = as.POSIXct(raw[, 1] / 1000, origin = "1970-01-01", tz = "UTC"),
    open      = as.numeric(raw[, 2]),
    high      = as.numeric(raw[, 3]),
    low       = as.numeric(raw[, 4]),
    close     = as.numeric(raw[, 5]),
    coin      = coin
  )
}

#' Price at a specific historical date
#'
#' Returns a single snapshot of price, market cap, and volume for a coin
#' on a given date. Note: CoinGecko free tier rate limits may apply.
#'
#' @param coin Character. CoinGecko coin ID.
#'   Example: \code{"bitcoin"}, \code{"ethereum"}
#' @param date Date or character. Target date. If character, use
#'   \code{"DD-MM-YYYY"} format. If Date object, converted automatically.
#'   Example: \code{as.Date("2024-01-01")} or \code{"01-01-2024"}
#' @return A tibble with 1 row and 5 columns:
#'   \describe{
#'     \item{coin}{Character. Coin ID.}
#'     \item{date}{Date. Requested date.}
#'     \item{price}{Numeric. USD price on that date.}
#'     \item{market_cap}{Numeric. USD market cap on that date.}
#'     \item{volume}{Numeric. USD 24h volume on that date.}
#'   }
#' @examples
#' cg_price_at("bitcoin", as.Date("2024-01-01"))
cg_price_at <- function(coin, date) {
  if (inherits(date, "Date")) date <- format(date, "%d-%m-%Y")
  raw <- .cg_get_list(paste0("coins/", coin, "/history"), list(date = date))
  if (is.null(raw$market_data)) return(tibble())
  md <- raw$market_data
  tibble(
    coin       = coin,
    date       = as.Date(date, format = "%d-%m-%Y"),
    price      = md$current_price$usd %||% NA_real_,
    market_cap = md$market_cap$usd %||% NA_real_,
    volume     = md$total_volume$usd %||% NA_real_
  )
}


# == Coin info =================================================================

#' Detailed coin information
#'
#' Returns comprehensive metadata and current market data for a single coin.
#'
#' @param coin Character. CoinGecko coin ID.
#'   Example: \code{"bitcoin"}, \code{"ethereum"}, \code{"solana"}
#' @return A tibble with 1 row and 14 columns:
#'   \describe{
#'     \item{id}{Character. CoinGecko ID.}
#'     \item{symbol}{Character. Ticker symbol.}
#'     \item{name}{Character. Full coin name.}
#'     \item{description}{Character. English description (HTML stripped).}
#'     \item{genesis_date}{Character. Genesis/launch date (YYYY-MM-DD or NA).}
#'     \item{categories}{Character. Semicolon-delimited category list.}
#'     \item{current_price}{Numeric. Current USD price.}
#'     \item{market_cap}{Numeric. Current USD market cap.}
#'     \item{ath}{Numeric. All-time high price in USD.}
#'     \item{ath_date}{Character. Date of ATH (ISO 8601).}
#'     \item{atl}{Numeric. All-time low price in USD.}
#'     \item{total_supply}{Numeric. Total coin supply.}
#'     \item{circulating}{Numeric. Circulating supply.}
#'     \item{max_supply}{Numeric. Maximum supply (NA if uncapped).}
#'   }
#' @examples
#' cg_coin("bitcoin")
#' cg_coin("ethereum")
cg_coin <- function(coin) {
  raw <- .cg_get_list(paste0("coins/", coin), list(
    localization = "false", tickers = "false",
    community_data = "false", developer_data = "false"
  ))
  if (is.null(raw$id)) return(tibble())

  md <- raw$market_data
  tibble(
    id               = raw$id,
    symbol           = raw$symbol,
    name             = raw$name,
    description      = raw$description$en %||% NA_character_,
    genesis_date     = raw$genesis_date %||% NA_character_,
    categories       = paste(raw$categories %||% character(), collapse = "; "),
    current_price    = md$current_price$usd %||% NA_real_,
    market_cap       = md$market_cap$usd %||% NA_real_,
    ath              = md$ath$usd %||% NA_real_,
    ath_date         = md$ath_date$usd %||% NA_character_,
    atl              = md$atl$usd %||% NA_real_,
    total_supply     = md$total_supply %||% NA_real_,
    circulating      = md$circulating_supply %||% NA_real_,
    max_supply       = md$max_supply %||% NA_real_
  )
}


# == Discovery =================================================================

#' Search coins by keyword
#'
#' Searches CoinGecko's coin database by name, symbol, or ID.
#'
#' @param query Character. Search string.
#'   Example: \code{"bitcoin"}, \code{"eth"}, \code{"solana"}
#' @return A tibble with 4 columns:
#'   \describe{
#'     \item{id}{Character. CoinGecko ID for use with other functions.}
#'     \item{name}{Character. Full coin name.}
#'     \item{symbol}{Character. Ticker symbol.}
#'     \item{market_cap_rank}{Integer. Rank by market cap (NA if unranked).}
#'   }
#' @examples
#' cg_search("bitcoin")
#' cg_search("defi")
cg_search <- function(query) {
  raw <- .cg_get("search", list(query = utils::URLencode(query)))
  coins <- raw$coins
  if (is.null(coins) || nrow(coins) == 0) return(.schema_search)
  as_tibble(coins) |>
    select(any_of(c("id", "name", "symbol", "market_cap_rank"))) |>
    mutate(market_cap_rank = as.integer(market_cap_rank))
}

#' Trending coins (top 15 by search volume)
#'
#' Returns the top 15 trending coins on CoinGecko based on recent search
#' activity. Refreshes periodically.
#'
#' @return A tibble with 5 columns:
#'   \describe{
#'     \item{id}{Character. CoinGecko coin ID.}
#'     \item{name}{Character. Full coin name.}
#'     \item{symbol}{Character. Ticker symbol.}
#'     \item{market_cap_rank}{Integer. Current market cap rank.}
#'     \item{score}{Integer. Trending rank (0 = most trending).}
#'   }
#' @examples
#' cg_trending()
cg_trending <- function() {
  raw <- .cg_get_list("search/trending")
  coins <- raw$coins
  if (is.null(coins) || length(coins) == 0) return(tibble())
  bind_rows(lapply(coins, function(c) {
    item <- c$item
    tibble(
      id              = item$id %||% NA_character_,
      name            = item$name %||% NA_character_,
      symbol          = item$symbol %||% NA_character_,
      market_cap_rank = as.integer(item$market_cap_rank %||% NA),
      score           = as.integer(item$score %||% NA)
    )
  }))
}

#' List all supported coins (ID, symbol, name)
#'
#' Returns the full list of coins supported by CoinGecko (15,000+ coins).
#' Useful for mapping symbols/names to CoinGecko IDs.
#'
#' @return A tibble with 3 columns:
#'   \describe{
#'     \item{id}{Character. CoinGecko coin ID for API calls.}
#'     \item{symbol}{Character. Ticker symbol (may not be unique).}
#'     \item{name}{Character. Full coin name.}
#'   }
#' @examples
#' cg_coins_list()
cg_coins_list <- function() {
  raw <- .cg_get("coins/list")
  as_tibble(raw)
}

#' List supported vs currencies
#'
#' Returns all quote currencies supported by CoinGecko's API for price
#' conversion (e.g. "usd", "eur", "btc").
#'
#' @return Character vector of supported currency codes (e.g. \code{"usd"},
#'   \code{"eur"}, \code{"gbp"}, \code{"btc"}, \code{"eth"}).
#' @examples
#' cg_currencies()
cg_currencies <- function() {
  .cg_get("simple/supported_vs_currencies")
}


# == Simple price ==============================================================

#' Current price for one or more coins
#'
#' Fast simple price lookup for one or more coins. More efficient than
#' \code{\link{cg_markets}} when you only need current prices.
#'
#' @param coins Character vector. One or more CoinGecko coin IDs.
#'   Example: \code{c("bitcoin", "ethereum")}, \code{"solana"}
#' @param vs_currencies Character. Quote currency or currencies (default \code{"usd"}).
#'   Example: \code{"usd"}, \code{c("usd", "eur")}
#' @param include_market_cap Logical. Include market cap (default \code{TRUE}).
#' @param include_24h_vol Logical. Include 24h volume (default \code{TRUE}).
#' @param include_24h_change Logical. Include 24h price change (default \code{TRUE}).
#' @return A tibble with one row per coin-currency pair and 6 columns:
#'   \describe{
#'     \item{coin}{Character. Coin ID.}
#'     \item{currency}{Character. Quote currency.}
#'     \item{price}{Numeric. Current price.}
#'     \item{market_cap}{Numeric. Current market cap (or NA).}
#'     \item{volume_24h}{Numeric. 24h trading volume (or NA).}
#'     \item{change_24h}{Numeric. 24h price change percent (or NA).}
#'   }
#' @examples
#' cg_price(c("bitcoin", "ethereum"))
#' cg_price("bitcoin", vs_currencies = c("usd", "eur"))
cg_price <- function(coins, vs_currencies = "usd", include_market_cap = TRUE,
                     include_24h_vol = TRUE, include_24h_change = TRUE) {
  raw <- .cg_get_list("simple/price", list(
    ids = paste(coins, collapse = ","),
    vs_currencies = paste(vs_currencies, collapse = ","),
    include_market_cap = tolower(include_market_cap),
    include_24hr_vol = tolower(include_24h_vol),
    include_24hr_change = tolower(include_24h_change)
  ))
  if (length(raw) == 0) return(tibble())

  bind_rows(lapply(names(raw), function(coin) {
    d <- raw[[coin]]
    bind_rows(lapply(vs_currencies, function(cur) {
      tibble(
        coin       = coin,
        currency   = cur,
        price      = d[[cur]] %||% NA_real_,
        market_cap = d[[paste0(cur, "_market_cap")]] %||% NA_real_,
        volume_24h = d[[paste0(cur, "_24h_vol")]] %||% NA_real_,
        change_24h = d[[paste0(cur, "_24h_change")]] %||% NA_real_
      )
    }))
  }))
}


# == Context ===================================================================

#' Get coingecko.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/coingecko.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "coingecko.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# coingecko.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# coingecko.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
