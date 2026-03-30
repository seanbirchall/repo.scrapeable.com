# == Market data ===============================================================

#' Top coins by market cap
#'
#' Returns current market data for top cryptocurrencies.
#'
#' @param vs_currency Quote currency (default "usd"). Options: usd, eur, btc, eth, etc.
#' @param per_page Results per page (default 100, max 250)
#' @param page Page number (default 1)
#' @param order Sort: "market_cap_desc" (default), "market_cap_asc",
#'   "volume_desc", "volume_asc", "id_asc", "id_desc"
#' @return tibble: id, symbol, name, current_price, market_cap, market_cap_rank,
#'   total_volume, price_change_24h, price_change_pct_24h, ...
#' @export
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
#' @param coin CoinGecko coin ID (e.g. "bitcoin", "ethereum", "solana").
#'   Use cg_search() to find IDs.
#' @param vs_currency Quote currency (default "usd")
#' @param days Number of days back: 1, 7, 14, 30, 90, 180, 365, "max"
#' @return tibble: timestamp (POSIXct), price, market_cap, volume, coin
#' @export
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
#' @param coin CoinGecko coin ID
#' @param vs_currency Quote currency (default "usd")
#' @param days Days back: 1, 7, 14, 30, 90, 180, 365, "max"
#' @return tibble: timestamp, open, high, low, close, coin
#' @export
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
#' @param coin CoinGecko coin ID
#' @param date Date (Date or "DD-MM-YYYY" string)
#' @return tibble: one row with coin, date, price, market_cap, volume
#' @export
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
#' @param coin CoinGecko coin ID
#' @return tibble: one row with id, symbol, name, description, categories,
#'   genesis_date, market_data (nested)
#' @export
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
#' @param query Search string (name, symbol, or ID)
#' @return tibble: id, name, symbol, market_cap_rank
#' @export
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
#' @return tibble: id, name, symbol, market_cap_rank, score
#' @export
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
#' @return tibble: id, symbol, name
#' @export
cg_coins_list <- function() {
  raw <- .cg_get("coins/list")
  as_tibble(raw)
}

#' List supported vs currencies
#'
#' @return character vector of supported quote currencies
#' @export
cg_currencies <- function() {
  .cg_get("simple/supported_vs_currencies")
}


# == Simple price ==============================================================

#' Current price for one or more coins
#'
#' @param coins Character vector of coin IDs (e.g. c("bitcoin", "ethereum"))
#' @param vs_currencies Quote currencies (default "usd")
#' @param include_market_cap Include market cap (default TRUE)
#' @param include_24h_vol Include 24h volume (default TRUE)
#' @param include_24h_change Include 24h price change (default TRUE)
#' @return tibble: coin, currency, price, market_cap, volume_24h, change_24h
#' @export
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

#' Generate LLM-friendly context for the coingecko.com package
#'
#' @return Character string (invisibly), also printed
#' @export
cg_context <- function() {
  .build_context("coingecko.com", header_lines = c(
    "# coingecko.com - Cryptocurrency Market Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none for free tier (10-30 req/min)",
    "# All functions return tibbles.",
    "#",
    "# Coin IDs: bitcoin, ethereum, solana, cardano, dogecoin, etc.",
    "#   Use cg_search('name') or cg_coins_list() to find IDs.",
    "# Currencies: usd, eur, btc, eth, gbp, jpy, etc.",
    "#   Use cg_currencies() for full list."
  ))
}
