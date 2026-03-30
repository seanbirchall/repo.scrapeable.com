# tiingo.R
# Tiingo stock price data client.
# All functions return data.frames. No database dependencies.
#
# Requires a Tiingo API token: https://api.tiingo.com

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

.tiingo_base <- "https://api.tiingo.com"

.tiingo_get <- function(url, token, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = "support@scrapeable.com",
      Authorization = paste("Token", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_perform(path = tmp)
  tmp
}


# == Daily prices ==============================================================

#' Fetch historical daily prices for a ticker
#'
#' @param ticker Stock ticker symbol
#' @param token Tiingo API token
#' @param start Start date (default: "1960-01-01")
#' @param end End date (default: today)
#' @return data.frame with OHLCV + adjusted prices
tiingo_prices <- function(ticker, token, start = "1960-01-01", end = NULL) {
  if (is.null(end)) end <- format(Sys.Date(), "%Y-%m-%d")
  url <- paste0(.tiingo_base, "/tiingo/daily/", ticker, "/prices",
                "?startDate=", start, "&endDate=", end, "&token=", token)
  path <- .tiingo_get(url, token)
  df <- jsonlite::fromJSON(path)
  if (length(df) == 0 || nrow(df) == 0) return(data.frame())

  df$date <- as.Date(substr(df$date, 1, 10))
  df$ticker <- ticker

  # Calculate change metrics
  if ("adjClose" %in% names(df) && "adjOpen" %in% names(df)) {
    df$change <- df$adjClose - df$adjOpen
    df$change_pct <- ifelse(df$adjOpen != 0, df$change / df$adjOpen, NA)
  }

  as.data.frame(df)
}

#' Fetch today's prices for a ticker (or all)
#'
#' @param ticker Ticker symbol (or NULL for all monitored tickers - requires
#'   paid Tiingo plan)
#' @param token Tiingo API token
#' @return data.frame with today's OHLCV data
tiingo_prices_today <- function(ticker = NULL, token) {
  if (is.null(ticker)) {
    url <- paste0(.tiingo_base, "/tiingo/daily/prices?token=", token)
  } else {
    url <- paste0(.tiingo_base, "/tiingo/daily/", ticker, "/prices?token=", token)
  }
  path <- .tiingo_get(url, token)
  df <- jsonlite::fromJSON(path)
  if (length(df) == 0) return(data.frame())

  if (is.data.frame(df)) {
    df$date <- as.Date(substr(df$date, 1, 10))
    if (!is.null(ticker)) df$ticker <- ticker
  }
  as.data.frame(df)
}


# == Intraday ==================================================================

#' Fetch intraday IEX prices for a ticker
#'
#' @param ticker Ticker symbol
#' @param token Tiingo API token
#' @param start Start date (default: today)
#' @param freq Resample frequency (default: "1min")
#' @return data.frame with intraday OHLCV
tiingo_intraday <- function(ticker, token, start = NULL, freq = "1min") {
  if (is.null(start)) start <- format(Sys.Date(), "%Y-%m-%d")
  url <- paste0(.tiingo_base, "/iex/", ticker, "/prices",
                "?startDate=", start, "&resampleFreq=", freq, "&token=", token)
  path <- .tiingo_get(url, token)
  df <- jsonlite::fromJSON(path)
  if (length(df) == 0) return(data.frame())

  df$date <- as.POSIXct(df$date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  df$ticker <- ticker
  as.data.frame(df)
}


# == Metadata ==================================================================

#' Fetch ticker metadata from Tiingo
#'
#' @param ticker Ticker symbol
#' @param token Tiingo API token
#' @return data.frame with one row of ticker metadata
tiingo_meta <- function(ticker, token) {
  url <- paste0(.tiingo_base, "/tiingo/daily/", ticker, "?token=", token)
  path <- .tiingo_get(url, token)
  df <- jsonlite::fromJSON(path)
  as.data.frame(df, stringsAsFactors = FALSE)
}


# == Supported tickers =========================================================

#' Fetch list of all Tiingo-supported tickers
#'
#' @return data.frame with ticker, exchange, asset type, currency, etc.
tiingo_tickers <- function() {
  url <- "https://apimedia.tiingo.com/docs/tiingo/daily/supported_tickers.zip"
  tmp <- http_get(url, ext = ".zip")
  files <- utils::unzip(tmp, list = TRUE)
  csv_file <- files$Name[grepl("\\.csv$", files$Name)][1]
  df <- read.csv(unz(tmp, csv_file), stringsAsFactors = FALSE)
  as.data.frame(df)
}
