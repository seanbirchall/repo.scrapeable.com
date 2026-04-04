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
#' Retrieves end-of-day OHLCV prices from the Tiingo API, including
#' split- and dividend-adjusted values. Also computes daily open-to-close
#' change and percent change.
#'
#' @param ticker Character. Stock ticker symbol (e.g. \code{"AAPL"},
#'   \code{"MSFT"}).
#' @param token Character. Tiingo API token (obtain from
#'   \url{https://api.tiingo.com}).
#' @param start Character. Start date in \code{"YYYY-MM-DD"} format
#'   (default \code{"1960-01-01"}).
#' @param end Character. End date in \code{"YYYY-MM-DD"} format (default:
#'   today).
#' @return A data.frame with columns: \code{date} (Date), \code{close},
#'   \code{high}, \code{low}, \code{open} (numeric, raw prices),
#'   \code{volume} (numeric), \code{adjClose}, \code{adjHigh}, \code{adjLow},
#'   \code{adjOpen}, \code{adjVolume} (numeric, adjusted), \code{ticker}
#'   (character), \code{change} (numeric), \code{change_pct} (numeric).
#' @examples
#' \dontrun{
#' tiingo_prices("AAPL", token = Sys.getenv("TIINGO_TOKEN"),
#'               start = "2024-01-01")
#' }
#' @export
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
#' Retrieves the most recent end-of-day price snapshot from Tiingo. Pass
#' \code{NULL} for ticker to fetch all monitored tickers (requires a paid
#' Tiingo plan).
#'
#' @param ticker Character or \code{NULL}. Ticker symbol (e.g. \code{"AAPL"}).
#'   \code{NULL} returns all monitored tickers.
#' @param token Character. Tiingo API token.
#' @return A data.frame with columns: \code{date} (Date), \code{close},
#'   \code{high}, \code{low}, \code{open}, \code{volume} (numeric),
#'   \code{ticker} (character, if a ticker was supplied).
#' @examples
#' \dontrun{
#' tiingo_prices_today("AAPL", token = Sys.getenv("TIINGO_TOKEN"))
#' }
#' @export
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
#' Retrieves intraday price bars from the Tiingo IEX endpoint. Data is
#' resampled to the specified frequency. Only available for US equities
#' traded on IEX.
#'
#' @param ticker Character. Ticker symbol (e.g. \code{"AAPL"}).
#' @param token Character. Tiingo API token.
#' @param start Character. Start date in \code{"YYYY-MM-DD"} format
#'   (default: today).
#' @param freq Character. Resample frequency (default \code{"1min"}).
#'   Other values: \code{"5min"}, \code{"15min"}, \code{"1hour"}.
#' @return A data.frame with columns: \code{date} (POSIXct, UTC),
#'   \code{close}, \code{high}, \code{low}, \code{open}, \code{volume}
#'   (numeric), \code{ticker} (character).
#' @examples
#' \dontrun{
#' tiingo_intraday("AAPL", token = Sys.getenv("TIINGO_TOKEN"))
#' }
#' @export
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
#' Returns descriptive metadata for a single ticker, including name,
#' description, exchange, start/end dates, and sector information.
#'
#' @param ticker Character. Ticker symbol (e.g. \code{"AAPL"}).
#' @param token Character. Tiingo API token.
#' @return A data.frame with one row and columns: \code{ticker},
#'   \code{name}, \code{description}, \code{exchangeCode},
#'   \code{startDate}, \code{endDate} (character).
#' @examples
#' \dontrun{
#' tiingo_meta("AAPL", token = Sys.getenv("TIINGO_TOKEN"))
#' }
#' @export
tiingo_meta <- function(ticker, token) {
  url <- paste0(.tiingo_base, "/tiingo/daily/", ticker, "?token=", token)
  path <- .tiingo_get(url, token)
  df <- jsonlite::fromJSON(path)
  as.data.frame(df, stringsAsFactors = FALSE)
}


# == Supported tickers =========================================================

#' Fetch list of all Tiingo-supported tickers
#'
#' Downloads and parses the full list of tickers supported by Tiingo. The
#' list is fetched as a zipped CSV from the Tiingo media server. No API
#' token is required.
#'
#' @return A data.frame with columns: \code{ticker} (character),
#'   \code{exchange} (character), \code{assetType} (character),
#'   \code{priceCurrency} (character), \code{startDate} (character),
#'   \code{endDate} (character).
#' @examples
#' \dontrun{
#' tickers <- tiingo_tickers()
#' head(tickers)
#' }
#' @export
tiingo_tickers <- function() {
  url <- "https://apimedia.tiingo.com/docs/tiingo/daily/supported_tickers.zip"
  tmp <- http_get(url, ext = ".zip")
  files <- utils::unzip(tmp, list = TRUE)
  csv_file <- files$Name[grepl("\\.csv$", files$Name)][1]
  df <- read.csv(unz(tmp, csv_file), stringsAsFactors = FALSE)
  as.data.frame(df)
}
