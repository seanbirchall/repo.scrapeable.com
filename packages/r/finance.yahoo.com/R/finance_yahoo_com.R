# finance.yahoo.com.R
# Self-contained Yahoo Finance client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (cookie/crumb handled internally)
# No xts/zoo/quantmod dependency.



# finance-yahoo.R
# Self-contained Yahoo Finance client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (cookie/crumb handled internally)
# Rate limits: undocumented, be polite (~2 req/sec max)


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.yf_cache <- new.env(parent = emptyenv())
.yf_base_chart   <- "https://query2.finance.yahoo.com/v8/finance/chart"
.yf_base_summary <- "https://query2.finance.yahoo.com/v10/finance/quoteSummary"
.yf_base_quote   <- "https://query2.finance.yahoo.com/v7/finance/quote"
.yf_base_search  <- "https://query2.finance.yahoo.com/v1/finance/search"
.yf_base_options <- "https://query2.finance.yahoo.com/v7/finance/options"
# -- Auth (cookie + crumb, cached per session) --------------------------------

.yf_auth <- function() {
  if (!is.null(.yf_cache$crumb) && !is.null(.yf_cache$cookie)) {
    return(list(crumb = .yf_cache$crumb, cookie = .yf_cache$cookie))
  }

  resp <- httr2::request("https://fc.yahoo.com/") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  cookie <- paste(httr2::resp_headers(resp, "set-cookie"), collapse = "; ")

  crumb <- httr2::request("https://query2.finance.yahoo.com/v1/test/getcrumb") |>
    httr2::req_headers(`User-Agent` = .ua, Cookie = cookie) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  .yf_cache$crumb <- crumb
  .yf_cache$cookie <- cookie
  list(crumb = crumb, cookie = cookie)
}

# -- Authenticated fetch -------------------------------------------------------

.yf_get <- function(url) {
  auth <- .yf_auth()
  sep <- if (grepl("\\?", url)) "&" else "?"
  full_url <- paste0(url, sep, "crumb=", utils::URLencode(auth$crumb))

  httr2::request(full_url) |>
    httr2::req_headers(`User-Agent` = .ua, Cookie = auth$cookie) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = FALSE)
}

# -- Extract raw value from Yahoo's {raw, fmt, longFmt} objects ---------------

.yf_raw <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  if (is.list(x) && !is.null(x$raw)) return(x$raw)
  if (is.numeric(x) || is.character(x)) return(x)
  default
}

.yf_chr <- function(x, default = NA_character_) {
  if (is.null(x)) return(default)
  if (is.list(x) && !is.null(x$fmt)) return(as.character(x$fmt))
  if (is.character(x)) return(x)
  as.character(x)
}

# -- quoteSummary module fetch ------------------------------------------------

.yf_summary <- function(ticker, modules) {
  modules_str <- paste(modules, collapse = ",")
  url <- sprintf("%s/%s?modules=%s", .yf_base_summary, ticker, modules_str)
  raw <- .yf_get(url)
  result <- raw$quoteSummary$result
  if (is.null(result) || length(result) == 0) return(NULL)
  result[[1]]
}


# == Schemas ===================================================================

.schema_history <- tibble(
  date = as.Date(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), adj_close = numeric(),
  volume = numeric(), ticker = character()
)

.schema_quote <- tibble(
  ticker = character(), name = character(), price = numeric(),
  change = numeric(), change_pct = numeric(), volume = numeric(),
  market_cap = numeric(), exchange = character(), type = character()
)

.schema_search <- tibble(
  symbol = character(), name = character(), type = character(),
  exchange = character()
)

.schema_profile <- tibble(
  ticker = character(), name = character(), sector = character(),
  industry = character(), employees = integer(), country = character(),
  city = character(), website = character(), description = character()
)

.schema_financials <- tibble(
  ticker = character(), date = as.Date(character()),
  period = character(), metric = character(), value = numeric()
)

.schema_statistics <- tibble(
  ticker = character(), metric = character(), value = numeric()
)

.schema_holders <- tibble(
  ticker = character(), holder = character(), shares = numeric(),
  date_reported = as.Date(character()), pct_held = numeric(),
  value = numeric(), type = character()
)

.schema_recommendations <- tibble(
  ticker = character(), firm = character(), to_grade = character(),
  from_grade = character(), action = character(),
  date = as.Date(character())
)

.schema_earnings <- tibble(
  ticker = character(), date = as.Date(character()),
  eps_actual = numeric(), eps_estimate = numeric(),
  revenue_actual = numeric(), revenue_estimate = numeric()
)

.schema_options <- tibble(
  ticker = character(), expiration = as.Date(character()),
  type = character(), strike = numeric(), last_price = numeric(),
  bid = numeric(), ask = numeric(), volume = integer(),
  open_interest = integer(), implied_volatility = numeric()
)



# == Discovery =================================================================

#' Search Yahoo Finance for tickers, ETFs, indices, and crypto
#'
#' Performs a fuzzy search across Yahoo Finance's symbol database. Matches
#' against ticker symbols, company names, fund names, and cryptocurrency
#' names. Returns up to 20 results ranked by relevance.
#'
#' @param query Character. Search string, which can be a ticker symbol
#'   (e.g., \code{"AAPL"}), company name (\code{"apple"}), fund name
#'   (\code{"vanguard 500"}), or cryptocurrency (\code{"bitcoin"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{symbol}{Character. Ticker symbol (e.g., \code{"AAPL"}, \code{"BTC-USD"}).}
#'     \item{name}{Character. Full instrument name (e.g., \code{"Apple Inc."}).}
#'     \item{type}{Character. Quote type: \code{"EQUITY"}, \code{"ETF"},
#'       \code{"MUTUALFUND"}, \code{"INDEX"}, \code{"CRYPTOCURRENCY"}, \code{"FUTURE"}.}
#'     \item{exchange}{Character. Exchange code (e.g., \code{"NMS"} for Nasdaq,
#'       \code{"NYQ"} for NYSE).}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_quote}} for live prices, \code{\link{yf_profile}} for company details
#' @examples
#' \dontrun{
#' yf_search("apple")
#' yf_search("bitcoin")
#' yf_search("vanguard 500")
#' }
yf_search <- function(query) {
  url <- sprintf("%s?q=%s&quotesCount=20&newsCount=0",
                 .yf_base_search, utils::URLencode(query))
  raw <- .yf_get(url)
  quotes <- raw$quotes
  if (is.null(quotes) || length(quotes) == 0) return(.schema_search)

  bind_rows(lapply(quotes, function(q) {
    tibble(
      symbol   = q$symbol %||% NA_character_,
      name     = q$longname %||% q$shortname %||% NA_character_,
      type     = q$quoteType %||% NA_character_,
      exchange = q$exchange %||% NA_character_
    )
  }))
}

#' Get real-time quotes for one or more tickers
#'
#' Fetches live market data for a vector of ticker symbols in a single
#' API call. Returns the latest price, daily change, volume, and market cap.
#' Works for equities, ETFs, indices, crypto, and futures.
#'
#' @param tickers Character vector. One or more ticker symbols
#'   (e.g., \code{c("AAPL", "MSFT", "GOOG")}, \code{"BTC-USD"}, \code{"^GSPC"}).
#' @return A tibble with one row per ticker and columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{name}{Character. Full instrument name.}
#'     \item{price}{Numeric. Latest regular-market price.}
#'     \item{change}{Numeric. Absolute price change today.}
#'     \item{change_pct}{Numeric. Percentage price change today.}
#'     \item{volume}{Numeric. Regular-market volume (shares traded).}
#'     \item{market_cap}{Numeric. Market capitalization in dollars, or \code{NA}.}
#'     \item{exchange}{Character. Exchange name (e.g., \code{"NasdaqGS"}).}
#'     \item{type}{Character. Quote type (\code{"EQUITY"}, \code{"ETF"}, etc.).}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_history}} for historical price data
#' @examples
#' \dontrun{
#' yf_quote("AAPL")
#' yf_quote(c("AAPL", "MSFT", "GOOG"))
#' }
yf_quote <- function(tickers) {
  symbols <- paste(tickers, collapse = ",")
  url <- sprintf("%s?symbols=%s", .yf_base_quote, symbols)
  raw <- .yf_get(url)
  results <- raw$quoteResponse$result
  if (is.null(results) || length(results) == 0) return(.schema_quote)

  bind_rows(lapply(results, function(q) {
    tibble(
      ticker     = q$symbol %||% NA_character_,
      name       = q$longName %||% q$shortName %||% NA_character_,
      price      = .yf_raw(q$regularMarketPrice),
      change     = .yf_raw(q$regularMarketChange),
      change_pct = .yf_raw(q$regularMarketChangePercent),
      volume     = .yf_raw(q$regularMarketVolume),
      market_cap = .yf_raw(q$marketCap),
      exchange   = q$fullExchangeName %||% NA_character_,
      type       = q$quoteType %||% NA_character_
    )
  }))
}


# == Price history =============================================================

#' Fetch OHLCV price history for a ticker
#'
#' Downloads historical Open/High/Low/Close/Volume bars for any Yahoo Finance
#' ticker. Supports intraday through monthly intervals. Specify a date range
#' with \code{start}/\code{end} or use the \code{range} shorthand.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}, \code{"BTC-USD"},
#'   \code{"^GSPC"}).
#' @param interval Character. Bar size: \code{"1m"}, \code{"5m"}, \code{"15m"},
#'   \code{"30m"}, \code{"1h"}, \code{"1d"} (default), \code{"1wk"}, \code{"1mo"}.
#'   Intraday intervals are limited to the last 30 days of data.
#' @param start Date or character. Start date in \code{YYYY-MM-DD} format.
#'   Default: 1 year ago. Ignored if \code{range} is set.
#' @param end Date or character. End date. Default: today.
#'   Ignored if \code{range} is set.
#' @param range Character or \code{NULL}. Shorthand period: \code{"1d"},
#'   \code{"5d"}, \code{"1mo"}, \code{"3mo"}, \code{"6mo"}, \code{"1y"},
#'   \code{"2y"}, \code{"5y"}, \code{"10y"}, \code{"ytd"}, \code{"max"}.
#'   Overrides \code{start}/\code{end} when set.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date (for daily+) or POSIXct (for intraday).}
#'     \item{open}{Numeric. Opening price.}
#'     \item{high}{Numeric. High price.}
#'     \item{low}{Numeric. Low price.}
#'     \item{close}{Numeric. Closing price.}
#'     \item{adj_close}{Numeric. Split- and dividend-adjusted close.}
#'     \item{volume}{Numeric. Volume (shares traded).}
#'     \item{ticker}{Character. The requested ticker symbol.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_dividends}} for dividend history,
#'   \code{\link{yf_splits}} for stock splits
#' @examples
#' \dontrun{
#' # Last 5 days of daily prices
#' yf_history("AAPL", range = "5d")
#'
#' # 1-hour bars for the past month
#' yf_history("MSFT", interval = "1h", range = "1mo")
#'
#' # Custom date range
#' yf_history("GOOG", start = "2024-01-01", end = "2024-12-31")
#' }
yf_history <- function(ticker, interval = "1d", start = NULL, end = NULL,
                       range = NULL) {
  url <- sprintf("%s/%s?interval=%s", .yf_base_chart, ticker, interval)

  if (!is.null(range)) {
    url <- paste0(url, "&range=", range)
  } else {
    if (is.null(start)) start <- Sys.Date() - 365
    if (is.null(end)) end <- Sys.Date()
    p1 <- as.integer(as.POSIXct(as.Date(start)))
    p2 <- as.integer(as.POSIXct(as.Date(end))) + 86400
    url <- paste0(url, "&period1=", p1, "&period2=", p2)
  }

  raw <- .yf_get(url)
  result <- raw$chart$result
  if (is.null(result) || length(result) == 0) return(.schema_history)
  r <- result[[1]]

  ts <- r$timestamp
  if (is.null(ts) || length(ts) == 0) return(.schema_history)

  q <- r$indicators$quote[[1]]
  adj <- r$indicators$adjclose
  adj_close <- if (!is.null(adj) && length(adj) > 0) adj[[1]]$adjclose else q$close

  # Replace NULLs with NA before unlisting (unlist drops NULLs)
  safe_unlist <- function(x) vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))

  n <- length(ts)
  dates <- as.POSIXct(vapply(ts, as.numeric, numeric(1)), origin = "1970-01-01", tz = "UTC")

  tibble(
    date      = if (interval %in% c("1d", "1wk", "1mo")) as.Date(dates) else dates,
    open      = safe_unlist(q$open),
    high      = safe_unlist(q$high),
    low       = safe_unlist(q$low),
    close     = safe_unlist(q$close),
    adj_close = safe_unlist(adj_close),
    volume    = safe_unlist(q$volume),
    ticker    = ticker
  )
}

#' Fetch dividend payment history
#'
#' Returns historical dividend payments for a stock, including ex-dividend
#' dates and per-share amounts. Results are sorted chronologically.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}, \code{"JNJ"}).
#' @param start Date or character. Start date for dividend history
#'   (default: 10 years ago). Format: \code{YYYY-MM-DD}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Ex-dividend date.}
#'     \item{dividend}{Numeric. Dividend amount per share in dollars.}
#'     \item{ticker}{Character. Ticker symbol.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_splits}} for stock split history
#' @examples
#' \dontrun{
#' yf_dividends("AAPL")
#' yf_dividends("JNJ", start = "2020-01-01")
#' }
yf_dividends <- function(ticker, start = Sys.Date() - 3650) {
  p1 <- as.integer(as.POSIXct(as.Date(start)))
  p2 <- as.integer(as.POSIXct(Sys.Date())) + 86400
  url <- sprintf("%s/%s?interval=1d&period1=%d&period2=%d&events=div",
                 .yf_base_chart, ticker, p1, p2)
  raw <- .yf_get(url)
  result <- raw$chart$result
  if (is.null(result) || length(result) == 0)
    return(tibble(date = as.Date(character()), dividend = numeric(), ticker = character()))

  events <- result[[1]]$events$dividends
  if (is.null(events) || length(events) == 0)
    return(tibble(date = as.Date(character()), dividend = numeric(), ticker = character()))

  bind_rows(lapply(events, function(d) {
    tibble(
      date     = as.Date(as.POSIXct(d$date, origin = "1970-01-01")),
      dividend = as.numeric(d$amount),
      ticker   = ticker
    )
  })) |> arrange(date)
}

#' Fetch stock split history
#'
#' Returns historical stock split events including the split ratio.
#' Results are sorted chronologically.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}, \code{"TSLA"}).
#' @param start Date or character. Start date for split history
#'   (default: 20 years ago). Format: \code{YYYY-MM-DD}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Split date.}
#'     \item{numerator}{Numeric. Shares received per \code{denominator} held.}
#'     \item{denominator}{Numeric. Shares held to receive \code{numerator}.}
#'     \item{ratio}{Character. Human-readable ratio (e.g., \code{"4:1"}).}
#'     \item{ticker}{Character. Ticker symbol.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_dividends}} for dividend history
#' @examples
#' \dontrun{
#' yf_splits("AAPL")
#' yf_splits("TSLA", start = "2019-01-01")
#' }
yf_splits <- function(ticker, start = Sys.Date() - 7300) {
  p1 <- as.integer(as.POSIXct(as.Date(start)))
  p2 <- as.integer(as.POSIXct(Sys.Date())) + 86400
  url <- sprintf("%s/%s?interval=1d&period1=%d&period2=%d&events=split",
                 .yf_base_chart, ticker, p1, p2)
  raw <- .yf_get(url)
  result <- raw$chart$result
  if (is.null(result) || length(result) == 0)
    return(tibble(date = as.Date(character()), numerator = numeric(),
                  denominator = numeric(), ratio = character(), ticker = character()))

  events <- result[[1]]$events$splits
  if (is.null(events) || length(events) == 0)
    return(tibble(date = as.Date(character()), numerator = numeric(),
                  denominator = numeric(), ratio = character(), ticker = character()))

  bind_rows(lapply(events, function(s) {
    tibble(
      date        = as.Date(as.POSIXct(s$date, origin = "1970-01-01")),
      numerator   = as.numeric(s$numerator),
      denominator = as.numeric(s$denominator),
      ratio       = paste0(s$numerator, ":", s$denominator),
      ticker      = ticker
    )
  })) |> arrange(date)
}


# == Fundamentals ==============================================================

#' Get company profile and business summary
#'
#' Fetches the asset profile for a publicly traded company, including
#' sector, industry, headcount, location, website, and a long-form
#' business description.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}, \code{"MSFT"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{name}{Character. Company name (e.g., \code{"Apple Inc."}).}
#'     \item{sector}{Character. GICS sector (e.g., \code{"Technology"}).}
#'     \item{industry}{Character. Industry (e.g., \code{"Consumer Electronics"}).}
#'     \item{employees}{Integer. Full-time employee count.}
#'     \item{country}{Character. Country of headquarters.}
#'     \item{city}{Character. City of headquarters.}
#'     \item{website}{Character. Corporate website URL.}
#'     \item{description}{Character. Long-form business summary.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @examples
#' \dontrun{
#' yf_profile("AAPL")
#' yf_profile("MSFT")
#' }
yf_profile <- function(ticker) {
  data <- .yf_summary(ticker, c("assetProfile", "price"))
  if (is.null(data)) return(.schema_profile)

  p <- data$assetProfile
  pr <- data$price
  tibble(
    ticker      = ticker,
    name        = pr$longName %||% pr$shortName %||% NA_character_,
    sector      = p$sector %||% NA_character_,
    industry    = p$industry %||% NA_character_,
    employees   = as.integer(p$fullTimeEmployees %||% NA),
    country     = p$country %||% NA_character_,
    city        = p$city %||% NA_character_,
    website     = p$website %||% NA_character_,
    description = p$longBusinessSummary %||% NA_character_
  )
}

#' Fetch financial statements (income, balance sheet, cash flow)
#'
#' Returns historical financial statement data in long format. Each row
#' is one metric for one reporting period, making it easy to filter
#' and pivot. Covers up to 4 annual or 4 quarterly periods.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}).
#' @param statement Character. Statement type: \code{"income"} (default),
#'   \code{"balance"} (balance sheet), or \code{"cashflow"} (cash flow statement).
#' @param period Character. Reporting period: \code{"annual"} (default) or
#'   \code{"quarterly"}.
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{date}{Date. End date of the reporting period.}
#'     \item{period}{Character. \code{"annual"} or \code{"quarterly"}.}
#'     \item{metric}{Character. Financial metric name (e.g.,
#'       \code{"totalRevenue"}, \code{"netIncome"}, \code{"totalAssets"}).}
#'     \item{value}{Numeric. Value in dollars.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_statistics}} for key ratios
#' @examples
#' \dontrun{
#' # Annual income statements
#' yf_financials("AAPL", "income")
#'
#' # Quarterly balance sheet
#' yf_financials("MSFT", "balance", period = "quarterly")
#' }
yf_financials <- function(ticker, statement = "income", period = "annual") {
  module_map <- list(
    income   = c("incomeStatementHistory", "incomeStatementHistoryQuarterly"),
    balance  = c("balanceSheetHistory", "balanceSheetHistoryQuarterly"),
    cashflow = c("cashflowStatementHistory", "cashflowStatementHistoryQuarterly")
  )
  modules <- module_map[[statement]]
  if (is.null(modules)) stop("statement must be 'income', 'balance', or 'cashflow'", call. = FALSE)

  module <- if (period == "quarterly") modules[2] else modules[1]
  data <- .yf_summary(ticker, module)
  if (is.null(data)) return(.schema_financials)

  stmts <- data[[module]]
  key <- names(stmts)[1]  # e.g. "incomeStatementHistory"
  rows <- stmts[[key]]
  if (is.null(rows) || length(rows) == 0) return(.schema_financials)

  bind_rows(lapply(rows, function(row) {
    dt <- as.Date(as.POSIXct(row$endDate$raw, origin = "1970-01-01"))
    metrics <- setdiff(names(row), c("endDate", "maxAge"))
    bind_rows(lapply(metrics, function(m) {
      tibble(
        ticker = ticker,
        date   = dt,
        period = period,
        metric = m,
        value  = .yf_raw(row[[m]])
      )
    }))
  }))
}

#' Fetch key financial statistics and ratios
#'
#' Returns key valuation metrics, profitability ratios, and financial
#' data points in long format. Combines data from Yahoo Finance's
#' \code{defaultKeyStatistics} and \code{financialData} modules.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}).
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{metric}{Character. Metric name (e.g., \code{"forwardPE"},
#'       \code{"trailingEps"}, \code{"bookValue"}, \code{"profitMargins"},
#'       \code{"returnOnEquity"}, \code{"revenueGrowth"}).}
#'     \item{value}{Numeric. Metric value.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_financials}} for full financial statements
#' @examples
#' \dontrun{
#' yf_statistics("AAPL")
#' }
yf_statistics <- function(ticker) {
  data <- .yf_summary(ticker, c("defaultKeyStatistics", "financialData"))
  if (is.null(data)) return(.schema_statistics)

  extract_metrics <- function(obj, prefix = "") {
    if (is.null(obj)) return(NULL)
    nms <- names(obj)
    nms <- nms[!nms %in% c("maxAge")]
    bind_rows(lapply(nms, function(m) {
      val <- .yf_raw(obj[[m]], default = NA)
      if (!is.numeric(val) && !is.na(val)) return(NULL)
      tibble(ticker = ticker, metric = m, value = as.numeric(val))
    }))
  }

  bind_rows(
    extract_metrics(data$defaultKeyStatistics),
    extract_metrics(data$financialData)
  ) |> filter(!is.na(value))
}

#' Fetch institutional and insider holders
#'
#' Returns top institutional holders (mutual funds, pension funds) and
#' insider holders (executives, board members) for a stock. Each row
#' includes the holder name, number of shares, percentage held, and value.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{holder}{Character. Holder name (e.g., \code{"Vanguard Group, Inc."}).}
#'     \item{shares}{Numeric. Number of shares held.}
#'     \item{date_reported}{Date. Date of the most recent filing.}
#'     \item{pct_held}{Numeric. Fraction of outstanding shares held (0--1).}
#'     \item{value}{Numeric. Dollar value of the position.}
#'     \item{type}{Character. \code{"institution"} or \code{"insider"}.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @examples
#' \dontrun{
#' yf_holders("AAPL")
#' }
yf_holders <- function(ticker) {
  data <- .yf_summary(ticker, c("institutionOwnership", "insiderHolders"))
  if (is.null(data)) return(.schema_holders)

  parse_holders <- function(obj, key, type) {
    holdings <- obj[[key]]
    if (is.null(holdings) || length(holdings) == 0) return(NULL)
    bind_rows(lapply(holdings, function(h) {
      tibble(
        ticker        = ticker,
        holder        = h$organization %||% h$name %||% NA_character_,
        shares        = .yf_raw(h$position %||% h$positionDirect),
        date_reported = tryCatch(
          as.Date(as.POSIXct(.yf_raw(h$reportDate), origin = "1970-01-01")),
          error = function(e) NA_real_
        ),
        pct_held      = .yf_raw(h$pctHeld),
        value         = .yf_raw(h$value),
        type          = type
      )
    }))
  }

  bind_rows(
    parse_holders(data$institutionOwnership, "ownershipList", "institution"),
    parse_holders(data$insiderHolders, "holders", "insider")
  )
}

#' Fetch analyst recommendations and upgrade/downgrade history
#'
#' Returns the history of analyst rating changes (upgrades, downgrades,
#' initiations, reiterations) from Wall Street firms. Results are sorted
#' by date descending (most recent first).
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{firm}{Character. Analyst firm name (e.g., \code{"Morgan Stanley"}).}
#'     \item{to_grade}{Character. New rating (e.g., \code{"Overweight"}, \code{"Buy"}).}
#'     \item{from_grade}{Character. Previous rating, or \code{""} for initiations.}
#'     \item{action}{Character. Action type: \code{"up"}, \code{"down"},
#'       \code{"main"} (maintained), \code{"init"} (initiated), \code{"reit"} (reiterated).}
#'     \item{date}{Date. Date of the rating change.}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @examples
#' \dontrun{
#' yf_recommendations("AAPL")
#' }
yf_recommendations <- function(ticker) {
  data <- .yf_summary(ticker, "upgradeDowngradeHistory")
  if (is.null(data)) return(.schema_recommendations)

  history <- data$upgradeDowngradeHistory$history
  if (is.null(history) || length(history) == 0) return(.schema_recommendations)

  bind_rows(lapply(history, function(h) {
    tibble(
      ticker     = ticker,
      firm       = h$firm %||% NA_character_,
      to_grade   = h$toGrade %||% NA_character_,
      from_grade = h$fromGrade %||% NA_character_,
      action     = h$action %||% NA_character_,
      date       = tryCatch(
        as.Date(as.POSIXct(h$epochGradeDate, origin = "1970-01-01")),
        error = function(e) NA_real_
      )
    )
  })) |> arrange(desc(date))
}

#' Fetch earnings history (EPS actuals vs. estimates)
#'
#' Returns quarterly earnings data showing actual vs. estimated EPS
#' (earnings per share). Useful for tracking earnings surprises.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{date}{Date. Quarter end date.}
#'     \item{eps_actual}{Numeric. Actual earnings per share.}
#'     \item{eps_estimate}{Numeric. Consensus EPS estimate.}
#'     \item{revenue_actual}{Numeric. Actual revenue (currently \code{NA}).}
#'     \item{revenue_estimate}{Numeric. Revenue estimate (currently \code{NA}).}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @seealso \code{\link{yf_financials}} for full income statements
#' @examples
#' \dontrun{
#' yf_earnings("AAPL")
#' }
yf_earnings <- function(ticker) {
  data <- .yf_summary(ticker, c("earningsHistory", "earnings"))
  if (is.null(data)) return(.schema_earnings)

  history <- data$earningsHistory$history
  if (is.null(history) || length(history) == 0) return(.schema_earnings)

  bind_rows(lapply(history, function(h) {
    tibble(
      ticker           = ticker,
      date             = tryCatch(
        as.Date(as.POSIXct(.yf_raw(h$quarter), origin = "1970-01-01")),
        error = function(e) NA_real_
      ),
      eps_actual       = .yf_raw(h$epsActual),
      eps_estimate     = .yf_raw(h$epsEstimate),
      revenue_actual   = NA_real_,
      revenue_estimate = NA_real_
    )
  }))
}

#' Fetch options chain (calls and puts)
#'
#' Downloads the full options chain for a given ticker and expiration date.
#' Returns both calls and puts with strike prices, bid/ask, volume,
#' open interest, and implied volatility.
#'
#' @param ticker Character. Ticker symbol (e.g., \code{"AAPL"}, \code{"SPY"}).
#' @param expiration Date, character (\code{YYYY-MM-DD}), or \code{NULL}.
#'   When \code{NULL} (default), returns the nearest available expiration.
#' @return A tibble with columns:
#'   \describe{
#'     \item{ticker}{Character. Ticker symbol.}
#'     \item{expiration}{Date. Option expiration date.}
#'     \item{type}{Character. \code{"call"} or \code{"put"}.}
#'     \item{strike}{Numeric. Strike price.}
#'     \item{last_price}{Numeric. Last traded price.}
#'     \item{bid}{Numeric. Current bid price.}
#'     \item{ask}{Numeric. Current ask price.}
#'     \item{volume}{Integer. Number of contracts traded today.}
#'     \item{open_interest}{Integer. Open interest (outstanding contracts).}
#'     \item{implied_volatility}{Numeric. Implied volatility (0--1+ scale).}
#'   }
#' @export
#' @family Yahoo Finance functions
#' @examples
#' \dontrun{
#' # Nearest expiration
#' yf_options("AAPL")
#'
#' # Specific expiration date
#' yf_options("SPY", expiration = "2026-06-20")
#' }
yf_options <- function(ticker, expiration = NULL) {
  url <- sprintf("%s/%s", .yf_base_options, ticker)
  if (!is.null(expiration)) {
    exp_ts <- as.integer(as.POSIXct(as.Date(expiration)))
    url <- paste0(url, "?date=", exp_ts)
  }

  raw <- .yf_get(url)
  result <- raw$optionChain$result
  if (is.null(result) || length(result) == 0) return(.schema_options)
  r <- result[[1]]

  opts <- r$options
  if (is.null(opts) || length(opts) == 0) return(.schema_options)

  parse_chain <- function(chain, type) {
    if (is.null(chain) || length(chain) == 0) return(NULL)
    bind_rows(lapply(chain, function(o) {
      tibble(
        ticker             = ticker,
        expiration         = tryCatch(
          as.Date(as.POSIXct(.yf_raw(o$expiration), origin = "1970-01-01")),
          error = function(e) NA_real_
        ),
        type               = type,
        strike             = .yf_raw(o$strike),
        last_price         = .yf_raw(o$lastPrice),
        bid                = .yf_raw(o$bid),
        ask                = .yf_raw(o$ask),
        volume             = as.integer(.yf_raw(o$volume, NA_integer_)),
        open_interest      = as.integer(.yf_raw(o$openInterest, NA_integer_)),
        implied_volatility = .yf_raw(o$impliedVolatility)
      )
    }))
  }

  o <- opts[[1]]
  bind_rows(
    parse_chain(o$calls, "call"),
    parse_chain(o$puts, "put")
  )
}


# == Context ===================================================================

#' Get finance.yahoo.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
yf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(yf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/finance.yahoo.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "finance.yahoo.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# finance.yahoo.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# finance.yahoo.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
