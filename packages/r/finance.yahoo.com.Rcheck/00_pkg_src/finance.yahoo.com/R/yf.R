# == Discovery =================================================================

#' Search Yahoo Finance for tickers
#'
#' @param query Search string (e.g. "apple", "AAPL", "bitcoin")
#' @return tibble: symbol, name, type, exchange
#' @export
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

#' Batch real-time quotes for multiple tickers
#'
#' @param tickers Character vector of ticker symbols
#' @return tibble: ticker, name, price, change, change_pct, volume,
#'   market_cap, exchange, type
#' @export
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

#' Fetch OHLCV price history
#'
#' @param ticker Ticker symbol
#' @param interval Bar size: "1m", "5m", "15m", "30m", "1h", "1d" (default),
#'   "1wk", "1mo"
#' @param start Start date (Date or string). Default: 1 year ago.
#' @param end End date. Default: today.
#' @param range Alternative to start/end: "1d", "5d", "1mo", "3mo", "6mo",
#'   "1y", "2y", "5y", "10y", "ytd", "max". Overrides start/end if set.
#' @return tibble: date, open, high, low, close, adj_close, volume, ticker
#' @export
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

#' Fetch dividend history
#'
#' @param ticker Ticker symbol
#' @param start Start date (default: 10 years ago)
#' @return tibble: date, dividend, ticker
#' @export
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
#' @param ticker Ticker symbol
#' @param start Start date (default: 20 years ago)
#' @return tibble: date, numerator, denominator, ratio, ticker
#' @export
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

#' Company profile
#'
#' @param ticker Ticker symbol
#' @return tibble: ticker, name, sector, industry, employees, country,
#'   city, website, description
#' @export
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

#' Financial statements (income, balance sheet, cash flow)
#'
#' @param ticker Ticker symbol
#' @param statement One of: "income", "balance", "cashflow"
#' @param period "annual" (default) or "quarterly"
#' @return tibble (long format): ticker, date, period, metric, value
#' @export
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

#' Key statistics
#'
#' @param ticker Ticker symbol
#' @return tibble (long format): ticker, metric, value
#' @export
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

#' Institutional and insider holders
#'
#' @param ticker Ticker symbol
#' @return tibble: ticker, holder, shares, date_reported, pct_held, value, type
#' @export
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

#' Analyst recommendations and upgrades/downgrades
#'
#' @param ticker Ticker symbol
#' @return tibble: ticker, firm, to_grade, from_grade, action, date
#' @export
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

#' Earnings history and estimates
#'
#' @param ticker Ticker symbol
#' @return tibble: ticker, date, eps_actual, eps_estimate,
#'   revenue_actual, revenue_estimate
#' @export
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

#' Options chain
#'
#' @param ticker Ticker symbol
#' @param expiration Expiration date (Date or string). NULL = nearest.
#' @return tibble: ticker, expiration, type (call/put), strike, last_price,
#'   bid, ask, volume, open_interest, implied_volatility
#' @export
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


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the finance.yahoo.com package
#'
#' @param compact If TRUE (default), concise output
#' @return Character string (invisibly), also printed
#' @export
yf_context <- function(compact = TRUE) {
  fns <- list(
    list("yf_search", "(query)",
         "Search/autocomplete tickers. Returns symbol, name, type, exchange."),
    list("yf_quote", "(tickers)",
         "Batch real-time quotes. Accepts vector of tickers."),
    list("yf_history", "(ticker, interval, start, end, range)",
         "OHLCV history. interval: 1m/5m/15m/30m/1h/1d/1wk/1mo. range: 1d/5d/1mo/3mo/6mo/1y/2y/5y/10y/ytd/max."),
    list("yf_dividends", "(ticker, start)",
         "Dividend history with dates and amounts."),
    list("yf_splits", "(ticker, start)",
         "Stock split history with ratios."),
    list("yf_profile", "(ticker)",
         "Company profile: sector, industry, employees, description."),
    list("yf_financials", "(ticker, statement, period)",
         "Financial statements (long format). statement: income/balance/cashflow. period: annual/quarterly."),
    list("yf_statistics", "(ticker)",
         "Key statistics (long format): PE, PB, beta, short interest, margins, etc."),
    list("yf_holders", "(ticker)",
         "Institutional + insider holders with shares and dates."),
    list("yf_recommendations", "(ticker)",
         "Analyst upgrades/downgrades with firm, grade, date."),
    list("yf_earnings", "(ticker)",
         "Earnings history: EPS actual vs estimate."),
    list("yf_options", "(ticker, expiration)",
         "Full options chain: calls + puts with greeks. expiration=NULL for nearest.")
  )

  lines <- c(
    "# finance.yahoo.com - Yahoo Finance Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (cookie/crumb handled internally)",
    "# All functions return tibbles with typed columns.",
    "# No xts/zoo/quantmod dependency.",
    "#",
    "# == Functions ==",
    "#"
  )

  for (fn in fns) {
    lines <- c(lines,
      sprintf("# %s%s", fn[[1]], fn[[2]]),
      sprintf("#   %s", fn[[3]]),
      sprintf("#   Run `%s` to view source or `?%s` for help.", fn[[1]], fn[[1]]),
      "#"
    )
  }

  lines <- c(lines,
    "# == Quick examples ==",
    "#",
    "# yf_quote(c('AAPL', 'MSFT', 'GOOGL'))       # batch quotes",
    "# yf_history('AAPL', range = '1y')             # 1 year daily",
    "# yf_history('SPY', interval = '5m', range = '1d')  # intraday",
    "# yf_financials('AAPL', 'income', 'quarterly') # quarterly income stmt",
    "# yf_statistics('AAPL') |> filter(metric == 'forwardPE')",
    "# yf_options('AAPL')                           # nearest expiration",
    "# yf_profile('AAPL')                           # company info",
    "#"
  )

  out <- paste(lines, collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
