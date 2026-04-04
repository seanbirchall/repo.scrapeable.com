# stooq.com.R - Self-contained stooq.com client




# stooq-com.R
# Self-contained Stooq historical price data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required (public CSV downloads)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: US/global stocks, indices, commodities, forex, bonds, crypto


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.stooq_base <- "https://stooq.com/q/d/l/"
# -- CSV fetch engine ----------------------------------------------------------

.stooq_fetch <- function(symbol, start = NULL, end = NULL, interval = "d") {
  params <- list(s = tolower(symbol), i = interval)
  if (!is.null(start)) params$d1 <- format(as.Date(start), "%Y%m%d")
  if (!is.null(end))   params$d2 <- format(as.Date(end), "%Y%m%d")

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.stooq_base, "?", query)

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  # Check if file has content
  lines <- readLines(tmp, n = 2, warn = FALSE)
  if (length(lines) < 2 || lines[1] == "No data") return(.schema_history)

  df <- read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(df) == 0 || !("Date" %in% names(df))) return(.schema_history)

  result <- as_tibble(df) |>
    transmute(
      date   = as.Date(Date),
      open   = as.numeric(Open),
      high   = as.numeric(High),
      low    = as.numeric(Low),
      close  = as.numeric(Close),
      volume = if ("Volume" %in% names(df)) as.numeric(Volume) else NA_real_,
      ticker = symbol
    ) |>
    arrange(date)
  result
}


# == Schemas ===================================================================

.schema_history <- tibble(
  date = as.Date(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), volume = numeric(),
  ticker = character()
)



# == Price history =============================================================

#' Fetch historical OHLCV price data from Stooq
#'
#' Downloads CSV data from Stooq for any supported instrument including
#' stocks, indices, forex, commodities, crypto, and bond yields. Coverage
#' spans US and global markets. No API key is required.
#'
#' @param symbol Character. Stooq ticker symbol. Examples by asset class:
#'   \describe{
#'     \item{US stocks}{\code{"aapl.us"}, \code{"msft.us"}, \code{"googl.us"}}
#'     \item{US indices}{\code{"^spx"} (S&P 500), \code{"^dji"} (Dow),
#'       \code{"^ndq"} (Nasdaq)}
#'     \item{Forex}{\code{"eurusd"}, \code{"gbpusd"}, \code{"usdjpy"}}
#'     \item{Commodities}{\code{"gc.f"} (gold), \code{"cl.f"} (crude oil),
#'       \code{"si.f"} (silver)}
#'     \item{Crypto}{\code{"btc.v"} (Bitcoin), \code{"eth.v"} (Ethereum)}
#'     \item{Bonds}{\code{"10usy.b"} (US 10Y yield), \code{"2usy.b"}
#'       (US 2Y yield)}
#'   }
#' @param start Date or character. Start date (default: one year ago).
#'   Accepts \code{Date} objects or \code{"YYYY-MM-DD"} strings.
#' @param end Date or character. End date (default: today).
#' @param interval Character. Bar size: \code{"d"} (daily, default),
#'   \code{"w"} (weekly), \code{"m"} (monthly).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Trading date.}
#'     \item{open}{Numeric. Opening price.}
#'     \item{high}{Numeric. Highest price.}
#'     \item{low}{Numeric. Lowest price.}
#'     \item{close}{Numeric. Closing price.}
#'     \item{volume}{Numeric. Trading volume (\code{NA} if not reported).}
#'     \item{ticker}{Character. The symbol queried.}
#'   }
#' @examples
#' stooq_history("aapl.us")
#' stooq_history("^spx", interval = "w")
stooq_history <- function(symbol, start = Sys.Date() - 365,
                          end = Sys.Date(), interval = "d") {
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch historical OHLCV data for multiple symbols
#'
#' Iterates over a vector of Stooq ticker symbols, fetching price history
#' for each and stacking the results. Includes a polite sleep between
#' requests to avoid rate limiting. Progress is printed via \code{message()}.
#'
#' @param symbols Character vector. Stooq ticker symbols (e.g.,
#'   \code{c("aapl.us", "msft.us", "googl.us")}). See
#'   \code{\link{stooq_history}} for symbol format.
#' @param start Date or character. Start date (default: one year ago).
#' @param end Date or character. End date (default: today).
#' @param interval Character. Bar size: \code{"d"}, \code{"w"}, or
#'   \code{"m"}.
#' @param sleep Numeric. Seconds to pause between requests (default
#'   \code{0.5}).
#' @return A tibble with the same columns as \code{\link{stooq_history}},
#'   with rows from all symbols stacked. The \code{ticker} column
#'   identifies each symbol.
#' @examples
#' stooq_history_bulk(c("aapl.us", "msft.us"), start = Sys.Date() - 30)
stooq_history_bulk <- function(symbols, start = Sys.Date() - 365,
                               end = Sys.Date(), interval = "d",
                               sleep = 0.5) {
  n <- length(symbols)
  results <- lapply(seq_along(symbols), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] %s", i, n, symbols[i]))
    tryCatch(
      .stooq_fetch(symbols[i], start, end, interval),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
  })
  bind_rows(results)
}


# == Convenience: US stocks ====================================================

#' Fetch US stock price history (convenience wrapper)
#'
#' Convenience wrapper around \code{\link{stooq_history}} that
#' automatically appends the \code{".us"} suffix to the ticker.
#'
#' @param ticker Character. US stock ticker in plain form (e.g.,
#'   \code{"AAPL"}, \code{"MSFT"}, \code{"TSLA"}). The \code{".us"}
#'   suffix is added automatically.
#' @param start Date or character. Start date (default: one year ago).
#' @param end Date or character. End date (default: today).
#' @param interval Character. Bar size: \code{"d"} (daily, default),
#'   \code{"w"} (weekly), \code{"m"} (monthly).
#' @return A tibble with columns: \code{date} (Date), \code{open},
#'   \code{high}, \code{low}, \code{close}, \code{volume} (all Numeric),
#'   and \code{ticker} (Character).
#' @seealso \code{\link{stooq_history}} for arbitrary symbol types.
#' @examples
#' stooq_stock("AAPL")
stooq_stock <- function(ticker, start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0(tolower(ticker), ".us")
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch US index price history (convenience wrapper)
#'
#' Convenience wrapper around \code{\link{stooq_history}} that
#' automatically prepends \code{"^"} to the index code.
#'
#' @param index Character. Index short code (default \code{"spx"}):
#'   \itemize{
#'     \item \code{"spx"} -- S&P 500
#'     \item \code{"dji"} -- Dow Jones Industrial Average
#'     \item \code{"ndq"} -- Nasdaq Composite
#'     \item \code{"rut"} -- Russell 2000
#'     \item \code{"vix"} -- CBOE Volatility Index
#'   }
#' @param start Date or character. Start date (default: one year ago).
#' @param end Date or character. End date (default: today).
#' @param interval Character. Bar size: \code{"d"}, \code{"w"}, or \code{"m"}.
#' @return A tibble with columns: \code{date} (Date), \code{open},
#'   \code{high}, \code{low}, \code{close}, \code{volume} (all Numeric),
#'   and \code{ticker} (Character).
#' @seealso \code{\link{stooq_history}} for arbitrary symbol types.
#' @examples
#' stooq_index("spx")
#' stooq_index("dji", interval = "m")
stooq_index <- function(index = "spx", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0("^", tolower(index))
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch forex pair price history (convenience wrapper)
#'
#' Convenience wrapper around \code{\link{stooq_history}} for foreign
#' exchange rates.
#'
#' @param pair Character. Currency pair code (default \code{"eurusd"}).
#'   Common pairs: \code{"eurusd"}, \code{"gbpusd"}, \code{"usdjpy"},
#'   \code{"usdchf"}, \code{"audusd"}.
#' @param start Date or character. Start date (default: one year ago).
#' @param end Date or character. End date (default: today).
#' @param interval Character. Bar size: \code{"d"}, \code{"w"}, or \code{"m"}.
#' @return A tibble with columns: \code{date} (Date), \code{open},
#'   \code{high}, \code{low}, \code{close}, \code{volume} (all Numeric),
#'   and \code{ticker} (Character).
#' @seealso \code{\link{stooq_history}} for arbitrary symbol types.
#' @examples
#' stooq_forex("eurusd")
#' stooq_forex("gbpusd", start = "2023-01-01")
stooq_forex <- function(pair = "eurusd", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  .stooq_fetch(tolower(pair), start, end, interval)
}

#' Fetch US Treasury yield history (convenience wrapper)
#'
#' Convenience wrapper around \code{\link{stooq_history}} for US Treasury
#' bond yields. Automatically constructs the Stooq bond ticker from the
#' maturity code.
#'
#' @param maturity Character. Maturity period (default \code{"10"}):
#'   \itemize{
#'     \item \code{"2"} -- 2-year Treasury yield
#'     \item \code{"5"} -- 5-year Treasury yield
#'     \item \code{"10"} -- 10-year Treasury yield
#'     \item \code{"30"} -- 30-year Treasury yield
#'   }
#' @param start Date or character. Start date (default: one year ago).
#' @param end Date or character. End date (default: today).
#' @param interval Character. Bar size: \code{"d"}, \code{"w"}, or \code{"m"}.
#' @return A tibble with columns: \code{date} (Date), \code{open},
#'   \code{high}, \code{low}, \code{close}, \code{volume} (all Numeric),
#'   and \code{ticker} (Character).
#' @seealso \code{\link{stooq_history}} for arbitrary symbol types.
#' @examples
#' stooq_yield("10")
#' stooq_yield("2", start = "2023-01-01")
stooq_yield <- function(maturity = "10", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0(maturity, "usy.b")
  .stooq_fetch(symbol, start, end, interval)
}


# == Context ===================================================================

#' Get stooq.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
stooq_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(stooq_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/stooq.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "stooq.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# stooq.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# stooq.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
