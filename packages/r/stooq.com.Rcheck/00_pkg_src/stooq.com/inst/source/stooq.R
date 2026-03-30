# stooq-com.R
# Self-contained Stooq historical price data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required (public CSV downloads)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: US/global stocks, indices, commodities, forex, bonds, crypto

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.stooq_base <- "https://stooq.com/q/d/l/"

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

#' Fetch historical OHLCV price data
#'
#' Downloads CSV data from Stooq for any supported instrument.
#'
#' @param symbol Stooq ticker symbol. Examples:
#'   US stocks: "aapl.us", "msft.us", "googl.us"
#'   US indices: "^spx" (S&P 500), "^dji" (Dow), "^ndq" (Nasdaq)
#'   Forex: "eurusd", "gbpusd", "usdjpy"
#'   Commodities: "gc.f" (gold), "cl.f" (crude oil), "si.f" (silver)
#'   Crypto: "btc.v" (Bitcoin), "eth.v" (Ethereum)
#'   Bonds: "10usy.b" (US 10Y yield), "2usy.b" (US 2Y yield)
#' @param start Start date (Date or "YYYY-MM-DD"). Default: 1 year ago.
#' @param end End date. Default: today.
#' @param interval Bar size: "d" (daily, default), "w" (weekly), "m" (monthly)
#' @return tibble: date, open, high, low, close, volume, ticker
stooq_history <- function(symbol, start = Sys.Date() - 365,
                          end = Sys.Date(), interval = "d") {
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch multiple symbols at once
#'
#' @param symbols Character vector of Stooq ticker symbols
#' @param start Start date
#' @param end End date
#' @param interval Bar size: "d", "w", "m"
#' @param sleep Seconds between requests (default 0.5)
#' @return tibble: stacked OHLCV for all symbols
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

#' Fetch US stock history (auto-appends .us suffix)
#'
#' @param ticker US stock ticker (e.g. "AAPL", "MSFT") — ".us" is added automatically
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
stooq_stock <- function(ticker, start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0(tolower(ticker), ".us")
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch US index history
#'
#' @param index Index name: "spx" (S&P 500), "dji" (Dow Jones),
#'   "ndq" (Nasdaq Composite), "rut" (Russell 2000), "vix" (VIX)
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
stooq_index <- function(index = "spx", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0("^", tolower(index))
  .stooq_fetch(symbol, start, end, interval)
}

#' Fetch forex pair history
#'
#' @param pair Forex pair (e.g. "eurusd", "gbpusd", "usdjpy")
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
stooq_forex <- function(pair = "eurusd", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  .stooq_fetch(tolower(pair), start, end, interval)
}

#' Fetch US Treasury yield history
#'
#' @param maturity Maturity: "2" (2Y), "5" (5Y), "10" (10Y), "30" (30Y)
#' @param start Start date
#' @param end End date
#' @param interval "d", "w", or "m"
#' @return tibble: date, open, high, low, close, volume, ticker
stooq_yield <- function(maturity = "10", start = Sys.Date() - 365,
                        end = Sys.Date(), interval = "d") {
  symbol <- paste0(maturity, "usy.b")
  .stooq_fetch(symbol, start, end, interval)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the stooq.com package
#'
#' @return Character string (invisibly), also printed
stooq_context <- function() {
  .build_context("stooq.com", header_lines = c(
    "# stooq.com - Historical Price Data Client for R",
    "# Dependencies: httr2, dplyr, tibble",
    "# Auth: none (public CSV downloads)",
    "# All functions return tibbles: date, open, high, low, close, volume, ticker",
    "#",
    "# Ticker formats:",
    "#   US stocks: aapl.us, msft.us, googl.us",
    "#   Indices: ^spx (S&P 500), ^dji (Dow), ^ndq (Nasdaq), ^rut (Russell)",
    "#   Forex: eurusd, gbpusd, usdjpy",
    "#   Commodities: gc.f (gold), cl.f (crude), si.f (silver)",
    "#   Crypto: btc.v (Bitcoin), eth.v (Ethereum)",
    "#   Bonds: 10usy.b (10Y yield), 2usy.b (2Y), 30usy.b (30Y)",
    "#",
    "# Intervals: d (daily), w (weekly), m (monthly)"
  ))
}
