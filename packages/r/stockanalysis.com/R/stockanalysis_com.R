# stockanalysis.com.R - Self-contained stockanalysis.com client




# stockanalysis-com.R
# Self-contained Stock Analysis financial data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tidyr, tibble
# Auth: none required (HTML scraping)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: income, balance sheet, cash flow, ratios for US public companies


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.sa_base <- "https://stockanalysis.com/stocks"
# -- Scraping engine -----------------------------------------------------------

.sa_fetch_table <- function(ticker, page) {
  url <- sprintf("%s/%s/%s/", .sa_base, tolower(ticker), page)

  tmp <- tempfile(fileext = ".html")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  doc <- xml2::read_html(tmp)
  tables <- xml2::xml_find_all(doc, ".//table")
  if (length(tables) == 0) return(tibble())

  tbl <- tables[[1]]
  rows <- xml2::xml_find_all(tbl, ".//tr")
  if (length(rows) < 3) return(tibble())

  # Parse all rows
  cells <- lapply(rows, function(r) {
    xml2::xml_text(xml2::xml_find_all(r, ".//td|.//th"), trim = TRUE)
  })

  # Row 1 = period labels (TTM, FY 2025, FY 2024, ...)
  # Row 2 = period ending dates
  # Row 3+ = metrics
  header <- cells[[1]]
  if (length(header) < 2) return(tibble())

  # Extract year columns (skip first which is the metric name)
  year_labels <- header[-1]
  n_cols <- length(year_labels)

  # Parse data rows (skip header rows 1-2)
  data_rows <- cells[-(1:2)]
  if (length(data_rows) == 0) return(tibble())

  # Build tibble: one row per metric-year combination (long format)
  results <- lapply(data_rows, function(r) {
    if (length(r) < 2) return(NULL)
    metric <- r[1]
    if (metric == "" || is.na(metric)) return(NULL)
    vals <- r[-1]
    # Pad if needed
    length(vals) <- n_cols

    tibble(
      metric = metric,
      period = year_labels[seq_along(vals)],
      value_raw = vals
    )
  })

  df <- bind_rows(results)
  if (nrow(df) == 0) return(tibble())

  # Clean values: remove $, %, commas, handle (negative), convert
  df |>
    mutate(
      ticker = toupper(ticker),
      # Extract fiscal year from period label
      fiscal_year = as.integer(gsub(".*?(\\d{4}).*", "\\1", period)),
      is_ttm = period == "TTM",
      # Clean numeric values
      value = gsub("[$,%]", "", value_raw),
      value = gsub("\\((.+)\\)", "-\\1", value),  # (123) -> -123
      value = gsub(",", "", value),
      value = ifelse(value %in% c("-", "n/a", "N/A", ""), NA_character_, value),
      value = as.numeric(value)
    ) |>
    select(ticker, metric, period, fiscal_year, is_ttm, value)
}


# == Schemas ===================================================================

.schema_financials <- tibble(
  ticker = character(), metric = character(), period = character(),
  fiscal_year = integer(), is_ttm = logical(), value = numeric()
)



# == Financial statements ======================================================

#' Fetch annual income statement data
#'
#' Scrapes the annual income statement from Stock Analysis for a US public
#' company. Returns metrics in long format with one row per metric-period
#' combination. Typical metrics include Revenue, Cost of Revenue, Gross
#' Profit, Operating Income, Net Income, EPS (Basic and Diluted), EBITDA,
#' and share counts. Covers multiple fiscal years plus TTM where available.
#'
#' @param ticker Character. US stock ticker symbol, case-insensitive
#'   (e.g. \code{"AAPL"}, \code{"MSFT"}, \code{"GOOGL"}, \code{"AMZN"}).
#' @return A tibble in long format with 6 columns:
#' \describe{
#'   \item{ticker}{Character. Uppercased ticker symbol.}
#'   \item{metric}{Character. Financial metric name (e.g. "Revenue", "Net Income").}
#'   \item{period}{Character. Period label (e.g. "TTM", "FY 2024", "FY 2023").}
#'   \item{fiscal_year}{Integer. Four-digit fiscal year extracted from period; \code{NA} for TTM.}
#'   \item{is_ttm}{Logical. \code{TRUE} if the period is trailing twelve months.}
#'   \item{value}{Numeric. Metric value in millions USD (or ratio/percentage depending on metric).}
#' }
#' @examples
#' \dontrun{
#' sa_income("AAPL")
#' sa_income("MSFT") |> dplyr::filter(metric == "Revenue")
#' }
#' @seealso \code{\link{sa_income_quarterly}} for quarterly data,
#'   \code{\link{sa_financials}} to fetch all statements at once.
#' @export
sa_income <- function(ticker) {
  .sa_fetch_table(ticker, "financials")
}

#' Fetch quarterly income statement
#'
#' Same as \code{\link{sa_income}} but returns quarterly periods instead of
#' annual fiscal years. Useful for tracking seasonal trends and recent
#' quarter-over-quarter performance.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"AAPL"}).
#' @return A tibble in long format with 6 columns: ticker, metric, period,
#'   fiscal_year, is_ttm, value. See \code{\link{sa_income}} for column details.
#' @examples
#' \dontrun{
#' sa_income_quarterly("TSLA")
#' }
#' @export
sa_income_quarterly <- function(ticker) {
  .sa_fetch_table(ticker, "financials/?p=quarterly")
}

#' Fetch annual balance sheet data
#'
#' Scrapes the annual balance sheet from Stock Analysis. Returns metrics
#' such as Total Assets, Total Liabilities, Total Equity, Cash and
#' Equivalents, Total Debt, Retained Earnings, and Book Value Per Share.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"AAPL"}).
#' @return A tibble in long format with 6 columns: ticker, metric, period,
#'   fiscal_year, is_ttm, value. See \code{\link{sa_income}} for column details.
#' @examples
#' \dontrun{
#' sa_balance("AAPL")
#' }
#' @export
sa_balance <- function(ticker) {
  .sa_fetch_table(ticker, "financials/balance-sheet")
}

#' Fetch quarterly balance sheet
#'
#' Same as \code{\link{sa_balance}} but returns quarterly snapshots. Useful
#' for monitoring liquidity and leverage changes within a fiscal year.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"MSFT"}).
#' @return A tibble in long format with 6 columns: ticker, metric, period,
#'   fiscal_year, is_ttm, value. See \code{\link{sa_income}} for column details.
#' @examples
#' \dontrun{
#' sa_balance_quarterly("MSFT")
#' }
#' @export
sa_balance_quarterly <- function(ticker) {
  .sa_fetch_table(ticker, "financials/balance-sheet/?p=quarterly")
}

#' Fetch annual cash flow statement
#'
#' Scrapes the annual cash flow statement from Stock Analysis. Returns
#' metrics including Operating Cash Flow, Capital Expenditure, Free Cash
#' Flow, Depreciation, Stock-Based Compensation, and Dividends Paid.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"GOOGL"}).
#' @return A tibble in long format with 6 columns: ticker, metric, period,
#'   fiscal_year, is_ttm, value. See \code{\link{sa_income}} for column details.
#' @examples
#' \dontrun{
#' sa_cashflow("GOOGL")
#' }
#' @export
sa_cashflow <- function(ticker) {
  .sa_fetch_table(ticker, "financials/cash-flow-statement")
}

#' Fetch quarterly cash flow statement
#'
#' Same as \code{\link{sa_cashflow}} but returns quarterly periods. Useful
#' for tracking seasonal cash generation patterns.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"AMZN"}).
#' @return A tibble in long format with 6 columns: ticker, metric, period,
#'   fiscal_year, is_ttm, value. See \code{\link{sa_income}} for column details.
#' @examples
#' \dontrun{
#' sa_cashflow_quarterly("AMZN")
#' }
#' @export
sa_cashflow_quarterly <- function(ticker) {
  .sa_fetch_table(ticker, "financials/cash-flow-statement/?p=quarterly")
}

#' Fetch financial ratios
#'
#' Scrapes valuation and profitability ratios from Stock Analysis. Includes
#' Market Cap, PE Ratio, PS Ratio, PB Ratio, Enterprise Value, Dividend
#' Yield, Gross Margin, Operating Margin, Net Margin, ROE, ROA, and more.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"NVDA"}).
#' @return A tibble in long format with 6 columns: ticker, metric, period,
#'   fiscal_year, is_ttm, value. See \code{\link{sa_income}} for column details.
#' @examples
#' \dontrun{
#' sa_ratios("NVDA")
#' sa_ratios("AAPL") |> dplyr::filter(metric == "PE Ratio")
#' }
#' @export
sa_ratios <- function(ticker) {
  .sa_fetch_table(ticker, "financials/ratios")
}


# == Convenience: all financials ===============================================

#' Fetch all financial statements for a ticker
#'
#' Convenience function that combines income statement, balance sheet, and
#' cash flow statement into a single tibble with an added \code{statement}
#' column to distinguish the source. Makes 3 HTTP requests. Any individual
#' statement that fails is silently omitted.
#'
#' @param ticker Character. US stock ticker symbol (e.g. \code{"META"}).
#' @param quarterly Logical. If \code{TRUE}, fetch quarterly data instead
#'   of annual. Default \code{FALSE}.
#' @return A tibble in long format with 7 columns:
#' \describe{
#'   \item{ticker}{Character. Uppercased ticker symbol.}
#'   \item{metric}{Character. Financial metric name.}
#'   \item{period}{Character. Period label.}
#'   \item{fiscal_year}{Integer. Four-digit fiscal year; \code{NA} for TTM.}
#'   \item{is_ttm}{Logical. Whether this row is trailing twelve months.}
#'   \item{value}{Numeric. Metric value.}
#'   \item{statement}{Character. One of \code{"income"}, \code{"balance"}, or \code{"cashflow"}.}
#' }
#' @examples
#' \dontrun{
#' sa_financials("META")
#' sa_financials("TSLA", quarterly = TRUE)
#' }
#' @seealso \code{\link{sa_income}}, \code{\link{sa_balance}}, \code{\link{sa_cashflow}}
#' @export
sa_financials <- function(ticker, quarterly = FALSE) {
  if (quarterly) {
    inc <- tryCatch(sa_income_quarterly(ticker), error = function(e) NULL)
    bal <- tryCatch(sa_balance_quarterly(ticker), error = function(e) NULL)
    cf  <- tryCatch(sa_cashflow_quarterly(ticker), error = function(e) NULL)
  } else {
    inc <- tryCatch(sa_income(ticker), error = function(e) NULL)
    bal <- tryCatch(sa_balance(ticker), error = function(e) NULL)
    cf  <- tryCatch(sa_cashflow(ticker), error = function(e) NULL)
  }

  bind_rows(
    if (!is.null(inc) && nrow(inc) > 0) mutate(inc, statement = "income") else NULL,
    if (!is.null(bal) && nrow(bal) > 0) mutate(bal, statement = "balance") else NULL,
    if (!is.null(cf) && nrow(cf) > 0)  mutate(cf, statement = "cashflow") else NULL
  )
}


# == Context ===================================================================

#' Get stockanalysis.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
sa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/stockanalysis.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "stockanalysis.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# stockanalysis.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# stockanalysis.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
