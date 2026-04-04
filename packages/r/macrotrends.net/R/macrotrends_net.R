# macrotrends.net.R - Self-contained macrotrends.net client




# macrotrends-net.R
# Self-contained Macrotrends financial data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tibble
# Auth: none required (HTML scraping)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: fundamental financial data for US public companies


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.mt_base <- "https://www.macrotrends.net/stocks/charts"
# -- Scraping engine -----------------------------------------------------------

.mt_fetch <- function(ticker, slug, metric) {
  # Macrotrends URL: /stocks/charts/{TICKER}/{slug}/{metric}
  url <- sprintf("%s/%s/%s/%s", .mt_base, toupper(ticker), tolower(slug), metric)

  tmp <- tempfile(fileext = ".html")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  doc <- xml2::read_html(tmp)
  tables <- xml2::xml_find_all(doc, ".//table")
  if (length(tables) == 0) return(list(annual = tibble(), quarterly = tibble()))

  parse_table <- function(tbl) {
    rows <- xml2::xml_find_all(tbl, ".//tr")
    if (length(rows) < 2) return(tibble())

    cells <- lapply(rows, function(r) {
      xml2::xml_text(xml2::xml_find_all(r, ".//td|.//th"), trim = TRUE)
    })

    # Keep only rows with 2+ cells (skip title/header rows with 1 cell)
    data <- Filter(function(r) length(r) >= 2, cells)
    if (length(data) == 0) return(tibble())

    mat <- do.call(rbind, lapply(data, function(r) r[1:2]))

    df <- tibble(
      date_raw = mat[, 1],
      value    = mat[, 2]
    )
    # Clean: remove $ signs, commas, % signs
    df$value <- gsub("[$,%]", "", df$value)
    df$value <- as.numeric(df$value)
    # Parse dates: try YYYY-MM-DD first, then year-only
    df$date <- tryCatch(as.Date(df$date_raw), error = function(e) rep(as.Date(NA), nrow(df)))
    year_only <- is.na(df$date) & grepl("^\\d{4}$", df$date_raw)
    if (any(year_only))
      df$date[year_only] <- as.Date(paste0(df$date_raw[year_only], "-12-31"))
    df <- df[!is.na(df$date), ]
    df |> select(date, value)
  }

  annual <- if (length(tables) >= 1) parse_table(tables[[1]]) else tibble()
  quarterly <- if (length(tables) >= 2) parse_table(tables[[2]]) else tibble()

  list(annual = annual, quarterly = quarterly)
}


# == Schemas ===================================================================

.schema_metric <- tibble(
  date = as.Date(character()), value = numeric(),
  ticker = character(), metric = character(), period = character()
)



# == Core metric fetching ======================================================

#' Fetch a financial metric from Macrotrends
#'
#' Scrapes the Macrotrends page for a given ticker and metric, parsing
#' the HTML table into a typed tibble. Supports annual and/or quarterly
#' data. See \code{\link{mt_metrics}} for the full list of 37 available
#' metric slugs.
#'
#' @param ticker Character. Stock ticker symbol, case-insensitive
#'   (e.g. \code{"AAPL"}, \code{"MSFT"}, \code{"GOOG"}).
#' @param slug Character. Company-name URL slug used by Macrotrends
#'   (e.g. \code{"apple"}, \code{"microsoft"}). If \code{NULL}
#'   (default), uses the lowercase ticker as the slug.
#' @param metric Character. Metric URL slug. Run \code{\link{mt_metrics}}
#'   for the full list. Common values: \code{"revenue"},
#'   \code{"net-income"}, \code{"eps-earnings-per-share-diluted"},
#'   \code{"pe-ratio"}, \code{"free-cash-flow"}.
#' @param period Character. One of \code{"annual"} (default),
#'   \code{"quarterly"}, or \code{"both"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. End-of-period date.}
#'     \item{value}{Numeric. Metric value (dollars, ratio, or
#'       percentage depending on the metric).}
#'     \item{ticker}{Character. Uppercase ticker symbol.}
#'     \item{metric}{Character. Metric slug as requested.}
#'     \item{period}{Character. \code{"annual"} or \code{"quarterly"}.}
#'   }
#' @examples
#' mt_get("AAPL", "apple", "revenue")
#' mt_get("MSFT", "microsoft", "pe-ratio", period = "quarterly")
mt_get <- function(ticker, slug = NULL, metric = "revenue",
                   period = "annual") {
  if (is.null(slug)) slug <- tolower(ticker)

  data <- .mt_fetch(ticker, slug, metric)

  result <- if (period == "both") {
    bind_rows(
      if (nrow(data$annual) > 0) mutate(data$annual, period = "annual") else NULL,
      if (nrow(data$quarterly) > 0) mutate(data$quarterly, period = "quarterly") else NULL
    )
  } else if (period == "quarterly" && nrow(data$quarterly) > 0) {
    mutate(data$quarterly, period = "quarterly")
  } else {
    mutate(data$annual, period = "annual")
  }

  if (nrow(result) == 0) return(.schema_metric)
  result$ticker <- toupper(ticker)
  result$metric <- metric
  result |> select(date, value, ticker, metric, period) |> arrange(date)
}

#' Fetch multiple metrics for a ticker
#'
#' Convenience wrapper that calls \code{\link{mt_get}} for each metric
#' in the vector and row-binds the results, with a polite delay between
#' requests.
#'
#' @param ticker Character. Stock ticker symbol (e.g. \code{"AAPL"}).
#' @param slug Character. Company-name URL slug, or \code{NULL} to
#'   default to the lowercase ticker.
#' @param metrics Character vector of metric URL slugs. See
#'   \code{\link{mt_metrics}} for valid values (e.g.
#'   \code{c("revenue", "net-income", "ebitda")}).
#' @param period Character. One of \code{"annual"} (default),
#'   \code{"quarterly"}, or \code{"both"}.
#' @param sleep Numeric. Seconds to wait between requests (default
#'   \code{0.5}).
#' @return A tibble with columns: \code{date} (Date), \code{value}
#'   (numeric), \code{ticker} (character), \code{metric} (character),
#'   \code{period} (character). Rows from all metrics are stacked.
#' @examples
#' mt_get_bulk("AAPL", "apple",
#'             metrics = c("revenue", "net-income"), period = "annual")
mt_get_bulk <- function(ticker, slug = NULL, metrics, period = "annual",
                        sleep = 0.5) {
  n <- length(metrics)
  results <- lapply(seq_along(metrics), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] %s: %s", i, n, ticker, metrics[i]))
    tryCatch(
      mt_get(ticker, slug, metrics[i], period),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
  })
  bind_rows(results)
}


# == Available metrics =========================================================

#' List available Macrotrends metrics
#'
#' Returns a reference table of all 37 known financial metric URL slugs
#' supported by the Macrotrends scraper, organised by category. Use the
#' \code{metric} column values as the \code{metric} argument to
#' \code{\link{mt_get}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{metric}{Character. URL slug to pass to \code{mt_get}
#'       (e.g. \code{"revenue"}, \code{"pe-ratio"}).}
#'     \item{category}{Character. One of \code{"income"},
#'       \code{"balance"}, \code{"cashflow"}, \code{"margin"},
#'       \code{"return"}, \code{"valuation"}, \code{"per_share"},
#'       \code{"other"}.}
#'     \item{description}{Character. Human-readable metric name
#'       (e.g. \code{"Total revenue"}, \code{"Diluted EPS"}).}
#'   }
#' @examples
#' mt_metrics()
#' mt_metrics() |> dplyr::filter(category == "valuation")
mt_metrics <- function() {
  tibble(
    metric = c(
      # Income statement
      "revenue", "cost-goods-sold", "gross-profit", "operating-income",
      "net-income", "ebitda", "eps-earnings-per-share-diluted",
      # Balance sheet
      "total-assets", "total-liabilities", "total-shareholder-equity",
      "long-term-debt", "current-ratio", "book-value-per-share",
      "cash-on-hand", "total-current-assets", "total-current-liabilities",
      # Cash flow
      "free-cash-flow", "operating-cash-flow", "capital-expenditures",
      # Margins
      "gross-profit-margin", "operating-profit-margin", "net-profit-margin",
      "ebitda-margin",
      # Returns
      "roe", "roa", "roi", "roic",
      # Valuation
      "pe-ratio", "price-book", "price-fcf", "ev-ebitda",
      # Per share
      "revenue-per-share", "cash-flow-per-share", "dividend-per-share",
      # Other
      "shares-outstanding", "number-of-employees", "dividend-yield-history"
    ),
    category = c(
      rep("income", 7), rep("balance", 9), rep("cashflow", 3),
      rep("margin", 4), rep("return", 4), rep("valuation", 4),
      rep("per_share", 3), rep("other", 3)
    ),
    description = c(
      "Total revenue", "Cost of goods sold", "Gross profit", "Operating income",
      "Net income", "EBITDA", "Diluted EPS",
      "Total assets", "Total liabilities", "Shareholder equity",
      "Long-term debt", "Current ratio", "Book value per share",
      "Cash and equivalents", "Total current assets", "Total current liabilities",
      "Free cash flow", "Operating cash flow", "Capital expenditures",
      "Gross margin %", "Operating margin %", "Net margin %", "EBITDA margin %",
      "Return on equity %", "Return on assets %", "Return on investment %",
      "Return on invested capital %",
      "Price to earnings", "Price to book", "Price to FCF", "EV to EBITDA",
      "Revenue per share", "Cash flow per share", "Dividend per share",
      "Shares outstanding", "Employee count", "Dividend yield %"
    )
  )
}


# == Convenience: common financial data ========================================

#' Fetch income statement metrics
#'
#' Convenience function that fetches revenue, gross profit, operating
#' income, net income, EBITDA, and diluted EPS for a given ticker.
#'
#' @param ticker Character. Stock ticker (e.g. \code{"AAPL"}).
#' @param slug Character. Company-name URL slug, or \code{NULL}.
#' @param period Character. \code{"annual"} (default), \code{"quarterly"},
#'   or \code{"both"}.
#' @return A tibble with columns: \code{date}, \code{value}, \code{ticker},
#'   \code{metric}, \code{period}. Metrics included: \code{"revenue"},
#'   \code{"gross-profit"}, \code{"operating-income"}, \code{"net-income"},
#'   \code{"ebitda"}, \code{"eps-earnings-per-share-diluted"}.
#' @examples
#' mt_income("AAPL", "apple")
mt_income <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("revenue", "gross-profit", "operating-income",
               "net-income", "ebitda", "eps-earnings-per-share-diluted")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Fetch balance sheet metrics
#'
#' Convenience function that fetches total assets, total liabilities,
#' shareholder equity, long-term debt, cash on hand, and current ratio.
#'
#' @param ticker Character. Stock ticker (e.g. \code{"MSFT"}).
#' @param slug Character. Company-name URL slug, or \code{NULL}.
#' @param period Character. \code{"annual"} (default), \code{"quarterly"},
#'   or \code{"both"}.
#' @return A tibble with columns: \code{date}, \code{value}, \code{ticker},
#'   \code{metric}, \code{period}. Metrics: \code{"total-assets"},
#'   \code{"total-liabilities"}, \code{"total-shareholder-equity"},
#'   \code{"long-term-debt"}, \code{"cash-on-hand"}, \code{"current-ratio"}.
#' @examples
#' mt_balance("MSFT", "microsoft")
mt_balance <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("total-assets", "total-liabilities", "total-shareholder-equity",
               "long-term-debt", "cash-on-hand", "current-ratio")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Fetch cash flow metrics
#'
#' Convenience function that fetches free cash flow, operating cash
#' flow, and capital expenditures for a given ticker.
#'
#' @param ticker Character. Stock ticker (e.g. \code{"GOOG"}).
#' @param slug Character. Company-name URL slug, or \code{NULL}.
#' @param period Character. \code{"annual"} (default), \code{"quarterly"},
#'   or \code{"both"}.
#' @return A tibble with columns: \code{date}, \code{value}, \code{ticker},
#'   \code{metric}, \code{period}. Metrics: \code{"free-cash-flow"},
#'   \code{"operating-cash-flow"}, \code{"capital-expenditures"}.
#' @examples
#' mt_cashflow("GOOG", "alphabet")
mt_cashflow <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("free-cash-flow", "operating-cash-flow", "capital-expenditures")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Fetch profitability margin metrics
#'
#' Convenience function that fetches gross margin, operating margin,
#' net margin, and EBITDA margin percentages.
#'
#' @param ticker Character. Stock ticker (e.g. \code{"AAPL"}).
#' @param slug Character. Company-name URL slug, or \code{NULL}.
#' @param period Character. \code{"annual"} (default), \code{"quarterly"},
#'   or \code{"both"}.
#' @return A tibble with columns: \code{date}, \code{value}, \code{ticker},
#'   \code{metric}, \code{period}. Metrics: \code{"gross-profit-margin"},
#'   \code{"operating-profit-margin"}, \code{"net-profit-margin"},
#'   \code{"ebitda-margin"}. Values are percentages.
#' @examples
#' mt_margins("AAPL", "apple")
mt_margins <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("gross-profit-margin", "operating-profit-margin",
               "net-profit-margin", "ebitda-margin")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Fetch valuation ratio metrics
#'
#' Convenience function that fetches price-to-earnings, price-to-book,
#' price-to-FCF, and EV/EBITDA ratios.
#'
#' @param ticker Character. Stock ticker (e.g. \code{"AMZN"}).
#' @param slug Character. Company-name URL slug, or \code{NULL}.
#' @param period Character. \code{"annual"} (default) or
#'   \code{"quarterly"}.
#' @return A tibble with columns: \code{date}, \code{value}, \code{ticker},
#'   \code{metric}, \code{period}. Metrics: \code{"pe-ratio"},
#'   \code{"price-book"}, \code{"price-fcf"}, \code{"ev-ebitda"}.
#' @examples
#' mt_valuation("AMZN", "amazon")
mt_valuation <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("pe-ratio", "price-book", "price-fcf", "ev-ebitda")
  mt_get_bulk(ticker, slug, metrics, period)
}


# == Context ===================================================================

#' Get macrotrends.net client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mt_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mt_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/macrotrends.net.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "macrotrends.net")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# macrotrends.net context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# macrotrends.net", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
