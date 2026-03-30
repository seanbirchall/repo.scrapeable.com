# macrotrends-net.R
# Self-contained Macrotrends financial data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tibble
# Auth: none required (HTML scraping)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: fundamental financial data for US public companies

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.mt_base <- "https://www.macrotrends.net/stocks/charts"

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
#' Scrapes the Macrotrends page for any available metric. Returns both
#' annual and quarterly data.
#'
#' @param ticker Stock ticker (e.g. "AAPL", "MSFT")
#' @param slug Company name slug (e.g. "apple", "microsoft"). If NULL,
#'   auto-resolved via a redirect.
#' @param metric Metric URL path. See mt_metrics() for available metrics.
#' @param period "annual" (default), "quarterly", or "both"
#' @return tibble: date, value, ticker, metric, period
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
#' @param ticker Stock ticker
#' @param slug Company name slug (NULL = auto)
#' @param metrics Character vector of metric names
#' @param period "annual", "quarterly", or "both"
#' @param sleep Seconds between requests (default 0.5)
#' @return tibble: date, value, ticker, metric, period (stacked)
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
#' Returns all known metric URL slugs with descriptions.
#'
#' @return tibble: metric (URL slug), category, description
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

#' Income statement metrics (revenue, net income, EPS, etc.)
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked income statement metrics
mt_income <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("revenue", "gross-profit", "operating-income",
               "net-income", "ebitda", "eps-earnings-per-share-diluted")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Balance sheet metrics
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked balance sheet metrics
mt_balance <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("total-assets", "total-liabilities", "total-shareholder-equity",
               "long-term-debt", "cash-on-hand", "current-ratio")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Cash flow metrics
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked cash flow metrics
mt_cashflow <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("free-cash-flow", "operating-cash-flow", "capital-expenditures")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Profitability margins
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual", "quarterly", or "both"
#' @return tibble: stacked margin metrics
mt_margins <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("gross-profit-margin", "operating-profit-margin",
               "net-profit-margin", "ebitda-margin")
  mt_get_bulk(ticker, slug, metrics, period)
}

#' Valuation ratios (PE, PB, EV/EBITDA)
#'
#' @param ticker Stock ticker
#' @param slug Company name slug
#' @param period "annual" or "quarterly"
#' @return tibble: stacked valuation metrics
mt_valuation <- function(ticker, slug = NULL, period = "annual") {
  metrics <- c("pe-ratio", "price-book", "price-fcf", "ev-ebitda")
  mt_get_bulk(ticker, slug, metrics, period)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the macrotrends.net package
#'
#' @return Character string (invisibly), also printed
mt_context <- function() {
  .build_context("macrotrends.net", header_lines = c(
    "# macrotrends.net - Financial Fundamental Data Client for R",
    "# Dependencies: httr2, xml2, dplyr, tibble",
    "# Auth: none (HTML scraping)",
    "# All functions return tibbles: date, value, ticker, metric, period",
    "#",
    "# Metrics available via mt_metrics():",
    "#   Income: revenue, net-income, ebitda, eps-earnings-per-share-diluted",
    "#   Balance: total-assets, long-term-debt, cash-on-hand",
    "#   Cash flow: free-cash-flow, operating-cash-flow",
    "#   Margins: gross-profit-margin, net-profit-margin",
    "#   Returns: roe, roa, roic",
    "#   Valuation: pe-ratio, price-book, ev-ebitda",
    "#",
    "# Note: slug param can usually be omitted (defaults to lowercase ticker)"
  ))
}
