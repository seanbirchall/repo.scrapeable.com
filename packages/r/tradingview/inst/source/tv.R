# tradingview.R
# TradingView symbol search client.
# All functions return data.frames. No database dependencies.

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

# == Symbol search =============================================================

#' Search TradingView for a symbol
#'
#' @param query Search text (ticker, company name, etc.)
#' @return data.frame with columns: symbol, description, type, exchange,
#'   currency_code, provider_id, country
tv_search <- function(query) {
  url <- paste0("https://symbol-search.tradingview.com/symbol_search/v3/?text=",
                utils::URLencode(query))
  tmp <- http_get(url, ext = ".json")
  raw <- jsonlite::fromJSON(tmp)
  symbols <- raw$symbols
  if (is.null(symbols) || length(symbols) == 0) return(data.frame())

  # Keep only scalar columns (drop nested lists)
  keep <- vapply(symbols, function(col) !is.list(col), logical(1))
  df <- symbols[, keep, drop = FALSE]
  as.data.frame(df)
}

#' Search TradingView and return best match for exact symbol
#'
#' @param symbol Exact ticker symbol to match
#' @return data.frame with one row (best match) or empty data.frame
tv_match <- function(symbol) {
  results <- tv_search(symbol)
  if (nrow(results) == 0) return(data.frame())

  # Filter to exact symbol match
  if ("symbol" %in% names(results)) {
    exact <- results[toupper(results$symbol) == toupper(symbol), ]
    if (nrow(exact) > 0) return(exact[1, , drop = FALSE])
  }

  # Fall back to first result
  results[1, , drop = FALSE]
}
