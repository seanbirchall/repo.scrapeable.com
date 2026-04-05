# tradingview.R
# TradingView symbol search client.
# All functions return data.frames. No database dependencies.

# == Setup =====================================================================


# == Symbol search =============================================================

#' Search TradingView for a symbol
#'
#' Queries the TradingView symbol search API for matching tickers, company
#' names, or other financial instruments. Returns metadata for each match
#' including exchange, type, and country of listing. No authentication
#' required.
#'
#' @param query Character. Search text -- can be a ticker symbol, company
#'   name, or partial match (e.g. \code{"AAPL"}, \code{"Apple"},
#'   \code{"bitcoin"}).
#' @return A data.frame with columns: \code{symbol} (character),
#'   \code{description} (character), \code{type} (character, e.g. "stock",
#'   "crypto"), \code{exchange} (character), \code{currency_code}
#'   (character), \code{provider_id} (character), \code{country}
#'   (character). Additional scalar columns may be present depending on the
#'   API response.
#' @examples
#' tv_search("AAPL")
#' tv_search("bitcoin")
#' @export

# -- HTTP helpers (inlined) ----------------------------------------------------
http_get <- function(url, ext = ".html", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

http_post <- function(url, body, ext = ".zip", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  tmp
}

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
#' Wrapper around \code{\link{tv_search}} that filters results to the
#' closest match for an exact ticker symbol. Returns a single-row
#' data.frame with the best match, or an empty data.frame if no match is
#' found.
#'
#' @param symbol Character. Exact ticker symbol to match
#'   (e.g. \code{"AAPL"}, \code{"TSLA"}).
#' @return A data.frame with one row and the same columns as
#'   \code{\link{tv_search}}: \code{symbol}, \code{description},
#'   \code{type}, \code{exchange}, \code{currency_code}, etc. Returns an
#'   empty data.frame if no match is found.
#' @examples
#' tv_match("AAPL")
#' @export
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
