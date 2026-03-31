
# == Search ====================================================================

#' Search SEC EDGAR full-text search index
#'
#' Searches the EDGAR full-text search system (EFTS) for filings matching
#' a query string. Supports date ranges and form type filters.
#'
#' @param query Search query string (e.g. "artificial intelligence", "13F")
#' @param forms Optional form type filter (e.g. "10-K", "10-Q", "8-K").
#'   Can be a single string or comma-separated list.
#' @param start_date Optional start date for filtering (Date or "YYYY-MM-DD")
#' @param end_date Optional end date for filtering (Date or "YYYY-MM-DD")
#' @param limit Maximum number of results to return (default 40, max 100)
#' @return tibble: file_date, form, display_names, file_number, cik,
#'   period_ending, file_description, adsh
#' @export
secft_search <- function(query, forms = NULL, start_date = NULL,
                         end_date = NULL, limit = 40) {
  params <- list(q = paste0('"', query, '"'))
  if (!is.null(forms)) params$forms <- forms
  if (!is.null(start_date) && !is.null(end_date)) {
    params$dateRange <- "custom"
    params$startdt <- as.character(start_date)
    params$enddt <- as.character(end_date)
  }

  query_str <- paste(names(params), vapply(params, utils::URLencode, character(1), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- paste0(.efts_base, "?", query_str)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("EFTS search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_search)

  hits <- raw$hits$hits
  if (is.null(hits) || length(hits) == 0) return(.schema_search)

  src <- hits$`_source`
  if (is.null(src)) return(.schema_search)

  result <- as_tibble(src)
  n_rows <- min(nrow(result), limit)
  result <- result[seq_len(n_rows), ]

  # Map display_names list column to character
  dnames <- if ("display_names" %in% names(result)) {
    vapply(result$display_names, function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
    }, character(1))
  } else rep(NA_character_, n_rows)

  tibble(
    file_date        = tryCatch(as.Date(result$file_date %||% NA_character_), error = function(e) as.Date(rep(NA, n_rows))),
    form             = as.character(result$form %||% NA_character_),
    display_names    = dnames,
    file_number      = as.character(result$file_num %||% NA_character_),
    cik              = vapply(result$ciks, function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
    }, character(1)),
    period_ending    = as.character(result$period_ending %||% NA_character_),
    file_description = as.character(result$file_description %||% NA_character_),
    adsh             = as.character(result$adsh %||% NA_character_)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the efts.sec.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
secft_context <- function() {
  .build_context("efts.sec.gov", header_lines = c(
    "# efts.sec.gov - SEC EDGAR Full-Text Search Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API with User-Agent header)",
    "# Rate limit: 10 requests/second (SEC fair use)",
    "# All functions return tibbles with typed columns."
  ))
}
