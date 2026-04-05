# efts.sec.gov.R - Self-contained efts.sec.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)


# efts-sec-gov.R
# Self-contained SEC EDGAR Full-Text Search (EFTS) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: respect SEC fair-use (10 req/sec max)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.efts_base <- "https://efts.sec.gov/LATEST/search-index"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  file_date = as.Date(character()), form = character(),
  display_names = character(), file_number = character(),
  cik = character(), period_ending = character(),
  file_description = character(), adsh = character()
)


# == Search ====================================================================

#' Search SEC EDGAR full-text search index
#'
#' Searches the EDGAR Full-Text Search System (EFTS) for SEC filings whose
#' content matches a query string. The EFTS indexes the full text of filings,
#' not just metadata, making it possible to find filings that mention specific
#' topics. Supports optional date-range and form-type filters. Please observe
#' SEC fair-use limits (max 10 requests per second).
#'
#' @param query Character. Search query string (e.g. \code{"artificial intelligence"},
#'   \code{"13F"}, \code{"climate risk"}). Wrapped in double quotes internally
#'   for phrase matching.
#' @param forms Character or \code{NULL}. Form type filter (e.g. \code{"10-K"},
#'   \code{"10-Q"}, \code{"8-K"}). Can be a single form or comma-separated
#'   list (e.g. \code{"10-K,10-Q"}).
#' @param start_date Date, character (\code{"YYYY-MM-DD"}), or \code{NULL}.
#'   Start of the date range filter. Must be used with \code{end_date}.
#' @param end_date Date, character (\code{"YYYY-MM-DD"}), or \code{NULL}.
#'   End of the date range filter. Must be used with \code{start_date}.
#' @param limit Integer. Maximum results to return (default 40, max 100).
#' @return A tibble with one row per filing and columns:
#' \describe{
#'   \item{file_date}{Date. Filing date.}
#'   \item{form}{Character. SEC form type (e.g. \code{"10-K"}, \code{"8-K"}).}
#'   \item{display_names}{Character. Filer names and ticker, semicolon-separated.}
#'   \item{file_number}{Character. SEC file number.}
#'   \item{cik}{Character. Central Index Key(s), semicolon-separated.}
#'   \item{period_ending}{Character. Reporting period end date, or \code{NA}.}
#'   \item{file_description}{Character. Filing description, or \code{NA}.}
#'   \item{adsh}{Character. Accession number (ADSH) for the filing, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' secft_search("artificial intelligence")
#' secft_search("climate risk", forms = "10-K")
#' secft_search("merger", start_date = "2024-01-01", end_date = "2024-12-31")
#' }
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

#' Search SEC EDGAR filings by company name
#'
#' Convenience wrapper around \code{\link{secft_search}} that uses a company
#' name as the query. Note that the EFTS searches full text, so results may
#' include filings that merely mention the company name rather than filings
#' by that company.
#'
#' @param company Character. Company name (e.g. \code{"Apple"}, \code{"Tesla"}).
#' @param forms Character or \code{NULL}. Optional form type filter
#'   (e.g. \code{"10-K"}, \code{"10-Q"}).
#' @param limit Integer. Maximum results (default 20).
#' @return A tibble with the same columns as \code{\link{secft_search}}.
#' @examples
#' \dontrun{
#' secft_company("Apple", forms = "10-K")
#' secft_company("Tesla", limit = 10)
#' }
#' @export
secft_company <- function(company, forms = NULL, limit = 20) {
  secft_search(company, forms = forms, limit = limit)
}

#' Get most recent SEC filings
#'
#' Retrieves the most recent SEC filings of a specific form type by
#' searching the EFTS with a wildcard query and a date-range filter
#' spanning the last \code{days} days.
#'
#' @param forms Character. Form type to filter on (e.g. \code{"10-K"},
#'   \code{"10-Q"}, \code{"8-K"}). Default \code{"10-K"}.
#' @param days Integer. Number of days to look back from today (default 30).
#' @param limit Integer. Maximum results to return (default 20).
#' @return A tibble with the same columns as \code{\link{secft_search}}.
#' @examples
#' \dontrun{
#' secft_recent()
#' secft_recent(forms = "8-K", days = 7, limit = 50)
#' }
#' @export
secft_recent <- function(forms = "10-K", days = 30, limit = 20) {
  end_date <- Sys.Date()
  start_date <- end_date - days
  secft_search("*", forms = forms, start_date = start_date,
               end_date = end_date, limit = limit)
}

# == Context ===================================================================

#' Get efts.sec.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
efts_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(efts_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/efts.sec.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "efts.sec.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# efts.sec.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# efts.sec.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
