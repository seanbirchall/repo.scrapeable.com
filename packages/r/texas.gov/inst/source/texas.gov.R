# data.texas.gov.R - Self-contained data.texas.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# data-texas-gov.R
# Self-contained Texas Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at data.texas.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tx_base <- "https://data.texas.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), rows_updated_at = as.POSIXct(character())
)

.schema_records <- tibble()

# == Datasets ==================================================================

#' List Texas Open Data datasets
#'
#' Returns metadata for datasets on the Texas Open Data portal
#' (\url{https://data.texas.gov}), powered by Socrata. Results can be
#' filtered by keyword to find specific datasets. Use the returned
#' \code{id} with \code{\link{txgov_query}} or \code{\link{txgov_fetch_all}}
#' to download actual records.
#'
#' @param limit Integer. Number of datasets to return (default 50).
#' @param query Character or \code{NULL}. Optional keyword to filter
#'   dataset names (case-insensitive grep). Applied client-side after
#'   fetching. Default \code{NULL} returns all.
#' @return A tibble with 5 columns:
#' \describe{
#'   \item{id}{Character. Socrata dataset identifier (e.g. "pmrv-k2ei").
#'     Use this with \code{\link{txgov_query}}.}
#'   \item{name}{Character. Dataset title.}
#'   \item{category}{Character. Dataset category, or \code{NA} if uncategorized.}
#'   \item{description}{Character. Truncated description (max 200 chars).}
#'   \item{rows_updated_at}{POSIXct. When the dataset rows were last updated.}
#' }
#' @examples
#' \dontrun{
#' txgov_datasets(10)
#' txgov_datasets(50, query = "vehicle")
#' }
#' @seealso \code{\link{txgov_query}} to query a specific dataset.
#' @export
txgov_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .tx_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Texas Open Data API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id               = as.character(raw$id),
    name             = as.character(raw$name),
    category         = as.character(raw$category %||% NA_character_),
    description      = ifelse(nchar(raw$description %||% "") > 200,
                              paste0(substr(raw$description, 1, 200), "..."),
                              as.character(raw$description %||% "")),
    rows_updated_at  = as.POSIXct(as.numeric(raw$rowsUpdatedAt), origin = "1970-01-01")
  )

  if (!is.null(query)) {
    result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  }
  result
}


#' Fetch records from a Texas Open Data dataset
#'
#' Queries a specific dataset on the Texas Open Data portal using the
#' Socrata Open Data API (SODA). Supports SoQL filtering, field selection,
#' grouping, ordering, and pagination. For full SoQL syntax, see
#' \url{https://dev.socrata.com/docs/queries/}.
#'
#' @param dataset_id Character. Socrata dataset identifier in the form
#'   \code{"xxxx-xxxx"} (e.g. \code{"pmrv-k2ei"}). Find IDs using
#'   \code{\link{txgov_datasets}}.
#' @param where Character or \code{NULL}. SoQL WHERE clause for filtering
#'   (e.g. \code{"county = 'TRAVIS'"}, \code{"date > '2024-01-01'"}).
#' @param select Character or \code{NULL}. Comma-separated field names to
#'   return (e.g. \code{"county, total_count"}). \code{NULL} returns all.
#' @param group Character or \code{NULL}. SoQL GROUP BY clause for
#'   aggregation (e.g. \code{"county"}).
#' @param order Character or \code{NULL}. SoQL ORDER BY clause
#'   (e.g. \code{"date DESC"}).
#' @param limit Integer. Maximum rows to return per request (default 1000).
#' @param offset Integer. Pagination offset (default 0). Use with
#'   \code{limit} for manual pagination, or use \code{\link{txgov_fetch_all}}.
#' @return A tibble with dataset-specific columns. Returns an empty tibble
#'   if no records match or the dataset is not found.
#' @examples
#' \dontrun{
#' txgov_query("pmrv-k2ei", limit = 10)
#' txgov_query("pmrv-k2ei", where = "county = 'TRAVIS'", limit = 50)
#' }
#' @seealso \code{\link{txgov_fetch_all}} for auto-paginated downloads.
#' @export
txgov_query <- function(dataset_id, where = NULL, select = NULL,
                        group = NULL, order = NULL,
                        limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .tx_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Texas Open Data query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)

  as_tibble(raw)
}


#' Fetch all records with auto-pagination
#'
#' Downloads all matching records from a Texas Open Data dataset by
#' automatically paginating through the Socrata API. Stops when all
#' records are retrieved, \code{max_rows} is reached, or a page returns
#' fewer results than \code{page_size}.
#'
#' @param dataset_id Character. Socrata dataset identifier
#'   (e.g. \code{"pmrv-k2ei"}).
#' @param where Character or \code{NULL}. Optional SoQL WHERE clause
#'   for filtering.
#' @param max_rows Integer. Maximum total rows to retrieve (default 10000).
#'   Acts as a safety limit.
#' @param page_size Integer. Rows per API request (default 1000).
#' @return A tibble with all matching records, with dataset-specific columns.
#' @examples
#' \dontrun{
#' txgov_fetch_all("pmrv-k2ei", max_rows = 500)
#' txgov_fetch_all("pmrv-k2ei", where = "county = 'HARRIS'")
#' }
#' @seealso \code{\link{txgov_query}} for single-page queries with full
#'   SoQL support.
#' @export
txgov_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                            page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- txgov_query(dataset_id, where = where,
                         limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Context ===================================================================

#' Get texas.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
txgov_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(txgov_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/texas.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "texas.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# texas.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# texas.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
