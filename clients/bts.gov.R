# bts.gov.R - Self-contained bts.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# bts-gov.R
# Self-contained Bureau of Transportation Statistics client (Socrata SODA).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at datahub.transportation.gov


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bts_base <- "https://datahub.transportation.gov"

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
  description = character()
)

.schema_records <- tibble()

# == Functions =================================================================

#' List BTS datasets from the Transportation Data Hub
#'
#' Queries the Socrata API at datahub.transportation.gov to list available
#' datasets published by the Bureau of Transportation Statistics.
#'
#' @param limit Integer. Maximum number of datasets to return (default 50).
#' @param query Character. Optional keyword filter applied to dataset names
#'   (case-insensitive grep). \code{NULL} returns all datasets up to \code{limit}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata dataset ID (e.g. \code{"tnkn-dmxi"}).
#'       Pass to \code{\link{bts_query}} or \code{\link{bts_fetch_all}}.}
#'     \item{name}{Character. Dataset title.}
#'     \item{category}{Character. Category (e.g. \code{"Roadways and Bridges"},
#'       \code{"Aviation"}).}
#'     \item{description}{Character. Brief description (truncated to 200 chars).}
#'   }
#' @examples
#' bts_datasets(limit = 10)
#' bts_datasets(query = "airline")
#' @export
bts_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .bts_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BTS API error: ", e$message); NULL
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id          = as.character(raw$id),
    name        = as.character(raw$name),
    category    = as.character(raw$category %||% NA_character_),
    description = ifelse(nchar(raw$description %||% "") > 200,
                         paste0(substr(raw$description, 1, 200), "..."),
                         as.character(raw$description %||% ""))
  )
  if (!is.null(query)) result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  result
}

#' Query a BTS dataset using SoQL
#'
#' Executes a Socrata Query Language (SoQL) query against a specific
#' BTS dataset. Supports filtering, column selection, grouping, and
#' ordering. Get dataset IDs from \code{\link{bts_datasets}}.
#'
#' @param dataset_id Character. Socrata dataset identifier (e.g.
#'   \code{"tnkn-dmxi"}). Obtain from \code{bts_datasets()$id}.
#' @param where Character. SoQL WHERE clause for filtering rows (e.g.
#'   \code{"year = '2023'"}). \code{NULL} for no filter.
#' @param select Character. SoQL SELECT clause for column selection (e.g.
#'   \code{"year, state, count(*)"}). \code{NULL} selects all columns.
#' @param group Character. SoQL GROUP BY clause. \code{NULL} for no grouping.
#' @param order Character. SoQL ORDER BY clause. \code{NULL} for default order.
#' @param limit Integer. Maximum rows to return (default 1000).
#' @param offset Integer. Number of rows to skip for pagination (default 0).
#' @return A tibble whose columns depend on the dataset. All values returned
#'   as character by default from the Socrata JSON API.
#' @examples
#' bts_query("tnkn-dmxi", limit = 5)
#' @export
bts_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL,
                      limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .bts_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BTS query error: ", e$message); NULL
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

#' Fetch all records from a BTS dataset with auto-pagination
#'
#' Repeatedly calls \code{\link{bts_query}} with increasing offsets until
#' all records are retrieved or \code{max_rows} is reached. Useful for
#' downloading complete datasets that exceed the single-request limit.
#'
#' @param dataset_id Character. Socrata dataset identifier.
#' @param where Character. Optional SoQL WHERE clause for filtering.
#' @param max_rows Integer. Maximum total rows to retrieve (default 10000).
#'   Acts as a safety cap to prevent accidental huge downloads.
#' @param page_size Integer. Rows per individual API request (default 1000).
#' @return A tibble with the same columns as \code{\link{bts_query}} for
#'   the given dataset, with up to \code{max_rows} rows.
#' @examples
#' bts_fetch_all("tnkn-dmxi", max_rows = 100)
#' @export
bts_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                          page_size = 1000) {
  results <- list(); offset <- 0
  while (offset < max_rows) {
    batch <- bts_query(dataset_id, where = where, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}

# == Context ===================================================================

#' Get bts.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bts_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bts_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bts.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bts.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bts.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bts.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
