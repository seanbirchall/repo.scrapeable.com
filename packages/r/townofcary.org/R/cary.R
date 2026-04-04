# townofcary.org.R - Town of Cary NC open data client (Opendatasoft v2 API)
#
# Data source: data.townofcary.org (Opendatasoft)
# Datasets: ~76 (police, developments, greenways, stream gages, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cary_base <- "https://data.townofcary.org/api/v2"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_datasets <- tibble(
  dataset_id = character(), title = character(), description = character(),
  theme = character(), records_count = integer(), modified = character()
)

.schema_records <- tibble()

# == Dataset discovery =========================================================

#' List datasets on Town of Cary open data portal
#'
#' Queries the Opendatasoft v2 catalog API for the Town of Cary, NC open
#' data portal (\url{https://data.townofcary.org}). Returns metadata for
#' available datasets including titles, descriptions, themes, and record
#' counts. The portal hosts approximately 76 datasets covering police
#' incidents, developments, greenways, stream gages, and other municipal data.
#'
#' @param limit Integer. Number of datasets to return (default 50).
#' @param q Character or \code{NULL}. Optional search query to filter
#'   datasets by keyword (e.g. \code{"police"}, \code{"greenway"}).
#' @return A tibble with 6 columns:
#' \describe{
#'   \item{dataset_id}{Character. Opendatasoft dataset identifier (e.g. "cpd-incidents").
#'     Use with \code{\link{cary_records}}, \code{\link{cary_metadata}}, or \code{\link{cary_export}}.}
#'   \item{title}{Character. Human-readable dataset title.}
#'   \item{description}{Character. Dataset description (may contain HTML).}
#'   \item{theme}{Character. Semicolon-separated theme tags.}
#'   \item{records_count}{Integer. Number of records in the dataset.}
#'   \item{modified}{Character. Last modification date.}
#' }
#' @examples
#' \dontrun{
#' cary_datasets()
#' cary_datasets(q = "police")
#' }
#' @export
cary_datasets <- function(limit = 50, q = NULL) {
  url <- sprintf("%s/catalog/datasets?limit=%d&offset=0", .cary_base, limit)
  if (!is.null(q)) url <- paste0(url, "&where=search(\"", utils::URLencode(q), "\")")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$datasets) || length(raw$datasets) == 0) return(.schema_datasets)
  ds <- raw$datasets
  metas <- ds$dataset$metas$default
  tibble(
    dataset_id    = as.character(ds$dataset$dataset_id),
    title         = as.character(metas$title %||% NA_character_),
    description   = as.character(metas$description %||% NA_character_),
    theme         = vapply(metas$theme %||% list(), function(x) paste(x, collapse = "; "), character(1)),
    records_count = as.integer(ds$dataset$has_records %||% NA),
    modified      = as.character(metas$modified %||% NA_character_)
  )
}

#' Get records from a Town of Cary dataset
#'
#' Fetches records from a specific dataset on the Town of Cary open data
#' portal using the Opendatasoft v2 records API. Supports filtering,
#' field selection, sorting, and pagination.
#'
#' @param dataset_id Character. Dataset identifier as returned by
#'   \code{\link{cary_datasets}} (e.g. \code{"cpd-incidents"}).
#' @param limit Integer. Number of records to return per request
#'   (default 100, API max 100).
#' @param offset Integer. Offset for pagination (default 0). Use with
#'   \code{limit} to page through large datasets.
#' @param where Character or \code{NULL}. Opendatasoft filter expression
#'   (e.g. \code{"crime_type = 'BURGLARY'"}, \code{"year = '2024'"}).
#' @param select Character or \code{NULL}. Comma-separated field names
#'   to return (e.g. \code{"crime_type, date_from, district"}).
#' @param order_by Character or \code{NULL}. Field and direction to sort
#'   by (e.g. \code{"date_from DESC"}).
#' @return A tibble with dataset-specific columns. Returns an empty tibble
#'   if no records match.
#' @examples
#' \dontrun{
#' cary_records("cpd-incidents", limit = 10)
#' cary_records("cpd-incidents", where = "year = '2024'", limit = 50)
#' }
#' @seealso \code{\link{cary_export}} to download an entire dataset as CSV.
#' @export
cary_records <- function(dataset_id, limit = 100, offset = 0,
                         where = NULL, select = NULL, order_by = NULL) {
  url <- sprintf("%s/catalog/datasets/%s/records?limit=%d&offset=%d",
                 .cary_base, dataset_id, limit, offset)
  if (!is.null(where))    url <- paste0(url, "&where=", utils::URLencode(where))
  if (!is.null(select))   url <- paste0(url, "&select=", utils::URLencode(select))
  if (!is.null(order_by)) url <- paste0(url, "&order_by=", utils::URLencode(order_by))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$records) || length(raw$records) == 0) return(tibble())
  recs <- raw$records$record$fields
  if (is.data.frame(recs)) return(as_tibble(recs))
  tibble()
}

#' Get metadata for a specific Town of Cary dataset
#'
#' Returns detailed metadata for a single dataset including its title,
#' description, themes, record count, and a comma-separated list of
#' field names. Useful for discovering available columns before querying
#' with \code{\link{cary_records}}.
#'
#' @param dataset_id Character. Dataset identifier
#'   (e.g. \code{"cpd-incidents"}).
#' @return A single-row tibble with 6 columns:
#' \describe{
#'   \item{dataset_id}{Character. Dataset identifier.}
#'   \item{title}{Character. Dataset title.}
#'   \item{description}{Character. Dataset description (may contain HTML).}
#'   \item{theme}{Character. Semicolon-separated theme tags.}
#'   \item{records_count}{Integer. Number of records.}
#'   \item{fields}{Character. Comma-separated list of field names in the dataset.}
#' }
#' @examples
#' \dontrun{
#' cary_metadata("cpd-incidents")
#' }
#' @export
cary_metadata <- function(dataset_id) {
  url <- sprintf("%s/catalog/datasets/%s", .cary_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$dataset)) return(tibble())
  ds <- raw$dataset
  m <- ds$metas$default
  fields_list <- ds$fields
  field_names <- if (!is.null(fields_list) && is.data.frame(fields_list))
    paste(fields_list$name, collapse = ", ") else NA_character_
  tibble(
    dataset_id    = as.character(ds$dataset_id),
    title         = as.character(m$title %||% NA_character_),
    description   = as.character(m$description %||% NA_character_),
    theme         = paste(m$theme %||% character(), collapse = "; "),
    records_count = as.integer(ds$has_records %||% NA),
    fields        = field_names
  )
}

#' Download a full Town of Cary dataset as CSV
#'
#' Exports an entire dataset from the Town of Cary open data portal as
#' CSV and returns it as a tibble. For large datasets, use the
#' \code{limit} parameter to cap the download size.
#'
#' @param dataset_id Character. Dataset identifier
#'   (e.g. \code{"cpd-incidents"}).
#' @param limit Integer. Maximum rows to download (default 10000).
#' @return A tibble with all fields from the dataset. Column names and
#'   types are dataset-specific.
#' @examples
#' \dontrun{
#' cary_export("cpd-incidents")
#' cary_export("cpd-incidents", limit = 500)
#' }
#' @seealso \code{\link{cary_records}} for filtered/paginated access.
#' @export
cary_export <- function(dataset_id, limit = 10000) {
  url <- sprintf("%s/catalog/datasets/%s/exports/csv?limit=%d&delimiter=,",
                 .cary_base, dataset_id, limit)
  tmp <- tryCatch(.fetch(url, ext = ".csv"), error = function(e) NULL)
  if (is.null(tmp)) return(tibble())
  tryCatch(as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE)),
           error = function(e) tibble())
}

# == Context ===================================================================

#' Get townofcary.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cary_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cary_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/townofcary.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "townofcary.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# townofcary.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# townofcary.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
