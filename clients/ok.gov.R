# ok.gov.R - State of Oklahoma Open Data (CKAN API)
# Self-contained client for data.ok.gov
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Base: https://data.ok.gov/api/3

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ok_base <- "https://data.ok.gov/api/3"

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

.schema_packages <- tibble(
  name = character(), title = character(), notes = character(),
  organization = character(), num_resources = integer(),
  metadata_modified = character()
)

# == Dataset discovery =========================================================

#' Search Oklahoma open datasets
#'
#' Searches the State of Oklahoma open data catalog (data.ok.gov) by keyword
#' using the CKAN package_search API. Returns dataset metadata including title,
#' description, owning organization, and number of associated resources (files).
#'
#' @param q Character. Search query string. When \code{NULL}, returns all
#'   datasets sorted by relevance.
#' @param rows Integer. Maximum results to return (default 20, max 1000).
#' @param start Integer. Pagination offset for paging through results
#'   (default 0).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{name}{Character. URL-safe dataset slug (use with \code{\link{ok_package}}).}
#'   \item{title}{Character. Human-readable dataset title.}
#'   \item{notes}{Character. Dataset description (truncated to 300 chars).}
#'   \item{organization}{Character. Publishing organization name.}
#'   \item{num_resources}{Integer. Number of downloadable resources (files).}
#'   \item{metadata_modified}{Character. ISO 8601 timestamp of last metadata update.}
#' }
#'
#' @examples
#' \dontrun{
#' ok_search("education")
#' ok_search("health", rows = 50)
#' }
#'
#' @seealso \code{\link{ok_package}} to get resources for a dataset,
#'   \code{\link{ok_list}} for all dataset slugs.
#' @export
ok_search <- function(q = NULL, rows = 20, start = 0) {
  url <- sprintf("%s/action/package_search?rows=%d&start=%d",
                 .ok_base, as.integer(rows), as.integer(start))
  if (!is.null(q)) url <- paste0(url, "&q=", utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(.schema_packages)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0) return(.schema_packages)
  tibble(
    name              = as.character(results$name %||% NA),
    title             = as.character(results$title %||% NA),
    notes             = substr(as.character(results$notes %||% ""), 1, 300),
    organization      = as.character(results$organization$title %||% NA),
    num_resources     = as.integer(results$num_resources %||% NA),
    metadata_modified = as.character(results$metadata_modified %||% NA)
  )
}

#' List all Oklahoma open dataset names
#'
#' Returns a character vector of all dataset slugs on data.ok.gov. These
#' slugs can be passed to \code{\link{ok_package}} to retrieve resource
#' details and download URLs.
#'
#' @param limit Integer. Maximum number of dataset names to return (default 500).
#'
#' @return Character vector of dataset slug identifiers.
#'
#' @examples
#' \dontrun{
#' slugs <- ok_list()
#' length(slugs)
#' head(slugs)
#' }
#'
#' @seealso \code{\link{ok_search}} for keyword search with metadata,
#'   \code{\link{ok_package}} for resource details.
#' @export
ok_list <- function(limit = 500) {
  url <- sprintf("%s/action/package_list?limit=%d", .ok_base, as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(character())
  as.character(raw$result)
}

#' Get resources for an Oklahoma dataset
#'
#' Retrieves the list of downloadable resources (files) associated with an
#' Oklahoma open data package. Returns resource IDs, names, formats, URLs,
#' and descriptions. Use the \code{id} with \code{\link{ok_data}} to fetch
#' records from CSV/datastore resources.
#'
#' @param name Character. Dataset slug as returned by \code{\link{ok_list}}
#'   or \code{\link{ok_search}} (e.g. \code{"purchase-card-pcard-fiscal-year-2026"}).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{id}{Character. Resource UUID (use with \code{\link{ok_data}}).}
#'   \item{name}{Character. Resource file name.}
#'   \item{format}{Character. File format (CSV, XLS, JSON, etc.).}
#'   \item{url}{Character. Direct download URL.}
#'   \item{description}{Character. Resource description.}
#' }
#'
#' @examples
#' \dontrun{
#' ok_package("purchase-card-pcard-fiscal-year-2026")
#' }
#'
#' @seealso \code{\link{ok_data}} to query records from a resource.
#' @export
ok_package <- function(name) {
  url <- sprintf("%s/action/package_show?id=%s", .ok_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  resources <- raw$result$resources
  if (is.null(resources) || !is.data.frame(resources)) return(tibble())
  tibble(
    id          = as.character(resources$id %||% NA),
    name        = as.character(resources$name %||% NA),
    format      = as.character(resources$format %||% NA),
    url         = as.character(resources$url %||% NA),
    description = as.character(resources$description %||% NA)
  )
}

# == Data access ===============================================================

#' Query Oklahoma datastore records
#'
#' Fetches actual data rows from an Oklahoma open data resource using the
#' CKAN DataStore API. Supports full-text search, field-level filters, sorting,
#' and pagination. The \code{resource_id} is the UUID obtained from
#' \code{\link{ok_package}}.
#'
#' @param resource_id Character. Resource UUID from \code{\link{ok_package}}
#'   (e.g. \code{"285c4942-0452-4d6f-afe2-28c511953087"}).
#' @param q Character. Optional full-text search query across all fields.
#' @param filters Named list. Optional field-level exact-match filters
#'   (e.g. \code{list(AGENCY_NAME = "HEALTH")}).
#' @param limit Integer. Maximum records to return (default 100, max 32000).
#' @param offset Integer. Pagination offset (default 0).
#' @param sort Character. Sort specification as \code{"field_name asc/desc"}
#'   (e.g. \code{"CALENDAR_YEAR desc"}).
#'
#' @return A tibble of data records. Columns and types depend on the resource
#'   schema. The internal \code{_id} column is automatically removed.
#'
#' @examples
#' \dontrun{
#' # Get a resource ID first
#' resources <- ok_package("purchase-card-pcard-fiscal-year-2026")
#' ok_data(resources$id[1], limit = 10)
#' }
#'
#' @seealso \code{\link{ok_package}} for finding resource IDs,
#'   \code{\link{ok_sql}} for SQL-based queries.
#' @export
ok_data <- function(resource_id, q = NULL, filters = NULL,
                    limit = 100, offset = 0, sort = NULL) {
  url <- sprintf("%s/action/datastore_search?resource_id=%s&limit=%d&offset=%d",
                 .ok_base, resource_id, as.integer(limit), as.integer(offset))
  if (!is.null(q)) url <- paste0(url, "&q=", utils::URLencode(q))
  if (!is.null(filters)) {
    fj <- jsonlite::toJSON(filters, auto_unbox = TRUE)
    url <- paste0(url, "&filters=", utils::URLencode(as.character(fj)))
  }
  if (!is.null(sort)) url <- paste0(url, "&sort=", utils::URLencode(sort))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || !is.data.frame(records) || nrow(records) == 0) return(tibble())
  as_tibble(records) |> select(-any_of("_id"))
}

#' Run a SQL query against Oklahoma datastore
#'
#' Executes a raw SQL query against the Oklahoma open data DataStore. The
#' query should reference resource tables by their UUID. This provides
#' maximum flexibility for complex joins, aggregations, and filters.
#'
#' @param sql Character. SQL query string referencing resource table UUIDs
#'   (e.g. \code{'SELECT * FROM "285c4942-..." LIMIT 10'}).
#'
#' @return A tibble of query results.
#'
#' @examples
#' \dontrun{
#' ok_sql('SELECT "AGENCY_NAME", COUNT(*) as n FROM "285c4942-..." GROUP BY "AGENCY_NAME"')
#' }
#'
#' @seealso \code{\link{ok_data}} for simpler record-level queries.
#' @export
ok_sql <- function(sql) {
  url <- paste0(.ok_base, "/action/datastore_search_sql?sql=", utils::URLencode(sql))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || !is.data.frame(records) || nrow(records) == 0) return(tibble())
  as_tibble(records)
}

# == Context ===================================================================

#' Get ok.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ok_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ok_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ok.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ok.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ok.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ok.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
