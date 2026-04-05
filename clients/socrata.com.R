# socrata.com.R
# Self-contained Socrata Open Data Discovery & Access client.
# Uses the Socrata Discovery API (api.us.socrata.com) for catalog search
# and the SODA 2.1 API for fetching actual data from any Socrata domain.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data. App tokens optional (increase rate limits).
# Rate limits: Without token ~1000/hr per IP. With token higher.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.socrata_disco <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.double(x)

# -- JSON fetch helper ---------------------------------------------------------

.socrata_fetch <- function(url, params = list(), simplify = TRUE) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = simplify, flatten = FALSE)
}

# -- Type date columns ---------------------------------------------------------

.type_dates <- function(df) {
  for (col in names(df)) {
    if (grepl("date|Date|updated|created|At$", col) && is.character(df[[col]])) {
      vals <- df[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike, na.rm = TRUE) > length(vals) * 0.5) {
        df[[col]] <- as.Date(substr(vals, 1, 10))
      }
    }
  }
  df
}


# == Discovery: Catalog Search ================================================

#' Search the Socrata open data catalog
#'
#' Searches across all Socrata-powered open data portals worldwide using
#' the Socrata Discovery API. Returns dataset metadata including IDs needed
#' for \code{\link{socrata_data}}. Over 200 government portals use Socrata
#' including CDC, NYC, Chicago, and many state/local agencies.
#'
#' @param query Character or NULL. Full-text search term across dataset name,
#'   description, and column names (e.g. \code{"covid deaths"},
#'   \code{"building permits"}).
#' @param domain Character or NULL. Filter to a specific portal domain (e.g.
#'   \code{"data.cdc.gov"}, \code{"data.cityofnewyork.us"},
#'   \code{"data.cityofchicago.org"}).
#' @param category Character or NULL. Filter by domain-specific category.
#' @param tags Character vector or NULL. Tags to filter by (combined with AND).
#' @param only Character or NULL. Resource type filter: \code{"datasets"},
#'   \code{"maps"}, \code{"calendars"}, \code{"links"}, \code{"filters"},
#'   \code{"files"}.
#' @param order Character. Sort order: \code{"relevance"} (default),
#'   \code{"page_views_total"}, \code{"updatedAt"}.
#' @param limit Integer. Maximum results per page (default 50, max 10000).
#' @param offset Integer. Starting offset for pagination (default 0).
#' @return A tibble with one row per dataset:
#' \describe{
#'   \item{id}{Character. Socrata four-by-four dataset ID.}
#'   \item{name}{Character. Dataset title.}
#'   \item{description}{Character. Description (truncated to 300 chars).}
#'   \item{type}{Character. Resource type (dataset, map, etc.).}
#'   \item{domain}{Character. Portal domain hosting the dataset.}
#'   \item{permalink}{Character. Direct URL to the dataset.}
#'   \item{updatedAt}{Date. Date of last data update.}
#'   \item{createdAt}{Date. Date the dataset was created.}
#'   \item{download_count}{Integer. Total downloads.}
#'   \item{page_views_total}{Integer. Total page views.}
#'   \item{columns}{Character. Semicolon-separated column names.}
#'   \item{domain_category}{Character. Portal-specific category.}
#' }
#' @export
#' @examples
#' \dontrun{
#' socrata_search("covid", domain = "data.cdc.gov", limit = 10)
#' socrata_search("budget", domain = "data.cityofchicago.org")
#' }
socrata_search <- function(query = NULL, domain = NULL, category = NULL,
                           tags = NULL, only = "datasets", order = "relevance",
                           limit = 50, offset = 0) {
  params <- list(limit = limit, offset = offset)
  if (!is.null(query))    params$q <- query
  if (!is.null(domain))   params$domains <- domain
  if (!is.null(category)) params$categories <- category
  if (!is.null(only))     params$only <- only
  if (!is.null(order))    params$order <- order
  if (!is.null(tags))     params$tags <- paste(tags, collapse = ",")

  raw <- .socrata_fetch(.socrata_disco, params, simplify = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  .ex <- function(item, ...) {
    keys <- list(...)
    val <- item
    for (k in keys) val <- val[[k]]
    if (is.null(val)) NA_character_ else as.character(val)
  }

  tibble(
    id               = vapply(results, function(r) .ex(r, "resource", "id"), character(1)),
    name             = vapply(results, function(r) .ex(r, "resource", "name"), character(1)),
    description      = vapply(results, function(r) substr(.ex(r, "resource", "description"), 1, 300), character(1)),
    type             = vapply(results, function(r) .ex(r, "resource", "type"), character(1)),
    domain           = vapply(results, function(r) .ex(r, "metadata", "domain"), character(1)),
    permalink        = vapply(results, function(r) .ex(r, "permalink"), character(1)),
    updatedAt        = as.Date(vapply(results, function(r) substr(.ex(r, "resource", "updatedAt"), 1, 10), character(1))),
    createdAt        = as.Date(vapply(results, function(r) substr(.ex(r, "resource", "createdAt"), 1, 10), character(1))),
    download_count   = vapply(results, function(r) {
      v <- r$resource$download_count; if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    page_views_total = vapply(results, function(r) {
      v <- r$resource$page_views$page_views_total; if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    columns          = vapply(results, function(r) {
      cn <- r$resource$columns_name
      if (is.null(cn)) NA_character_ else paste(cn, collapse = "; ")
    }, character(1)),
    domain_category  = vapply(results, function(r) .ex(r, "classification", "domain_category"), character(1))
  )
}


#' List datasets on a specific Socrata portal
#'
#' Convenience wrapper around \code{\link{socrata_search}} for browsing a
#' single portal domain. Returns datasets sorted by popularity (page views)
#' by default.
#'
#' @param domain Character. Portal domain (e.g. \code{"data.cdc.gov"},
#'   \code{"data.cityofnewyork.us"}).
#' @param order Character. Sort order: \code{"page_views_total"} (default),
#'   \code{"updatedAt"}, \code{"relevance"}.
#' @param limit Integer. Maximum results (default 50).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with the same columns as \code{\link{socrata_search}}.
#' @export
#' @examples
#' \dontrun{
#' socrata_list("data.cdc.gov", limit = 10)
#' }
socrata_list <- function(domain, order = "page_views_total", limit = 50, offset = 0) {
  socrata_search(domain = domain, order = order, limit = limit, offset = offset)
}


# == Discovery: Dataset Metadata ==============================================

#' Get metadata for a specific dataset
#'
#' Fetches full SODA metadata (views API) for a dataset including column
#' definitions, attribution, license, and view statistics. The \code{columns}
#' list-column contains a nested tibble with field names, display names,
#' data types, and descriptions for each column.
#'
#' @param domain Character. Portal domain hosting the dataset (e.g.
#'   \code{"data.cdc.gov"}).
#' @param dataset_id Character. Four-by-four dataset ID (e.g.
#'   \code{"9bhg-hcku"}).
#' @return A tibble with one row containing:
#' \describe{
#'   \item{id}{Character. Dataset ID.}
#'   \item{name}{Character. Dataset title.}
#'   \item{description}{Character. Full description.}
#'   \item{category}{Character. Portal category.}
#'   \item{attribution}{Character. Data source attribution.}
#'   \item{license}{Character. License name.}
#'   \item{rowsUpdatedAt}{Character. Timestamp of last data update.}
#'   \item{createdAt}{Character. Timestamp of dataset creation.}
#'   \item{viewCount}{Integer. Number of views.}
#'   \item{downloadCount}{Integer. Number of downloads.}
#'   \item{columns}{List of tibble. Column schema with field_name, name, datatype, description.}
#' }
#' @export
#' @examples
#' \dontrun{
#' meta <- socrata_meta("data.cdc.gov", "9bhg-hcku")
#' # View column schema
#' meta$columns[[1]]
#' }
socrata_meta <- function(domain, dataset_id) {
  url <- sprintf("https://%s/api/views/%s.json", domain, dataset_id)
  raw <- .socrata_fetch(url, simplify = FALSE)

  cols <- if (!is.null(raw$columns)) {
    tibble(
      field_name = vapply(raw$columns, function(c) c$fieldName %||% NA_character_, character(1)),
      name       = vapply(raw$columns, function(c) c$name %||% NA_character_, character(1)),
      datatype   = vapply(raw$columns, function(c) c$dataTypeName %||% NA_character_, character(1)),
      description = vapply(raw$columns, function(c) c$description %||% NA_character_, character(1))
    )
  } else {
    tibble(field_name = character(), name = character(), datatype = character(),
           description = character())
  }

  tibble(
    id            = .safe_chr(raw$id),
    name          = .safe_chr(raw$name),
    description   = .safe_chr(raw$description),
    category      = .safe_chr(raw$category),
    attribution   = .safe_chr(raw$attribution),
    license       = .safe_chr(raw$license$name),
    rowsUpdatedAt = .safe_chr(raw$rowsUpdatedAt),
    createdAt     = .safe_chr(raw$createdAt),
    viewCount     = .safe_int(raw$viewCount),
    downloadCount = .safe_int(raw$downloadCount),
    columns       = list(cols)
  )
}


# == Access: Fetch Data ========================================================

#' Fetch data from any Socrata dataset
#'
#' Uses the SODA 2.1 API to query and retrieve actual data rows from any
#' public Socrata dataset. Supports SoQL (Socrata Query Language) for
#' filtering, aggregation, and sorting. Numeric columns are auto-detected
#' and converted; date-like columns are parsed to Date objects.
#'
#' @param domain Character. Portal domain (e.g. \code{"data.cdc.gov"},
#'   \code{"data.cityofnewyork.us"}).
#' @param dataset_id Character. Four-by-four dataset ID (e.g.
#'   \code{"9bhg-hcku"}).
#' @param select Character or NULL. SoQL SELECT clause (e.g.
#'   \code{"state, age_group, covid_19_deaths"}).
#' @param where Character or NULL. SoQL WHERE clause (e.g.
#'   \code{"state = 'United States'"}).
#' @param order Character or NULL. SoQL ORDER BY clause (e.g.
#'   \code{"date DESC"}).
#' @param group Character or NULL. SoQL GROUP BY clause for aggregation.
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Maximum rows to return (default 1000, max 50000).
#' @param offset Integer. Row offset for pagination (default 0).
#' @param app_token Character or NULL. Optional Socrata app token for higher
#'   rate limits. Without a token, rate-limited to approximately 1000
#'   requests per hour per IP.
#' @return A tibble with dataset rows. Numeric and date columns are
#'   automatically typed.
#' @export
#' @examples
#' \dontrun{
#' socrata_data("data.cdc.gov", "9bhg-hcku", limit = 10)
#' socrata_data("data.cdc.gov", "9bhg-hcku",
#'              where = "state = 'United States'",
#'              order = "end_date DESC", limit = 50)
#' }
socrata_data <- function(domain, dataset_id, select = NULL, where = NULL,
                         order = NULL, group = NULL, q = NULL,
                         limit = 1000, offset = 0, app_token = NULL) {
  url <- sprintf("https://%s/resource/%s.json", domain, dataset_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]] <- where
  if (!is.null(order))  params[["$order"]] <- order
  if (!is.null(group))  params[["$group"]] <- group
  if (!is.null(q))      params[["$q"]] <- q

  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  if (!is.null(app_token)) {
    req <- req |> httr2::req_headers(`X-App-Token` = app_token)
  }

  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)

  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)

  # Auto-type numeric columns
 for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) > 0 && all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
  }

  .type_dates(df)
}


#' Fetch all rows from a Socrata dataset with automatic pagination
#'
#' Iteratively pages through the SODA API to retrieve all matching rows up
#' to \code{max_rows}. Use with caution on large datasets -- check row count
#' with \code{\link{socrata_count}} first.
#'
#' @param domain Character. Portal domain.
#' @param dataset_id Character. Four-by-four dataset ID.
#' @param select Character or NULL. SoQL SELECT clause.
#' @param where Character or NULL. SoQL WHERE clause.
#' @param order Character or NULL. SoQL ORDER BY clause. Recommended for
#'   consistent pagination ordering.
#' @param max_rows Integer. Maximum total rows to fetch (default 10000).
#' @param page_size Integer. Rows per API call (default 5000).
#' @param app_token Character or NULL. Optional Socrata app token.
#' @return A tibble with all matching rows.
#' @export
#' @examples
#' \dontrun{
#' socrata_data_all("data.cdc.gov", "9bhg-hcku",
#'                  where = "state = 'New York'",
#'                  max_rows = 5000)
#' }
socrata_data_all <- function(domain, dataset_id, select = NULL, where = NULL,
                             order = NULL, max_rows = 10000, page_size = 5000,
                             app_token = NULL) {
  all_data <- list()
  offset <- 0
  repeat {
    remaining <- max_rows - offset
    if (remaining <= 0) break
    lim <- min(page_size, remaining)
    chunk <- socrata_data(domain, dataset_id, select = select, where = where,
                          order = order, limit = lim, offset = offset,
                          app_token = app_token)
    if (nrow(chunk) == 0) break
    all_data[[length(all_data) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(all_data) == 0) return(tibble())
  bind_rows(all_data)
}


#' Count rows in a Socrata dataset
#'
#' Uses SoQL \code{count(*)} to efficiently get the number of rows in a
#' dataset without downloading any data. Useful for checking dataset size
#' before calling \code{\link{socrata_data_all}}.
#'
#' @param domain Character. Portal domain.
#' @param dataset_id Character. Four-by-four dataset ID.
#' @param where Character or NULL. Optional SoQL WHERE clause to count a
#'   subset (e.g. \code{"state = 'New York'"}).
#' @param app_token Character or NULL. Optional Socrata app token.
#' @return Integer. Total row count.
#' @export
#' @examples
#' \dontrun{
#' socrata_count("data.cdc.gov", "9bhg-hcku")
#' socrata_count("data.cdc.gov", "9bhg-hcku",
#'               where = "state = 'New York'")
#' }
socrata_count <- function(domain, dataset_id, where = NULL, app_token = NULL) {
  df <- socrata_data(domain, dataset_id, select = "count(*) as n",
                     where = where, limit = 1, app_token = app_token)
  if (nrow(df) == 0) return(0L)
  as.integer(df$n[1])
}


# == Context ===================================================================

#' Get socrata.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
socrata_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(socrata_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/socrata.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "socrata.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# socrata.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# socrata.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
