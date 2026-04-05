# iowa.gov.R
# Self-contained State of Iowa Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app tokens optional for higher rate limits)
# API: Socrata SODA 2.0 at data.iowa.gov
# Datasets: 426 (417 Socrata views across 10 subdomains)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.iowa_base <- "https://data.iowa.gov"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- SODA query builder -------------------------------------------------------

._soda_query <- function(dataset_id, where = NULL, select = NULL,
                         group = NULL, order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query_str <- paste(names(params),
                     vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), character(1)),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .iowa_base, dataset_id, query_str)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

.schema_records <- tibble()

# == Discovery =================================================================

#' List datasets on the Iowa Open Data portal
#'
#' Returns catalog metadata for datasets on data.iowa.gov, which hosts
#' over 400 Socrata views across 10 subdomains covering revenue,
#' transportation, employment, liquor sales, and more.
#'
#' @param limit Integer. Number of datasets to return. Default 50, max 200.
#' @param category Character or NULL. Category filter applied after fetch
#'   (e.g. `"Economy"`, `"Transportation"`).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata dataset ID (e.g. `"m3tr-qhgy"`).}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type.}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{view_count}{Integer. Total views.}
#'   }
#'
#' @family iowa discovery
#' @seealso [iowa_search()] for keyword search, [iowa_view()] for single-dataset info
#'
#' @examples
#' \dontrun{
#' iowa_list(limit = 20)
#' iowa_list(category = "Economy")
#' }
#' @export
iowa_list <- function(limit = 50, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .iowa_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Iowa Open Data API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)
  if (!is.data.frame(raw)) return(.schema_datasets)

  df <- as_tibble(raw)
  result <- tibble(
    id          = as.character(df$id),
    name        = as.character(df$name),
    description = vapply(as.character(df$description), function(d) {
      if (is.na(d)) return(NA_character_)
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1), USE.NAMES = FALSE),
    category    = as.character(df$category %||% NA_character_),
    type        = as.character(df$displayType %||% NA_character_),
    updated_at  = as.POSIXct(as.numeric(df$viewLastModified %||% NA),
                              origin = "1970-01-01", tz = "UTC"),
    view_count  = as.integer(df$viewCount %||% NA_integer_)
  )

  if (!is.null(category)) {
    result <- result |> filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  result
}

#' Search Iowa Open Data datasets by keyword
#'
#' Full-text search across dataset names and descriptions using the
#' Socrata Discovery API. Searches all data.iowa.gov content.
#'
#' @param query Character. Search term (e.g. `"liquor"`, `"unemployment"`,
#'   `"revenue"`, `"roads"`).
#' @param limit Integer. Maximum results to return. Default 20.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata dataset ID.}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Truncated description.}
#'     \item{category}{Character. Domain category.}
#'     \item{updated_at}{Character. Last update timestamp.}
#'   }
#'
#' @family iowa discovery
#' @seealso [iowa_list()] for browsing, [iowa_view()] for dataset metadata
#'
#' @examples
#' \dontrun{
#' iowa_search("liquor")
#' iowa_search("unemployment")
#' }
#' @export
iowa_search <- function(query, limit = 20) {
  schema <- tibble(
    id = character(), name = character(), description = character(),
    category = character(), updated_at = character()
  )
  url <- sprintf(
    "https://api.us.socrata.com/api/catalog/v1?domains=data.iowa.gov&search_context=data.iowa.gov&q=%s&limit=%d",
    utils::URLencode(query, reserved = TRUE), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Iowa search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(schema)

  results <- raw$results
  res <- results$resource
  cls <- results$classification
  tibble(
    id          = as.character(res$id %||% NA_character_),
    name        = as.character(res$name %||% NA_character_),
    description = vapply(as.character(res$description), function(d) {
      if (is.na(d)) return(NA_character_)
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1), USE.NAMES = FALSE),
    category    = as.character(cls$domain_category %||% NA_character_),
    updated_at  = as.character(res$updatedAt %||% NA_character_)
  )
}

#' Get metadata for a specific Iowa dataset
#'
#' Retrieves detailed metadata for a single dataset, including row count,
#' column count, and download count. Useful for inspecting a dataset
#' before querying it.
#'
#' @param dataset_id Character. Socrata dataset identifier
#'   (e.g. `"m3tr-qhgy"` for Iowa Liquor Sales).
#'
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{id}{Character. Dataset ID.}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Full description.}
#'     \item{category}{Character. Category.}
#'     \item{type}{Character. Display type.}
#'     \item{updated_at}{POSIXct. Last modification timestamp.}
#'     \item{row_count}{Integer. Number of rows.}
#'     \item{column_count}{Integer. Number of columns.}
#'     \item{download_count}{Integer. Total downloads.}
#'   }
#'
#' @family iowa discovery
#' @seealso [iowa_query()] to fetch data from the dataset
#'
#' @examples
#' \dontrun{
#' iowa_view("m3tr-qhgy")
#' }
#' @export
iowa_view <- function(dataset_id) {
  schema <- tibble(
    id = character(), name = character(), description = character(),
    category = character(), type = character(),
    updated_at = as.POSIXct(character()),
    row_count = integer(), column_count = integer(),
    download_count = integer()
  )
  url <- sprintf("%s/api/views/%s.json", .iowa_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$id)) return(schema)

  tibble(
    id             = as.character(raw$id %||% NA_character_),
    name           = as.character(raw$name %||% NA_character_),
    description    = as.character(raw$description %||% NA_character_),
    category       = as.character(raw$category %||% NA_character_),
    type           = as.character(raw$displayType %||% NA_character_),
    updated_at     = as.POSIXct(as.numeric(raw$viewLastModified %||% NA),
                                origin = "1970-01-01", tz = "UTC"),
    row_count      = as.integer(raw$rowCount %||% NA_integer_),
    column_count   = as.integer(length(raw$columns %||% list())),
    download_count = as.integer(raw$downloadCount %||% NA_integer_)
  )
}

# == Generic query =============================================================

#' Query any Socrata dataset on Iowa Open Data
#'
#' Runs a SoQL (Socrata Query Language) query against any dataset on
#' data.iowa.gov via the SODA 2.0 API. Supports filtering, column
#' selection, ordering, grouping, and full-text search.
#'
#' @param dataset_id Character. Socrata dataset identifier
#'   (e.g. `"m3tr-qhgy"` for liquor sales).
#' @param where Character or NULL. SoQL WHERE clause
#'   (e.g. `"upper(city)='DES MOINES'"`).
#' @param select Character or NULL. SoQL SELECT clause
#'   (e.g. `"city, count(*) as n"`).
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param order Character or NULL. SoQL ORDER BY clause
#'   (e.g. `"date DESC"`).
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Maximum rows to return. Default 1000, max 50000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family iowa data
#' @seealso [iowa_fetch_all()] for auto-paginated retrieval,
#'   [iowa_count()] for row counts
#'
#' @examples
#' \dontrun{
#' iowa_query("m3tr-qhgy", where = "upper(city)='DES MOINES'", limit = 50)
#' }
#' @export
iowa_query <- function(dataset_id, where = NULL, select = NULL,
                       group = NULL, order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}

#' Fetch all records with auto-pagination
#'
#' Repeatedly queries a dataset in pages and combines all results into
#' a single tibble. Stops when all rows are retrieved or `max_rows`
#' is reached, whichever comes first.
#'
#' @param dataset_id Character. Socrata dataset identifier.
#' @param where Character or NULL. SoQL WHERE clause to filter.
#' @param max_rows Integer. Maximum total rows to fetch. Default 10000.
#' @param page_size Integer. Rows per API request. Default 1000.
#'
#' @return A tibble with all matching records (up to `max_rows`).
#'
#' @family iowa data
#' @seealso [iowa_query()] for single-page queries,
#'   [iowa_count()] to check total row count first
#'
#' @examples
#' \dontrun{
#' iowa_fetch_all("m3tr-qhgy", where = "upper(city)='AMES'", max_rows = 5000)
#' }
#' @export
iowa_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                           page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(dataset_id, where = where,
                          limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

#' Get row count for a dataset query
#'
#' Returns the total number of rows matching a query, without
#' downloading the actual data. Useful for planning pagination.
#'
#' @param dataset_id Character. Socrata dataset identifier.
#' @param where Character or NULL. SoQL WHERE clause to filter.
#'
#' @return Integer. Total number of matching rows, or `NA` on error.
#'
#' @family iowa data
#' @seealso [iowa_query()], [iowa_fetch_all()]
#'
#' @examples
#' \dontrun{
#' iowa_count("m3tr-qhgy")
#' iowa_count("m3tr-qhgy", where = "upper(city)='AMES'")
#' }
#' @export
iowa_count <- function(dataset_id, where = NULL) {
  result <- ._soda_query(dataset_id, select = "count(*) as n", where = where, limit = 1)
  if (nrow(result) == 0) return(NA_integer_)
  as.integer(result$n[1])
}

# == Named convenience functions ===============================================

#' Query Iowa liquor sales
#'
#' Queries the Iowa Liquor Sales dataset (`m3tr-qhgy`), one of the most
#' popular open datasets in the US. Contains every wholesale liquor
#' purchase by Iowa Class "E" liquor licensees since 2012.
#'
#' @param city Character or NULL. City filter
#'   (e.g. `"DES MOINES"`, `"CEDAR RAPIDS"`). Case-insensitive.
#' @param county Character or NULL. County filter
#'   (e.g. `"POLK"`, `"LINN"`). Case-insensitive.
#' @param category_name Character or NULL. Liquor category filter
#'   (e.g. `"VODKA"`, `"WHISKEY"`). Partial match.
#' @param start_date Character or NULL. Start date in `"YYYY-MM-DD"` format.
#' @param end_date Character or NULL. End date in `"YYYY-MM-DD"` format.
#' @param limit Integer. Maximum rows to return. Default 1000.
#'
#' @return A tibble with columns including invoice_line_no, date, store,
#'   name, city, county, category_name, item_description, bottles_sold,
#'   sale_dollars, volume_sold_liters, and more.
#'
#' @family iowa data
#' @seealso [iowa_liquor_stores()] for store location data
#'
#' @examples
#' \dontrun{
#' iowa_liquor(city = "DES MOINES", category_name = "VODKA", limit = 50)
#' iowa_liquor(start_date = "2024-01-01", end_date = "2024-01-31")
#' }
#' @export
iowa_liquor <- function(city = NULL, county = NULL, category_name = NULL,
                        start_date = NULL, end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(city))          clauses <- c(clauses, sprintf("upper(city)='%s'", toupper(city)))
  if (!is.null(county))        clauses <- c(clauses, sprintf("upper(county)='%s'", toupper(county)))
  if (!is.null(category_name)) clauses <- c(clauses, sprintf("upper(category_name) LIKE upper('%%%s%%')", category_name))
  if (!is.null(start_date))    clauses <- c(clauses, sprintf("date >= '%s'", start_date))
  if (!is.null(end_date))      clauses <- c(clauses, sprintf("date <= '%s'", end_date))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("m3tr-qhgy", where = where, order = "date DESC", limit = limit)
}

#' Query Iowa liquor stores
#'
#' Queries the Iowa Liquor Stores dataset (`ykb6-ywnd`). Lists all
#' Class "E" liquor licensees with location and status information.
#'
#' @param city Character or NULL. City filter. Case-insensitive.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble with columns including store, name, store_status,
#'   address, city, state, and zipcode.
#'
#' @family iowa data
#' @seealso [iowa_liquor()] for sales transaction data
#'
#' @examples
#' \dontrun{
#' iowa_liquor_stores(city = "AMES")
#' }
#' @export
iowa_liquor_stores <- function(city = NULL, limit = 1000) {
  where <- if (!is.null(city)) sprintf("upper(city)='%s'", toupper(city)) else NULL
  ._soda_query("ykb6-ywnd", where = where, limit = limit)
}

#' Query Iowa state revenue
#'
#' Queries the State of Iowa Revenue dataset (`urps-v5ck`). Revenue is
#' summarized by fiscal year, month, fund, and department.
#'
#' @param fiscal_year Integer or NULL. Fiscal year filter (e.g. `2024`).
#' @param department Character or NULL. Department name filter.
#'   Partial match, case-insensitive.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of state revenue records.
#'
#' @family iowa data
#' @seealso [iowa_travel()] for state employee travel reimbursements
#'
#' @examples
#' \dontrun{
#' iowa_revenue(fiscal_year = 2024, limit = 50)
#' }
#' @export
iowa_revenue <- function(fiscal_year = NULL, department = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  if (!is.null(department))  clauses <- c(clauses, sprintf("upper(department) LIKE upper('%%%s%%')", department))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("urps-v5ck", where = where, order = "fiscal_year DESC", limit = limit)
}

#' Query Iowa 511 road conditions
#'
#' Queries the Iowa 511 dataset (`yata-4k7n`) for real-time road events
#' from the Iowa DOT. Data is updated approximately every 10 minutes
#' and includes construction, closures, incidents, and weather events.
#'
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of current road event records.
#'
#' @family iowa data
#'
#' @examples
#' \dontrun{
#' iowa_511()
#' }
#' @export
iowa_511 <- function(limit = 1000) {
  ._soda_query("yata-4k7n", limit = limit)
}

#' Query Iowa unemployment insurance claims (weekly)
#'
#' Queries the Iowa Unemployment Insurance Claims dataset (`nqiw-f9td`).
#' Contains weekly initial and continued claims data.
#'
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of weekly unemployment insurance claim records.
#'
#' @family iowa data
#'
#' @examples
#' \dontrun{
#' iowa_unemployment(limit = 100)
#' }
#' @export
iowa_unemployment <- function(limit = 1000) {
  # Weekly claims dataset
  ._soda_query("nqiw-f9td", order = ":id DESC", limit = limit)
}

#' Query Iowa state employee travel reimbursements
#'
#' Queries the State of Iowa Executive Branch Out-of-State Travel
#' Reimbursements dataset (`vtga-efxt`). Contains individual travel
#' reimbursement records with department, employee, destination, and
#' amount information.
#'
#' @param department Character or NULL. Department name filter.
#'   Partial match, case-insensitive.
#' @param fiscal_year Integer or NULL. Fiscal year filter.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of travel reimbursement records.
#'
#' @family iowa data
#' @seealso [iowa_revenue()] for state revenue data
#'
#' @examples
#' \dontrun{
#' iowa_travel(department = "Education", limit = 50)
#' }
#' @export
iowa_travel <- function(department = NULL, fiscal_year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(department))  clauses <- c(clauses, sprintf("upper(department) LIKE upper('%%%s%%')", department))
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("vtga-efxt", where = where, order = ":id DESC", limit = limit)
}

# == Context ===================================================================

#' Get iowa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
iowa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(iowa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/iowa.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "iowa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# iowa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# iowa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
