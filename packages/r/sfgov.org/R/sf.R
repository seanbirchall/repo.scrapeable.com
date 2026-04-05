# sfgov.org.R
# Self-contained San Francisco Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.sfgov.org


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sf_base <- "https://data.sfgov.org"

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

# Core SODA query builder
._soda_query <- function(dataset_id, where = NULL, select = NULL,
                         group = NULL, order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query_str <- paste(
    names(params),
    vapply(params, as.character, character(1)),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .sf_base, dataset_id,
                 utils::URLencode(query_str, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF Open Data query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), updated_at = character(), type = character()
)

.schema_view <- tibble(
  field_name = character(), data_type = character(), description = character()
)

.schema_records <- tibble()

# == Discovery =================================================================

#' List San Francisco Open Data datasets
#'
#' Browses the full catalog of datasets hosted on data.sfgov.org (Socrata).
#' Returns dataset metadata including four-by-four IDs needed for
#' \code{\link{sf_query}} and \code{\link{sf_view}}. Supports pagination
#' for browsing large catalogs.
#'
#' @param limit Integer. Number of datasets to return per page (default 50).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with one row per dataset:
#' \describe{
#'   \item{id}{Character. Socrata four-by-four dataset ID (e.g. "wg3w-h783").}
#'   \item{name}{Character. Dataset title.}
#'   \item{category}{Character. Domain category (e.g. "Public Safety").}
#'   \item{description}{Character. Dataset description (truncated to 200 chars).}
#'   \item{updated_at}{Character. ISO timestamp of last update.}
#'   \item{type}{Character. Asset type ("dataset", "map", "filter", etc.).}
#' }
#' @export
#' @examples
#' \dontrun{
#' sf_list(limit = 10)
#' sf_list(limit = 10, page = 2)
#' }
sf_list <- function(limit = 50, page = 1) {
  offset <- (page - 1) * limit
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.sfgov.org&search_context=data.sfgov.org&limit=%d&offset=%d",
    .sf_base, limit, offset
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) return(.schema_datasets)

  results <- raw$results
  res_list <- results$resource
  tibble(
    id          = as.character(res_list$id),
    name        = as.character(res_list$name),
    category    = vapply(seq_len(nrow(res_list)), function(i) {
      cats <- results$classification$domain_category
      if (is.null(cats)) NA_character_ else as.character(cats[i]) %||% NA_character_
    }, character(1)),
    description = vapply(as.character(res_list$description), function(d) {
      if (is.na(d) || nchar(d) <= 200) d else paste0(substr(d, 1, 200), "...")
    }, character(1)),
    updated_at  = as.character(res_list$updatedAt),
    type        = as.character(res_list$type)
  )
}


#' Search San Francisco Open Data datasets
#'
#' Full-text search across the data.sfgov.org catalog. Searches dataset
#' names, descriptions, column names, and tags. Returns the same metadata
#' as \code{\link{sf_list}} but filtered by relevance to the query.
#'
#' @param query Character. Search terms (e.g. \code{"police incidents"},
#'   \code{"building permits"}).
#' @param limit Integer. Maximum results to return (default 20).
#' @param only Character or NULL. Filter by asset type: \code{"datasets"},
#'   \code{"maps"}, \code{"charts"}, etc. Default \code{NULL} returns all types.
#' @return A tibble with one row per matching dataset:
#' \describe{
#'   \item{id}{Character. Socrata four-by-four dataset ID.}
#'   \item{name}{Character. Dataset title.}
#'   \item{category}{Character. Domain category.}
#'   \item{description}{Character. Dataset description (truncated to 200 chars).}
#'   \item{updated_at}{Character. ISO timestamp of last update.}
#'   \item{type}{Character. Asset type.}
#' }
#' @export
#' @examples
#' \dontrun{
#' sf_search("police")
#' sf_search("housing", only = "datasets")
#' }
sf_search <- function(query, limit = 20, only = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.sfgov.org&search_context=data.sfgov.org&q=%s&limit=%d",
    .sf_base, utils::URLencode(query, reserved = TRUE), limit
  )
  if (!is.null(only)) url <- paste0(url, "&only=", only)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) return(.schema_datasets)

  results <- raw$results
  res_list <- results$resource
  tibble(
    id          = as.character(res_list$id),
    name        = as.character(res_list$name),
    category    = vapply(seq_len(nrow(res_list)), function(i) {
      cats <- results$classification$domain_category
      if (is.null(cats)) NA_character_ else as.character(cats[i]) %||% NA_character_
    }, character(1)),
    description = vapply(as.character(res_list$description), function(d) {
      if (is.na(d) || nchar(d) <= 200) d else paste0(substr(d, 1, 200), "...")
    }, character(1)),
    updated_at  = as.character(res_list$updatedAt),
    type        = as.character(res_list$type)
  )
}


#' View metadata and schema for an SF dataset
#'
#' Returns the column schema (field names, data types, and descriptions) for
#' a specific Socrata dataset. Useful for understanding dataset structure
#' before writing \code{\link{sf_query}} filters.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID
#'   (e.g. \code{"wg3w-h783"} for police incidents).
#' @return A tibble with one row per column in the dataset:
#' \describe{
#'   \item{field_name}{Character. Column name used in SoQL queries.}
#'   \item{data_type}{Character. Socrata data type (text, number, calendar_date, etc.).}
#'   \item{description}{Character. Column description (may be NA).}
#' }
#' @export
#' @examples
#' \dontrun{
#' sf_view("wg3w-h783")
#' }
sf_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .sf_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF view metadata error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$columns)) return(.schema_view)

  cols <- raw$columns
  tibble(
    field_name  = as.character(cols$fieldName),
    data_type   = as.character(cols$dataTypeName),
    description = vapply(cols$description %||% rep(NA_character_, nrow(cols)),
                         function(d) if (is.null(d) || is.na(d)) NA_character_ else as.character(d),
                         character(1))
  )
}


# == Data access ===============================================================

#' Query a San Francisco Open Data dataset
#'
#' Runs a SoQL (Socrata Query Language) query against any dataset on
#' data.sfgov.org. SoQL is SQL-like and supports WHERE, SELECT, GROUP BY,
#' ORDER BY, and full-text search. Use \code{\link{sf_view}} to discover
#' column names for a dataset.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID (e.g.
#'   \code{"wg3w-h783"}).
#' @param where Character or NULL. SoQL WHERE clause (e.g.
#'   \code{"incident_year='2024'"}).
#' @param select Character or NULL. SoQL SELECT clause (e.g.
#'   \code{"incident_category, count(*) as n"}).
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param order Character or NULL. SoQL ORDER BY clause.
#' @param q Character or NULL. Full-text search query across all text fields.
#' @param limit Integer. Maximum rows to return (default 1000, max 50000).
#' @param offset Integer. Row offset for pagination (default 0).
#' @return A tibble of query results. Columns vary by dataset.
#' @export
#' @examples
#' \dontrun{
#' # Recent police incidents
#' sf_query("wg3w-h783", limit = 10)
#' # Incidents by category
#' sf_query("wg3w-h783",
#'          select = "incident_category, count(*) as n",
#'          group = "incident_category",
#'          order = "n DESC", limit = 10)
#' }
sf_query <- function(dataset_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from an SF dataset with auto-pagination
#'
#' Iteratively pages through the SODA API to retrieve all matching rows
#' from a dataset, up to \code{max_rows}. Use with caution on large
#' datasets -- check row count with \code{\link{sf_query}} first using
#' \code{select = "count(*)"}.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID.
#' @param where Character or NULL. Optional SoQL WHERE clause to filter rows.
#' @param select Character or NULL. Optional SoQL SELECT clause.
#' @param order Character or NULL. Optional SoQL ORDER BY clause. Recommended
#'   when paginating to ensure consistent ordering.
#' @param max_rows Integer. Maximum total rows to fetch (default 50000).
#' @param page_size Integer. Rows per API request (default 10000).
#' @return A tibble of all matching records. Columns vary by dataset.
#' @export
#' @examples
#' \dontrun{
#' # Get all 2024 police incidents (may be large)
#' sf_fetch_all("wg3w-h783", where = "incident_year='2024'",
#'              order = "incident_datetime DESC")
#' }
sf_fetch_all <- function(dataset_id, where = NULL, select = NULL,
                         order = NULL, max_rows = 50000,
                         page_size = 10000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(dataset_id, where = where, select = select,
                          order = order, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Named convenience functions ===============================================

#' Fetch SF police incident reports
#'
#' Convenience wrapper querying dataset \code{wg3w-h783} (Police Department
#' Incident Reports: 2018 to Present). Supports filtering by category,
#' neighborhood, and year. Returns results ordered by most recent first.
#'
#' @param category Character or NULL. Incident category filter (e.g.
#'   \code{"Larceny Theft"}, \code{"Assault"}, \code{"Burglary"}).
#' @param neighborhood Character or NULL. Analysis neighborhood filter
#'   (e.g. \code{"Mission"}, \code{"Tenderloin"}, \code{"Financial District/South Beach"}).
#' @param year Character or NULL. Incident year filter (e.g. \code{"2024"}).
#' @param limit Integer. Maximum rows to return (default 1000).
#' @return A tibble of police incidents with columns including
#'   incident_datetime, incident_category, incident_subcategory,
#'   incident_description, analysis_neighborhood, latitude, longitude, etc.
#' @export
#' @examples
#' \dontrun{
#' sf_police_incidents(category = "Larceny Theft", year = "2024", limit = 100)
#' sf_police_incidents(neighborhood = "Mission", limit = 50)
#' }
sf_police_incidents <- function(category = NULL, neighborhood = NULL,
                                year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(category))     clauses <- c(clauses, sprintf("incident_category='%s'", category))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("analysis_neighborhood='%s'", neighborhood))
  if (!is.null(year))         clauses <- c(clauses, sprintf("incident_year='%s'", year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("wg3w-h783", where = where, order = "incident_datetime DESC", limit = limit)
}


#' Fetch SF 311 service requests
#'
#' Convenience wrapper querying dataset \code{vw6y-z8j6} (311 Cases).
#' San Francisco 311 handles non-emergency city service requests such as
#' street cleaning, graffiti removal, and pothole repair.
#'
#' @param category Character or NULL. Service request category filter
#'   (e.g. \code{"Graffiti"}, \code{"Street and Sidewalk Cleaning"}).
#' @param neighborhood Character or NULL. Neighborhood filter.
#' @param status Character or NULL. Status filter (\code{"Open"} or
#'   \code{"Closed"}).
#' @param limit Integer. Maximum rows to return (default 1000).
#' @return A tibble of 311 cases with columns including requested_datetime,
#'   service_name, status_description, neighborhoods_sffind_boundaries, etc.
#' @export
#' @examples
#' \dontrun{
#' sf_311_cases(status = "Open", limit = 50)
#' }
sf_311_cases <- function(category = NULL, neighborhood = NULL,
                         status = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(category))     clauses <- c(clauses, sprintf("service_name='%s'", category))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhoods_sffind_boundaries='%s'", neighborhood))
  if (!is.null(status))       clauses <- c(clauses, sprintf("status_description='%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("vw6y-z8j6", where = where, order = "requested_datetime DESC", limit = limit)
}


#' Fetch SF fire incidents
#'
#' Convenience wrapper querying dataset \code{wr8u-xric} (Fire Incidents).
#' Includes structure fires, vehicle fires, medical calls, and other fire
#' department responses.
#'
#' @param year Character or NULL. Incident year filter (e.g. \code{"2024"}).
#' @param neighborhood Character or NULL. Neighborhood district filter.
#' @param limit Integer. Maximum rows to return (default 1000).
#' @return A tibble of fire incidents with columns including incident_date,
#'   incident_type, neighborhood_district, primary_situation, etc.
#' @export
#' @examples
#' \dontrun{
#' sf_fire_incidents(year = "2024", limit = 100)
#' }
sf_fire_incidents <- function(year = NULL, neighborhood = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year))         clauses <- c(clauses, sprintf("incident_year='%s'", year))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhood_district='%s'", neighborhood))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("wr8u-xric", where = where, order = "incident_date DESC", limit = limit)
}


#' Fetch SF budget data
#'
#' Convenience wrapper querying dataset \code{xdgd-c79v} (City of San
#' Francisco Budget). Contains line-item budget data by department and
#' fiscal year.
#'
#' @param department Character or NULL. Department name filter.
#' @param fiscal_year Character or NULL. Fiscal year filter.
#' @param limit Integer. Maximum rows to return (default 1000).
#' @return A tibble of budget line items with columns including department,
#'   fiscal_year, program, object, amount, etc.
#' @export
#' @examples
#' \dontrun{
#' sf_budget(fiscal_year = "2024", limit = 100)
#' }
sf_budget <- function(department = NULL, fiscal_year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(department))  clauses <- c(clauses, sprintf("department='%s'", department))
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("xdgd-c79v", where = where, limit = limit)
}


#' Fetch SF building permits
#'
#' Convenience wrapper querying dataset \code{tyz3-vt28} (PermitSF
#' Permitting Data). Covers building permits, alterations, demolitions,
#' and new construction across San Francisco.
#'
#' @param permit_type Character or NULL. Permit type filter (e.g.
#'   \code{"alterations"}, \code{"new construction"}).
#' @param status Character or NULL. Current status filter (e.g.
#'   \code{"complete"}, \code{"issued"}).
#' @param neighborhood Character or NULL. Neighborhood filter.
#' @param limit Integer. Maximum rows to return (default 1000).
#' @return A tibble of building permits with columns including
#'   permit_type_definition, status, filed_date, address, description, etc.
#' @export
#' @examples
#' \dontrun{
#' sf_permits(status = "issued", limit = 50)
#' }
sf_permits <- function(permit_type = NULL, status = NULL,
                       neighborhood = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(permit_type))  clauses <- c(clauses, sprintf("permit_type_definition='%s'", permit_type))
  if (!is.null(status))       clauses <- c(clauses, sprintf("status='%s'", status))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhoods_analysis_boundaries='%s'", neighborhood))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tyz3-vt28", where = where, order = "filed_date DESC", limit = limit)
}


# == Context ===================================================================

#' Get sfgov.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
sf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/sfgov.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "sfgov.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# sfgov.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# sfgov.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
