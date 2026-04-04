# lacity.org.R
# Self-contained City of Los Angeles Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app tokens optional for higher rate limits)
# API: Socrata SODA 2.0 at data.lacity.org
# Datasets: 344 (343 Socrata views)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.la_base <- "https://data.lacity.org"

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
  url <- sprintf("%s/resource/%s.json?%s", .la_base, dataset_id, query_str)

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

#' List datasets on the LA City Open Data portal
#'
#' Returns catalog metadata for datasets on data.lacity.org, which
#' hosts over 340 Socrata datasets covering public safety, building
#' permits, transportation, 311 requests, and more.
#'
#' @param limit Integer. Number of datasets to return. Default 50, max 200.
#' @param category Character or NULL. Category filter
#'   (e.g. `"Public Safety"`, `"City Infrastructure"`).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata dataset ID (e.g. `"2nrs-mtv8"`).}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type.}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{view_count}{Integer. Total views.}
#'   }
#'
#' @family lacity discovery
#' @seealso [la_search()] for keyword search, [la_view()] for single-dataset info
#'
#' @examples
#' \dontrun{
#' la_list(limit = 20)
#' la_list(category = "Public Safety")
#' }
#' @export
la_list <- function(limit = 50, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .la_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("LA Open Data API error: ", e$message)
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

#' Search LA City Open Data datasets by keyword
#'
#' Full-text search across dataset names and descriptions using the
#' Socrata Discovery API. Searches all data.lacity.org content.
#'
#' @param query Character. Search term (e.g. `"crime"`, `"parking"`,
#'   `"permits"`, `"311"`).
#' @param limit Integer. Maximum results. Default 20.
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
#' @family lacity discovery
#' @seealso [la_list()] for browsing, [la_view()] for dataset metadata
#'
#' @examples
#' \dontrun{
#' la_search("crime")
#' la_search("building permits")
#' }
#' @export
la_search <- function(query, limit = 20) {
  schema <- tibble(
    id = character(), name = character(), description = character(),
    category = character(), updated_at = character()
  )
  url <- sprintf(
    "https://api.us.socrata.com/api/catalog/v1?domains=data.lacity.org&search_context=data.lacity.org&q=%s&limit=%d",
    utils::URLencode(query, reserved = TRUE), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("LA search error: ", e$message)
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

#' Get metadata for a specific LA City dataset
#'
#' Retrieves detailed metadata for a single dataset including row count,
#' column count, and download count.
#'
#' @param dataset_id Character. Socrata dataset identifier
#'   (e.g. `"2nrs-mtv8"` for LAPD crime data).
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
#' @family lacity discovery
#' @seealso [la_query()] to fetch data from the dataset
#'
#' @examples
#' \dontrun{
#' la_view("2nrs-mtv8")
#' }
#' @export
la_view <- function(dataset_id) {
  schema <- tibble(
    id = character(), name = character(), description = character(),
    category = character(), type = character(),
    updated_at = as.POSIXct(character()),
    row_count = integer(), column_count = integer(),
    download_count = integer()
  )
  url <- sprintf("%s/api/views/%s.json", .la_base, dataset_id)
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

#' Query any Socrata dataset on LA City Open Data
#'
#' Runs a SoQL (Socrata Query Language) query against any dataset on
#' data.lacity.org via the SODA 2.0 API. Supports filtering, column
#' selection, ordering, grouping, and full-text search.
#'
#' @param dataset_id Character. Socrata dataset identifier
#'   (e.g. `"2nrs-mtv8"` for crime data).
#' @param where Character or NULL. SoQL WHERE clause
#'   (e.g. `"area_name='Hollywood'"`).
#' @param select Character or NULL. SoQL SELECT clause
#'   (e.g. `"date_occ, crm_cd_desc"`).
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param order Character or NULL. SoQL ORDER BY clause
#'   (e.g. `"date_occ DESC"`).
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Maximum rows. Default 1000, max 50000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family lacity data
#' @seealso [la_fetch_all()] for auto-pagination, [la_count()] for row counts
#'
#' @examples
#' \dontrun{
#' la_query("2nrs-mtv8", where = "area_name='Hollywood'", limit = 50)
#' }
#' @export
la_query <- function(dataset_id, where = NULL, select = NULL,
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
#' is reached.
#'
#' @param dataset_id Character. Socrata dataset identifier.
#' @param where Character or NULL. SoQL WHERE clause.
#' @param max_rows Integer. Maximum total rows to fetch. Default 10000.
#' @param page_size Integer. Rows per API request. Default 1000.
#'
#' @return A tibble with all matching records (up to `max_rows`).
#'
#' @family lacity data
#' @seealso [la_query()] for single-page queries, [la_count()] for row counts
#'
#' @examples
#' \dontrun{
#' la_fetch_all("2nrs-mtv8", where = "area_name='Hollywood'", max_rows = 5000)
#' }
#' @export
la_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
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
#' Returns the total number of rows matching a query without
#' downloading the data. Useful for planning pagination.
#'
#' @param dataset_id Character. Socrata dataset identifier.
#' @param where Character or NULL. SoQL WHERE clause.
#'
#' @return Integer. Total number of matching rows, or `NA` on error.
#'
#' @family lacity data
#' @seealso [la_query()], [la_fetch_all()]
#'
#' @examples
#' \dontrun{
#' la_count("2nrs-mtv8")
#' la_count("2nrs-mtv8", where = "area_name='Hollywood'")
#' }
#' @export
la_count <- function(dataset_id, where = NULL) {
  result <- ._soda_query(dataset_id, select = "count(*) as n", where = where, limit = 1)
  if (nrow(result) == 0) return(NA_integer_)
  as.integer(result$n[1])
}

# == Named convenience functions ===============================================

#' Query LAPD crime data (2020-2024)
#'
#' Queries the LAPD Crime Data from 2020 to 2024 dataset (`2nrs-mtv8`).
#' Each row represents a single crime incident with victim demographics,
#' location, and crime classification.
#'
#' @param area_name Character or NULL. LAPD area filter
#'   (e.g. `"Hollywood"`, `"Central"`, `"77th Street"`).
#' @param crime_desc Character or NULL. Crime description filter.
#'   Partial match via SoQL LIKE, case-insensitive.
#' @param start_date Character or NULL. Start date in `"YYYY-MM-DD"` format.
#' @param end_date Character or NULL. End date in `"YYYY-MM-DD"` format.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble with columns including dr_no, date_rptd, date_occ,
#'   area_name, crm_cd_desc, vict_age, vict_sex, premis_desc,
#'   status_desc, lat, and lon.
#'
#' @family lacity data
#' @seealso [la_collisions()] for traffic collision data,
#'   [la_calls()] for calls for service
#'
#' @examples
#' \dontrun{
#' la_crime(area_name = "Hollywood", limit = 50)
#' la_crime(crime_desc = "BURGLARY", start_date = "2024-01-01")
#' }
#' @export
la_crime <- function(area_name = NULL, crime_desc = NULL,
                     start_date = NULL, end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(area_name))  clauses <- c(clauses, sprintf("area_name='%s'", area_name))
  if (!is.null(crime_desc)) clauses <- c(clauses, sprintf("upper(crm_cd_desc) LIKE upper('%%%s%%')", crime_desc))
  if (!is.null(start_date)) clauses <- c(clauses, sprintf("date_occ >= '%s'", start_date))
  if (!is.null(end_date))   clauses <- c(clauses, sprintf("date_occ <= '%s'", end_date))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("2nrs-mtv8", where = where, order = "date_occ DESC", limit = limit)
}

#' Query MyLA311 service requests (2026)
#'
#' Queries the MyLA311 Cases dataset (`2cy6-i7zn`). Contains service
#' requests for bulky item pickup, graffiti removal, potholes, illegal
#' dumping, and other city services.
#'
#' @param type Character or NULL. Request type filter
#'   (e.g. `"Bulky Items"`, `"Graffiti Removal"`). Partial match.
#' @param status Character or NULL. Status filter
#'   (e.g. `"Open"`, `"Closed"`).
#' @param zipcode Character or NULL. Zip code filter.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of 311 service request records.
#'
#' @family lacity data
#'
#' @examples
#' \dontrun{
#' la_311(type = "Bulky Items", limit = 50)
#' la_311(status = "Open", zipcode = "90028")
#' }
#' @export
la_311 <- function(type = NULL, status = NULL, zipcode = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(type))    clauses <- c(clauses, sprintf("upper(type) LIKE upper('%%%s%%')", type))
  if (!is.null(status))  clauses <- c(clauses, sprintf("status='%s'", status))
  if (!is.null(zipcode)) clauses <- c(clauses, sprintf("zipcode__c='%s'", zipcode))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("2cy6-i7zn", where = where, order = "createddate DESC", limit = limit)
}

#' Query traffic collision data (2010-present)
#'
#' Queries the LAPD Traffic Collision dataset (`d5tf-ez2w`). Contains
#' collision reports from 2010 to present with location, victim, and
#' involved-party details.
#'
#' @param area_name Character or NULL. LAPD area filter.
#' @param start_date Character or NULL. Start date in `"YYYY-MM-DD"` format.
#' @param end_date Character or NULL. End date in `"YYYY-MM-DD"` format.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of traffic collision records.
#'
#' @family lacity data
#' @seealso [la_crime()] for crime data
#'
#' @examples
#' \dontrun{
#' la_collisions(area_name = "Central", limit = 50)
#' }
#' @export
la_collisions <- function(area_name = NULL, start_date = NULL,
                          end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(area_name))  clauses <- c(clauses, sprintf("area_name='%s'", area_name))
  if (!is.null(start_date)) clauses <- c(clauses, sprintf("date_occ >= '%s'", start_date))
  if (!is.null(end_date))   clauses <- c(clauses, sprintf("date_occ <= '%s'", end_date))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("d5tf-ez2w", where = where, order = "date_occ DESC", limit = limit)
}

#' Query building permits (2020-present)
#'
#' Queries the LA Building and Safety permits dataset (`pi9x-tg5x`).
#' Contains building permits issued from 2020 to the present.
#'
#' @param zip Character or NULL. Zip code filter.
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of building permit records.
#'
#' @family lacity data
#'
#' @examples
#' \dontrun{
#' la_permits(zip = "90028", limit = 50)
#' }
#' @export
la_permits <- function(zip = NULL, limit = 1000) {
  where <- if (!is.null(zip)) sprintf("zip_code='%s'", zip) else NULL
  ._soda_query("pi9x-tg5x", where = where, limit = limit)
}

#' Query LAPD calls for service (2024-present)
#'
#' Queries the LAPD Calls for Service dataset from 2024 to present
#' (`xjgu-z4ju`). Each row represents a dispatched call.
#'
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of calls for service records.
#'
#' @family lacity data
#' @seealso [la_crime()]
#'
#' @examples
#' \dontrun{
#' la_calls(limit = 100)
#' }
#' @export
la_calls <- function(limit = 1000) {
  ._soda_query("xjgu-z4ju", order = ":id", limit = limit)
}

#' Query metered parking inventory
#'
#' Queries the LADOT Metered Parking Inventory and Policies dataset
#' (`s49e-q6j2`). Contains meter locations, rates, and time limits.
#'
#' @param limit Integer. Maximum rows. Default 1000.
#'
#' @return A tibble of metered parking records.
#'
#' @family lacity data
#'
#' @examples
#' \dontrun{
#' la_parking(limit = 100)
#' }
#' @export
la_parking <- function(limit = 1000) {
  ._soda_query("s49e-q6j2", limit = limit)
}

# == Context ===================================================================

#' Get lacity.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
la_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(la_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/lacity.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "lacity.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# lacity.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# lacity.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
