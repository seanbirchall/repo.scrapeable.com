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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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
#' Returns catalog metadata for datasets on data.iowa.gov.
#'
#' @param limit Number of datasets to return (default 50, max 200)
#' @param category Optional category filter (e.g. "Economy")
#' @return tibble: id, name, description, category, type, updated_at, view_count
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
#' Socrata Discovery API.
#'
#' @param query Search term (e.g. "liquor", "unemployment", "revenue")
#' @param limit Number of results (default 20)
#' @return tibble: id, name, description, category, updated_at
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
#' Returns detailed info about a single dataset including row/column counts.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "m3tr-qhgy")
#' @return tibble: id, name, description, category, type, updated_at,
#'   row_count, column_count, download_count
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
#' Runs a SoQL query against a specific dataset via the SODA 2.0 API.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "m3tr-qhgy")
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param group Optional SoQL GROUP BY clause
#' @param order Optional SoQL ORDER BY clause
#' @param q Optional full-text search within the dataset
#' @param limit Number of rows to return (default 1000, max 50000)
#' @param offset Offset for pagination (default 0)
#' @return tibble with columns from the dataset
iowa_query <- function(dataset_id, where = NULL, select = NULL,
                       group = NULL, order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}

#' Fetch all records with auto-pagination
#'
#' @param dataset_id Socrata dataset identifier
#' @param where Optional SoQL WHERE clause
#' @param max_rows Maximum total rows to fetch (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble of all matching records
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
#' @param dataset_id Socrata dataset identifier
#' @param where Optional SoQL WHERE clause to filter
#' @return integer: total number of matching rows
iowa_count <- function(dataset_id, where = NULL) {
  result <- ._soda_query(dataset_id, select = "count(*) as n", where = where, limit = 1)
  if (nrow(result) == 0) return(NA_integer_)
  as.integer(result$n[1])
}

# == Named convenience functions ===============================================

#' Query Iowa liquor sales
#'
#' Queries the Iowa Liquor Sales dataset (m3tr-qhgy) -- one of the most
#' popular open datasets in the US. Contains every wholesale liquor purchase
#' by Iowa Class "E" liquor licensees.
#'
#' @param city Optional city filter (e.g. "DES MOINES", "CEDAR RAPIDS")
#' @param county Optional county filter (e.g. "POLK", "LINN")
#' @param category_name Optional category filter (e.g. "VODKA", "WHISKEY")
#' @param start_date Optional start date (format "YYYY-MM-DD")
#' @param end_date Optional end date (format "YYYY-MM-DD")
#' @param limit Number of rows (default 1000)
#' @return tibble: invoice_line_no, date, store, name, city, county,
#'   category_name, item_description, bottles_sold, sale_dollars, etc.
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
#' Queries the Iowa Liquor Stores dataset (ykb6-ywnd).
#'
#' @param city Optional city filter
#' @param limit Number of rows (default 1000)
#' @return tibble: store, name, store_status, address, city, state, zipcode
iowa_liquor_stores <- function(city = NULL, limit = 1000) {
  where <- if (!is.null(city)) sprintf("upper(city)='%s'", toupper(city)) else NULL
  ._soda_query("ykb6-ywnd", where = where, limit = limit)
}

#' Query Iowa state revenue
#'
#' Queries the State of Iowa Revenue dataset (urps-v5ck).
#' Revenue summarized by fiscal year, month, fund, department.
#'
#' @param fiscal_year Optional fiscal year filter (e.g. 2024)
#' @param department Optional department name filter (partial match)
#' @param limit Number of rows (default 1000)
#' @return tibble of state revenue records
iowa_revenue <- function(fiscal_year = NULL, department = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  if (!is.null(department))  clauses <- c(clauses, sprintf("upper(department) LIKE upper('%%%s%%')", department))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("urps-v5ck", where = where, order = "fiscal_year DESC", limit = limit)
}

#' Query Iowa 511 road conditions
#'
#' Queries the Iowa 511 dataset (yata-4k7n) with real-time road events
#' from the Iowa DOT. Updated every 10 minutes.
#'
#' @param limit Number of rows (default 1000)
#' @return tibble of current road events
iowa_511 <- function(limit = 1000) {
  ._soda_query("yata-4k7n", limit = limit)
}

#' Query Iowa unemployment insurance claims (weekly)
#'
#' Queries the Iowa Unemployment Insurance Claims Data dataset.
#'
#' @param limit Number of rows (default 1000)
#' @return tibble of weekly unemployment insurance claims
iowa_unemployment <- function(limit = 1000) {
  # Weekly claims dataset
  ._soda_query("nqiw-f9td", order = ":id DESC", limit = limit)
}

#' Query Iowa state employee travel reimbursements
#'
#' Queries the State of Iowa Executive Branch Out-of-State Travel
#' Reimbursements dataset (vtga-efxt).
#'
#' @param department Optional department filter (partial match)
#' @param fiscal_year Optional fiscal year filter
#' @param limit Number of rows (default 1000)
#' @return tibble of travel reimbursement records
iowa_travel <- function(department = NULL, fiscal_year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(department))  clauses <- c(clauses, sprintf("upper(department) LIKE upper('%%%s%%')", department))
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query("vtga-efxt", where = where, order = ":id DESC", limit = limit)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the Iowa Open Data package
#'
#' @return Character string (invisibly), also printed
iowa_context <- function() {
  .build_context("iowa.gov", header_lines = c(
    "# iowa.gov - State of Iowa Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Popular datasets:",
    "#   m3tr-qhgy = Iowa Liquor Sales (famous dataset)",
    "#   ykb6-ywnd = Iowa Liquor Stores",
    "#   urps-v5ck = State of Iowa Revenue",
    "#   yata-4k7n = Iowa 511 Road Conditions",
    "#   nqiw-f9td = Unemployment Insurance Claims (Weekly)",
    "#   vtga-efxt = State Employee Travel Reimbursements",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering",
    "# Workflow: iowa_search() -> iowa_view() -> iowa_query() or use named helpers"
  ))
}
