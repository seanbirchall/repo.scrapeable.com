# austintexas.gov.R
# Self-contained City of Austin Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.austintexas.gov
# Datasets: ~1400 Socrata views covering crime, permits, 311, traffic, etc.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.atx_base <- "https://data.austintexas.gov"

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
  url <- sprintf("%s/resource/%s.json?%s", .atx_base, dataset_id,
                 utils::URLencode(query_str, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Austin Open Data query error: ", e$message)
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

# == Discovery =================================================================

#' List City of Austin Open Data datasets
#'
#' Browse the catalog of datasets on data.austintexas.gov.
#'
#' @param limit Number of datasets to return (default 50)
#' @param page Page number (default 1)
#' @return tibble: id, name, category, description, updated_at, type
#' @export
atx_list <- function(limit = 50, page = 1) {
  offset <- (page - 1) * limit
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.austintexas.gov&search_context=data.austintexas.gov&limit=%d&offset=%d",
    .atx_base, limit, offset
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Austin catalog error: ", e$message)
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


#' Search City of Austin Open Data datasets
#'
#' Full-text search across the data.austintexas.gov catalog.
#'
#' @param query Search terms (e.g. "crime", "permit", "traffic")
#' @param limit Max results (default 20)
#' @param only Filter by asset type: "datasets", "maps", "charts" (optional)
#' @return tibble: id, name, category, description, updated_at, type
#' @export
atx_search <- function(query, limit = 20, only = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.austintexas.gov&search_context=data.austintexas.gov&q=%s&limit=%d",
    .atx_base, utils::URLencode(query, reserved = TRUE), limit
  )
  if (!is.null(only)) url <- paste0(url, "&only=", only)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Austin search error: ", e$message)
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


#' View metadata and schema for an Austin dataset
#'
#' Returns column names, types, and descriptions for a Socrata view.
#'
#' @param dataset_id Four-by-four Socrata view ID (e.g. "fdj4-gpfu")
#' @return tibble: field_name, data_type, description
#' @export
atx_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .atx_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Austin view metadata error: ", e$message)
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

#' Query any City of Austin Open Data dataset
#'
#' Runs a SoQL query against any Socrata dataset on data.austintexas.gov.
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Max rows to return (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
atx_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL, q = NULL,
                      limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from an Austin dataset with auto-pagination
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param order Optional SoQL ORDER BY clause
#' @param max_rows Maximum total rows to fetch (default 50000)
#' @param page_size Rows per request (default 10000)
#' @return tibble of all matching records
#' @export
atx_fetch_all <- function(dataset_id, where = NULL, select = NULL,
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


#' Get row count for any Austin dataset
#'
#' @param dataset_id Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @return integer row count
#' @export
atx_count <- function(dataset_id, where = NULL) {
  ._soda_query(dataset_id, select = "count(*) as n", where = where) |>
    pull("n") |>
    as.integer()
}


# == Named convenience functions ===============================================

#' Fetch Austin crime reports
#'
#' Queries dataset fdj4-gpfu (Crime Reports).
#'
#' @param crime_type Crime type filter (e.g. "THEFT", "BURGLARY")
#' @param district APD district filter (e.g. "1", "2")
#' @param family_violence Family violence flag ("Y" or "N")
#' @param limit Max rows (default 1000)
#' @return tibble of crime reports
#' @export
atx_crimes <- function(crime_type = NULL, district = NULL,
                       family_violence = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(crime_type))       clauses <- c(clauses, sprintf("upper(crime_type) LIKE '%%%s%%'", toupper(crime_type)))
  if (!is.null(district))         clauses <- c(clauses, sprintf("district='%s'", district))
  if (!is.null(family_violence))  clauses <- c(clauses, sprintf("family_violence='%s'", family_violence))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("fdj4-gpfu", where = where, order = "occ_date DESC", limit = limit)
}


#' Fetch Austin 311 service requests
#'
#' Queries dataset xwdj-i9he (Austin 311 Public Data).
#'
#' @param sr_type Service request type description filter (partial match)
#' @param department Department description filter
#' @param status Status description filter (e.g. "Closed", "Open")
#' @param zip_code ZIP code filter
#' @param limit Max rows (default 1000)
#' @return tibble of 311 service requests
#' @export
atx_311 <- function(sr_type = NULL, department = NULL, status = NULL,
                    zip_code = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(sr_type))    clauses <- c(clauses, sprintf("upper(sr_type_desc) LIKE '%%%s%%'", toupper(sr_type)))
  if (!is.null(department)) clauses <- c(clauses, sprintf("upper(sr_department_desc) LIKE '%%%s%%'", toupper(department)))
  if (!is.null(status))     clauses <- c(clauses, sprintf("sr_status_desc='%s'", status))
  if (!is.null(zip_code))   clauses <- c(clauses, sprintf("sr_location_zip_code='%s'", zip_code))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("xwdj-i9he", where = where, order = "sr_created_date DESC", limit = limit)
}


#' Fetch Austin food establishment inspection scores
#'
#' Queries dataset ecmv-9xxi (Food Establishment Inspection Scores).
#'
#' @param restaurant_name Restaurant name filter (partial match)
#' @param zip_code ZIP code filter
#' @param min_score Minimum inspection score filter
#' @param limit Max rows (default 1000)
#' @return tibble of food inspection results
#' @export
atx_food_inspections <- function(restaurant_name = NULL, zip_code = NULL,
                                 min_score = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(restaurant_name)) clauses <- c(clauses, sprintf("upper(restaurant_name) LIKE '%%%s%%'", toupper(restaurant_name)))
  if (!is.null(zip_code))        clauses <- c(clauses, sprintf("zip_code LIKE '%s%%'", zip_code))
  if (!is.null(min_score))       clauses <- c(clauses, sprintf("score >= %s", min_score))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("ecmv-9xxi", where = where, order = "inspection_date DESC", limit = limit)
  if (nrow(df) > 0 && "score" %in% names(df)) {
    df$score <- as.numeric(df$score)
  }
  df
}


#' Fetch Austin traffic incident reports
#'
#' Queries dataset dx9v-zd7x (Real-Time Traffic Incident Reports).
#'
#' @param issue Issue reported filter (e.g. "Crash", "Stalled Vehicle")
#' @param status Status filter (e.g. "ACTIVE", "ARCHIVED")
#' @param limit Max rows (default 1000)
#' @return tibble of traffic incidents with lat/long
#' @export
atx_traffic_incidents <- function(issue = NULL, status = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(issue))  clauses <- c(clauses, sprintf("upper(issue_reported) LIKE '%%%s%%'", toupper(issue)))
  if (!is.null(status)) clauses <- c(clauses, sprintf("traffic_report_status='%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("dx9v-zd7x", where = where, order = "published_date DESC", limit = limit)
  if (nrow(df) > 0) {
    if ("latitude" %in% names(df))  df$latitude  <- as.numeric(df$latitude)
    if ("longitude" %in% names(df)) df$longitude <- as.numeric(df$longitude)
  }
  df
}


#' Fetch Austin building permits
#'
#' Queries dataset 3z4i-4ta5 (Issued Building Permits).
#'
#' @param permit_type Permit type filter
#' @param status Status filter
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble of building permits
#' @export
atx_building_permits <- function(permit_type = NULL, status = NULL,
                                 q = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(permit_type)) clauses <- c(clauses, sprintf("upper(permit_type) LIKE '%%%s%%'", toupper(permit_type)))
  if (!is.null(status))      clauses <- c(clauses, sprintf("upper(status) LIKE '%%%s%%'", toupper(status)))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("3z4i-4ta5", where = where, q = q, limit = limit)
}


#' Fetch Austin construction permits
#'
#' Queries dataset 3syk-w9eu (Issued Construction Permits).
#'
#' @param permit_type Permit type filter
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble of construction permits
#' @export
atx_construction_permits <- function(permit_type = NULL, q = NULL,
                                     limit = 1000) {
  clauses <- character()
  if (!is.null(permit_type)) clauses <- c(clauses, sprintf("upper(permit_type) LIKE '%%%s%%'", toupper(permit_type)))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("3syk-w9eu", where = where, q = q, limit = limit)
}


#' Fetch Austin traffic camera counts
#'
#' Queries dataset sh59-i6y9 (Camera Traffic Counts).
#'
#' @param q Full-text search for location
#' @param limit Max rows (default 1000)
#' @return tibble of traffic camera count data
#' @export
atx_traffic_cameras <- function(q = NULL, limit = 1000) {
  ._soda_query("sh59-i6y9", q = q, limit = limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the Austin Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
atx_context <- function() {
  .build_context("austintexas.gov", header_lines = c(
    "# austintexas.gov - City of Austin Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Popular datasets (use 4x4 IDs with atx_query):",
    "#   fdj4-gpfu = Crime Reports",
    "#   xwdj-i9he = Austin 311 Public Data",
    "#   ecmv-9xxi = Food Establishment Inspection Scores",
    "#   dx9v-zd7x = Real-Time Traffic Incident Reports",
    "#   3z4i-4ta5 = Issued Building Permits",
    "#   3syk-w9eu = Issued Construction Permits",
    "#   sh59-i6y9 = Camera Traffic Counts",
    "#   b4k4-adkb = Traffic Cameras",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering"
  ))
}
