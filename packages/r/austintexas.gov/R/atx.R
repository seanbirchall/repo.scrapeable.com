# austintexas.gov.R
# Self-contained City of Austin Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.austintexas.gov
# Datasets: ~1400 Socrata views covering crime, permits, 311, traffic, etc.


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
#' Browse the catalog of ~1,400 datasets on data.austintexas.gov, covering
#' crime, permits, 311 requests, traffic, food inspections, and more.
#' Results are paginated; use \code{page} to step through.
#'
#' @param limit Integer. Number of datasets to return per page (default 50).
#'   Example: \code{limit = 10}
#' @param page Integer. Page number for pagination, starting at 1 (default 1).
#'   Example: \code{page = 2}
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata four-by-four dataset identifier (e.g. "3syk-w9eu").}
#'     \item{name}{Character. Human-readable dataset name (e.g. "Issued Construction Permits").}
#'     \item{category}{Character. Domain category (e.g. "Building and Development", "Transportation").}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{updated_at}{Character. ISO 8601 timestamp of last update.}
#'     \item{type}{Character. Asset type (e.g. "dataset", "map", "filter").}
#'   }
#' @examples
#' # List the first 10 datasets
#' atx_list(limit = 10)
#'
#' # Page through results
#' atx_list(limit = 20, page = 3)
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
#' Full-text search across the data.austintexas.gov catalog. Matches dataset
#' names, descriptions, and categories. Returns the same schema as
#' \code{\link{atx_list}}.
#'
#' @param query Character. Search terms to match against datasets.
#'   Example: \code{"crime"}, \code{"traffic"}, \code{"food inspection"}
#' @param limit Integer. Max results to return (default 20).
#'   Example: \code{limit = 5}
#' @param only Character or NULL. Filter by asset type: \code{"datasets"},
#'   \code{"maps"}, or \code{"charts"}. Default NULL returns all types.
#'   Example: \code{only = "datasets"}
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata four-by-four ID (e.g. "fdj4-gpfu").}
#'     \item{name}{Character. Dataset name (e.g. "Crime Reports").}
#'     \item{category}{Character. Domain category (e.g. "Public Safety").}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{updated_at}{Character. ISO 8601 timestamp of last update.}
#'     \item{type}{Character. Asset type (e.g. "dataset", "filter", "story").}
#'   }
#' @examples
#' atx_search("crime", limit = 5)
#' atx_search("permit", only = "datasets")
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
#' Returns column names, data types, and descriptions for a given Socrata
#' dataset. Useful for understanding what fields are available before
#' querying with \code{\link{atx_query}}.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID.
#'   Example: \code{"fdj4-gpfu"} (Crime Reports),
#'   \code{"ecmv-9xxi"} (Food Inspections)
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{Character. Column name (e.g. "crime_type", "occ_date").}
#'     \item{data_type}{Character. Socrata data type (e.g. "text", "number", "calendar_date").}
#'     \item{description}{Character. Human-readable column description, or NA.}
#'   }
#' @examples
#' # View schema for the Crime Reports dataset
#' atx_view("fdj4-gpfu")
#'
#' # View schema for the Food Inspections dataset
#' atx_view("ecmv-9xxi")
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
#' Runs a SoQL (Socrata Query Language) query against any dataset on
#' data.austintexas.gov. Supports filtering, selection, grouping,
#' ordering, full-text search, and pagination. Columns vary by dataset.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID.
#'   Example: \code{"fdj4-gpfu"} (Crime Reports)
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#'   Example: \code{"crime_type='THEFT'"}
#' @param select Character or NULL. SoQL SELECT clause for column selection.
#'   Example: \code{"crime_type, occ_date, district"}
#' @param group Character or NULL. SoQL GROUP BY clause.
#'   Example: \code{"crime_type"}
#' @param order Character or NULL. SoQL ORDER BY clause.
#'   Example: \code{"occ_date DESC"}
#' @param q Character or NULL. Full-text search query across all fields.
#'   Example: \code{"downtown"}
#' @param limit Integer. Max rows to return (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble whose columns depend on the dataset queried.
#' @examples
#' # Get 10 recent crime reports
#' atx_query("fdj4-gpfu", order = "occ_date DESC", limit = 10)
#'
#' # Count crimes by type
#' atx_query("fdj4-gpfu", select = "crime_type, count(*) as n",
#'           group = "crime_type", order = "n DESC", limit = 10)
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
#' Automatically pages through a Socrata dataset, fetching up to
#' \code{max_rows} total records. Useful for downloading complete datasets
#' that exceed the single-request limit of 50,000 rows.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID.
#'   Example: \code{"ecmv-9xxi"} (Food Inspections)
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"score >= 90"}
#' @param select Character or NULL. SoQL SELECT clause.
#' @param order Character or NULL. SoQL ORDER BY clause.
#' @param max_rows Integer. Maximum total rows to fetch (default 50000).
#' @param page_size Integer. Rows per API request (default 10000).
#' @return A tibble of all matching records, with columns depending on the dataset.
#' @examples
#' # Download all food inspection records with score >= 90
#' atx_fetch_all("ecmv-9xxi", where = "score >= 90", max_rows = 5000)
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
#' Returns the total number of rows in a Socrata dataset, optionally
#' filtered by a WHERE clause. Useful for checking dataset size before
#' downloading with \code{\link{atx_fetch_all}}.
#'
#' @param dataset_id Character. Socrata four-by-four view ID.
#'   Example: \code{"fdj4-gpfu"} (Crime Reports -- returns ~2.6 million)
#' @param where Character or NULL. SoQL WHERE clause to count a filtered subset.
#'   Example: \code{"crime_type='THEFT'"}
#' @return Integer. Total row count.
#' @examples
#' # Total crime reports (returns ~2,630,000)
#' atx_count("fdj4-gpfu")
#'
#' # Count thefts only
#' atx_count("fdj4-gpfu", where = "crime_type='THEFT'")
#' @export
atx_count <- function(dataset_id, where = NULL) {
  ._soda_query(dataset_id, select = "count(*) as n", where = where) |>
    pull("n") |>
    as.integer()
}


# == Named convenience functions ===============================================

#' Fetch Austin crime reports
#'
#' Queries the Austin Police Department Crime Reports dataset (fdj4-gpfu),
#' which contains ~2.6 million incident records. Results are ordered by
#' occurrence date descending (most recent first).
#'
#' @param crime_type Character or NULL. Crime type filter, partial match
#'   (case-insensitive). Example: \code{"THEFT"}, \code{"BURGLARY"},
#'   \code{"TERRORISTIC THREAT"}
#' @param district Character or NULL. APD district number.
#'   Example: \code{"1"}, \code{"2"}
#' @param family_violence Character or NULL. Family violence flag.
#'   \code{"Y"} or \code{"N"}.
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{incident_report_number}{Character. Report number.}
#'     \item{crime_type}{Character. Crime description (e.g. "THEFT", "POSS OF DRUG PARAPHERNALIA").}
#'     \item{ucr_code}{Character. Uniform Crime Reporting code.}
#'     \item{family_violence}{Character. "Y" or "N".}
#'     \item{occ_date_time}{Character. Occurrence date/time string.}
#'     \item{occ_date}{Character. Occurrence date.}
#'     \item{location_type}{Character. Location type description.}
#'     \item{council_district}{Character. City council district number.}
#'     \item{sector, district}{Character. APD sector and district.}
#'   }
#' @examples
#' atx_crimes(limit = 10)
#' atx_crimes(crime_type = "THEFT", limit = 5)
#' atx_crimes(district = "1", family_violence = "N", limit = 20)
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
#' Queries the Austin 311 Public Data dataset (xwdj-i9he), which contains
#' citizen requests for city services. Results are ordered by creation date
#' descending. Covers requests like short-term rental complaints, speed
#' management, outdoor commercial activity, and more.
#'
#' @param sr_type Character or NULL. Service request type description
#'   (partial match, case-insensitive).
#'   Example: \code{"Speed Management"}, \code{"Short Term Rental"}
#' @param department Character or NULL. Department description filter.
#'   Example: \code{"Austin Transportation"}, \code{"Austin Development"}
#' @param status Character or NULL. Status description.
#'   Example: \code{"Closed"}, \code{"Open"}
#' @param zip_code Character or NULL. ZIP code filter.
#'   Example: \code{"78701"}
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with 22 columns including:
#'   \describe{
#'     \item{sr_number}{Character. Service request number (e.g. "26-00101874").}
#'     \item{sr_type_desc}{Character. Request type (e.g. "TPW - Speed Management").}
#'     \item{sr_department_desc}{Character. Responsible department.}
#'     \item{sr_status_desc}{Character. Current status.}
#'     \item{sr_created_date}{Character. Date/time the request was created.}
#'     \item{sr_location}{Character. Full address.}
#'     \item{sr_location_zip_code}{Character. ZIP code.}
#'     \item{sr_location_lat, sr_location_long}{Character. Coordinates.}
#'   }
#' @examples
#' atx_311(limit = 10)
#' atx_311(sr_type = "Speed Management", limit = 5)
#' atx_311(zip_code = "78701", status = "Open", limit = 20)
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
#' Queries the Food Establishment Inspection Scores dataset (ecmv-9xxi).
#' Results are ordered by inspection date descending. The \code{score}
#' column is converted to numeric.
#'
#' @param restaurant_name Character or NULL. Restaurant name filter
#'   (partial match, case-insensitive).
#'   Example: \code{"Chili's"}, \code{"Whataburger"}
#' @param zip_code Character or NULL. ZIP code prefix filter.
#'   Example: \code{"78701"}, \code{"78723"}
#' @param min_score Numeric or NULL. Minimum inspection score (0--100).
#'   Example: \code{90}
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with columns:
#'   \describe{
#'     \item{restaurant_name}{Character. Establishment name.}
#'     \item{zip_code}{Character. ZIP code.}
#'     \item{inspection_date}{Character. ISO 8601 date of inspection.}
#'     \item{score}{Numeric. Inspection score (0--100).}
#'     \item{address}{Character. Street address.}
#'     \item{facility_id}{Character. Facility identifier.}
#'     \item{process_description}{Character. Type of inspection.}
#'   }
#' @examples
#' atx_food_inspections(limit = 10)
#' atx_food_inspections(min_score = 90, limit = 20)
#' atx_food_inspections(restaurant_name = "Whataburger", limit = 5)
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
#' Queries the Real-Time Traffic Incident Reports dataset (dx9v-zd7x).
#' Includes crash reports, stalled vehicles, traffic hazards, and more.
#' The \code{latitude} and \code{longitude} columns are converted to numeric.
#'
#' @param issue Character or NULL. Issue reported filter (partial match,
#'   case-insensitive).
#'   Example: \code{"Crash"}, \code{"Stalled Vehicle"}, \code{"Traffic Hazard"}
#' @param status Character or NULL. Report status filter.
#'   Example: \code{"ACTIVE"}, \code{"ARCHIVED"}
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{traffic_report_id}{Character. Unique report identifier.}
#'     \item{published_date}{Character. ISO 8601 date/time published.}
#'     \item{issue_reported}{Character. Type of incident (e.g. "Crash Service", "Traffic Hazard").}
#'     \item{latitude}{Numeric. Latitude coordinate.}
#'     \item{longitude}{Numeric. Longitude coordinate.}
#'     \item{address}{Character. Location description.}
#'     \item{traffic_report_status}{Character. Current status.}
#'     \item{agency}{Character. Responding agency.}
#'   }
#' @examples
#' atx_traffic_incidents(limit = 10)
#' atx_traffic_incidents(issue = "Crash", limit = 20)
#' atx_traffic_incidents(status = "ACTIVE", limit = 5)
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
#' Queries the Issued Building Permits dataset (3z4i-4ta5). Note: this
#' dataset may be empty or deprecated; consider using
#' \code{\link{atx_construction_permits}} for active permit data.
#'
#' @param permit_type Character or NULL. Permit type filter (partial match,
#'   case-insensitive). Example: \code{"Building"}, \code{"Demolition"}
#' @param status Character or NULL. Status filter (partial match).
#'   Example: \code{"Issued"}, \code{"Expired"}
#' @param q Character or NULL. Full-text search across all fields.
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of building permit records. Columns vary by dataset.
#' @examples
#' atx_building_permits(limit = 10)
#' atx_building_permits(q = "downtown", limit = 20)
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
#' Queries the Issued Construction Permits dataset (3syk-w9eu), which
#' contains electrical, plumbing, mechanical, and building permits. Each
#' record includes project details, valuation, and issue/expiration dates.
#'
#' @param permit_type Character or NULL. Permit type filter (partial match,
#'   case-insensitive).
#'   Example: \code{"EP"} (Electrical), \code{"PP"} (Plumbing),
#'   \code{"MP"} (Mechanical)
#' @param q Character or NULL. Full-text search across all fields.
#'   Example: \code{"solar"}, \code{"remodel"}
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with ~57 columns including:
#'   \describe{
#'     \item{permittype}{Character. Permit code (e.g. "EP", "PP").}
#'     \item{permit_type_desc}{Character. Full permit type name.}
#'     \item{permit_number}{Character. Permit tracking number.}
#'     \item{permit_class_mapped}{Character. "Commercial" or "Residential".}
#'     \item{work_class}{Character. Type of work.}
#'     \item{description}{Character. Project description.}
#'     \item{applieddate, issue_date, expiresdate}{Character. Key dates.}
#'     \item{status_current}{Character. Current permit status.}
#'     \item{original_address1}{Character. Property address.}
#'   }
#' @examples
#' atx_construction_permits(limit = 10)
#' atx_construction_permits(permit_type = "EP", limit = 5)
#' atx_construction_permits(q = "solar", limit = 20)
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
#' Queries the Camera Traffic Counts dataset (sh59-i6y9), which records
#' vehicle volume and speed data from intersection traffic cameras.
#' Each row is one directional movement at an intersection for a time bin.
#'
#' @param q Character or NULL. Full-text search for intersection or location.
#'   Example: \code{"Congress"}, \code{"Lamar"}
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with ~19 columns including:
#'   \describe{
#'     \item{record_id}{Character. Unique record identifier.}
#'     \item{atd_device_id}{Character. Camera device ID.}
#'     \item{read_date}{Character. Date of the traffic reading.}
#'     \item{intersection_name}{Character. Intersection location (e.g. "WEST GATE BLVD / BEN WHITE BLVD").}
#'     \item{direction}{Character. Direction of travel (e.g. "EASTBOUND", "NORTHBOUND").}
#'     \item{movement}{Character. Movement type (e.g. "THRU", "LEFT TURN").}
#'     \item{volume}{Character. Vehicle count.}
#'     \item{speed_average}{Character. Average speed.}
#'     \item{day_of_week}{Character. Day of the week.}
#'   }
#' @examples
#' atx_traffic_cameras(limit = 10)
#' atx_traffic_cameras(q = "Congress", limit = 20)
#' @export
atx_traffic_cameras <- function(q = NULL, limit = 1000) {
  ._soda_query("sh59-i6y9", q = q, limit = limit)
}


# == Context ===================================================================

#' Get austintexas.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
atx_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(atx_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/austintexas.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "austintexas.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# austintexas.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# austintexas.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
