# somervillema.gov.R - Self-contained City of Somerville Open Data (Socrata) client
#
# Socrata SODA 2.0 portal at data.somervillema.gov
# 46 datasets (27 queryable Socrata views)
# No auth required. Rate limits: ~1000 req/hour without app token.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.somer_base <- "https://data.somervillema.gov"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- SODA query builder -------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL,
                         order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query_str <- paste(
    names(params),
    utils::URLencode(as.character(params), reserved = TRUE),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .somer_base, view_id, query_str)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# -- SODA paginator (fetches all rows up to max_rows) -------------------------

._soda_all <- function(view_id, where = NULL, select = NULL,
                       order = NULL, q = NULL,
                       page_size = 5000, max_rows = 50000) {
  collected <- list()
  offset <- 0
  repeat {
    lim <- min(page_size, max_rows - offset)
    if (lim <= 0) break
    chunk <- ._soda_query(view_id, where = where, select = select,
                          order = order, q = q,
                          limit = lim, offset = offset)
    if (nrow(chunk) == 0) break
    collected[[length(collected) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(collected) == 0) return(tibble::tibble())
  dplyr::bind_rows(collected)
}


# == Schemas ===================================================================

.schema_catalog <- tibble::tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

.schema_cad <- tibble::tibble(
  incnum = character(), day_and_month = character(), year = integer(),
  police_shift = character(), inctype = character(), incdesc = character(),
  ward = character()
)

.schema_crimes <- tibble::tibble(
  incnum = character(), day_and_month = character(), year = integer(),
  police_shift = character(), offense = character(), offensetype = character(),
  category = character(), ward = character()
)

.schema_311 <- tibble::tibble(
  id = character(), classification = character(), category = character(),
  type = character(), most_recent_status = character(),
  date_created = as.POSIXct(character())
)

.schema_permits <- tibble::tibble(
  id = character(), type = character(), status = character(),
  address = character(), work = character(),
  application_date = as.POSIXct(character()),
  issue_date = as.POSIXct(character()), amount = numeric()
)

.schema_citations <- tibble::tibble(
  citationnum = character(), dtissued = as.POSIXct(character()),
  address = character(), chgdesc = character(), warning = character(),
  ward = character()
)


# == Dataset discovery =========================================================

#' List available datasets on Somerville Open Data
#'
#' Returns a catalog of all datasets from the City of Somerville, MA open
#' data portal (data.somervillema.gov, Socrata). Includes dataset IDs for
#' use with \code{\link{somer_view}} and other query functions.
#'
#' @param limit Integer. Number of datasets to return (default 100, max 200).
#' @param category Character or NULL. Optional category filter using
#'   case-insensitive substring matching (e.g. \code{"Public Safety"},
#'   \code{"Finance"}).
#' @return A tibble with one row per dataset:
#' \describe{
#'   \item{id}{Character. Socrata view ID for use with query functions.}
#'   \item{name}{Character. Dataset title.}
#'   \item{description}{Character. Dataset description.}
#'   \item{category}{Character. Dataset category.}
#'   \item{type}{Character. Display type (table, chart, map, etc.).}
#'   \item{updated_at}{POSIXct. Last modification timestamp.}
#'   \item{view_count}{Integer. Number of views.}
#' }
#' @export
#' @examples
#' \dontrun{
#' somer_list(limit = 20)
#' somer_list(category = "Public Safety")
#' }
somer_list <- function(limit = 100, category = NULL) {
  cat_filter <- category
  url <- sprintf("%s/api/views?limit=%d", .somer_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_catalog)

  df <- as_tibble(raw)
  if (!"description" %in% names(df)) df$description <- NA_character_
  if (!"category" %in% names(df))    df$category <- NA_character_
  if (!"displayType" %in% names(df)) df$displayType <- NA_character_
  result <- df |>
    transmute(
      id         = as.character(id),
      name       = as.character(name),
      description = as.character(.data$description),
      category   = as.character(.data$category),
      type       = as.character(displayType),
      updated_at = as.POSIXct(as.numeric(viewLastModified %||% NA),
                              origin = "1970-01-01", tz = "UTC"),
      view_count = as.integer(viewCount %||% NA)
    )

  if (!is.null(cat_filter)) {
    result <- result |> dplyr::filter(grepl(cat_filter, .data$category, ignore.case = TRUE))
  }
  result
}

#' Search Somerville datasets by keyword
#'
#' Full-text search across dataset names and descriptions on the Somerville
#' open data portal.
#'
#' @param query Character. Search string (e.g. \code{"crime"}, \code{"permit"}).
#' @param limit Integer. Maximum results to return (default 50).
#' @return A tibble with the same columns as \code{\link{somer_list}}.
#' @export
#' @examples
#' \dontrun{
#' somer_search("crime")
#' somer_search("budget")
#' }
somer_search <- function(query, limit = 50) {
  url <- sprintf("%s/api/views?limit=%d&q=%s",
                 .somer_base, limit,
                 utils::URLencode(query, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_catalog)

  df <- as_tibble(raw)
  if (!"description" %in% names(df)) df$description <- NA_character_
  if (!"category" %in% names(df))    df$category <- NA_character_
  if (!"displayType" %in% names(df)) df$displayType <- NA_character_
  df |>
    transmute(
      id         = as.character(id),
      name       = as.character(name),
      description = as.character(.data$description),
      category   = as.character(.data$category),
      type       = as.character(displayType),
      updated_at = as.POSIXct(as.numeric(viewLastModified %||% NA),
                              origin = "1970-01-01", tz = "UTC"),
      view_count = as.integer(viewCount %||% NA)
    )
}


# == Generic SODA query ========================================================

#' Query any Somerville dataset by view ID
#'
#' Runs a SoQL query against any dataset on the Somerville open data portal.
#' This is the generic query function -- use it when no convenience wrapper
#' exists for the dataset you need.
#'
#' @param view_id Character. Socrata view ID (e.g. \code{"mdb2-mgc7"}).
#'   Find IDs via \code{\link{somer_list}} or \code{\link{somer_search}}.
#' @param where Character or NULL. SoQL WHERE clause (e.g.
#'   \code{"year = '2024'"}).
#' @param select Character or NULL. SoQL SELECT clause.
#' @param order Character or NULL. SoQL ORDER BY clause.
#' @param q Character or NULL. Full-text search query across all text fields.
#' @param limit Integer. Rows to return (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble. Columns vary by dataset.
#' @export
#' @examples
#' \dontrun{
#' somer_view("mdb2-mgc7", limit = 10)
#' }
somer_view <- function(view_id, where = NULL, select = NULL,
                       order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select,
               order = order, q = q, limit = limit, offset = offset)
}

#' Get metadata for a Somerville dataset
#'
#' Fetches dataset-level metadata including name, description, row count,
#' and column count for a Socrata view on the Somerville portal.
#'
#' @param view_id Character. Socrata view ID (e.g. \code{"mdb2-mgc7"}).
#' @return A tibble with one row:
#' \describe{
#'   \item{id}{Character. View ID.}
#'   \item{name}{Character. Dataset name.}
#'   \item{description}{Character. Dataset description.}
#'   \item{category}{Character. Dataset category.}
#'   \item{row_count}{Integer. Number of rows.}
#'   \item{column_count}{Integer. Number of columns.}
#' }
#' @export
#' @examples
#' \dontrun{
#' somer_metadata("mdb2-mgc7")
#' }
somer_metadata <- function(view_id) {
  url <- sprintf("%s/api/views/%s.json", .somer_base, view_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$id)) {
    return(tibble(id = character(), name = character(), description = character(),
                  category = character(), row_count = integer(), column_count = integer()))
  }
  tibble(
    id           = as.character(raw$id %||% NA_character_),
    name         = as.character(raw$name %||% NA_character_),
    description  = as.character(raw$description %||% NA_character_),
    category     = as.character(raw$category %||% NA_character_),
    row_count    = as.integer(raw$rowCount %||% NA_integer_),
    column_count = as.integer(length(raw$columns %||% list()))
  )
}

#' Get row count for a Somerville dataset
#'
#' Efficiently counts rows using SoQL \code{count(*)} without downloading data.
#'
#' @param view_id Character. Socrata view ID.
#' @param where Character or NULL. Optional SoQL WHERE filter.
#' @return Integer. Row count (or \code{NA_integer_} on error).
#' @export
#' @examples
#' \dontrun{
#' somer_count("mdb2-mgc7")
#' somer_count("mdb2-mgc7", where = "year = '2024'")
#' }
somer_count <- function(view_id, where = NULL) {
  params <- list(`$select` = "count(*)")
  if (!is.null(where)) params[["$where"]] <- where
  query_str <- paste(names(params),
                     utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .somer_base, view_id, query_str)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(NA_integer_)
  as.integer(raw[[1]][[1]])
}


# == Police CAD (Computer Aided Dispatch) ======================================

#' Query Somerville Police CAD data (2017-present)
#'
#' Returns calls for service from the Computer Aided Dispatch (CAD) system
#' of the Somerville Police Department. Each row is a dispatched incident.
#' Dataset view ID: \code{mdb2-mgc7}.
#'
#' @param year Character or integer or NULL. Filter by year (e.g. \code{"2024"}).
#' @param ward Character or NULL. Filter by ward number (e.g. \code{"1"}).
#' @param inctype Character or NULL. Filter by incident type (e.g.
#'   \code{"ID THEFT"}, \code{"DISTURBANCE"}).
#' @param q Character or NULL. Full-text search across all fields.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including incnum, day_and_month, year,
#'   police_shift, inctype, incdesc, incdefex, blockcode, ward.
#' @export
#' @examples
#' \dontrun{
#' somer_cad(year = "2024", limit = 50)
#' somer_cad(inctype = "DISTURBANCE", limit = 20)
#' }
somer_cad <- function(year = NULL, ward = NULL, inctype = NULL,
                      q = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(year))    clauses <- c(clauses, sprintf("year = '%s'", year))
  if (!is.null(ward))    clauses <- c(clauses, sprintf("ward = '%s'", ward))
  if (!is.null(inctype)) clauses <- c(clauses, sprintf("inctype = '%s'", inctype))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("mdb2-mgc7", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_cad)
  df |>
    mutate(
      year = as.integer(year),
      ward = trimws(as.character(ward %||% NA_character_))
    )
}


# == Crime Reports =============================================================

#' Query Somerville crime reports (2017-present)
#'
#' Incident-level crime data from Somerville PD records management system.
#' Each row is a crime report with offense classification and location.
#' Dataset view ID: \code{aghs-hqvg}.
#'
#' @param year Character or integer or NULL. Filter by year.
#' @param ward Character or NULL. Filter by ward number.
#' @param category Character or NULL. Filter by crime category (e.g.
#'   \code{"Crimes against Person"}, \code{"Crimes against Property"}).
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including incnum, day_and_month, year,
#'   police_shift, offense, offensetype, category, incdesc, blockcode, ward.
#' @export
#' @examples
#' \dontrun{
#' somer_crimes(year = "2024", limit = 50)
#' somer_crimes(category = "Crimes against Person")
#' }
somer_crimes <- function(year = NULL, ward = NULL, category = NULL,
                         q = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(year))     clauses <- c(clauses, sprintf("year = '%s'", year))
  if (!is.null(ward))     clauses <- c(clauses, sprintf("ward = '%s'", ward))
  if (!is.null(category)) clauses <- c(clauses, sprintf("category = '%s'", category))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("aghs-hqvg", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_crimes)
  df |>
    mutate(
      year = as.integer(year),
      ward = trimws(as.character(ward %||% NA_character_))
    )
}


# == 311 Service Requests ======================================================

#' Query Somerville 311 service requests (2015-present)
#'
#' Service, information, and feedback requests submitted to the City of
#' Somerville. Each row is a constituent request with category, status,
#' and timestamps. Dataset view ID: \code{4pyi-uqq6}.
#'
#' @param classification Character or NULL. Filter by classification:
#'   \code{"Service"}, \code{"Information"}, or \code{"Feedback"}.
#' @param category Character or NULL. Filter by category (e.g.
#'   \code{"Trash & Recycling"}, \code{"Snow & Ice"}).
#' @param status Character or NULL. Filter by most recent status
#'   (e.g. \code{"Closed"}, \code{"Open"}).
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including id, classification, category,
#'   type, origin_of_request, most_recent_status, date_created,
#'   most_recent_status_date.
#' @export
#' @examples
#' \dontrun{
#' somer_311(classification = "Service", limit = 50)
#' somer_311(category = "Trash & Recycling", status = "Open")
#' }
somer_311 <- function(classification = NULL, category = NULL,
                      status = NULL, q = NULL,
                      limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(classification)) clauses <- c(clauses, sprintf("classification = '%s'", classification))
  if (!is.null(category))       clauses <- c(clauses, sprintf("category = '%s'", category))
  if (!is.null(status))         clauses <- c(clauses, sprintf("most_recent_status = '%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("4pyi-uqq6", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_311)
  df |>
    mutate(
      date_created = as.POSIXct(date_created, format = "%Y-%m-%d %H:%M:%S"),
      most_recent_status_date = as.POSIXct(most_recent_status_date, format = "%Y-%m-%d %H:%M:%S")
    )
}


# == Permits & Licenses (Applications) ========================================

#' Query Somerville permit and license applications
#'
#' Returns permit and license applications from Inspectional Services and
#' the City Clerk. Covers food establishments, entertainment licenses,
#' and other regulated activities. Dataset view ID: \code{nneb-s3f7}.
#'
#' @param type Character or NULL. Filter by application type (e.g.
#'   \code{"Food Establishment"}).
#' @param status Character or NULL. Filter by status (e.g. \code{"Issued"},
#'   \code{"Denied"}).
#' @param ward Character or NULL. Filter by ward number.
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with application details including application_type,
#'   status, address, application_amount, amount_paid, latitude, longitude.
#' @export
#' @examples
#' \dontrun{
#' somer_applications(type = "Food Establishment", limit = 50)
#' }
somer_applications <- function(type = NULL, status = NULL, ward = NULL,
                               q = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))   clauses <- c(clauses, sprintf("application_type = '%s'", type))
  if (!is.null(status)) clauses <- c(clauses, sprintf("status = '%s'", status))
  if (!is.null(ward))   clauses <- c(clauses, sprintf("application_ward = '%s'", ward))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("nneb-s3f7", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |>
    mutate(
      application_amount = as.numeric(application_amount %||% NA),
      amount_paid = as.numeric(amount_paid %||% NA),
      application_latitude = as.numeric(application_latitude %||% NA),
      application_longitude = as.numeric(application_longitude %||% NA)
    )
}


# == Building Permits ==========================================================

#' Query Somerville building permits (2014-present)
#'
#' Building permit data from the City of Somerville Inspectional Services
#' department. Includes residential and commercial permits with valuations
#' and geographic coordinates. Dataset view ID: \code{vxgw-vmky}.
#'
#' @param type Character or NULL. Filter by permit type (e.g.
#'   \code{"Residential Building"}, \code{"Commercial Building"}).
#' @param status Character or NULL. Filter by status (e.g. \code{"Issued"},
#'   \code{"Pending"}).
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including id, type, status, address, work,
#'   amount (numeric), application_date (POSIXct), issue_date (POSIXct),
#'   latitude (numeric), longitude (numeric).
#' @export
#' @examples
#' \dontrun{
#' somer_permits(status = "Issued", limit = 50)
#' }
somer_permits <- function(type = NULL, status = NULL,
                          q = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))   clauses <- c(clauses, sprintf("type = '%s'", type))
  if (!is.null(status)) clauses <- c(clauses, sprintf("status = '%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("vxgw-vmky", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_permits)
  df |>
    mutate(
      amount = as.numeric(amount %||% NA),
      latitude = as.numeric(latitude %||% NA),
      longitude = as.numeric(longitude %||% NA),
      application_date = as.POSIXct(application_date, format = "%Y-%m-%dT%H:%M:%S"),
      issue_date = as.POSIXct(issue_date, format = "%Y-%m-%dT%H:%M:%S")
    )
}


# == Traffic Citations =========================================================

#' Query Somerville traffic citations (2017-present)
#'
#' Traffic and parking citation data from the Somerville Police Department.
#' Each row is a single citation with charge description, location, and
#' whether it was a warning. Dataset view ID: \code{3mqx-eye9}.
#'
#' @param ward Character or NULL. Filter by ward number.
#' @param warning Character or NULL. Filter by warning status:
#'   \code{"Y"} for warnings only, \code{"N"} for actual citations only.
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including citationnum, dtissued (POSIXct),
#'   police_shift, address, chgdesc, chgcategory, ward, warning,
#'   lat (numeric), long (numeric).
#' @export
#' @examples
#' \dontrun{
#' somer_citations(ward = "1", limit = 50)
#' somer_citations(warning = "N", limit = 100)
#' }
somer_citations <- function(ward = NULL, warning = NULL,
                            q = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(ward))    clauses <- c(clauses, sprintf("ward = '%s'", ward))
  if (!is.null(warning)) clauses <- c(clauses, sprintf("warning = '%s'", warning))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("3mqx-eye9", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_citations)
  df |>
    mutate(
      dtissued = as.POSIXct(dtissued, format = "%Y-%m-%d %H:%M:%S"),
      lat = as.numeric(lat %||% NA),
      long = as.numeric(long %||% NA)
    )
}


# == Happiness Survey ==========================================================

#' Query Somerville Happiness Survey responses (2011-present)
#'
#' Biennial survey of resident happiness, wellbeing, and satisfaction with
#' city services. Somerville's Happiness Survey is nationally recognized
#' as a model for measuring community wellbeing. Dataset view ID:
#' \code{wmeh-zuz2}.
#'
#' @param q Character or NULL. Full-text search across responses.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with survey response columns (varies by survey year).
#' @export
#' @examples
#' \dontrun{
#' somer_happiness(limit = 50)
#' }
somer_happiness <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("wmeh-zuz2", q = q, limit = limit, offset = offset)
}


# == Website Analytics =========================================================

#' Query Somerville city website analytics (2020-present)
#'
#' Daily top-20 most-viewed pages on somervillema.gov. Useful for tracking
#' which city services residents are most interested in. Dataset view ID:
#' \code{754v-8e35}.
#'
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#' \describe{
#'   \item{url}{Character. Page URL.}
#'   \item{date}{Date. Date of pageview count.}
#'   \item{title}{Character. Page title.}
#'   \item{pageviews}{Integer. Number of views that day.}
#'   \item{id}{Character. Row identifier.}
#' }
#' @export
#' @examples
#' \dontrun{
#' somer_web_analytics(limit = 50)
#' }
somer_web_analytics <- function(q = NULL, limit = 1000, offset = 0) {
  df <- ._soda_query("754v-8e35", q = q, limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |>
    mutate(
      date = as.Date(substr(date, 1, 10)),
      pageviews = as.integer(pageviews)
    )
}


# == Capital Investment Plan ===================================================

#' Query Somerville Capital Investment Plan projects (FY16-26)
#'
#' Multi-year capital investment plan for infrastructure, buildings, and
#' equipment. Includes funding sources and yearly budget allocations.
#' Dataset view ID: \code{wz6k-gm5k}.
#'
#' @param department Character or NULL. Filter by department (e.g.
#'   \code{"DPW"}, \code{"Fire"}).
#' @param status Character or NULL. Filter by status (e.g. \code{"Approved"}).
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including project, department, type,
#'   funding_source, status, total (numeric), yearly amounts, description,
#'   justification.
#' @export
#' @examples
#' \dontrun{
#' somer_capital(department = "DPW", limit = 50)
#' }
somer_capital <- function(department = NULL, status = NULL,
                          q = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(department)) clauses <- c(clauses, sprintf("department = '%s'", department))
  if (!is.null(status))     clauses <- c(clauses, sprintf("status = '%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- ._soda_query("wz6k-gm5k", where = where, q = q,
                      limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |>
    mutate(total = as.numeric(total %||% NA))
}


# == Crash Reports =============================================================

#' Query Somerville motor vehicle crash reports (2010-2018)
#'
#' Motor vehicle crash reports filed with the Somerville Police Department.
#' Dataset view ID: \code{ezmv-8wys}.
#'
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with crash report data (columns vary).
#' @export
#' @examples
#' \dontrun{
#' somer_crashes(limit = 50)
#' }
somer_crashes <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("ezmv-8wys", q = q, limit = limit, offset = offset)
}


# == Parking Permits ===========================================================

#' Query Somerville parking permits (2017-2020)
#'
#' Residential and visitor parking permit data for the City of Somerville.
#' Dataset view ID: \code{xavb-4s9w}.
#'
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with parking permit data (columns vary).
#' @export
#' @examples
#' \dontrun{
#' somer_parking(limit = 50)
#' }
somer_parking <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("xavb-4s9w", q = q, limit = limit, offset = offset)
}


# == Participatory Budgeting ===================================================

#' Query Somerville Participatory Budgeting voting results (2023)
#'
#' Results of the city's participatory budgeting process where residents
#' vote on capital projects. Dataset view ID: \code{2tt6-zua8}.
#'
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with project names and vote counts.
#' @export
#' @examples
#' \dontrun{
#' somer_pb_votes()
#' }
somer_pb_votes <- function(limit = 1000, offset = 0) {
  ._soda_query("2tt6-zua8", limit = limit, offset = offset)
}

#' Query Somerville Participatory Budgeting submissions (2023)
#'
#' Resident-submitted project ideas for the participatory budgeting process.
#' Dataset view ID: \code{brrj-v9a4}.
#'
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with submission details.
#' @export
#' @examples
#' \dontrun{
#' somer_pb_submissions()
#' }
somer_pb_submissions <- function(limit = 1000, offset = 0) {
  ._soda_query("brrj-v9a4", limit = limit, offset = offset)
}


# == Demographics ==============================================================

#' Query Somerville at a Glance demographics
#'
#' Census and American Community Survey (ACS) data on population, housing,
#' education, income, and other demographic indicators for the City of
#' Somerville (2010-2023). Dataset view ID: \code{jnde-mi6j}.
#'
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with demographic indicator data.
#' @export
#' @examples
#' \dontrun{
#' somer_demographics()
#' }
somer_demographics <- function(limit = 1000, offset = 0) {
  ._soda_query("jnde-mi6j", limit = limit, offset = offset)
}


# == Jobs ======================================================================

#' Query annual count of jobs in Somerville (2001-present)
#'
#' Annual employment data for the City of Somerville. Dataset view ID:
#' \code{pm7h-ga9w}.
#'
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with year and job count.
#' @export
#' @examples
#' \dontrun{
#' somer_jobs()
#' }
somer_jobs <- function(limit = 1000, offset = 0) {
  ._soda_query("pm7h-ga9w", limit = limit, offset = offset)
}


# == Bike/Ped Counts ===========================================================

#' Query Somerville bicycle and pedestrian counts (2010-present)
#'
#' Annual volunteer-collected bicycle and pedestrian counts at intersections
#' and paths throughout Somerville. Dataset view ID: \code{qu9x-4xq5}.
#'
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with count data by location and year.
#' @export
#' @examples
#' \dontrun{
#' somer_bike_ped(limit = 50)
#' }
somer_bike_ped <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("qu9x-4xq5", q = q, limit = limit, offset = offset)
}


# == Context ===================================================================

#' Get somervillema.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
somer_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(somer_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/somervillema.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "somervillema.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# somervillema.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# somervillema.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
