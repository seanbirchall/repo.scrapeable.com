# == Dataset discovery =========================================================

#' List available datasets on Somerville Open Data
#'
#' Returns a catalog of datasets from data.somervillema.gov.
#'
#' @param limit Number of datasets to return (default 100, max 200)
#' @param category Optional category filter (case-insensitive substring)
#' @return tibble: id, name, description, category, type, updated_at, view_count
#' @export
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
#' Full-text search across dataset names and descriptions.
#'
#' @param query Search string
#' @param limit Max results (default 50)
#' @return tibble: id, name, description, category, type, updated_at, view_count
#' @export
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
#' Runs a SoQL query against a SODA 2.0 endpoint.
#'
#' @param view_id Socrata view ID (e.g. "mdb2-mgc7")
#' @param where SoQL WHERE clause (e.g. "year = '2024'")
#' @param select SoQL SELECT clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Rows to return (default 1000, max 50000)
#' @param offset Pagination offset
#' @return tibble
#' @export
somer_view <- function(view_id, where = NULL, select = NULL,
                       order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select,
               order = order, q = q, limit = limit, offset = offset)
}

#' Get metadata for a Somerville dataset
#'
#' @param view_id Socrata view ID
#' @return tibble with id, name, description, category, row_count, column_count
#' @export
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
#' @param view_id Socrata view ID
#' @param where Optional SoQL WHERE filter
#' @return integer
#' @export
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
#' Calls for service from the Computer Aided Dispatch system.
#'
#' @param year Filter by year (character or integer, e.g. "2024")
#' @param ward Filter by ward number (character)
#' @param inctype Filter by incident type (e.g. "ID THEFT")
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: incnum, day_and_month, year, police_shift, inctype, incdesc,
#'   incdefex, blockcode, ward
#' @export
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
#' Incident-level crime data from Somerville PD records management.
#'
#' @param year Filter by year
#' @param ward Filter by ward
#' @param category Filter by crime category (e.g. "Crimes against Person")
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: incnum, day_and_month, year, police_shift, offense,
#'   offensetype, category, incdesc, blockcode, ward
#' @export
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
#' Service, information, and feedback requests to the city.
#'
#' @param classification Filter: "Service", "Information", or "Feedback"
#' @param category Filter by category (e.g. "Trash & Recycling")
#' @param status Filter by status (e.g. "Closed", "Open")
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: id, classification, category, type, origin_of_request,
#'   most_recent_status, date_created, most_recent_status_date
#' @export
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
#' Applications from Inspectional Services and City Clerk.
#'
#' @param type Filter by application type (e.g. "Food Establishment")
#' @param status Filter by status (e.g. "Issued", "Denied")
#' @param ward Filter by ward number
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with application details including address, dates, amounts
#' @export
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
#' @param type Filter by permit type (e.g. "Residential Building")
#' @param status Filter by status (e.g. "Issued")
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: id, type, status, address, work, amount, application_date,
#'   issue_date, latitude, longitude
#' @export
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
#' @param ward Filter by ward
#' @param warning Filter by warning status ("Y" or "N")
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: citationnum, dtissued, police_shift, address, chgdesc,
#'   chgcategory, ward, warning, lat, long
#' @export
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
#' Biennial survey of resident happiness, wellbeing, and city satisfaction.
#'
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with survey response columns
#' @export
somer_happiness <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("wmeh-zuz2", q = q, limit = limit, offset = offset)
}


# == Website Analytics =========================================================

#' Query Somerville city website analytics (2020-present)
#'
#' Daily top-20 page view counts for somervillema.gov.
#'
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: url, date, title, pageviews, id
#' @export
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
#' @param department Filter by department (e.g. "DPW")
#' @param status Filter by status (e.g. "Approved")
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with project, department, type, funding_source, status, total,
#'   yearly amounts, description, justification
#' @export
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
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with crash data
#' @export
somer_crashes <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("ezmv-8wys", q = q, limit = limit, offset = offset)
}


# == Parking Permits ===========================================================

#' Query Somerville parking permits (2017-2020)
#'
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with parking permit data
#' @export
somer_parking <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("xavb-4s9w", q = q, limit = limit, offset = offset)
}


# == Participatory Budgeting ===================================================

#' Query Somerville Participatory Budgeting voting results (2023)
#'
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with project names and vote counts
#' @export
somer_pb_votes <- function(limit = 1000, offset = 0) {
  ._soda_query("2tt6-zua8", limit = limit, offset = offset)
}

#' Query Somerville Participatory Budgeting submissions (2023)
#'
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with submission details
#' @export
somer_pb_submissions <- function(limit = 1000, offset = 0) {
  ._soda_query("brrj-v9a4", limit = limit, offset = offset)
}


# == Demographics ==============================================================

#' Query Somerville at a Glance demographics
#'
#' Census and ACS data on population, housing, education, etc. (2010-2023)
#'
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with demographic indicators
#' @export
somer_demographics <- function(limit = 1000, offset = 0) {
  ._soda_query("jnde-mi6j", limit = limit, offset = offset)
}


# == Jobs ======================================================================

#' Query annual count of jobs in Somerville (2001-present)
#'
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with year and job count
#' @export
somer_jobs <- function(limit = 1000, offset = 0) {
  ._soda_query("pm7h-ga9w", limit = limit, offset = offset)
}


# == Bike/Ped Counts ===========================================================

#' Query Somerville bicycle and pedestrian counts (2010-present)
#'
#' Annual volunteer-collected bike and pedestrian counts.
#'
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Pagination offset
#' @return tibble with count data by location
#' @export
somer_bike_ped <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("qu9x-4xq5", q = q, limit = limit, offset = offset)
}


# == Context ===================================================================

#' Generate LLM-friendly context for somervillema.gov client
#'
#' Returns all public function signatures and bodies as a single string.
#'
#' @return character (invisibly); also prints to console
#' @export
somer_context <- function() {
  .build_context(
    pkg_name = "somervillema.gov",
    header_lines = c(
      "# somervillema.gov - City of Somerville Open Data (Socrata SODA 2.0)",
      "# Portal: https://data.somervillema.gov",
      "# 46 datasets (27 queryable Socrata views)",
      "# No auth required"
    )
  )
}
