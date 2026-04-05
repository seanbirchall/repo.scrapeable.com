# nola.gov.R - Self-contained City of New Orleans open data client
# Source: data.nola.gov (Socrata SODA API)
# Datasets: 185 covering police, permits, 311 calls, code enforcement, etc.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nola_base <- "https://data.nola.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.safe_date <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  tryCatch(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
           error = function(e) as.POSIXct(NA))
}

.safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

.strip_computed <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(df)
  keep <- !grepl("^:@computed_region_", names(df))
  df[, keep, drop = FALSE]
}

# == Core SODA query ===========================================================

#' Query any NOLA dataset via SODA
#'
#' Low-level function to query any dataset on data.nola.gov using SoQL
#' (Socrata Query Language). Use \code{nola_list()} or \code{nola_search()}
#' to discover dataset IDs.
#'
#' @param dataset_id Character. Four-by-four Socrata dataset identifier
#'   (e.g. \code{"es9j-6y5d"} for calls for service, \code{"2jgv-pqrq"}
#'   for 311 requests, \code{"rcm3-fn58"} for permits).
#' @param where Character or NULL. SoQL WHERE clause for filtering
#'   (e.g. \code{"request_type='Pothole'"}).
#' @param select Character or NULL. SoQL SELECT clause for column selection
#'   (e.g. \code{"request_type,count(*) as n"}).
#' @param group Character or NULL. SoQL GROUP BY clause for aggregation.
#' @param order Character or NULL. SoQL ORDER BY clause for sorting
#'   (e.g. \code{"date_created DESC"}).
#' @param q Character or NULL. Full-text search query.
#' @param limit Integer. Max rows to return (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble whose columns depend on the queried dataset. Computed
#'   region columns are automatically stripped.
#' @examples
#' nola_query("es9j-6y5d", limit = 5)
#' nola_query("2jgv-pqrq", where = "request_type='Pothole'", limit = 10)
nola_query <- function(dataset_id, where = NULL, select = NULL,
                       group = NULL, order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .nola_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw) |> .strip_computed()
}

# == Discovery =================================================================

#' List datasets on data.nola.gov
#'
#' Returns metadata for datasets available on the New Orleans open data
#' portal (~185 datasets covering police, permits, 311 calls, code
#' enforcement, and more).
#'
#' @param limit Integer. Number of datasets to return (default 100).
#' @param category Character or NULL. Category filter. Known categories:
#'   \code{"Public Safety and Preparedness"}, \code{"Housing, Land Use, and Blight"},
#'   \code{"Transportation and Infrastructure"}, \code{"Economy and Workforce Development"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- four-by-four dataset ID (pass to \code{nola_query()})}
#'     \item{name}{character -- human-readable dataset name}
#'     \item{category}{character -- domain category}
#'     \item{description}{character -- dataset description (truncated to 200 chars)}
#'     \item{updated_at}{character -- last update timestamp}
#'     \item{views}{integer -- total page views}
#'   }
#' @examples
#' nola_list(limit = 10)
#' nola_list(category = "Public Safety and Preparedness")
nola_list <- function(limit = 100, category = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.nola.gov&limit=%d&only=datasets",
    .nola_base, limit
  )
  if (!is.null(category)) {
    url <- paste0(url, "&categories=", utils::URLencode(category))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) {
    return(tibble(id = character(), name = character(), category = character(),
                  description = character(), updated_at = character(),
                  views = integer()))
  }
  res <- raw$results$resource
  tibble(
    id          = as.character(res$id),
    name        = as.character(res$name),
    category    = as.character(raw$results$classification$domain_category %||% NA_character_),
    description = vapply(res$description, function(d) {
      d <- d %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    updated_at  = as.character(res$updatedAt),
    views       = as.integer(res$page_views$page_views_total %||% 0L)
  )
}

#' Search NOLA datasets by keyword
#'
#' Full-text search across dataset names and descriptions on
#' data.nola.gov.
#'
#' @param query Character. Search query string (e.g. \code{"police"},
#'   \code{"permit"}, \code{"blight"}).
#' @param limit Integer. Maximum results to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- four-by-four dataset ID}
#'     \item{name}{character -- dataset name}
#'     \item{category}{character -- domain category}
#'     \item{description}{character -- dataset description (truncated)}
#'     \item{updated_at}{character -- last update timestamp}
#'     \item{views}{integer -- total page views}
#'   }
#' @examples
#' nola_search("police")
#' nola_search("short term rental")
nola_search <- function(query, limit = 20) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.nola.gov&q=%s&limit=%d&only=datasets",
    .nola_base, utils::URLencode(query), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) {
    return(tibble(id = character(), name = character(), category = character(),
                  description = character(), updated_at = character(),
                  views = integer()))
  }
  res <- raw$results$resource
  tibble(
    id          = as.character(res$id),
    name        = as.character(res$name),
    category    = as.character(raw$results$classification$domain_category %||% NA_character_),
    description = vapply(res$description, function(d) {
      d <- d %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    updated_at  = as.character(res$updatedAt),
    views       = as.integer(res$page_views$page_views_total %||% 0L)
  )
}

#' View metadata for a single NOLA dataset
#'
#' Returns column names, types, and descriptions for a Socrata
#' dataset. Useful for understanding fields before querying.
#'
#' @param dataset_id Character. Four-by-four Socrata dataset identifier
#'   (e.g. \code{"es9j-6y5d"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{character -- column name in the dataset}
#'     \item{data_type}{character -- Socrata data type (e.g. \code{"text"},
#'       \code{"calendar_date"}, \code{"number"})}
#'     \item{description}{character -- column description (may be empty)}
#'   }
#' @examples
#' nola_view("es9j-6y5d")  # calls for service schema
#' nola_view("2jgv-pqrq")  # 311 requests schema
nola_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s/columns.json", .nola_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA view error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) {
    return(tibble(field_name = character(), data_type = character(),
                  description = character()))
  }
  tibble(
    field_name  = as.character(raw$fieldName),
    data_type   = as.character(raw$dataTypeName),
    description = as.character(raw$description %||% NA_character_)
  )
}

# == Named data functions =====================================================

#' NOLA 311 service requests
#'
#' Fetch 311 calls from the Orleans Parish Communications District.
#' Dataset: \code{2jgv-pqrq}. Covers potholes, streetlights, trash,
#' code enforcement complaints, and other city service requests.
#'
#' @param type Character or NULL. Request type filter. Examples:
#'   \code{"Pothole/Roadway Surface Defect"}, \code{"Traffic Signal"},
#'   \code{"Abandoned Vehicle"}, \code{"Streetlight"}.
#' @param status Character or NULL. Status filter: \code{"Closed"} or
#'   \code{"Open"}.
#' @param since Character or NULL. ISO date string (\code{"YYYY-MM-DD"})
#'   to filter requests created after this date.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: service_request, request_type,
#'   request_reason, date_created (POSIXct), date_modified (POSIXct),
#'   request_status, responsible_agency, final_address, latitude (numeric),
#'   longitude (numeric), and more.
#' @examples
#' nola_311(limit = 10)
#' nola_311(type = "Pothole/Roadway Surface Defect", status = "Open")
nola_311 <- function(type = NULL, status = NULL, since = NULL,
                     limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))   clauses <- c(clauses, sprintf("request_type='%s'", type))
  if (!is.null(status)) clauses <- c(clauses, sprintf("request_status='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("date_created>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("2jgv-pqrq", where = where, limit = limit, offset = offset,
                    order = "date_created DESC")
  if (nrow(df) == 0) return(df)
  if ("date_created" %in% names(df)) df$date_created <- .safe_date(df$date_created)
  if ("date_modified" %in% names(df)) df$date_modified <- .safe_date(df$date_modified)
  if ("case_close_date" %in% names(df)) df$case_close_date <- .safe_date(df$case_close_date)
  if ("latitude" %in% names(df)) df$latitude <- .safe_num(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- .safe_num(df$longitude)
  df
}

#' NOLA calls for service (police)
#'
#' Incidents reported to NOPD (New Orleans Police Department).
#' Dataset: \code{es9j-6y5d} (current year). Includes dispatch
#' records with type, priority, and response times.
#'
#' @param type Character or NULL. Call type filter. Examples:
#'   \code{"SHOOTING"}, \code{"DISTURBANCE"}, \code{"TRAFFIC STOP"},
#'   \code{"THEFT"}, \code{"BATTERY"}.
#' @param district Character or NULL. Police district number
#'   (e.g. \code{"1"} through \code{"8"}).
#' @param since Character or NULL. ISO date (\code{"YYYY-MM-DD"})
#'   to filter calls created after this date.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: nopd_item, typetext,
#'   priority, timecreate (POSIXct), timearrive (POSIXct),
#'   timeclosed (POSIXct), disposition, dispositiontext,
#'   policedistrict, block_address, location.
#' @examples
#' nola_calls_for_service(limit = 10)
#' nola_calls_for_service(type = "SHOOTING", since = "2026-01-01")
nola_calls_for_service <- function(type = NULL, district = NULL, since = NULL,
                                   limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))     clauses <- c(clauses, sprintf("typetext='%s'", type))
  if (!is.null(district)) clauses <- c(clauses, sprintf("policedistrict='%s'", district))
  if (!is.null(since))    clauses <- c(clauses, sprintf("timecreate>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("es9j-6y5d", where = where, limit = limit, offset = offset,
                    order = "timecreate DESC")
  if (nrow(df) == 0) return(df)
  if ("timecreate" %in% names(df)) df$timecreate <- .safe_date(df$timecreate)
  if ("timeclosed" %in% names(df)) df$timeclosed <- .safe_date(df$timeclosed)
  if ("timearrive" %in% names(df)) df$timearrive <- .safe_date(df$timearrive)
  df
}

#' NOLA building permits
#'
#' City of New Orleans permits since 2012. Dataset: \code{rcm3-fn58}.
#' Covers building, mechanical, plumbing, and electrical permits.
#'
#' @param type Character or NULL. Permit type code. Valid values:
#'   \code{"MECH"} (mechanical), \code{"PLMB"} (plumbing),
#'   \code{"ELEC"} (electrical), \code{"BLDG"} (building),
#'   \code{"DEMO"} (demolition).
#' @param status Character or NULL. Permit status. Examples:
#'   \code{"Permit Issued"}, \code{"Application Received"},
#'   \code{"Inspection Complete"}.
#' @param since Character or NULL. ISO date (\code{"YYYY-MM-DD"})
#'   for filing date filter.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: address, description, type,
#'   code, issuedate (POSIXct), filingdate (POSIXct), currentstatus,
#'   totalfees (numeric), constrval (numeric), bldgarea (numeric),
#'   applicant, owner, contractors.
#' @examples
#' nola_permits(limit = 10)
#' nola_permits(type = "ELEC", since = "2025-01-01")
nola_permits <- function(type = NULL, status = NULL, since = NULL,
                         limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))   clauses <- c(clauses, sprintf("code='%s'", type))
  if (!is.null(status)) clauses <- c(clauses, sprintf("currentstatus='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("filingdate>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("rcm3-fn58", where = where, limit = limit, offset = offset,
                    order = "filingdate DESC")
  if (nrow(df) == 0) return(df)
  if ("filingdate" %in% names(df)) df$filingdate <- .safe_date(df$filingdate)
  if ("issuedate" %in% names(df)) df$issuedate <- .safe_date(df$issuedate)
  if ("totalfees" %in% names(df)) df$totalfees <- .safe_num(df$totalfees)
  if ("constrval" %in% names(df)) df$constrval <- .safe_num(df$constrval)
  if ("bldgarea" %in% names(df)) df$bldgarea <- .safe_num(df$bldgarea)
  df
}

#' NOLA code enforcement cases
#'
#' All code enforcement / blight cases. Dataset: \code{u6yx-v2tw}.
#' Covers property inspections, blight violations, and remediation.
#'
#' @param status Character or NULL. Case status: \code{"Open"} or
#'   \code{"Closed"}.
#' @param since Character or NULL. ISO date (\code{"YYYY-MM-DD"}) for
#'   cases filed after this date.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: o_c (open/closed), caseno,
#'   location, stage, keystatus, statdate (POSIXct), casefiled (POSIXct),
#'   geoaddress, city, state, zipcode, xpos, ypos.
#' @examples
#' nola_code_enforcement(limit = 10)
#' nola_code_enforcement(status = "Open", since = "2025-01-01")
nola_code_enforcement <- function(status = NULL, since = NULL,
                                  limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(status)) clauses <- c(clauses, sprintf("o_c='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("casefiled>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("u6yx-v2tw", where = where, limit = limit, offset = offset,
                    order = "casefiled DESC")
  if (nrow(df) == 0) return(df)
  if ("casefiled" %in% names(df)) df$casefiled <- .safe_date(df$casefiled)
  if ("statdate" %in% names(df)) df$statdate <- .safe_date(df$statdate)
  df
}

#' NOLA police use of force incidents
#'
#' Use of force reports from NOPD. Dataset: \code{9mnw-mbde}. Published
#' as part of the federal consent decree monitoring.
#'
#' @param level Character or NULL. Force level filter: \code{"L1"},
#'   \code{"L2"}, \code{"L3"}, or \code{"L4"} (escalating severity).
#' @param disposition Character or NULL. Disposition filter. Example:
#'   \code{"Use Of Force Authorized"}.
#' @param since Character or NULL. ISO date (\code{"YYYY-MM-DD"})
#'   for incidents after this date.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: pib_file_number,
#'   originating_bureau, division, use_of_force_type,
#'   use_of_force_level, disposition, officer_race_ethnicity,
#'   officer_gender, officer_age, subject_gender, subject_ethnicity,
#'   subject_age, date_occurred (POSIXct).
#' @examples
#' nola_use_of_force(limit = 10)
#' nola_use_of_force(level = "L2", since = "2025-01-01")
nola_use_of_force <- function(level = NULL, disposition = NULL, since = NULL,
                              limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(level))       clauses <- c(clauses, sprintf("use_of_force_level='%s'", level))
  if (!is.null(disposition)) clauses <- c(clauses, sprintf("disposition='%s'", disposition))
  if (!is.null(since))       clauses <- c(clauses, sprintf("date_occurred>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("9mnw-mbde", where = where, limit = limit, offset = offset,
                    order = "date_occurred DESC")
  if (nrow(df) == 0) return(df)
  if ("date_occurred" %in% names(df)) df$date_occurred <- .safe_date(df$date_occurred)
  df
}

#' NOLA misconduct complaints
#'
#' NOPD misconduct complaints and internal affairs data.
#' Dataset: \code{gz2m-ef5u}. Published as part of consent decree.
#'
#' @param disposition Character or NULL. Disposition filter. Examples:
#'   \code{"Sustained"}, \code{"Not Sustained"}, \code{"Exonerated"},
#'   \code{"Unfounded"}, \code{"Pending"}.
#' @param since Character or NULL. ISO date (\code{"YYYY-MM-DD"})
#'   for complaints received after this date.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: incident_type,
#'   complaint_tracking_number, complaint_classification,
#'   investigation_status, disposition, rule_violation,
#'   paragraph_violation, officer_race_ethnicity, officer_gender,
#'   officer_age, subject_gender, subject_ethnicity, subject_age,
#'   date_complaint_received_by_nopd_pib.
#' @examples
#' nola_misconduct(limit = 10)
#' nola_misconduct(disposition = "Sustained")
nola_misconduct <- function(disposition = NULL, since = NULL,
                            limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(disposition)) clauses <- c(clauses, sprintf("disposition='%s'", disposition))
  if (!is.null(since))       clauses <- c(clauses, sprintf("date_complaint_received_by_nopd_pib>='%s'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("gz2m-ef5u", where = where, limit = limit, offset = offset,
                    order = "date_complaint_received_by_nopd_pib DESC")
  if (nrow(df) == 0) return(df)
  df
}

#' NOLA business licenses
#'
#' Active occupational licenses in New Orleans.
#' Dataset: \code{iqay-p646}.
#'
#' @param q Character or NULL. Full-text search (e.g. business name
#'   or address).
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: businessname,
#'   businessaddress, businessphone, businesstype,
#'   businesslicensenumber, ownername, city, state, zip,
#'   businessstartdate, locationx, locationy.
#' @examples
#' nola_businesses(limit = 10)
#' nola_businesses(q = "restaurant")
nola_businesses <- function(q = NULL, limit = 1000, offset = 0) {
  df <- nola_query("iqay-p646", q = q, limit = limit, offset = offset)
  if (nrow(df) == 0) return(df)
  df
}

#' NOLA short-term rental licenses
#'
#' Active short-term rental (Airbnb, VRBO, etc.) licenses in
#' New Orleans. Dataset: \code{ufdg-ajws}.
#'
#' @param license_type Character or NULL. License type:
#'   \code{"Commercial"} or \code{"Non-Commercial"}.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: license_number, address,
#'   license_type, residential_subtype, expiration_date,
#'   license_holder_name, application_date, issue_date, latitude,
#'   longitude.
#' @examples
#' nola_str_licenses(limit = 10)
#' nola_str_licenses(license_type = "Commercial")
nola_str_licenses <- function(license_type = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(license_type)) {
    where <- sprintf("license_type='%s'", license_type)
  }
  df <- nola_query("ufdg-ajws", where = where, limit = limit, offset = offset)
  if (nrow(df) == 0) return(df)
  df
}

# == Pagination helper ========================================================

#' Fetch all rows from a NOLA dataset with auto-pagination
#'
#' Automatically pages through a Socrata dataset until all matching
#' rows are retrieved or \code{max_rows} is reached.
#'
#' @param dataset_id Character. Socrata four-by-four identifier
#'   (e.g. \code{"2jgv-pqrq"}).
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @param q Character or NULL. Full-text search query.
#' @param max_rows Integer. Maximum total rows to fetch (default 10000).
#' @param page_size Integer. Rows per request (default 1000).
#' @return A tibble of all matching rows, with columns depending on
#'   the dataset.
#' @examples
#' nola_fetch_all("2jgv-pqrq", max_rows = 500)
nola_fetch_all <- function(dataset_id, where = NULL, q = NULL,
                           max_rows = 10000, page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- nola_query(dataset_id, where = where, q = q,
                        limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Context ===================================================================

#' Get nola.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nola_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nola_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nola.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nola.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nola.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nola.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
