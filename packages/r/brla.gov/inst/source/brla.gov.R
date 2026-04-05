# brla.gov.R - Self-contained City of Baton Rouge open data client
# Source: data.brla.gov (Socrata SODA API)
# Datasets: 246 covering crime, 311, permits, employees, budget, etc.

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.brla_base <- "https://data.brla.gov"

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

#' Query any BRLA dataset via SODA
#'
#' Low-level function to query any of the 246 datasets on data.brla.gov
#' using SoQL (Socrata Query Language). Computed region columns are
#' automatically stripped from results.
#'
#' @param dataset_id Character. Four-by-four Socrata dataset identifier.
#'   Example: \code{"pbin-pcm7"} (Crime Incidents), \code{"7ixm-mnvx"} (311)
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"statute_category='THEFT'"}
#' @param select Character or NULL. SoQL SELECT clause.
#'   Example: \code{"statute_category, count(*) as n"}
#' @param group Character or NULL. SoQL GROUP BY clause.
#'   Example: \code{"statute_category"}
#' @param order Character or NULL. SoQL ORDER BY clause.
#'   Example: \code{"charge_date DESC"}
#' @param q Character or NULL. Full-text search query.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of query results. Columns depend on the dataset.
#' @examples
#' brla_query("pbin-pcm7", limit = 5)
#' brla_query("pbin-pcm7", select = "statute_category, count(*) as n",
#'            group = "statute_category", order = "n DESC", limit = 10)
brla_query <- function(dataset_id, where = NULL, select = NULL,
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
  url <- sprintf("%s/resource/%s.json?%s", .brla_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BRLA SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw) |> .strip_computed()
}

# == Discovery =================================================================

#' List datasets on data.brla.gov
#'
#' Returns metadata for datasets on the Baton Rouge open data portal.
#' Sorted by view count (most popular first). Covers public safety,
#' housing, government, transportation, and other categories.
#'
#' @param limit Integer. Number of datasets to return (default 100).
#'   Example: \code{limit = 20}
#' @param category Character or NULL. Optional category filter.
#'   Example: \code{"Public Safety"}, \code{"Housing & Development"}
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata four-by-four ID (e.g. "fabb-cnnu").}
#'     \item{name}{Character. Dataset name (e.g. "Baton Rouge Police Crime Incidents").}
#'     \item{category}{Character. Domain category (e.g. "Public Safety").}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{updated_at}{Character. ISO 8601 last-modified timestamp.}
#'     \item{views}{Integer. Total page views (e.g. 182760).}
#'   }
#' @examples
#' brla_list(limit = 10)
#' brla_list(category = "Public Safety")
brla_list <- function(limit = 100, category = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.brla.gov&limit=%d&only=datasets",
    .brla_base, limit
  )
  if (!is.null(category)) {
    url <- paste0(url, "&categories=", utils::URLencode(category))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BRLA catalog error: ", e$message)
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

#' Search BRLA datasets by keyword
#'
#' Full-text search across dataset names and descriptions on the Baton
#' Rouge open data portal. Returns the same schema as \code{\link{brla_list}}.
#'
#' @param query Character. Search terms to match.
#'   Example: \code{"crime"}, \code{"traffic"}, \code{"employee"}
#' @param limit Integer. Maximum results (default 20).
#' @return A tibble with columns: id, name, category, description,
#'   updated_at, views (same as \code{\link{brla_list}}).
#' @examples
#' brla_search("crime", limit = 5)
#' brla_search("employee")
brla_search <- function(query, limit = 20) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.brla.gov&q=%s&limit=%d&only=datasets",
    .brla_base, utils::URLencode(query), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BRLA search error: ", e$message)
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

#' View metadata for a single BRLA dataset
#'
#' Returns column names, data types, and descriptions for any dataset.
#' Useful for understanding field structure before querying with
#' \code{\link{brla_query}}.
#'
#' @param dataset_id Character. Four-by-four Socrata dataset identifier.
#'   Example: \code{"pbin-pcm7"} (Crime Incidents), \code{"7ixm-mnvx"} (311)
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{Character. Column name (e.g. "incident_number", "charge_date").}
#'     \item{data_type}{Character. Socrata data type (e.g. "text", "calendar_date", "number").}
#'     \item{description}{Character. Human-readable column description, or NA.}
#'   }
#' @examples
#' brla_view("pbin-pcm7")
brla_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s/columns.json", .brla_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BRLA view error: ", e$message)
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

#' BRLA 311 service requests
#'
#' Citizen requests for service via the Baton Rouge 311 call center.
#' Dataset: 7ixm-mnvx. Ordered by creation date descending. Date and
#' coordinate columns are auto-typed.
#'
#' @param type Character or NULL. Parent type filter (exact match).
#'   Example: \code{"DRAINAGE, EROSION, FLOODING OR HOLES"},
#'   \code{"STREET/TRAFFIC SIGNS AND MARKINGS"}
#' @param status Character or NULL. Status filter.
#'   Example: \code{"OPEN"}, \code{"CLOSED"}
#' @param department Character or NULL. Department filter.
#'   Example: \code{"DPW-MAINTENANCE"}
#' @param since Character or NULL. Date string (YYYY-MM-DD) to filter
#'   requests created on or after this date.
#'   Example: \code{"2025-01-01"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~21 columns including:
#'   \describe{
#'     \item{id}{Character. Request ID.}
#'     \item{statusdesc}{Character. "OPEN" or "CLOSED".}
#'     \item{createdate}{POSIXct. Date/time the request was created.}
#'     \item{lastaction}{POSIXct. Date/time of the last status change.}
#'     \item{parenttype}{Character. Category of request.}
#'     \item{typename}{Character. Specific request type (e.g. "STREET LIGHT OUT").}
#'     \item{streetaddress}{Character. Location address.}
#'     \item{department}{Character. Responsible department.}
#'     \item{latitude, longitude}{Numeric. Coordinates.}
#'   }
#' @examples
#' brla_311(limit = 10)
#' brla_311(status = "OPEN", limit = 20)
#' brla_311(since = "2025-01-01", limit = 50)
brla_311 <- function(type = NULL, status = NULL, department = NULL,
                     since = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))       clauses <- c(clauses, sprintf("parenttype='%s'", type))
  if (!is.null(status))     clauses <- c(clauses, sprintf("statusdesc='%s'", status))
  if (!is.null(department)) clauses <- c(clauses, sprintf("department='%s'", department))
  if (!is.null(since))      clauses <- c(clauses, sprintf("createdate>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("7ixm-mnvx", where = where, limit = limit, offset = offset,
                    order = "createdate DESC")
  if (nrow(df) == 0) return(df)
  if ("createdate" %in% names(df)) df$createdate <- .safe_date(df$createdate)
  if ("lastaction" %in% names(df)) df$lastaction <- .safe_date(df$lastaction)
  if ("latitude" %in% names(df)) df$latitude <- .safe_num(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- .safe_num(df$longitude)
  df
}

#' BRLA crime incidents
#'
#' Baton Rouge Police Department crime incident reports since 2021.
#' Dataset: pbin-pcm7. Ordered by charge date descending. Date and
#' coordinate columns are auto-typed.
#'
#' @param category Character or NULL. Statute category (exact match).
#'   Example: \code{"SIMPLE ASSAULT"}, \code{"THEFT"}, \code{"BURGLARY"}
#' @param neighborhood Character or NULL. Neighborhood name (exact match).
#' @param since Character or NULL. Date string (YYYY-MM-DD) for charges
#'   on or after this date. Example: \code{"2025-01-01"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~24 columns including:
#'   \describe{
#'     \item{incident_number}{Character. Report number (e.g. "26-131555").}
#'     \item{charge_id}{Character. Unique charge identifier.}
#'     \item{charge_date}{POSIXct. Date/time of the charge.}
#'     \item{report_date}{POSIXct. Date/time the report was filed.}
#'     \item{statute_category}{Character. Crime category.}
#'     \item{statute_description}{Character. Detailed statute description.}
#'     \item{offense_description}{Character. NIBRS offense description.}
#'     \item{neighborhood}{Character. Neighborhood name.}
#'     \item{street}{Character. Street location.}
#'     \item{latitude, longitude}{Numeric. Coordinates.}
#'   }
#' @examples
#' brla_crime(limit = 10)
#' brla_crime(category = "THEFT", limit = 20)
#' brla_crime(since = "2025-06-01", limit = 50)
brla_crime <- function(category = NULL, neighborhood = NULL, since = NULL,
                       limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(category))     clauses <- c(clauses, sprintf("statute_category='%s'", category))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhood='%s'", neighborhood))
  if (!is.null(since))        clauses <- c(clauses, sprintf("charge_date>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("pbin-pcm7", where = where, limit = limit, offset = offset,
                    order = "charge_date DESC")
  if (nrow(df) == 0) return(df)
  if ("charge_date" %in% names(df)) df$charge_date <- .safe_date(df$charge_date)
  if ("report_date" %in% names(df)) df$report_date <- .safe_date(df$report_date)
  if ("approved_date" %in% names(df)) df$approved_date <- .safe_date(df$approved_date)
  if ("latitude" %in% names(df)) df$latitude <- .safe_num(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- .safe_num(df$longitude)
  df
}

#' BRLA building permits
#'
#' Construction and occupancy permits in East Baton Rouge Parish.
#' Dataset: 7fq7-8j7r. Ordered by creation date descending. Numeric
#' columns (square footage, project value, permit fee) are auto-typed.
#'
#' @param type Character or NULL. Permit type (exact match).
#'   Example: \code{"Building Permit"}, \code{"Occupancy Permit (C)"}
#' @param designation Character or NULL. Designation (exact match).
#'   Example: \code{"Commercial"}, \code{"Residential"}
#' @param since Character or NULL. Date string (YYYY-MM-DD) for permits
#'   created on or after this date. Example: \code{"2025-01-01"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~24 columns including:
#'   \describe{
#'     \item{permitnumber}{Character. Permit number.}
#'     \item{permittype}{Character. Type (e.g. "Occupancy Permit (C)").}
#'     \item{designation}{Character. "Commercial" or "Residential".}
#'     \item{projectdescription}{Character. Description of the project.}
#'     \item{squarefootage}{Numeric. Project square footage.}
#'     \item{projectvalue}{Numeric. Dollar value of the project.}
#'     \item{permitfee}{Numeric. Permit fee amount.}
#'     \item{creationdate, issueddate}{POSIXct. Key dates.}
#'     \item{streetaddress}{Character. Property address.}
#'   }
#' @examples
#' brla_permits(limit = 10)
#' brla_permits(designation = "Commercial", limit = 20)
brla_permits <- function(type = NULL, designation = NULL, since = NULL,
                         limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))        clauses <- c(clauses, sprintf("permittype='%s'", type))
  if (!is.null(designation)) clauses <- c(clauses, sprintf("designation='%s'", designation))
  if (!is.null(since))       clauses <- c(clauses, sprintf("creationdate>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("7fq7-8j7r", where = where, limit = limit, offset = offset,
                    order = "creationdate DESC")
  if (nrow(df) == 0) return(df)
  if ("creationdate" %in% names(df)) df$creationdate <- .safe_date(df$creationdate)
  if ("issueddate" %in% names(df)) df$issueddate <- .safe_date(df$issueddate)
  if ("squarefootage" %in% names(df)) df$squarefootage <- .safe_num(df$squarefootage)
  if ("projectvalue" %in% names(df)) df$projectvalue <- .safe_num(df$projectvalue)
  if ("permitfee" %in% names(df)) df$permitfee <- .safe_num(df$permitfee)
  df
}

#' BRLA city employees
#'
#' Active and inactive city-parish employees from the payroll system.
#' Dataset: bj3z-jksg. Ordered by department name. Pay and service
#' columns are auto-typed to numeric.
#'
#' @param department Character or NULL. Department name (exact match).
#'   Example: \code{"ANIMAL CONTROL OFFICE"}, \code{"FIRE DEPARTMENT"}
#' @param job_title Character or NULL. Job title (exact match).
#'   Example: \code{"FIREFIGHTER"}, \code{"POLICE OFFICER"}
#' @param status Character or NULL. Employment status code.
#'   \code{"A"} = active. Example: \code{"A"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~21 columns including:
#'   \describe{
#'     \item{last_name, first_name}{Character. Employee name.}
#'     \item{department_name}{Character. Department (e.g. "ANIMAL CONTROL OFFICE").}
#'     \item{job_title}{Character. Job title.}
#'     \item{annual_pay}{Numeric. Annual salary.}
#'     \item{hrly_rate}{Numeric. Hourly rate.}
#'     \item{years_service}{Numeric. Years of service.}
#'     \item{current_hire_date}{POSIXct. Hire date.}
#'     \item{employment_status}{Character. Status code ("A" = active).}
#'   }
#' @examples
#' brla_employees(limit = 10)
#' brla_employees(department = "FIRE DEPARTMENT", limit = 20)
#' brla_employees(status = "A", limit = 50)
brla_employees <- function(department = NULL, job_title = NULL, status = NULL,
                           limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(department)) clauses <- c(clauses, sprintf("department_name='%s'", department))
  if (!is.null(job_title))  clauses <- c(clauses, sprintf("job_title='%s'", job_title))
  if (!is.null(status))     clauses <- c(clauses, sprintf("employment_status='%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("bj3z-jksg", where = where, limit = limit, offset = offset,
                    order = "department_name ASC")
  if (nrow(df) == 0) return(df)
  if ("annual_pay" %in% names(df)) df$annual_pay <- .safe_num(df$annual_pay)
  if ("hrly_rate" %in% names(df)) df$hrly_rate <- .safe_num(df$hrly_rate)
  if ("years_service" %in% names(df)) df$years_service <- .safe_num(df$years_service)
  if ("current_hire_date" %in% names(df)) df$current_hire_date <- .safe_date(df$current_hire_date)
  df
}

#' BRLA expenditures
#'
#' City-Parish department-level spending since 2018. Dataset: vwa8-ps2a.
#' Ordered by check date descending. Contains detailed vendor and fund
#' information with ~49 columns including minority/disadvantaged flags.
#'
#' @param department Character or NULL. Department filter (exact match).
#'   Example: \code{"FIRE DEPARTMENT"}
#' @param vendor Character or NULL. Vendor name filter (partial match,
#'   case-insensitive). Example: \code{"ACME"}
#' @param since Character or NULL. Date string (YYYY-MM-DD) for payments
#'   on or after this date. Example: \code{"2025-01-01"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~49 columns including:
#'   \describe{
#'     \item{year}{Character. Fiscal year.}
#'     \item{fund}{Character. Fund name (e.g. "GENERAL FUND").}
#'     \item{department}{Character. Department name.}
#'     \item{vendor_name_1}{Character. Primary vendor name.}
#'     \item{invoice_line_item_amount}{Numeric. Dollar amount.}
#'     \item{invoice_date}{POSIXct. Invoice date.}
#'     \item{expense_category}{Character. Expense category.}
#'     \item{payment_type, payment_status}{Character. Payment details.}
#'   }
#' @examples
#' brla_expenditures(limit = 10)
#' brla_expenditures(vendor = "ACME", limit = 20)
brla_expenditures <- function(department = NULL, vendor = NULL, since = NULL,
                              limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(department)) clauses <- c(clauses, sprintf("department='%s'", department))
  if (!is.null(vendor))     clauses <- c(clauses, sprintf("upper(vendor_name) like upper('%%%s%%')", vendor))
  if (!is.null(since))      clauses <- c(clauses, sprintf("check_date>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("vwa8-ps2a", where = where, limit = limit, offset = offset,
                    order = "check_date DESC")
  if (nrow(df) == 0) return(df)
  if ("check_date" %in% names(df)) df$check_date <- .safe_date(df$check_date)
  if ("invoice_date" %in% names(df)) df$invoice_date <- .safe_date(df$invoice_date)
  if ("invoice_line_item_amount" %in% names(df)) df$invoice_line_item_amount <- .safe_num(df$invoice_line_item_amount)
  df
}

#' BRLA fire incidents
#'
#' Baton Rouge Fire Department emergency responses. Dataset: dakq-4sda.
#' Ordered by dispatch date descending. Includes incident types, apparatus
#' counts, and casualty information.
#'
#' @param type Character or NULL. Incident type filter (exact match).
#'   Example: \code{"321"} (EMS call)
#' @param since Character or NULL. Date string (YYYY-MM-DD) for incidents
#'   dispatched on or after this date. Example: \code{"2025-01-01"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~33 columns including:
#'   \describe{
#'     \item{inci_no}{Character. Incident number (e.g. "BRFD-021106-0001").}
#'     \item{disp_date}{POSIXct. Dispatch date.}
#'     \item{disp_time}{Character. Dispatch time.}
#'     \item{inci_type}{Character. Incident type code.}
#'     \item{inci_descript}{Character. Incident description (e.g. "EMS call, excluding vehicle accident").}
#'     \item{formattedstreet}{Character. Full street address.}
#'     \item{fatal_fs, inj_fs}{Character. Fire service fatalities/injuries.}
#'     \item{fatal_civ, inj_civ}{Character. Civilian fatalities/injuries.}
#'     \item{clr_date}{Character. Clearance date.}
#'   }
#' @examples
#' brla_fire(limit = 10)
#' brla_fire(since = "2025-01-01", limit = 50)
brla_fire <- function(type = NULL, since = NULL, limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))  clauses <- c(clauses, sprintf("incident_type='%s'", type))
  if (!is.null(since)) clauses <- c(clauses, sprintf("disp_date>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("dakq-4sda", where = where, limit = limit, offset = offset,
                    order = "disp_date DESC")
  if (nrow(df) == 0) return(df)
  if ("disp_date" %in% names(df)) df$disp_date <- .safe_date(df$disp_date)
  if ("latitude" %in% names(df)) df$latitude <- .safe_num(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- .safe_num(df$longitude)
  df
}

#' BRLA traffic crashes
#'
#' Traffic crash incident reports since September 2022. Dataset: 7wah-qncc.
#' Ordered by crash date descending. Includes severity, weather, road
#' conditions, and involvement flags.
#'
#' @param severity Character or NULL. Crash severity filter (exact match).
#' @param since Character or NULL. Date string (YYYY-MM-DD) for crashes
#'   on or after this date. Example: \code{"2025-01-01"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with ~28 columns including:
#'   \describe{
#'     \item{incident_number}{Character. Report number (e.g. "26-131435").}
#'     \item{crash_date}{POSIXct. Date/time of the crash.}
#'     \item{street_name}{Character. Location (e.g. "2000 AIRLINE HWY").}
#'     \item{city}{Character. City (e.g. "Baton Rouge").}
#'     \item{weather}{Character. Weather conditions.}
#'     \item{lighting}{Character. Lighting conditions.}
#'     \item{has_injury, has_fatality}{Character. Injury/fatality flags.}
#'     \item{is_hit_and_run}{Character. Hit-and-run flag.}
#'     \item{total_vehicles}{Character. Number of vehicles involved.}
#'     \item{primary_factor}{Character. Primary contributing factor.}
#'     \item{latitude, longitude}{Numeric. Coordinates.}
#'   }
#' @examples
#' brla_crashes(limit = 10)
#' brla_crashes(since = "2025-01-01", limit = 50)
brla_crashes <- function(severity = NULL, since = NULL,
                         limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(severity)) clauses <- c(clauses, sprintf("crash_severity='%s'", severity))
  if (!is.null(since))    clauses <- c(clauses, sprintf("crash_date>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- brla_query("7wah-qncc", where = where, limit = limit, offset = offset,
                    order = "crash_date DESC")
  if (nrow(df) == 0) return(df)
  if ("crash_date" %in% names(df)) df$crash_date <- .safe_date(df$crash_date)
  if ("latitude" %in% names(df)) df$latitude <- .safe_num(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- .safe_num(df$longitude)
  df
}

# == Pagination helper ========================================================

#' Fetch all rows from a BRLA dataset with auto-pagination
#'
#' Automatically pages through a Socrata dataset, fetching up to
#' \code{max_rows} total records. Stops early if fewer rows remain.
#'
#' @param dataset_id Character. Socrata four-by-four identifier.
#'   Example: \code{"pbin-pcm7"} (Crime), \code{"7ixm-mnvx"} (311)
#' @param where Character or NULL. SoQL WHERE clause.
#' @param q Character or NULL. Full-text search.
#' @param max_rows Integer. Maximum total rows to fetch (default 10000).
#' @param page_size Integer. Rows per API request (default 1000).
#' @return A tibble of all matching rows concatenated together.
#' @examples
#' brla_fetch_all("7ixm-mnvx", where = "statusdesc='OPEN'", max_rows = 5000)
brla_fetch_all <- function(dataset_id, where = NULL, q = NULL,
                           max_rows = 10000, page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- brla_query(dataset_id, where = where, q = q,
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

#' Get brla.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
brla_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(brla_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/brla.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "brla.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# brla.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# brla.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
