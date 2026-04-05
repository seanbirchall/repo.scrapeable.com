# providenceri.gov.R
# Self-contained City of Providence Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at data.providenceri.gov


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pvd_base <- "https://data.providenceri.gov"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

._soda_query <- function(view_id, where = NULL, select = NULL,
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
  url <- sprintf("%s/resource/%s.json?%s", .pvd_base, view_id, query_str)
  url <- utils::URLencode(url, reserved = FALSE)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Providence SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

._soda_fetch_all <- function(view_id, where = NULL, select = NULL,
                             order = NULL, max_rows = 50000,
                             page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(view_id, where = where, select = select,
                          order = order, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), updated_at = as.POSIXct(character())
)

# == Discovery =================================================================

#' List Providence Open Data datasets
#'
#' Returns catalog metadata from the Socrata open data portal at
#' data.providenceri.gov. Each dataset has a unique four-character
#' Socrata view ID (e.g. \code{"vank-fyx9"}) that can be used with
#' \code{pvd_view()} for custom SoQL queries.
#'
#' @param limit Integer. Maximum number of datasets to return
#'   (default 100).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 view identifier.}
#'     \item{name}{Character. Dataset name.}
#'     \item{category}{Character. Dataset category (e.g. \code{"Finance"},
#'       \code{"Public Safety"}).}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{updated_at}{POSIXct. Last update timestamp.}
#'   }
#'
#' @examples
#' pvd_list(limit = 10)
#'
#' @seealso \code{\link{pvd_search}}, \code{\link{pvd_view}}
#' @export
pvd_list <- function(limit = 100) {
  url <- sprintf("%s/api/views?limit=%d", .pvd_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Providence catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)

  tibble(
    id          = as.character(raw$id),
    name        = as.character(raw$name),
    category    = as.character(raw$category %||% NA_character_),
    description = vapply(raw$description %||% rep(NA_character_, length(raw$id)),
                         function(x) {
                           x <- x %||% ""
                           if (is.na(x)) x <- ""
                           if (nchar(x) > 200) paste0(substr(x, 1, 200), "...") else x
                         }, character(1)),
    updated_at  = as.POSIXct(as.numeric(raw$rowsUpdatedAt %||% NA_real_),
                             origin = "1970-01-01")
  )
}

#' Search Providence Open Data datasets by keyword
#'
#' Downloads the full dataset catalog via \code{pvd_list()} and filters
#' locally by case-insensitive keyword match against both dataset name
#' and description.
#'
#' @param query Character. Search term (case-insensitive, matched against
#'   \code{name} and \code{description} via \code{grepl}).
#' @param limit Integer. Maximum number of catalog entries to scan
#'   (default 300).
#'
#' @return A tibble with columns: \code{id}, \code{name}, \code{category},
#'   \code{description}, \code{updated_at}. Same schema as \code{pvd_list()}.
#'
#' @examples
#' pvd_search("tax")
#' pvd_search("police")
#'
#' @seealso \code{\link{pvd_list}}, \code{\link{pvd_view}}
#' @export
pvd_search <- function(query, limit = 300) {
  all_ds <- pvd_list(limit = limit)
  if (nrow(all_ds) == 0) return(.schema_datasets)
  all_ds |> filter(
    grepl(query, name, ignore.case = TRUE) |
    grepl(query, description, ignore.case = TRUE)
  )
}

# == Generic SODA query ========================================================

#' Query any Providence dataset via Socrata SoQL
#'
#' A generic SODA (Socrata Open Data API) query interface for any dataset
#' on data.providenceri.gov. Pass a 4x4 view identifier and optional
#' SoQL clauses. Discover view IDs with \code{pvd_list()} or
#' \code{pvd_search()}.
#'
#' @param view_id Character. Socrata 4x4 view identifier (e.g.
#'   \code{"vank-fyx9"} for arrests).
#' @param where Character or \code{NULL}. SoQL WHERE clause for filtering
#'   rows (e.g. \code{"year = '2024'"}).
#' @param select Character or \code{NULL}. SoQL SELECT clause for choosing
#'   columns (e.g. \code{"name, amount"}).
#' @param group Character or \code{NULL}. SoQL GROUP BY clause.
#' @param order Character or \code{NULL}. SoQL ORDER BY clause
#'   (e.g. \code{"date DESC"}).
#' @param q Character or \code{NULL}. Full-text search query.
#' @param limit Integer. Maximum number of rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#'
#' @return A tibble of query results. Column names and types depend on the
#'   dataset.
#'
#' @examples
#' pvd_view("vank-fyx9", limit = 5)
#'
#' @seealso \code{\link{pvd_list}}, \code{\link{pvd_search}}
#' @export
pvd_view <- function(view_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select, group = group,
               order = order, q = q, limit = limit, offset = offset)
}

# == Police & Public Safety ====================================================

#' Providence police arrests and citations (rolling 180 days)
#'
#' Returns adult arrest and citation records from the Providence Police
#' Department. Data covers a rolling 180-day window and includes
#' demographics, statute information, and arresting officer details.
#'
#' @param statute_type Character or \code{NULL}. Filter by statute type
#'   (e.g. \code{"RI Statute Violation"}, \code{"Municipal Ordinance"}).
#' @param gender Character or \code{NULL}. Filter by gender
#'   (e.g. \code{"Male"}, \code{"Female"}).
#' @param start_date Character or \code{NULL}. Start date in
#'   \code{"YYYY-MM-DD"} format.
#' @param end_date Character or \code{NULL}. End date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble with columns including \code{arrest_date} (POSIXct),
#'   \code{last_name}, \code{first_name}, \code{gender}, \code{race},
#'   \code{age} (integer), \code{statute_type}, \code{statute_code},
#'   \code{statute_desc}, \code{counts} (integer), and
#'   \code{arresting_officers}.
#'
#' @examples
#' pvd_arrests(limit = 5)
#' pvd_arrests(start_date = "2026-03-01", limit = 10)
#'
#' @seealso \code{\link{pvd_case_log}}, \code{\link{pvd_view}}
#' @export
pvd_arrests <- function(statute_type = NULL, gender = NULL,
                        start_date = NULL, end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(statute_type))
    clauses <- c(clauses, sprintf("statute_type = '%s'", statute_type))
  if (!is.null(gender))
    clauses <- c(clauses, sprintf("gender = '%s'", gender))
  if (!is.null(start_date))
    clauses <- c(clauses, sprintf("arrest_date >= '%s'", start_date))
  if (!is.null(end_date))
    clauses <- c(clauses, sprintf("arrest_date <= '%s'", end_date))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("vank-fyx9", where = where,
                      order = "arrest_date DESC", limit = limit)
  if (nrow(df) > 0) {
    if ("arrest_date" %in% names(df))
      df$arrest_date <- as.POSIXct(df$arrest_date, format = "%Y-%m-%dT%H:%M:%S")
    if ("counts" %in% names(df))
      df$counts <- as.integer(df$counts)
    if ("age" %in% names(df))
      df$age <- as.integer(df$age)
  }
  df
}

#' Providence police case log (rolling 180 days)
#'
#' Returns recorded state and municipal offenses from the Providence
#' Police Department AEGIS records management system. Covers a rolling
#' 180-day window with offense descriptions, locations, and report dates.
#'
#' @param start_date Character or \code{NULL}. Start date in
#'   \code{"YYYY-MM-DD"} format (filters on \code{reported_date}).
#' @param end_date Character or \code{NULL}. End date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of case log records, ordered by reported date
#'   descending.
#'
#' @examples
#' pvd_case_log(limit = 5)
#'
#' @seealso \code{\link{pvd_arrests}}, \code{\link{pvd_view}}
#' @export
pvd_case_log <- function(start_date = NULL, end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(start_date))
    clauses <- c(clauses, sprintf("reported_date >= '%s'", start_date))
  if (!is.null(end_date))
    clauses <- c(clauses, sprintf("reported_date <= '%s'", end_date))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("rz3y-pz8v", where = where,
               order = "reported_date DESC", limit = limit)
}

# == Finance ===================================================================

#' City of Providence expenditures
#'
#' Returns vendor payment records from the City of Providence, available
#' from FY2017 onward. Supports filtering by fiscal year, vendor name,
#' and department. The \code{amount} column is coerced to numeric.
#'
#' @param fiscal_year Character or \code{NULL}. Filter by fiscal year
#'   (e.g. \code{"2023"}).
#' @param vendor_name Character or \code{NULL}. Partial, case-insensitive
#'   vendor name filter (e.g. \code{"construction"}).
#' @param department Character or \code{NULL}. Partial, case-insensitive
#'   department name filter (e.g. \code{"police"}).
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of expenditure records with \code{amount} as numeric.
#'
#' @examples
#' pvd_expenditures(fiscal_year = "2023", limit = 10)
#'
#' @seealso \code{\link{pvd_purchase_orders}}, \code{\link{pvd_revenue_budget}}
#' @export
pvd_expenditures <- function(fiscal_year = NULL, vendor_name = NULL,
                             department = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(fiscal_year))
    clauses <- c(clauses, sprintf("fiscal_year = '%s'", fiscal_year))
  if (!is.null(vendor_name))
    clauses <- c(clauses, sprintf("upper(vendor_name) LIKE '%%%s%%'",
                                  toupper(vendor_name)))
  if (!is.null(department))
    clauses <- c(clauses, sprintf("upper(department) LIKE '%%%s%%'",
                                  toupper(department)))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("u4ks-kiwa", where = where, limit = limit)
  if (nrow(df) > 0 && "amount" %in% names(df))
    df$amount <- as.numeric(df$amount)
  df
}

#' City of Providence purchase orders
#'
#' Returns City and School Department purchase orders from FY2020 onward.
#'
#' @param fiscal_year Character or \code{NULL}. Filter by fiscal year
#'   (e.g. \code{"2023"}).
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of purchase order records.
#'
#' @examples
#' pvd_purchase_orders(limit = 5)
#'
#' @seealso \code{\link{pvd_expenditures}}, \code{\link{pvd_expense_budget}}
#' @export
pvd_purchase_orders <- function(fiscal_year = NULL, limit = 1000) {
  where <- if (!is.null(fiscal_year))
    sprintf("fiscal_year = '%s'", fiscal_year) else NULL
  ._soda_query("425y-pm5m", where = where, limit = limit)
}

#' City of Providence revenue budget
#'
#' Returns annual revenue budget data per department, available from
#' FY2012 onward.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of revenue budget records.
#'
#' @examples
#' pvd_revenue_budget(limit = 10)
#'
#' @seealso \code{\link{pvd_expense_budget}}, \code{\link{pvd_expenditures}}
#' @export
pvd_revenue_budget <- function(limit = 1000) {
  ._soda_query("9jte-7uk8", limit = limit)
}

#' City of Providence pension payroll
#'
#' Returns monthly pension payroll data grouped by benefit type.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of pension payroll records.
#'
#' @examples
#' pvd_pension_payroll(limit = 10)
#'
#' @seealso \code{\link{pvd_expenditures}}
#' @export
pvd_pension_payroll <- function(limit = 1000) {
  ._soda_query("rehs-z98t", limit = limit)
}

#' Unclaimed property (uncashed city checks)
#'
#' Returns records of checks issued by the City of Providence that have
#' not been cashed after one year, constituting unclaimed property.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of unclaimed property records.
#'
#' @examples
#' pvd_unclaimed_property(limit = 10)
#'
#' @export
pvd_unclaimed_property <- function(limit = 1000) {
  ._soda_query("4hhd-fzq6", limit = limit)
}

# == Property Tax ==============================================================

#' Providence property tax roll
#'
#' Returns property tax assessment data for a given calendar year.
#' Includes residential, commercial, and publicly held parcels with
#' assessed values, exemptions, and computed taxes. Numeric columns
#' \code{total_assmt}, \code{total_exempt}, and \code{total_taxes}
#' are coerced to numeric.
#'
#' @param year Integer. Calendar year (default 2025). Available years:
#'   2002--2010, 2012--2014, 2016--2025 (not all years are present).
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble with ~31 columns including \code{plat}, \code{lot},
#'   \code{class}, \code{formated_address}, \code{total_assmt} (numeric),
#'   \code{total_exempt} (numeric), and \code{total_taxes} (numeric).
#'
#' @examples
#' pvd_property_tax(year = 2025, limit = 5)
#'
#' @seealso \code{\link{pvd_property_ids}}, \code{\link{pvd_motor_vehicle_tax}}
#' @export
pvd_property_tax <- function(year = 2025, limit = 1000) {
  year_map <- c(
    "2025" = "6ub4-iebe", "2024" = "xvti-7dtw", "2023" = "fd8d-n74v",
    "2022" = "c3q4-f95q", "2021" = "fawc-z8zq", "2020" = "y9h5-fefu",
    "2019" = "twey-459r", "2018" = "yghh-hsch", "2017" = "ku9m-5rhr",
    "2016" = "czje-unnn", "2014" = "t2vi-f8qs", "2013" = "a6uy-vymr",
    "2012" = "fgwg-7viq", "2010" = "p6bs-8exs", "2009" = "9p3h-3q5i",
    "2008" = "swgn-agtw", "2007" = "iafn-rvmp", "2006" = "aymi-rpqq",
    "2005" = "yqdi-fbw5", "2004" = "fvsz-rky3", "2003" = "fek9-hu38",
    "2002" = "e3qk-fibu"
  )
  yr <- as.character(year)
  if (!(yr %in% names(year_map))) {
    warning("Year ", yr, " not available. Available: ",
            paste(sort(names(year_map)), collapse = ", "))
    return(tibble())
  }
  df <- ._soda_query(year_map[[yr]], limit = limit)
  if (nrow(df) > 0) {
    for (col in c("total_assmt", "total_exempt", "total_taxes")) {
      if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
    }
  }
  df
}

#' Providence motor vehicle tax roll (2017)
#'
#' Returns motor vehicle tax records for the 2017 tax year.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of motor vehicle tax records.
#'
#' @examples
#' pvd_motor_vehicle_tax(limit = 5)
#'
#' @seealso \code{\link{pvd_property_tax}}
#' @export
pvd_motor_vehicle_tax <- function(limit = 1000) {
  ._soda_query("npyn-gmfa", limit = limit)
}

# == Budgets ===================================================================

#' Providence proposed expense budget
#'
#' Returns city proposed expense budgets broken out by department and
#' line item for a given fiscal year. Each fiscal year is a separate
#' Socrata dataset.
#'
#' @param fiscal_year Character. Fiscal year to query. One of:
#'   \code{"2023"}, \code{"2022"}, \code{"2021"}, \code{"2020"},
#'   \code{"2019"}, \code{"2018"}, \code{"2017"}, \code{"2015"},
#'   \code{"2014"}, \code{"2013"} (default \code{"2023"}).
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of expense budget line items.
#'
#' @examples
#' pvd_expense_budget("2023", limit = 10)
#'
#' @seealso \code{\link{pvd_revenue_budget}}, \code{\link{pvd_expenditures}}
#' @export
pvd_expense_budget <- function(fiscal_year = "2023", limit = 1000) {
  fy_map <- c(
    "2023" = "ka8x-eb9x", "2022" = "ejse-evtt", "2021" = "khch-nbyg",
    "2020" = "xxiz-eb4k", "2019" = "hfdy-4mye", "2018" = "jcrz-ukry",
    "2017" = "63nj-zuyh", "2015" = "xkd5-s9ff", "2014" = "sa6n-hp9d",
    "2013" = "7wa4-fft7"
  )
  fy <- as.character(fiscal_year)
  if (!(fy %in% names(fy_map))) {
    warning("FY ", fy, " not available. Available: ",
            paste(sort(names(fy_map)), collapse = ", "))
    return(tibble())
  }
  ._soda_query(fy_map[[fy]], limit = limit)
}

# == Neighborhoods & Infrastructure ============================================

#' Providence building permits (2009--2018)
#'
#' Returns building, electrical, mechanical, and plumbing permits issued
#' by the Providence Department of Inspections and Standards, covering
#' the period 2009--2018.
#'
#' @param permit_type Character or \code{NULL}. Filter by permit type
#'   (case-insensitive partial match, e.g. \code{"Building"},
#'   \code{"Electrical"}).
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of permit records.
#'
#' @examples
#' pvd_permits(limit = 5)
#' pvd_permits(permit_type = "Building", limit = 10)
#'
#' @seealso \code{\link{pvd_property_tax}}, \code{\link{pvd_view}}
#' @export
pvd_permits <- function(permit_type = NULL, limit = 1000) {
  where <- if (!is.null(permit_type))
    sprintf("upper(permit_type) LIKE '%%%s%%'", toupper(permit_type)) else NULL
  ._soda_query("ufmm-rbej", where = where, limit = limit)
}

#' Providence fire hydrant locations
#'
#' Returns fire hydrant locations from a 2013 survey of Providence.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of hydrant location records with coordinates.
#'
#' @examples
#' pvd_fire_hydrants(limit = 10)
#'
#' @export
pvd_fire_hydrants <- function(limit = 1000) {
  ._soda_query("iuy2-4fux", limit = limit)
}

#' Providence street tree inventory
#'
#' Returns the complete inventory of street trees from the 2006 tree
#' census (last updated 2016). Includes species, size, and location data.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of tree inventory records.
#'
#' @examples
#' pvd_trees(limit = 10)
#'
#' @export
pvd_trees <- function(limit = 1000) {
  ._soda_query("uv9w-h8i4", limit = limit)
}

#' Community gardens in Providence
#'
#' Returns public and privately owned community garden locations from
#' 2014 data.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of community garden records.
#'
#' @examples
#' pvd_community_gardens(limit = 10)
#'
#' @export
pvd_community_gardens <- function(limit = 1000) {
  ._soda_query("uj2v-k4s7", limit = limit)
}

#' Goodwill collection bin locations in Providence
#'
#' Returns locations of Goodwill donation/collection bins across
#' Providence.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of Goodwill bin location records.
#'
#' @examples
#' pvd_goodwill_bins(limit = 10)
#'
#' @export
pvd_goodwill_bins <- function(limit = 1000) {
  ._soda_query("2azu-r5bu", limit = limit)
}

#' Providence trash pickup schedule
#'
#' Returns scheduled trash collection areas and days for Providence
#' neighborhoods.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of trash pickup schedule area records.
#'
#' @examples
#' pvd_trash_schedule(limit = 10)
#'
#' @export
pvd_trash_schedule <- function(limit = 1000) {
  ._soda_query("7es9-emf3", limit = limit)
}

# == Licensing =================================================================

#' Active business licenses in Providence (historical)
#'
#' Returns business license records. Note: this dataset is no longer
#' being updated by the city.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of business license records.
#'
#' @examples
#' pvd_business_licenses(limit = 10)
#'
#' @seealso \code{\link{pvd_food_trucks}},
#'   \code{\link{pvd_entertainment_licenses}}
#' @export
pvd_business_licenses <- function(limit = 1000) {
  ._soda_query("ui7z-kv69", limit = limit)
}

#' Mobile food establishment licenses
#'
#' Returns active food truck and mobile food vendor licenses in
#' Providence.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of mobile food establishment license records.
#'
#' @examples
#' pvd_food_trucks(limit = 10)
#'
#' @seealso \code{\link{pvd_business_licenses}}
#' @export
pvd_food_trucks <- function(limit = 1000) {
  ._soda_query("u7ik-g787", limit = limit)
}

#' Monthly entertainment licenses
#'
#' Returns entertainment license application records for Providence
#' venues and events.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of entertainment license records.
#'
#' @examples
#' pvd_entertainment_licenses(limit = 10)
#'
#' @seealso \code{\link{pvd_business_licenses}}
#' @export
pvd_entertainment_licenses <- function(limit = 1000) {
  ._soda_query("2f79-9nkc", limit = limit)
}

# == Schools & Energy ==========================================================

#' Providence public schools
#'
#' Returns location and contact information for public schools in
#' Providence.
#'
#' @param limit Integer. Maximum number of rows (default 200).
#'
#' @return A tibble of public school records with addresses and
#'   coordinates.
#'
#' @examples
#' pvd_schools()
#'
#' @export
pvd_schools <- function(limit = 200) {
  ._soda_query("pu8z-v46s", limit = limit)
}

#' Municipal building energy use
#'
#' Returns energy consumption data for Providence municipal buildings,
#' broken out by year and building.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of energy use records.
#'
#' @examples
#' pvd_building_energy(limit = 10)
#'
#' @seealso \code{\link{pvd_electricity_use}}
#' @export
pvd_building_energy <- function(limit = 1000) {
  ._soda_query("dmye-wwhm", limit = limit)
}

#' Total municipal electricity use (FY2010--FY2016)
#'
#' Returns aggregate municipal electricity consumption records for
#' Providence, covering fiscal years 2010 through 2016.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of electricity use records.
#'
#' @examples
#' pvd_electricity_use(limit = 10)
#'
#' @seealso \code{\link{pvd_building_energy}}
#' @export
pvd_electricity_use <- function(limit = 1000) {
  ._soda_query("5726-aqx9", limit = limit)
}

# == GIS / Zoning ==============================================================

#' Providence property ID catalog
#'
#' Returns property identifiers with address and Plat/Lot/Unit data,
#' useful for homestead exemption lookups and cross-referencing with
#' tax roll data.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of property ID records.
#'
#' @examples
#' pvd_property_ids(limit = 10)
#'
#' @seealso \code{\link{pvd_property_tax}}
#' @export
pvd_property_ids <- function(limit = 1000) {
  ._soda_query("k6gu-363f", limit = limit)
}

#' Providence zip code boundaries
#'
#' Returns zip code boundary records for Providence.
#'
#' @param limit Integer. Maximum number of rows (default 100).
#'
#' @return A tibble of zip code boundary records.
#'
#' @examples
#' pvd_zip_codes()
#'
#' @export
pvd_zip_codes <- function(limit = 100) {
  ._soda_query("sm57-du4m", limit = limit)
}

#' Providence enterprise zones
#'
#' Returns areas designated for business tax incentives in Providence.
#'
#' @param limit Integer. Maximum number of rows (default 100).
#'
#' @return A tibble of enterprise zone records.
#'
#' @examples
#' pvd_enterprise_zones()
#'
#' @export
pvd_enterprise_zones <- function(limit = 100) {
  ._soda_query("vynu-sgur", limit = limit)
}

# == Holidays ==================================================================

#' Providence city holiday schedule
#'
#' Returns the official City of Providence holiday schedule for a given
#' calendar year.
#'
#' @param year Integer. Calendar year (default 2025). Available years:
#'   2013, 2014, 2025.
#' @param limit Integer. Maximum number of rows (default 50).
#'
#' @return A tibble of city holiday dates and names.
#'
#' @examples
#' pvd_holidays(year = 2025)
#'
#' @export
pvd_holidays <- function(year = 2025, limit = 50) {
  year_map <- c(
    "2025" = "9zbu-vjd2",
    "2014" = "c6wc-vavf",
    "2013" = "ubra-37q7"
  )
  yr <- as.character(year)
  if (!(yr %in% names(year_map))) {
    warning("Year ", yr, " not available. Available: ",
            paste(names(year_map), collapse = ", "))
    return(tibble())
  }
  ._soda_query(year_map[[yr]], limit = limit)
}

# == 1940 Assessment Records ===================================================

#' 1940 City field assessment records
#'
#' Returns digitized residential assessment records (field cards) from
#' the 1940 city assessment. Useful for historical property research.
#'
#' @param limit Integer. Maximum number of rows (default 1000).
#'
#' @return A tibble of assessment record metadata.
#'
#' @examples
#' pvd_1940_assessments(limit = 10)
#'
#' @export
pvd_1940_assessments <- function(limit = 1000) {
  ._soda_query("2pca-62ru", limit = limit)
}

# == Context ===================================================================

#' Get providenceri.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pvd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pvd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/providenceri.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "providenceri.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# providenceri.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# providenceri.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
