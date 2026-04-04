# maryland.gov.R
# Self-contained State of Maryland Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at opendata.maryland.gov
# Datasets: ~1500 Socrata views covering crime, health, budget, workforce, etc.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.md_base <- "https://opendata.maryland.gov"

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
  url <- sprintf("%s/resource/%s.json?%s", .md_base, dataset_id,
                 utils::URLencode(query_str, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Maryland Open Data query error: ", e$message)
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

#' List Maryland Open Data datasets
#'
#' Browse the catalog of approximately 1,500 Socrata datasets published
#' on opendata.maryland.gov, with pagination support.
#'
#' @param limit Integer. Number of datasets to return per page
#'   (default \code{50}).
#' @param page Integer. Page number, 1-indexed (default \code{1}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Four-by-four Socrata dataset identifier
#'       (e.g. \code{"jwfa-fdxs"}).}
#'     \item{name}{Character. Dataset title.}
#'     \item{category}{Character. Domain category
#'       (e.g. \code{"Public Safety"}, \code{"Transportation"}),
#'       or \code{NA}.}
#'     \item{description}{Character. Description, truncated to 200 chars.}
#'     \item{updated_at}{Character. ISO-8601 last-updated timestamp.}
#'     \item{type}{Character. Asset type (e.g. \code{"dataset"},
#'       \code{"story"}, \code{"map"}).}
#'   }
#' @examples
#' md_list(limit = 10)
#' md_list(limit = 20, page = 2)
#' @export
md_list <- function(limit = 50, page = 1) {
  offset <- (page - 1) * limit
  url <- sprintf(
    "%s/api/catalog/v1?domains=opendata.maryland.gov&search_context=opendata.maryland.gov&limit=%d&offset=%d",
    .md_base, limit, offset
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Maryland catalog error: ", e$message)
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


#' Search Maryland Open Data datasets
#'
#' Full-text search across the opendata.maryland.gov catalog. Returns
#' matching dataset metadata ranked by relevance.
#'
#' @param query Character. Search terms (e.g. \code{"crime"},
#'   \code{"health"}, \code{"budget"}, \code{"vehicle sales"}).
#' @param limit Integer. Maximum results to return (default \code{20}).
#' @param only Character. Optional asset-type filter. One of
#'   \code{"datasets"}, \code{"maps"}, \code{"charts"}, or \code{NULL}
#'   (default, no filter).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata four-by-four dataset ID.}
#'     \item{name}{Character. Dataset title.}
#'     \item{category}{Character. Domain category, or \code{NA}.}
#'     \item{description}{Character. Description, truncated to 200 chars.}
#'     \item{updated_at}{Character. ISO-8601 last-updated timestamp.}
#'     \item{type}{Character. Asset type.}
#'   }
#' @examples
#' md_search("crime")
#' md_search("education", only = "datasets")
#' @export
md_search <- function(query, limit = 20, only = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=opendata.maryland.gov&search_context=opendata.maryland.gov&q=%s&limit=%d",
    .md_base, utils::URLencode(query, reserved = TRUE), limit
  )
  if (!is.null(only)) url <- paste0(url, "&only=", only)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Maryland search error: ", e$message)
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


#' View metadata and schema for a Maryland dataset
#'
#' Returns column names, data types, and descriptions for any Socrata
#' dataset, useful for building \code{\link{md_query}} calls.
#'
#' @param dataset_id Character. Socrata four-by-four view ID
#'   (e.g. \code{"jwfa-fdxs"}, \code{"un65-7ipd"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{Character. Column/field name in the dataset.}
#'     \item{data_type}{Character. Socrata data type
#'       (e.g. \code{"text"}, \code{"number"}, \code{"calendar_date"}).}
#'     \item{description}{Character. Column description, or \code{NA}.}
#'   }
#' @examples
#' md_view("jwfa-fdxs")
#' @export
md_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .md_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Maryland view metadata error: ", e$message)
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

#' Query any Maryland Open Data dataset
#'
#' Runs an arbitrary SoQL query against any Socrata dataset on
#' opendata.maryland.gov. Use \code{\link{md_view}} to inspect available
#' columns before building queries.
#'
#' @param dataset_id Character. Socrata four-by-four view ID
#'   (e.g. \code{"jwfa-fdxs"}).
#' @param where Character. SoQL WHERE clause for filtering rows
#'   (e.g. \code{"year='2020'"}). \code{NULL} for no filter.
#' @param select Character. SoQL SELECT clause for choosing columns
#'   (e.g. \code{"jurisdiction, year, murder"}). \code{NULL} returns all.
#' @param group Character. SoQL GROUP BY clause
#'   (e.g. \code{"jurisdiction"}). \code{NULL} for no grouping.
#' @param order Character. SoQL ORDER BY clause
#'   (e.g. \code{"year DESC"}). \code{NULL} for default order.
#' @param q Character. Full-text search string. \code{NULL} for none.
#' @param limit Integer. Maximum rows to return (default \code{1000}).
#' @param offset Integer. Pagination offset (default \code{0}).
#' @return A tibble whose columns depend on the queried dataset.
#' @examples
#' md_query("jwfa-fdxs", where = "year='2020'", limit = 10)
#' md_query("jwfa-fdxs", select = "jurisdiction, year, murder",
#'          order = "year DESC")
#' @export
md_query <- function(dataset_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from a Maryland dataset with auto-pagination
#'
#' Automatically pages through the Socrata API until all matching rows
#' are retrieved (up to \code{max_rows}). Useful for datasets that
#' exceed the single-request limit of 1,000 rows.
#'
#' @param dataset_id Character. Socrata four-by-four view ID
#'   (e.g. \code{"jwfa-fdxs"}).
#' @param where Character. Optional SoQL WHERE clause. \code{NULL}
#'   for no filter.
#' @param select Character. Optional SoQL SELECT clause. \code{NULL}
#'   returns all columns.
#' @param order Character. Optional SoQL ORDER BY clause. \code{NULL}
#'   for default order.
#' @param max_rows Integer. Maximum total rows to fetch across all
#'   pages (default \code{50000}).
#' @param page_size Integer. Number of rows per API request
#'   (default \code{10000}).
#' @return A tibble of all matching records, columns depending on
#'   the dataset.
#' @examples
#' md_fetch_all("jwfa-fdxs")
#' @export
md_fetch_all <- function(dataset_id, where = NULL, select = NULL,
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


#' Get row count for any Maryland dataset
#'
#' Returns the total number of rows in a Socrata dataset, optionally
#' filtered by a WHERE clause. Useful for planning pagination or
#' checking data volume before a full fetch.
#'
#' @param dataset_id Character. Socrata four-by-four view ID
#'   (e.g. \code{"jwfa-fdxs"}).
#' @param where Character. Optional SoQL WHERE clause to count a
#'   subset (e.g. \code{"jurisdiction='Baltimore City'"}). \code{NULL}
#'   counts all rows.
#' @return Integer. Total row count.
#' @examples
#' md_count("jwfa-fdxs")
#' md_count("jwfa-fdxs", where = "year='2020'")
#' @export
md_count <- function(dataset_id, where = NULL) {
  ._soda_query(dataset_id, select = "count(*) as n", where = where) |>
    pull("n") |>
    as.integer()
}


# == Named convenience functions ===============================================

#' Fetch Maryland violent crime and property crime by county
#'
#' Queries Socrata dataset \code{jwfa-fdxs} (Violent Crime & Property Crime
#' by County: 1975 to Present). Returns counts and per-100K rates for
#' murder, rape, robbery, aggravated assault, breaking & entering,
#' larceny/theft, and motor vehicle theft.
#'
#' @param jurisdiction Character. County or jurisdiction name filter
#'   (e.g. \code{"Baltimore City"}, \code{"Montgomery County"},
#'   \code{"Allegany County"}). \code{NULL} (default) returns all.
#' @param year Character. Year filter as a string
#'   (e.g. \code{"2020"}, \code{"2023"}). \code{NULL} for all years.
#' @param limit Integer. Maximum rows to return (default \code{1000}).
#' @return A tibble with 38 columns including: \code{jurisdiction},
#'   \code{year}, \code{population}, \code{murder}, \code{rape},
#'   \code{robbery}, \code{agg_assault}, \code{b_e},
#'   \code{larceny_theft}, \code{m_v_theft}, \code{grand_total},
#'   \code{violent_crime_total}, \code{property_crime_totals}, and
#'   corresponding \code{*_per_100_000_people} rate columns. Counts
#'   are integer; rates are numeric.
#' @examples
#' md_crimes(jurisdiction = "Baltimore City", year = "2020")
#' md_crimes(year = "2023", limit = 30)
#' @export
md_crimes <- function(jurisdiction = NULL, year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(jurisdiction)) clauses <- c(clauses, sprintf("jurisdiction='%s'", jurisdiction))
  if (!is.null(year))         clauses <- c(clauses, sprintf("year='%s'", year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("jwfa-fdxs", where = where, order = "year DESC", limit = limit)
  if (nrow(df) == 0) return(df)
  num_cols <- c("population", "murder", "rape", "robbery", "agg_assault",
                "b_e", "larceny_theft", "m_v_theft", "grand_total",
                "violent_crime_total", "property_crime_totals")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }
  rate_cols <- grep("per_100_000", names(df), value = TRUE)
  for (col in rate_cols) df[[col]] <- as.numeric(df[[col]])
  df
}


#' Fetch Maryland operating budget data
#'
#' Queries Socrata dataset \code{yu65-jmmv} (Maryland State Operating
#' Budget). Returns budget line items by agency, fund type, and fiscal
#' year.
#'
#' @param agency_name Character. Partial-match filter on agency name
#'   (case-insensitive), e.g. \code{"education"}, \code{"health"},
#'   \code{"transportation"}. \code{NULL} for all agencies.
#' @param fiscal_year Character. Fiscal year filter
#'   (e.g. \code{"2024"}). \code{NULL} for all years.
#' @param fund_type Character. Fund type filter, e.g.
#'   \code{"General Funds"}, \code{"Special Funds"},
#'   \code{"Federal Funds"}. \code{NULL} for all fund types.
#' @param limit Integer. Maximum rows (default \code{1000}).
#' @return A tibble of budget line items. Key columns include
#'   \code{agency_name}, \code{fiscal_year}, \code{fund_type_name},
#'   \code{budget} (numeric, dollar amount), among others.
#' @examples
#' md_budget(agency_name = "education", fiscal_year = "2024")
#' md_budget(fund_type = "General Funds", limit = 50)
#' @export
md_budget <- function(agency_name = NULL, fiscal_year = NULL,
                      fund_type = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(agency_name)) clauses <- c(clauses, sprintf("upper(agency_name) LIKE '%%%s%%'", toupper(agency_name)))
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  if (!is.null(fund_type))   clauses <- c(clauses, sprintf("fund_type_name='%s'", fund_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("yu65-jmmv", where = where, limit = limit)
  if (nrow(df) > 0 && "budget" %in% names(df)) df$budget <- as.numeric(df$budget)
  df
}


#' Fetch Maryland vehicle sales data by month
#'
#' Queries Socrata dataset \code{un65-7ipd} (MVA Vehicle Sales Counts
#' by Month: 2002 to Present). Returns monthly counts and dollar totals
#' for new and used vehicle sales.
#'
#' @param year Character. Year filter (e.g. \code{"2023"},
#'   \code{"2024"}). \code{NULL} for all years.
#' @param month Character. Three-letter uppercase month abbreviation
#'   (e.g. \code{"JAN"}, \code{"FEB"}, \code{"MAR"}, ... \code{"DEC"}).
#'   \code{NULL} for all months.
#' @param limit Integer. Maximum rows (default \code{1000}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Character. Calendar year.}
#'     \item{month}{Character. Month abbreviation (e.g. \code{"JAN"}).}
#'     \item{new}{Integer. Number of new vehicles sold.}
#'     \item{used}{Integer. Number of used vehicles sold.}
#'     \item{total_sales_new}{Numeric. Total dollar value of new vehicle
#'       sales.}
#'     \item{total_sales_used}{Numeric. Total dollar value of used vehicle
#'       sales.}
#'   }
#' @examples
#' md_vehicle_sales(year = "2023")
#' md_vehicle_sales(year = "2024", month = "JAN")
#' @export
md_vehicle_sales <- function(year = NULL, month = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year))  clauses <- c(clauses, sprintf("year='%s'", year))
  if (!is.null(month)) clauses <- c(clauses, sprintf("month='%s'", toupper(month)))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("un65-7ipd", where = where, order = "year DESC", limit = limit)
  if (nrow(df) == 0) return(df)
  for (col in c("new", "used")) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }
  for (col in c("total_sales_new", "total_sales_used")) {
    if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
  }
  df
}


#' Fetch Maryland power outage data by county
#'
#' Queries Socrata dataset \code{uxq4-6wxf} (Power Outages by County).
#' Returns timestamped snapshots of outage counts and affected customers
#' by county/area.
#'
#' @param area Character. County or area filter
#'   (e.g. \code{"Baltimore City"}, \code{"Montgomery"}).
#'   \code{NULL} (default) returns all areas.
#' @param limit Integer. Maximum rows (default \code{1000}).
#' @return A tibble with columns including: \code{area},
#'   \code{dt_stamp} (timestamp), \code{outages} (integer),
#'   \code{customers} (integer), \code{percent_out} (numeric).
#'   Sorted by timestamp descending (most recent first).
#' @examples
#' md_power_outages(area = "Montgomery", limit = 20)
#' @export
md_power_outages <- function(area = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(area)) clauses <- c(clauses, sprintf("area='%s'", area))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("uxq4-6wxf", where = where, order = "dt_stamp DESC", limit = limit)
  if (nrow(df) == 0) return(df)
  if ("outages" %in% names(df))    df$outages   <- as.integer(df$outages)
  if ("customers" %in% names(df))  df$customers <- as.integer(df$customers)
  if ("percent_out" %in% names(df)) df$percent_out <- as.numeric(df$percent_out)
  df
}


#' Fetch Maryland county education comparison data
#'
#' Queries Socrata dataset \code{63pe-mygy} (Choose Maryland: Compare
#' Counties - Education). Returns educational attainment, school
#' expenditure, and enrollment metrics by county.
#'
#' @param county Character. County name filter
#'   (e.g. \code{"Montgomery County"}, \code{"Baltimore County"}).
#'   \code{NULL} (default) returns all counties.
#' @param limit Integer. Maximum rows (default \code{100}).
#' @return A tibble with columns including: \code{county},
#'   \code{annual_number_of_public_high_school_graduates} (integer),
#'   \code{public_student_teacher_ratio} (numeric),
#'   \code{public_school_expenditures_per_pupil} (integer),
#'   \code{bachelors_degree_attainment} (numeric),
#'   \code{high_school_attainment} (numeric),
#'   \code{number_2yr_colleges} (integer),
#'   \code{number_4yr_colleges_universities} (integer),
#'   \code{enrollment_2yr_college} (integer),
#'   \code{enrollment_4yr_college_university} (integer).
#' @examples
#' md_education()
#' md_education(county = "Montgomery County")
#' @export
md_education <- function(county = NULL, limit = 100) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses, sprintf("county='%s'", county))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("63pe-mygy", where = where, limit = limit)
  if (nrow(df) == 0) return(df)
  num_cols <- c("annual_number_of_public_high_school_graduates",
                "public_school_expenditures_per_pupil",
                "number_2yr_colleges", "number_4yr_colleges_universities",
                "enrollment_2yr_college", "enrollment_4yr_college_university")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }
  pct_cols <- c("public_student_teacher_ratio", "bachelors_degree_attainment", "high_school_attainment")
  for (col in pct_cols) {
    if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
  }
  df
}


#' Fetch Maryland county workforce comparison data
#'
#' Queries Socrata dataset \code{q7q7-usgm} (Choose Maryland: Compare
#' Counties - Workforce). Returns labor force, employment, unemployment,
#' and business establishment metrics by county.
#'
#' @param county Character. County name filter
#'   (e.g. \code{"Montgomery County"}, \code{"Prince George's County"}).
#'   \code{NULL} (default) returns all counties.
#' @param limit Integer. Maximum rows (default \code{100}).
#' @return A tibble with columns including: \code{county},
#'   \code{labor_force} (integer), \code{employment} (integer),
#'   \code{unemployment} (integer), \code{unemployment_rate} (numeric),
#'   \code{labor_force_participation_rate_total} (numeric),
#'   \code{average_weekly_wage_total_dollars} (numeric),
#'   \code{average_annual_employment_total} (integer),
#'   \code{number_of_business_establishments} (integer),
#'   \code{number_of_business_establishments_with_100_or_more_workers}
#'   (integer).
#' @examples
#' md_workforce()
#' md_workforce(county = "Montgomery County")
#' @export
md_workforce <- function(county = NULL, limit = 100) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses, sprintf("county='%s'", county))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("q7q7-usgm", where = where, limit = limit)
  if (nrow(df) == 0) return(df)
  num_cols <- c("labor_force", "employment", "unemployment",
                "average_annual_employment_total",
                "number_of_business_establishments",
                "number_of_business_establishments_with_100_or_more_workers")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }
  for (col in c("unemployment_rate", "labor_force_participation_rate_total",
                "average_weekly_wage_total_dollars")) {
    if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
  }
  df
}


#' Fetch Maryland sewer overflow reports
#'
#' Queries Socrata dataset \code{stgj-u72u} (Reported Sewer Overflows,
#' 2023 to present). Returns records of sanitary sewer overflow events
#' across Maryland.
#'
#' @param q Character. Full-text search string to match against
#'   location or facility fields (e.g. \code{"Baltimore"},
#'   \code{"pump station"}). \code{NULL} (default) returns all records.
#' @param limit Integer. Maximum rows (default \code{1000}).
#' @return A tibble of sewer overflow reports. Columns depend on the
#'   dataset schema; use \code{md_view("stgj-u72u")} to inspect fields.
#' @examples
#' md_sewer_overflows(limit = 10)
#' md_sewer_overflows(q = "Baltimore")
#' @export
md_sewer_overflows <- function(q = NULL, limit = 1000) {
  ._soda_query("stgj-u72u", q = q, order = ":id", limit = limit)
}


#' Fetch Maryland MDTA accident reports
#'
#' Queries Socrata dataset \code{rqid-652u} (MDTA Accidents). Returns
#' accident records from the Maryland Transportation Authority,
#' ordered by date descending.
#'
#' @param accident_type Character. Accident type filter. Common values:
#'   \code{"PD"} (property damage), \code{"PI"} (personal injury).
#'   \code{NULL} (default) returns all types.
#' @param limit Integer. Maximum rows (default \code{1000}).
#' @return A tibble of accident records. Columns depend on the dataset
#'   schema; use \code{md_view("rqid-652u")} to inspect fields. Key
#'   columns typically include \code{date}, \code{accident_type},
#'   \code{road}, and location fields.
#' @examples
#' md_accidents(limit = 20)
#' md_accidents(accident_type = "PI", limit = 50)
#' @export
md_accidents <- function(accident_type = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(accident_type)) clauses <- c(clauses, sprintf("accident_type='%s'", accident_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("rqid-652u", where = where, order = "date DESC", limit = limit)
}


# == Context ===================================================================

#' Get maryland.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
md_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(md_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/maryland.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "maryland.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# maryland.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# maryland.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
