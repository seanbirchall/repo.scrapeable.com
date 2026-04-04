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
#' Browse the catalog of datasets on opendata.maryland.gov.
#'
#' @param limit Number of datasets to return (default 50)
#' @param page Page number (default 1)
#' @return tibble: id, name, category, description, updated_at, type
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
#' Full-text search across the opendata.maryland.gov catalog.
#'
#' @param query Search terms (e.g. "crime", "health", "budget")
#' @param limit Max results (default 20)
#' @param only Filter by asset type: "datasets", "maps", "charts" (optional)
#' @return tibble: id, name, category, description, updated_at, type
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
#' Returns column names, types, and descriptions for a Socrata view.
#'
#' @param dataset_id Four-by-four Socrata view ID (e.g. "jwfa-fdxs")
#' @return tibble: field_name, data_type, description
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
#' Runs a SoQL query against any Socrata dataset on opendata.maryland.gov.
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
md_query <- function(dataset_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from a Maryland dataset with auto-pagination
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param order Optional SoQL ORDER BY clause
#' @param max_rows Maximum total rows to fetch (default 50000)
#' @param page_size Rows per request (default 10000)
#' @return tibble of all matching records
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
#' @param dataset_id Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @return integer row count
#' @export
md_count <- function(dataset_id, where = NULL) {
  ._soda_query(dataset_id, select = "count(*) as n", where = where) |>
    pull("n") |>
    as.integer()
}


# == Named convenience functions ===============================================

#' Fetch Maryland violent crime and property crime by county
#'
#' Queries dataset jwfa-fdxs (Violent Crime & Property Crime by County: 1975 to Present).
#'
#' @param jurisdiction County/jurisdiction filter (e.g. "Allegany County", "Baltimore City")
#' @param year Year filter (e.g. "2023")
#' @param limit Max rows (default 1000)
#' @return tibble of crime statistics with rates per 100K
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
#' Queries dataset yu65-jmmv (Maryland Operating Budget, current).
#'
#' @param agency_name Agency name filter (partial match)
#' @param fiscal_year Fiscal year filter
#' @param fund_type Fund type filter (e.g. "General Funds", "Special Funds")
#' @param limit Max rows (default 1000)
#' @return tibble of budget line items
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
#' Queries dataset un65-7ipd (MVA Vehicle Sales Counts by Month: 2002 to Present).
#'
#' @param year Year filter (e.g. "2024")
#' @param month Month filter (e.g. "JAN", "FEB")
#' @param limit Max rows (default 1000)
#' @return tibble of vehicle sales: year, month, new, used, total_sales_new, total_sales_used
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
#' Queries dataset uxq4-6wxf (Power Outages by County).
#'
#' @param area County/area filter (e.g. "Baltimore City", "Montgomery")
#' @param limit Max rows (default 1000)
#' @return tibble of power outage snapshots
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
#' Queries dataset 63pe-mygy (Choose Maryland: Compare Counties - Education).
#'
#' @param county County filter (e.g. "Montgomery County")
#' @param limit Max rows (default 100)
#' @return tibble of education metrics by county
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
#' Queries dataset q7q7-usgm (Choose Maryland: Compare Counties - Workforce).
#'
#' @param county County filter (e.g. "Montgomery County")
#' @param limit Max rows (default 100)
#' @return tibble of workforce metrics by county
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
#' Queries dataset stgj-u72u (Reported Sewer Overflows, 2023+).
#'
#' @param q Full-text search for location or facility
#' @param limit Max rows (default 1000)
#' @return tibble of sewer overflow reports
#' @export
md_sewer_overflows <- function(q = NULL, limit = 1000) {
  ._soda_query("stgj-u72u", q = q, order = ":id", limit = limit)
}


#' Fetch Maryland MDTA accident reports
#'
#' Queries dataset rqid-652u (MDTA Accidents).
#'
#' @param accident_type Accident type filter (e.g. "PD", "PI")
#' @param limit Max rows (default 1000)
#' @return tibble of MDTA accident records
#' @export
md_accidents <- function(accident_type = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(accident_type)) clauses <- c(clauses, sprintf("accident_type='%s'", accident_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("rqid-652u", where = where, order = "date DESC", limit = limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the Maryland Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
md_context <- function() {
  .build_context("maryland.gov", header_lines = c(
    "# maryland.gov - Maryland Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Popular datasets (use 4x4 IDs with md_query):",
    "#   jwfa-fdxs = Violent Crime & Property Crime by County (1975+)",
    "#   2p5g-xrcb = Crime by Municipality (2000+)",
    "#   yu65-jmmv = Maryland Operating Budget (current)",
    "#   un65-7ipd = MVA Vehicle Sales by Month (2002+)",
    "#   uxq4-6wxf = Power Outages by County",
    "#   63pe-mygy = Compare Counties - Education",
    "#   q7q7-usgm = Compare Counties - Workforce",
    "#   stgj-u72u = Reported Sewer Overflows (2023+)",
    "#   rqid-652u = MDTA Accidents",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering"
  ))
}
