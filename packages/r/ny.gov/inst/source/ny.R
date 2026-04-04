# ny.gov.R
# Self-contained New York State Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.ny.gov
# Datasets: ~1000 Socrata views covering budget, crime, transit, health, etc.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyg_base <- "https://data.ny.gov"

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
  url <- sprintf("%s/resource/%s.json?%s", .nyg_base, dataset_id,
                 utils::URLencode(query_str, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NY Open Data query error: ", e$message)
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

#' List New York State Open Data datasets
#'
#' Browse the catalog of datasets on data.ny.gov.
#'
#' @param limit Number of datasets to return (default 50)
#' @param page Page number (default 1)
#' @return tibble: id, name, category, description, updated_at, type
#' @export
nyg_list <- function(limit = 50, page = 1) {
  offset <- (page - 1) * limit
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.ny.gov&search_context=data.ny.gov&limit=%d&offset=%d",
    .nyg_base, limit, offset
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NY catalog error: ", e$message)
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


#' Search New York State Open Data datasets
#'
#' Full-text search across the data.ny.gov catalog.
#'
#' @param query Search terms (e.g. "crime", "MTA", "budget")
#' @param limit Max results (default 20)
#' @param only Filter by asset type: "datasets", "maps", "charts" (optional)
#' @return tibble: id, name, category, description, updated_at, type
#' @export
nyg_search <- function(query, limit = 20, only = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.ny.gov&search_context=data.ny.gov&q=%s&limit=%d",
    .nyg_base, utils::URLencode(query, reserved = TRUE), limit
  )
  if (!is.null(only)) url <- paste0(url, "&only=", only)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NY search error: ", e$message)
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


#' View metadata and schema for an NY dataset
#'
#' Returns column names, types, and descriptions for a Socrata view.
#'
#' @param dataset_id Four-by-four Socrata view ID (e.g. "ca8h-8gjq")
#' @return tibble: field_name, data_type, description
#' @export
nyg_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .nyg_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NY view metadata error: ", e$message)
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

#' Query any New York State Open Data dataset
#'
#' Runs a SoQL query against any Socrata dataset on data.ny.gov.
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where SoQL WHERE clause (e.g. "county='Albany'")
#' @param select SoQL SELECT clause (e.g. "county,year,total_index_crimes")
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Max rows to return (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
nyg_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL, q = NULL,
                      limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from an NY dataset with auto-pagination
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param order Optional SoQL ORDER BY clause
#' @param max_rows Maximum total rows to fetch (default 50000)
#' @param page_size Rows per request (default 10000)
#' @return tibble of all matching records
#' @export
nyg_fetch_all <- function(dataset_id, where = NULL, select = NULL,
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


#' Get row count for any NY dataset
#'
#' @param dataset_id Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @return integer row count
#' @export
nyg_count <- function(dataset_id, where = NULL) {
  ._soda_query(dataset_id, select = "count(*) as n", where = where) |>
    pull("n") |>
    as.integer()
}


# == Named convenience functions ===============================================

#' Fetch NY index crime statistics by county and agency
#'
#' Queries dataset ca8h-8gjq (Index Crimes by County and Agency: Beginning 1990).
#'
#' @param county County filter (e.g. "Albany", "Kings")
#' @param year Year filter (e.g. "2024")
#' @param region Region filter ("New York City" or "Non-New York City")
#' @param limit Max rows (default 1000)
#' @return tibble of crime statistics
#' @export
nyg_crimes <- function(county = NULL, year = NULL, region = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses, sprintf("county='%s'", county))
  if (!is.null(year))   clauses <- c(clauses, sprintf("year='%s'", year))
  if (!is.null(region)) clauses <- c(clauses, sprintf("region='%s'", region))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("ca8h-8gjq", where = where, order = "year DESC", limit = limit)
  if (nrow(df) == 0) return(df)
  num_cols <- c("total_index_crimes", "violent", "murder", "forcible_rape",
                "robbery", "aggravated_assault", "property", "burglary",
                "larceny", "motor_vehicle_theft", "months_reported")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }
  df
}


#' Fetch NY hate crime data by county and bias type
#'
#' Queries dataset 6xda-q7ev (Hate Crimes by County and Bias Type: Beginning 2010).
#'
#' @param county County filter (e.g. "Albany")
#' @param year Year filter (e.g. "2024")
#' @param crime_type Crime type filter (e.g. "Crimes Against Persons")
#' @param limit Max rows (default 1000)
#' @return tibble of hate crime data with bias-type counts
#' @export
nyg_hate_crimes <- function(county = NULL, year = NULL, crime_type = NULL,
                            limit = 1000) {
  clauses <- character()
  if (!is.null(county))     clauses <- c(clauses, sprintf("county='%s'", county))
  if (!is.null(year))       clauses <- c(clauses, sprintf("year='%s'", year))
  if (!is.null(crime_type)) clauses <- c(clauses, sprintf("crime_type='%s'", crime_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("6xda-q7ev", where = where, order = "year DESC", limit = limit)
}


#' Fetch MTA financial data (Statement of Operations)
#'
#' Queries dataset yg77-3tkj (MTA Statement of Operations: Beginning 2019).
#'
#' @param agency MTA agency filter (e.g. "NYCT", "LIRR", "MTABC")
#' @param fiscal_year Fiscal year filter (e.g. "2026")
#' @param scenario Filter by scenario: "Actual" or plan year
#' @param limit Max rows (default 1000)
#' @return tibble of MTA financial line items
#' @export
nyg_mta_finances <- function(agency = NULL, fiscal_year = NULL,
                             scenario = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(agency))      clauses <- c(clauses, sprintf("agency='%s'", agency))
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  if (!is.null(scenario))    clauses <- c(clauses, sprintf("scenario='%s'", scenario))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("yg77-3tkj", where = where, order = "month DESC", limit = limit)
  if (nrow(df) > 0 && "amount" %in% names(df)) {
    df$amount <- as.numeric(df$amount)
  }
  df
}


#' Fetch NY State budget appropriations
#'
#' Queries dataset yv78-9wbn (Executive Budget Appropriations: 2017-2018).
#' For current-year budget, search nyg_search("budget appropriations") to find latest.
#'
#' @param agency_name Agency name filter (e.g. "CUNY")
#' @param fund_type Fund type filter (e.g. "General Fund")
#' @param limit Max rows (default 1000)
#' @return tibble of budget appropriations
#' @export
nyg_budget <- function(agency_name = NULL, fund_type = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(agency_name)) clauses <- c(clauses, sprintf("agency_name='%s'", agency_name))
  if (!is.null(fund_type))   clauses <- c(clauses, sprintf("fund_type='%s'", fund_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("yv78-9wbn", where = where, limit = limit)
}


#' Fetch NY bridge condition data
#'
#' Queries dataset wpyb-cjy8 (Bridge Conditions, NYS DOT).
#'
#' @param county County filter (e.g. "Albany")
#' @param poor_status Filter by poor status ("Y" or "N")
#' @param owner Owner filter (e.g. "NYSDOT", "NYS Thruway Authority")
#' @param limit Max rows (default 1000)
#' @return tibble of bridge conditions
#' @export
nyg_bridges <- function(county = NULL, poor_status = NULL, owner = NULL,
                        limit = 1000) {
  clauses <- character()
  if (!is.null(county))      clauses <- c(clauses, sprintf("county='%s'", county))
  if (!is.null(poor_status)) clauses <- c(clauses, sprintf("poor_status='%s'", poor_status))
  if (!is.null(owner))       clauses <- c(clauses, sprintf("owner='%s'", owner))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("wpyb-cjy8", where = where, limit = limit)
}


#' Fetch NY State title and salary listing
#'
#' Queries dataset t3vp-5tka (Title and Salary Listing).
#'
#' @param title_name Title name filter (partial match with LIKE)
#' @param agency_code Agency code filter
#' @param grade Grade filter
#' @param limit Max rows (default 1000)
#' @return tibble of title and salary data
#' @export
nyg_salaries <- function(title_name = NULL, agency_code = NULL,
                         grade = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(title_name))  clauses <- c(clauses, sprintf("upper(title_name) LIKE '%%%s%%'", toupper(title_name)))
  if (!is.null(agency_code)) clauses <- c(clauses, sprintf("agency_code='%s'", agency_code))
  if (!is.null(grade))       clauses <- c(clauses, sprintf("grade='%s'", grade))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("t3vp-5tka", where = where, limit = limit)
  if (nrow(df) > 0 && "filled_positions" %in% names(df)) {
    df$filled_positions <- as.integer(df$filled_positions)
  }
  df
}


#' Fetch NY real estate broker/salesperson data
#'
#' Queries dataset yg7h-zjbf (Active Real Estate Salespersons and Brokers).
#'
#' @param county County filter
#' @param license_type License type ("ASSOCIATE BROKER", "REAL ESTATE SALESPERSON", etc.)
#' @param q Full-text search (e.g. name or business)
#' @param limit Max rows (default 1000)
#' @return tibble of active real estate licenses
#' @export
nyg_real_estate <- function(county = NULL, license_type = NULL, q = NULL,
                            limit = 1000) {
  clauses <- character()
  if (!is.null(county))       clauses <- c(clauses, sprintf("county='%s'", county))
  if (!is.null(license_type)) clauses <- c(clauses, sprintf("license_type='%s'", license_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("yg7h-zjbf", where = where, q = q, limit = limit)
}


#' Fetch NY lottery winning numbers (Cash 4 Life)
#'
#' Queries dataset kwxv-fwze (Lottery Cash 4 Life Winning Numbers: Beginning 2014).
#'
#' @param limit Max rows (default 100)
#' @return tibble: draw_date, winning_numbers, cash_ball
#' @export
nyg_lottery <- function(limit = 100) {
  df <- ._soda_query("kwxv-fwze", order = "draw_date DESC", limit = limit)
  if (nrow(df) > 0 && "draw_date" %in% names(df)) {
    df$draw_date <- as.Date(substr(df$draw_date, 1, 10))
  }
  df
}


#' Fetch NY youth detention admissions by county
#'
#' Queries dataset ybg9-s6bm (Annual Youth Detention Admissions: Beginning 2006).
#'
#' @param county County filter
#' @param year Year filter
#' @param limit Max rows (default 1000)
#' @return tibble of youth detention data
#' @export
nyg_youth_detention <- function(county = NULL, year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses, sprintf("county='%s'", county))
  if (!is.null(year))   clauses <- c(clauses, sprintf("year='%s'", year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("ybg9-s6bm", where = where, order = "year DESC", limit = limit)
  if (nrow(df) == 0) return(df)
  num_cols <- c("specialized_secure_admissions", "secure_mixed_admissions",
                "non_secure_admissions", "specialized_secure_unique_youth",
                "secure_mixed_unique_youth", "non_secure_unique_youth")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }
  df
}


# == Context ===================================================================

#' Generate LLM-friendly context for the NY State Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
nyg_context <- function() {
  .build_context("ny.gov", header_lines = c(
    "# ny.gov - New York State Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Popular datasets (use 4x4 IDs with nyg_query):",
    "#   ca8h-8gjq = Index Crimes by County and Agency (1990+)",
    "#   6xda-q7ev = Hate Crimes by County and Bias Type (2010+)",
    "#   yg77-3tkj = MTA Statement of Operations (2019+)",
    "#   yv78-9wbn = Executive Budget Appropriations",
    "#   wpyb-cjy8 = Bridge Conditions (NYS DOT)",
    "#   t3vp-5tka = Title and Salary Listing",
    "#   yg7h-zjbf = Active Real Estate Brokers",
    "#   kwxv-fwze = Lottery Cash 4 Life Winning Numbers",
    "#   ybg9-s6bm = Youth Detention Admissions by County",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering"
  ))
}
