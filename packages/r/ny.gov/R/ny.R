# ny.gov.R
# Self-contained New York State Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.ny.gov
# Datasets: ~1000 Socrata views covering budget, crime, transit, health, etc.


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
#' Browse the catalog of ~1,000 datasets on data.ny.gov covering
#' budget, crime, transit, health, education, and more.
#'
#' @param limit Integer. Number of datasets to return (default 50).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- Socrata four-by-four view ID}
#'     \item{name}{character -- dataset name}
#'     \item{category}{character -- domain category}
#'     \item{description}{character -- description (truncated to 200 chars)}
#'     \item{updated_at}{character -- last update timestamp}
#'     \item{type}{character -- asset type (e.g. \code{"dataset"}, \code{"map"})}
#'   }
#' @examples
#' nyg_list(limit = 10)
#' nyg_list(limit = 20, page = 2)
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
#' @param query Character. Search terms (e.g. \code{"crime"},
#'   \code{"MTA"}, \code{"budget"}, \code{"bridge"}).
#' @param limit Integer. Max results (default 20).
#' @param only Character or NULL. Filter by asset type:
#'   \code{"datasets"}, \code{"maps"}, \code{"charts"}.
#' @return A tibble with columns: id, name, category, description,
#'   updated_at, type (same schema as \code{nyg_list()}).
#' @examples
#' nyg_search("crime")
#' nyg_search("MTA", limit = 5, only = "datasets")
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
#' Useful for understanding a dataset's schema before querying.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID
#'   (e.g. \code{"ca8h-8gjq"} for crime data).
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{character -- column name}
#'     \item{data_type}{character -- Socrata type (\code{"text"},
#'       \code{"number"}, \code{"calendar_date"}, etc.)}
#'     \item{description}{character -- column description}
#'   }
#' @examples
#' nyg_view("ca8h-8gjq")
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
#' Use \code{nyg_search()} to discover datasets and \code{nyg_view()}
#' to inspect column schemas.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID.
#' @param where Character or NULL. SoQL WHERE clause
#'   (e.g. \code{"county='Albany'"}).
#' @param select Character or NULL. SoQL SELECT clause
#'   (e.g. \code{"county,year,total_index_crimes"}).
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param order Character or NULL. SoQL ORDER BY clause
#'   (e.g. \code{"year DESC"}).
#' @param q Character or NULL. Full-text search query.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble whose columns depend on the queried dataset.
#' @examples
#' nyg_query("ca8h-8gjq", where = "county='Albany'", limit = 5)
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
#' Automatically pages through a Socrata dataset until all matching
#' rows are retrieved or \code{max_rows} is reached.
#'
#' @param dataset_id Character. Four-by-four Socrata view ID.
#' @param where Character or NULL. SoQL WHERE clause.
#' @param select Character or NULL. SoQL SELECT clause.
#' @param order Character or NULL. SoQL ORDER BY clause.
#' @param max_rows Integer. Maximum total rows to fetch (default 50000).
#' @param page_size Integer. Rows per request (default 10000).
#' @return A tibble of all matching records.
#' @examples
#' nyg_fetch_all("kwxv-fwze", max_rows = 500)
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
#' Returns the total number of rows in a dataset (optionally filtered).
#'
#' @param dataset_id Character. Socrata view ID.
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @return Integer. Total row count.
#' @examples
#' nyg_count("ca8h-8gjq")
#' nyg_count("ca8h-8gjq", where = "county='Albany'")
#' @export
nyg_count <- function(dataset_id, where = NULL) {
  ._soda_query(dataset_id, select = "count(*) as n", where = where) |>
    pull("n") |>
    as.integer()
}


# == Named convenience functions ===============================================

#' Fetch NY index crime statistics by county and agency
#'
#' Queries dataset \code{ca8h-8gjq} (Index Crimes by County and
#' Agency: Beginning 1990). ~23,800 rows covering all NY counties.
#'
#' @param county Character or NULL. County filter (e.g. \code{"Albany"},
#'   \code{"Kings"}, \code{"New York"}, \code{"Erie"}).
#' @param year Character or NULL. Year filter (e.g. \code{"2024"}).
#' @param region Character or NULL. Region filter: \code{"New York City"}
#'   or \code{"Non-New York City"}.
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: county, agency, year,
#'   months_reported (integer), total_index_crimes (integer),
#'   violent (integer), murder (integer), forcible_rape (integer),
#'   robbery (integer), aggravated_assault (integer), property
#'   (integer), burglary (integer), larceny (integer),
#'   motor_vehicle_theft (integer), region.
#' @examples
#' nyg_crimes(county = "Albany", limit = 10)
#' nyg_crimes(year = "2024", region = "New York City")
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
#' Queries dataset \code{6xda-q7ev} (Hate Crimes by County and
#' Bias Type: Beginning 2010).
#'
#' @param county Character or NULL. County filter (e.g. \code{"Albany"}).
#' @param year Character or NULL. Year filter (e.g. \code{"2024"}).
#' @param crime_type Character or NULL. Crime type filter:
#'   \code{"Crimes Against Persons"}, \code{"Crimes Against Property"},
#'   \code{"Crimes Against Society"}.
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: county, year, crime_type,
#'   and bias-type count columns (anti_jewish, anti_black,
#'   anti_white, anti_gay_male, etc.).
#' @examples
#' nyg_hate_crimes(county = "Albany")
#' nyg_hate_crimes(year = "2023", limit = 50)
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
#' Queries dataset \code{yg77-3tkj} (MTA Statement of Operations:
#' Beginning 2019). Monthly line-item financial data for all MTA agencies.
#'
#' @param agency Character or NULL. MTA agency filter: \code{"NYCT"}
#'   (NYC Transit), \code{"LIRR"} (Long Island Rail Road),
#'   \code{"MTABC"} (MTA Bus Company), \code{"MNR"} (Metro-North),
#'   \code{"TBTA"} (Bridges & Tunnels), \code{"MTA HQ"}.
#' @param fiscal_year Character or NULL. Fiscal year (e.g. \code{"2026"}).
#' @param scenario Character or NULL. Filter by scenario:
#'   \code{"Actual"} or a plan year.
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: agency, fiscal_year,
#'   month, scenario, line_item, amount (numeric).
#' @examples
#' nyg_mta_finances(agency = "NYCT", limit = 10)
#' nyg_mta_finances(fiscal_year = "2025", scenario = "Actual")
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
#' Queries dataset \code{yv78-9wbn} (Executive Budget Appropriations:
#' 2017--2018). For current-year budgets, use
#' \code{nyg_search("budget appropriations")} to find the latest ID.
#'
#' @param agency_name Character or NULL. Agency name filter
#'   (e.g. \code{"CUNY"}, \code{"SUNY"}).
#' @param fund_type Character or NULL. Fund type filter
#'   (e.g. \code{"General Fund"}, \code{"Special Revenue Funds - Federal"}).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble of budget appropriation line items.
#' @examples
#' nyg_budget(limit = 10)
#' nyg_budget(agency_name = "CUNY")
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
#' Queries dataset \code{wpyb-cjy8} (Bridge Conditions, NYS DOT).
#' Includes structural ratings for all state-inspected bridges.
#'
#' @param county Character or NULL. County filter (e.g. \code{"Albany"},
#'   \code{"Kings"}, \code{"Westchester"}).
#' @param poor_status Character or NULL. Filter by poor status:
#'   \code{"Y"} (poor condition) or \code{"N"}.
#' @param owner Character or NULL. Bridge owner filter
#'   (e.g. \code{"NYSDOT"}, \code{"NYS Thruway Authority"},
#'   \code{"County Highway Department"}).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: region, county,
#'   municipality, bin, location, feature_carried,
#'   feature_crossed, owner.
#' @examples
#' nyg_bridges(county = "Albany", limit = 10)
#' nyg_bridges(poor_status = "Y", limit = 20)
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
#' Queries dataset \code{t3vp-5tka} (Title and Salary Listing).
#' Contains civil service titles, grades, and filled positions
#' across all state agencies.
#'
#' @param title_name Character or NULL. Title name filter (partial
#'   match, case-insensitive). Examples: \code{"ENGINEER"},
#'   \code{"ATTORNEY"}, \code{"NURSE"}.
#' @param agency_code Character or NULL. Agency code filter.
#' @param grade Character or NULL. Grade/salary level filter.
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: title_code, title_name,
#'   grade, jurisdictional_classification, negotiating_unit,
#'   agency_code, agency_description, filled_positions (integer),
#'   effective_date.
#' @examples
#' nyg_salaries(title_name = "ENGINEER", limit = 10)
#' nyg_salaries(title_name = "ATTORNEY")
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
#' Queries dataset \code{yg7h-zjbf} (Active Real Estate Salespersons
#' and Brokers in New York State).
#'
#' @param county Character or NULL. County filter.
#' @param license_type Character or NULL. License type:
#'   \code{"ASSOCIATE BROKER"}, \code{"REAL ESTATE SALESPERSON"},
#'   \code{"REAL ESTATE BROKER"}.
#' @param q Character or NULL. Full-text search (e.g. name or business).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble of active real estate licenses.
#' @examples
#' nyg_real_estate(county = "Albany", limit = 10)
#' nyg_real_estate(q = "Coldwell Banker")
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
#' Queries dataset \code{kwxv-fwze} (Lottery Cash 4 Life Winning
#' Numbers: Beginning 2014). Sorted by draw date descending.
#'
#' @param limit Integer. Max rows to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{draw_date}{Date -- date of the drawing}
#'     \item{winning_numbers}{character -- space-separated winning numbers}
#'     \item{cash_ball}{character -- Cash Ball number}
#'   }
#' @examples
#' nyg_lottery(limit = 10)
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
#' Queries dataset \code{ybg9-s6bm} (Annual Youth Detention
#' Admissions: Beginning 2006). Covers secure, non-secure, and
#' specialized secure detention types.
#'
#' @param county Character or NULL. County filter.
#' @param year Character or NULL. Year filter (e.g. \code{"2023"}).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: county, year,
#'   specialized_secure_admissions (integer),
#'   secure_mixed_admissions (integer),
#'   non_secure_admissions (integer),
#'   specialized_secure_unique_youth (integer),
#'   secure_mixed_unique_youth (integer),
#'   non_secure_unique_youth (integer).
#' @examples
#' nyg_youth_detention(county = "Albany")
#' nyg_youth_detention(year = "2023", limit = 20)
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

#' Get ny.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nyg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nyg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ny.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ny.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ny.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ny.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
