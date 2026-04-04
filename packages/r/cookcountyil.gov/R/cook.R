# cookcountyil.gov.R - Self-contained Cook County, IL Open Data client
# Socrata SODA portal at datacatalog.cookcountyil.gov
# 489 datasets: property assessments, medical examiner, procurement, payroll, etc.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cook_base <- "https://datacatalog.cookcountyil.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- Core SODA query engine ----------------------------------------------------

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
    vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE), character(1)),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .cook_base, dataset_id, query_str)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Cook County SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

.schema_me_cases <- tibble(
  casenumber = character(), death_date = as.POSIXct(character()),
  incident_date = as.POSIXct(character()), age = integer(),
  gender = character(), race = character(), manner = character(),
  primarycause = character(), incident_city = character(),
  incident_zip = character(), latitude = numeric(), longitude = numeric()
)

# == Discovery: list & search =================================================

#' List datasets on Cook County Open Data
#'
#' Returns catalog metadata for datasets on datacatalog.cookcountyil.gov.
#' Cook County hosts ~489 datasets covering property assessments, medical
#' examiner cases, procurement, payroll, and more. Sorted by view count.
#'
#' @param limit Integer. Number of datasets to return (default 50, max 200).
#'   Example: \code{limit = 10}
#' @param category Character or NULL. Category keyword filter (used as search term).
#'   Example: \code{"property"}, \code{"medical"}, \code{"payroll"}
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata four-by-four ID (e.g. "cjeq-bs86").}
#'     \item{name}{Character. Dataset name (e.g. "Medical Examiner Case Archive").}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{category}{Character. Dataset type.}
#'     \item{type}{Character. Asset type (e.g. "dataset").}
#'     \item{updated_at}{POSIXct. Last update timestamp.}
#'     \item{view_count}{Integer. Total page views (e.g. 650033).}
#'   }
#' @examples
#' cook_list(limit = 10)
#' cook_list(category = "medical")
cook_list <- function(limit = 50, category = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=datacatalog.cookcountyil.gov&limit=%d&only=datasets",
    .cook_base, limit
  )
  if (!is.null(category)) {
    url <- paste0(url, "&q=", utils::URLencode(category, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Cook County API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(.schema_datasets)

  res <- raw$results$resource
  tibble(
    id          = as.character(res$id),
    name        = as.character(res$name),
    description = substr(as.character(res$description %||% NA_character_), 1, 200),
    category    = as.character(res$type %||% NA_character_),
    type        = as.character(res$type %||% NA_character_),
    updated_at  = as.POSIXct(res$updatedAt %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    view_count  = as.integer(res$page_views$page_views_total %||% NA_integer_)
  )
}


#' Search Cook County Open Data catalog
#'
#' Full-text search across dataset names and descriptions on
#' datacatalog.cookcountyil.gov. Returns matching datasets with metadata.
#'
#' @param query Character. Search terms.
#'   Example: \code{"medical examiner"}, \code{"property tax"}, \code{"payroll"}
#' @param limit Integer. Max results (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata four-by-four ID (e.g. "cjeq-bs86").}
#'     \item{name}{Character. Dataset name.}
#'     \item{description}{Character. Truncated description.}
#'     \item{category}{Character. Dataset type.}
#'     \item{updated_at}{POSIXct. Last update timestamp.}
#'     \item{view_count}{Integer. Total page views.}
#'   }
#' @examples
#' cook_search("medical examiner", limit = 5)
#' cook_search("property")
cook_search <- function(query, limit = 20) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=datacatalog.cookcountyil.gov&q=%s&limit=%d",
    .cook_base, utils::URLencode(query, reserved = TRUE), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Cook County search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) {
    return(.schema_datasets[, c("id", "name", "description", "category", "updated_at", "view_count")])
  }

  res <- raw$results$resource
  tibble(
    id          = as.character(res$id),
    name        = as.character(res$name),
    description = substr(as.character(res$description %||% NA_character_), 1, 200),
    category    = as.character(res$type %||% NA_character_),
    updated_at  = as.POSIXct(res$updatedAt %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    view_count  = as.integer(res$page_views$page_views_total %||% NA_integer_)
  )
}


# == View dataset metadata & query ============================================

#' Get metadata for a Cook County dataset
#'
#' Returns structural metadata for a single Socrata dataset, including
#' row count, column names, and last modification time. Useful for
#' inspecting a dataset before querying with \code{\link{cook_query}}.
#'
#' @param dataset_id Character. Socrata 4x4 identifier.
#'   Example: \code{"cjeq-bs86"} (Medical Examiner),
#'   \code{"xu6t-uvny"} (Employee Payroll)
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. Dataset ID.}
#'     \item{name}{Character. Dataset name (e.g. "Medical Examiner Case Archive").}
#'     \item{description}{Character. Full description.}
#'     \item{row_count}{Integer. Number of rows (may be NA for some datasets).}
#'     \item{column_count}{Integer. Number of columns.}
#'     \item{columns}{Character. Semicolon-delimited list of column names.}
#'     \item{updated_at}{POSIXct. Last modification timestamp.}
#'   }
#' @examples
#' cook_view("cjeq-bs86")
cook_view <- function(dataset_id) {
  schema <- tibble(
    id = character(), name = character(), description = character(),
    row_count = integer(), column_count = integer(),
    columns = character(), updated_at = as.POSIXct(character())
  )
  url <- sprintf("%s/api/views/%s.json", .cook_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$id)) return(schema)

  col_names <- if (!is.null(raw$columns)) {
    cnames <- if (is.data.frame(raw$columns)) {
      raw$columns$fieldName %||% raw$columns$name
    } else {
      vapply(raw$columns, function(c) c$fieldName %||% c$name, character(1))
    }
    paste(cnames, collapse = "; ")
  } else NA_character_

  tibble(
    id           = as.character(raw$id),
    name         = as.character(raw$name %||% NA_character_),
    description  = as.character(raw$description %||% NA_character_),
    row_count    = as.integer(raw$rowCount %||% NA_integer_),
    column_count = as.integer(length(raw$columns %||% list())),
    columns      = col_names,
    updated_at   = as.POSIXct(as.numeric(raw$viewLastModified %||% NA),
                               origin = "1970-01-01", tz = "UTC")
  )
}


#' Query any Cook County dataset via SoQL
#'
#' General-purpose SODA query against any Cook County dataset. Supports
#' full SoQL syntax including WHERE, SELECT, GROUP BY, ORDER BY, and
#' full-text search.
#'
#' @param dataset_id Character. Socrata 4x4 identifier.
#'   Example: \code{"cjeq-bs86"} (Medical Examiner)
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"gender='Male' AND race='White'"}
#' @param select Character or NULL. SoQL SELECT clause.
#'   Example: \code{"gender, count(*) as n"}
#' @param group Character or NULL. SoQL GROUP BY clause.
#'   Example: \code{"gender"}
#' @param order Character or NULL. SoQL ORDER BY clause.
#'   Example: \code{"death_date DESC"}
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Max rows (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of query results. Columns depend on the dataset.
#' @examples
#' cook_query("cjeq-bs86", limit = 5)
#' cook_query("cjeq-bs86", select = "gender, count(*) as n",
#'            group = "gender", order = "n DESC")
cook_query <- function(dataset_id, where = NULL, select = NULL,
                       group = NULL, order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


# == Named dataset functions ===================================================

#' Medical Examiner case archive
#'
#' Deaths investigated by the Cook County Medical Examiner, August 2014
#' to present. Dataset: cjeq-bs86. One of the most-viewed Cook County
#' datasets (~650k views). Returns all upstream columns with basic type
#' coercion (age as integer, coordinates as numeric).
#'
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"gender='Male' AND race='Black'"}
#' @param year Integer or NULL. Filter to a specific year of death.
#'   Example: \code{2024}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with 22 columns including:
#'   \describe{
#'     \item{casenumber}{Character. Case number (e.g. "ME2026-01767").}
#'     \item{incident_date}{Character. ISO 8601 date/time of incident.}
#'     \item{death_date}{Character. ISO 8601 date/time of death.}
#'     \item{age}{Integer. Age of decedent.}
#'     \item{gender}{Character. Gender (e.g. "Male", "Female").}
#'     \item{race}{Character. Race (e.g. "White", "Black").}
#'     \item{latino}{Logical. Hispanic/Latino indicator.}
#'     \item{cold_related}{Logical. Cold-related death flag.}
#'     \item{heat_related}{Logical. Heat-related death flag.}
#'     \item{incident_street}{Character. Street address of incident.}
#'     \item{incident_city}{Character. City where incident occurred.}
#'     \item{incident_zip}{Character. ZIP code of incident.}
#'     \item{residence_city}{Character. Decedent's city of residence.}
#'     \item{residence_zip}{Character. Decedent's ZIP code.}
#'     \item{covid_related}{Logical. COVID-related flag.}
#'     \item{commissioner_district}{Character. Commissioner district.}
#'     \item{longitude}{Numeric. Longitude coordinate.}
#'     \item{latitude}{Numeric. Latitude coordinate.}
#'     \item{chi_ward}{Character. Chicago ward (if applicable).}
#'     \item{chi_commarea}{Character. Chicago community area (if applicable).}
#'   }
#' @examples
#' cook_medical_examiner(limit = 10)
#' cook_medical_examiner(year = 2024, limit = 20)
cook_medical_examiner <- function(where = NULL, year = NULL, limit = 1000, offset = 0) {
  if (!is.null(year)) {
    yr_clause <- sprintf("death_date >= '%d-01-01' AND death_date < '%d-01-01'", year, year + 1)
    where <- if (is.null(where)) yr_clause else paste(where, "AND", yr_clause)
  }
  df <- ._soda_query("cjeq-bs86", where = where, limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_me_cases)

  # Return all available columns with basic type coercion
  if ("age" %in% names(df)) df$age <- as.integer(df$age)
  if ("latitude" %in% names(df)) df$latitude <- as.numeric(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- as.numeric(df$longitude)
  as_tibble(df)
}


#' Property sales (Assessor parcel sales)
#'
#' Cook County property sales data from the Assessor's Office.
#' Dataset: wvhk-k5uv. Contains sale prices, dates, and property details
#' for parcels across Cook County.
#'
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"sale_price > 500000"}
#' @param pin Character or NULL. Filter by 14-digit Parcel Index Number.
#'   Example: \code{"17321110030000"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of parcel sales. Columns depend on the dataset and
#'   typically include pin, sale_price, sale_date, deed_type, etc.
#' @examples
#' cook_property_sales(limit = 10)
#' cook_property_sales(pin = "17321110030000")
cook_property_sales <- function(where = NULL, pin = NULL, limit = 1000, offset = 0) {
  if (!is.null(pin)) {
    pin_clause <- sprintf("pin='%s'", pin)
    where <- if (is.null(where)) pin_clause else paste(where, "AND", pin_clause)
  }
  ._soda_query("wvhk-k5uv", where = where, limit = limit, offset = offset)
}


#' Assessed property values
#'
#' Land, building, and total assessed values for Cook County parcels,
#' 1999 to present. Dataset: uzyt-m557. Useful for tracking property
#' assessment history over time.
#'
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"total_assessed_value > 100000"}
#' @param pin Character or NULL. 14-digit Parcel Index Number.
#'   Example: \code{"17321110030000"}
#' @param year Integer or Character or NULL. Filter by tax year.
#'   Example: \code{2023}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of assessed values. Typically includes pin, year,
#'   land_assessed_value, building_assessed_value, total_assessed_value.
#' @examples
#' cook_assessed_values(limit = 10)
#' cook_assessed_values(year = 2023, limit = 20)
cook_assessed_values <- function(where = NULL, pin = NULL, year = NULL,
                                 limit = 1000, offset = 0) {
  if (!is.null(pin)) {
    pin_clause <- sprintf("pin='%s'", pin)
    where <- if (is.null(where)) pin_clause else paste(where, "AND", pin_clause)
  }
  if (!is.null(year)) {
    yr_clause <- sprintf("year='%s'", as.character(year))
    where <- if (is.null(where)) yr_clause else paste(where, "AND", yr_clause)
  }
  ._soda_query("uzyt-m557", where = where, limit = limit, offset = offset)
}


#' Procurement awarded contracts and amendments
#'
#' Cook County awarded contracts and amendments from the procurement office.
#' Dataset: qh8j-6k63 (~204k views). Includes vendor details, contract
#' amounts, award dates, and department information.
#'
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"contract_value > 1000000"}
#' @param q Character or NULL. Full-text search across all fields.
#'   Example: \code{"technology"}, \code{"construction"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of contract records. Columns typically include
#'   contract_number, vendor_name, contract_value, award_date,
#'   department, description, etc.
#' @examples
#' cook_contracts(limit = 10)
#' cook_contracts(q = "technology", limit = 20)
cook_contracts <- function(where = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query("qh8j-6k63", where = where, q = q, limit = limit, offset = offset)
}


#' Employee payroll data
#'
#' Quarterly payroll for all Cook County employees (excluding Forest
#' Preserves). Dataset: xu6t-uvny (~257k views). Each row is one employee
#' for one quarter with salary and department information.
#'
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"annual_salary > 100000"}
#' @param q Character or NULL. Full-text search (e.g. department or title).
#'   Example: \code{"Sheriff"}, \code{"Health"}
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of employee payroll records. Columns typically include
#'   employee_name, department, title, annual_salary, quarterly_salary,
#'   pay_period, etc.
#' @examples
#' cook_payroll(limit = 10)
#' cook_payroll(q = "Sheriff", limit = 20)
cook_payroll <- function(where = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query("xu6t-uvny", where = where, q = q, limit = limit, offset = offset)
}


# == Context ===================================================================

#' Get cookcountyil.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cook_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cook_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cookcountyil.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cookcountyil.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cookcountyil.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cookcountyil.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
