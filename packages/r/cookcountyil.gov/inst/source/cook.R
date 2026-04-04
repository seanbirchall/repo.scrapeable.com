# cookcountyil.gov.R - Self-contained Cook County, IL Open Data client
# Socrata SODA portal at datacatalog.cookcountyil.gov
# 489 datasets: property assessments, medical examiner, procurement, payroll, etc.

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

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
#'
#' @param limit Number of datasets to return (default 50, max 200)
#' @param category Optional category filter (case-insensitive substring)
#' @return tibble: id, name, description, category, type, updated_at, view_count
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
#' Full-text search across dataset names and descriptions.
#'
#' @param query Search query string
#' @param limit Max results (default 20)
#' @return tibble: id, name, description, category, updated_at, view_count
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
#' @param dataset_id Socrata 4x4 identifier (e.g. "cjeq-bs86")
#' @return tibble with one row: id, name, description, row_count, column_count,
#'   columns (semicolon-delimited), updated_at
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
#' General-purpose SODA query against any dataset by ID.
#'
#' @param dataset_id Socrata 4x4 identifier
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search within dataset
#' @param limit Max rows (default 1000, max 50000)
#' @param offset Pagination offset
#' @return tibble of query results
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
#' Deaths investigated by the Cook County Medical Examiner, Aug 2014-present.
#'
#' @param where SoQL WHERE clause (e.g. "manner_of_death='HOMICIDE'")
#' @param year Filter to a specific year of death
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: casenumber, death_date, incident_date, age, gender, race,
#'   manner_of_death, primarycause, incident_city, incident_zip, latitude, longitude
cook_medical_examiner <- function(where = NULL, year = NULL, limit = 1000, offset = 0) {
  if (!is.null(year)) {
    yr_clause <- sprintf("death_date >= '%d-01-01' AND death_date < '%d-01-01'", year, year + 1)
    where <- if (is.null(where)) yr_clause else paste(where, "AND", yr_clause)
  }
  df <- ._soda_query("cjeq-bs86", where = where, limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_me_cases)

  df |> transmute(
    casenumber     = as.character(casenumber %||% NA_character_),
    death_date     = as.POSIXct(death_date %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    incident_date  = as.POSIXct(incident_date %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    age            = as.integer(age %||% NA_integer_),
    gender         = as.character(gender %||% NA_character_),
    race           = as.character(race %||% NA_character_),
    manner         = as.character(manner %||% NA_character_),
    primarycause   = as.character(primarycause %||% NA_character_),
    incident_city  = as.character(incident_city %||% NA_character_),
    incident_zip   = as.character(incident_zip %||% NA_character_),
    latitude       = as.numeric(latitude %||% NA_real_),
    longitude      = as.numeric(longitude %||% NA_real_)
  )
}


#' Property sales (Assessor parcel sales)
#'
#' Cook County property sales data from the Assessor's Office.
#'
#' @param where SoQL WHERE clause
#' @param pin Filter by 14-digit Parcel Index Number
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of parcel sales
cook_property_sales <- function(where = NULL, pin = NULL, limit = 1000, offset = 0) {
  if (!is.null(pin)) {
    pin_clause <- sprintf("pin='%s'", pin)
    where <- if (is.null(where)) pin_clause else paste(where, "AND", pin_clause)
  }
  ._soda_query("wvhk-k5uv", where = where, limit = limit, offset = offset)
}


#' Assessed property values
#'
#' Land, building, and total assessed values for Cook County parcels, 1999-present.
#'
#' @param where SoQL WHERE clause
#' @param pin Filter by Parcel Index Number
#' @param year Filter by tax year
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of assessed values
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
#'
#' @param where SoQL WHERE clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of contracts
cook_contracts <- function(where = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query("qh8j-6k63", where = where, q = q, limit = limit, offset = offset)
}


#' Employee payroll data
#'
#' Quarterly payroll for all Cook County employees (excluding Forest Preserves).
#'
#' @param where SoQL WHERE clause
#' @param q Full-text search (e.g. department name)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of employee payroll records
cook_payroll <- function(where = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query("xu6t-uvny", where = where, q = q, limit = limit, offset = offset)
}


# == Context ===================================================================

#' Generate LLM-friendly context for cookcountyil.gov
#'
#' Reads own source and returns all public function signatures and bodies.
#'
#' @return Character string (invisibly)
cook_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/cookcountyil.gov.R"
  if (!file.exists(src_file)) {
    cat("# cookcountyil.gov context - source not found\n")
    return(invisible("# cookcountyil.gov context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
