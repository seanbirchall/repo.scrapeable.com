# montgomerycountymd.gov.R - Self-contained Montgomery County, MD Open Data client
# Socrata SODA portal at data.montgomerycountymd.gov
# 422 datasets: crime, permits, police, traffic crashes, animal services, etc.

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.moco_base <- "https://data.montgomerycountymd.gov"

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
  url <- sprintf("%s/resource/%s.json?%s", .moco_base, dataset_id, query_str)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Montgomery County SODA query error: ", e$message)
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

.schema_crime <- tibble(
  incident_id = character(), date = as.POSIXct(character()),
  crime_type = character(), place = character(),
  district = character(), city = character(),
  zip_code = character(), latitude = numeric(), longitude = numeric()
)

# == Discovery: list & search =================================================

#' List datasets on Montgomery County Open Data
#'
#' Returns catalog metadata for datasets on data.montgomerycountymd.gov.
#'
#' @param limit Number of datasets to return (default 50, max 200)
#' @param category Optional category filter (case-insensitive substring)
#' @return tibble: id, name, description, category, type, updated_at, view_count
moco_list <- function(limit = 50, category = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.montgomerycountymd.gov&limit=%d&only=datasets",
    .moco_base, limit
  )
  if (!is.null(category)) {
    url <- paste0(url, "&q=", utils::URLencode(category, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Montgomery County API error: ", e$message)
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


#' Search Montgomery County Open Data catalog
#'
#' Full-text search across dataset names and descriptions.
#'
#' @param query Search query string
#' @param limit Max results (default 20)
#' @return tibble: id, name, description, category, updated_at, view_count
moco_search <- function(query, limit = 20) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.montgomerycountymd.gov&q=%s&limit=%d",
    .moco_base, utils::URLencode(query, reserved = TRUE), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Montgomery County search error: ", e$message)
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

#' Get metadata for a Montgomery County dataset
#'
#' @param dataset_id Socrata 4x4 identifier (e.g. "icn6-v9z3")
#' @return tibble with one row: id, name, description, row_count, column_count,
#'   columns (semicolon-delimited), updated_at
moco_view <- function(dataset_id) {
  schema <- tibble(
    id = character(), name = character(), description = character(),
    row_count = integer(), column_count = integer(),
    columns = character(), updated_at = as.POSIXct(character())
  )
  url <- sprintf("%s/api/views/%s.json", .moco_base, dataset_id)
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


#' Query any Montgomery County dataset via SoQL
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
moco_query <- function(dataset_id, where = NULL, select = NULL,
                       group = NULL, order = NULL, q = NULL,
                       limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


# == Named dataset functions ===================================================

#' Crime incidents
#'
#' Crime data reported by Montgomery County Police Department.
#'
#' @param where SoQL WHERE clause
#' @param crime_type Filter by crime type (substring, case-insensitive)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of crime incidents
moco_crime <- function(where = NULL, crime_type = NULL, limit = 1000, offset = 0) {
  if (!is.null(crime_type)) {
    ct_clause <- sprintf("upper(crime_name2) LIKE '%%%s%%'", toupper(crime_type))
    where <- if (is.null(where)) ct_clause else paste(where, "AND", ct_clause)
  }
  ._soda_query("icn6-v9z3", where = where, limit = limit, offset = offset)
}


#' Daily arrest records
#'
#' Arrest information from Montgomery County Central Processing Unit.
#' Records are removed after 30 days; refreshed every 2 hours.
#'
#' @param where SoQL WHERE clause
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: last_name, first_name, age, city, state, arrest_date, offense
moco_arrests <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("xhwt-7h2h", where = where, limit = limit, offset = offset)
}


#' Traffic crash incidents
#'
#' Crash reporting incidents from Montgomery County.
#'
#' @param where SoQL WHERE clause
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of crash incidents
moco_crashes <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("bhju-22kf", where = where, limit = limit, offset = offset)
}


#' Residential permits
#'
#' Residential building permits from Permitting Services.
#'
#' @param where SoQL WHERE clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of residential permits
moco_permits <- function(where = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query("m88u-pqki", where = where, q = q, limit = limit, offset = offset)
}


#' Housing code violations
#'
#' Housing code enforcement violations in Montgomery County.
#'
#' @param where SoQL WHERE clause
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of housing code violations
moco_housing_violations <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("k9nj-z35d", where = where, limit = limit, offset = offset)
}


#' Police dispatched incidents
#'
#' Police dispatched incident data from Montgomery County.
#'
#' @param where SoQL WHERE clause
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of police dispatched incidents
moco_police_incidents <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("98cc-bc7d", where = where, limit = limit, offset = offset)
}


# == Context ===================================================================

#' Generate LLM-friendly context for montgomerycountymd.gov
#'
#' Reads own source and returns all public function signatures and bodies.
#'
#' @return Character string (invisibly)
moco_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/montgomerycountymd.gov.R"
  if (!file.exists(src_file)) {
    cat("# montgomerycountymd.gov context - source not found\n")
    return(invisible("# montgomerycountymd.gov context - source not found"))
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
