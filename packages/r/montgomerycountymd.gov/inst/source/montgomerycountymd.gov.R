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
#' The portal hosts over 400 datasets covering crime, permits, traffic,
#' police incidents, animal services, and more.
#'
#' @param limit Integer. Number of datasets to return (default 50, max 200).
#' @param category Character or NULL. Category keyword filter (case-insensitive
#'   substring search across dataset metadata).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 dataset identifier.}
#'     \item{name}{Character. Dataset name.}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{category}{Character. Dataset type/category.}
#'     \item{type}{Character. Resource type (e.g. "dataset").}
#'     \item{updated_at}{POSIXct. Last update timestamp.}
#'     \item{view_count}{Integer. Total page views.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' moco_list(limit = 20)
#' moco_list(category = "crime")
#' }
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
#' Full-text search across dataset names and descriptions on the
#' Montgomery County, MD Open Data portal.
#'
#' @param query Character. Search query string (e.g. \code{"traffic"},
#'   \code{"building permits"}).
#' @param limit Integer. Max results to return (default 20).
#' @return A tibble with columns: id, name, description, category,
#'   updated_at, view_count.
#' @export
#' @examples
#' \dontrun{
#' moco_search("crime")
#' moco_search("building permits", limit = 10)
#' }
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
#' Returns structural metadata for a single dataset including row count,
#' column names, and last update time. Useful for inspecting a dataset
#' before querying it.
#'
#' @param dataset_id Character. Socrata 4x4 identifier (e.g. \code{"icn6-v9z3"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. Dataset identifier.}
#'     \item{name}{Character. Dataset name.}
#'     \item{description}{Character. Full description.}
#'     \item{row_count}{Integer. Number of rows in the dataset.}
#'     \item{column_count}{Integer. Number of columns.}
#'     \item{columns}{Character. Semicolon-delimited column names.}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' moco_view("icn6-v9z3")
#' }
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
#' General-purpose SODA query against any dataset by its 4x4 identifier.
#' Use \code{\link{moco_list}} or \code{\link{moco_search}} to discover
#' dataset IDs, and \code{\link{moco_view}} to inspect column names.
#'
#' @param dataset_id Character. Socrata 4x4 identifier (e.g. \code{"icn6-v9z3"}).
#' @param where Character or NULL. SoQL WHERE clause for row filtering.
#' @param select Character or NULL. SoQL SELECT clause for column selection.
#' @param group Character or NULL. SoQL GROUP BY clause for aggregation.
#' @param order Character or NULL. SoQL ORDER BY clause for sorting.
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Max rows to return (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of query results. Columns depend on the dataset.
#' @export
#' @examples
#' \dontrun{
#' moco_query("icn6-v9z3", limit = 10)
#' moco_query("icn6-v9z3", where = "city = 'SILVER SPRING'")
#' }
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
#' Crime data reported by the Montgomery County Police Department.
#' Updated daily with incident-level records including crime type,
#' location, date, and district.
#'
#' @param where Character or NULL. SoQL WHERE clause for additional filtering.
#' @param crime_type Character or NULL. Filter by crime type name
#'   (case-insensitive substring match, e.g. \code{"theft"}, \code{"assault"}).
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of crime incident records.
#' @export
#' @examples
#' \dontrun{
#' moco_crime(limit = 50)
#' moco_crime(crime_type = "theft", limit = 100)
#' }
moco_crime <- function(where = NULL, crime_type = NULL, limit = 1000, offset = 0) {
  if (!is.null(crime_type)) {
    ct_clause <- sprintf("upper(crime_name2) LIKE '%%%s%%'", toupper(crime_type))
    where <- if (is.null(where)) ct_clause else paste(where, "AND", ct_clause)
  }
  ._soda_query("icn6-v9z3", where = where, limit = limit, offset = offset)
}


#' Daily arrest records
#'
#' Arrest information from the Montgomery County Central Processing Unit.
#' Records are removed after 30 days and refreshed every 2 hours.
#'
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including last_name, first_name, age,
#'   city, state, arrest_date, and offense.
#' @export
moco_arrests <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("xhwt-7h2h", where = where, limit = limit, offset = offset)
}


#' Traffic crash incidents
#'
#' Crash reporting incidents from Montgomery County, MD, including
#' location, date, severity, and contributing factors.
#'
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of crash incident records.
#' @export
moco_crashes <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("bhju-22kf", where = where, limit = limit, offset = offset)
}


#' Residential permits
#'
#' Residential building permits issued by the Montgomery County
#' Department of Permitting Services.
#'
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @param q Character or NULL. Full-text search across all fields.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of residential building permit records.
#' @export
moco_permits <- function(where = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query("m88u-pqki", where = where, q = q, limit = limit, offset = offset)
}


#' Housing code violations
#'
#' Housing code enforcement violations in Montgomery County, MD,
#' from the Department of Housing and Community Affairs.
#'
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of housing code violation records.
#' @export
moco_housing_violations <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("k9nj-z35d", where = where, limit = limit, offset = offset)
}


#' Police dispatched incidents
#'
#' Police dispatched incident data from the Montgomery County Police
#' Department. Includes call type, location, date, and district.
#'
#' @param where Character or NULL. SoQL WHERE clause for filtering.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of police dispatched incident records.
#' @export
moco_police_incidents <- function(where = NULL, limit = 1000, offset = 0) {
  ._soda_query("98cc-bc7d", where = where, limit = limit, offset = offset)
}


# == Context ===================================================================

#' Get montgomerycountymd.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
moco_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(moco_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/montgomerycountymd.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "montgomerycountymd.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# montgomerycountymd.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# montgomerycountymd.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
