# data.cityofchicago.org.R - Self-contained data.cityofchicago.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# data-cityofchicago-org.R
# Self-contained Chicago Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app tokens optional for higher rate limits)
# Rate limits: 1000 requests/hour without app token


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.chi_base <- "https://data.cityofchicago.org"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

.schema_query <- tibble()



# == Dataset discovery =========================================================

#' List available datasets on Chicago Open Data
#'
#' Returns a catalog of datasets available on data.cityofchicago.org.
#' Filter by category to narrow results. Dataset IDs from this listing
#' can be passed to \code{chi_query()} for data retrieval.
#'
#' @param limit Integer. Number of datasets to return (default 50, max 200).
#' @param category Character or NULL. Case-insensitive category filter.
#'   Common categories: \code{"Public Safety"}, \code{"Transportation"},
#'   \code{"Health & Human Services"}, \code{"Sanitation"},
#'   \code{"Buildings"}, \code{"Community & Economic Development"},
#'   \code{"Administration & Finance"}, \code{"Environment"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- Socrata dataset ID (e.g. "ijzp-q8t2"). Pass
#'       to \code{chi_query()}.}
#'     \item{name}{character -- Dataset title}
#'     \item{description}{character -- Dataset description (may be NA)}
#'     \item{category}{character -- Category label (may be NA)}
#'     \item{type}{character -- Display type: "table", "story", "chart",
#'       "map", etc.}
#'     \item{updated_at}{POSIXct -- Last modification timestamp (UTC)}
#'     \item{view_count}{integer -- Number of views}
#'   }
#' @examples
#' chi_datasets(limit = 10)
#' chi_datasets(limit = 20, category = "Public Safety")
chi_datasets <- function(limit = 50, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .chi_base, limit)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)
  if (!is.data.frame(raw)) return(.schema_datasets)

  df <- as_tibble(raw)
  result <- df |>
    transmute(
      id          = as.character(id),
      name        = as.character(name),
      description = as.character(description %||% NA),
      category    = as.character(category %||% NA),
      type        = as.character(displayType %||% NA),
      updated_at  = as.POSIXct(as.numeric(viewLastModified %||% NA),
                                origin = "1970-01-01", tz = "UTC"),
      view_count  = as.integer(viewCount %||% NA)
    )

  if (!is.null(category)) {
    result <- result |> filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  result
}


# == Query a dataset ===========================================================

#' Query a Socrata dataset on Chicago Open Data
#'
#' Runs a SoQL (Socrata Query Language) query against a specific dataset
#' using the SODA 2.0 API. Columns returned depend on the dataset.
#'
#' @param dataset_id Character. Socrata dataset identifier. Well-known IDs:
#'   \code{"ijzp-q8t2"} (Crimes), \code{"6zsd-86xi"} (311 Service Requests),
#'   \code{"85ca-t3if"} (Building Permits), \code{"yhhz-zm2v"} (Taxi Trips).
#'   Find IDs via \code{chi_datasets()}.
#' @param where Character or NULL. SoQL WHERE clause for filtering. Examples:
#'   \code{"primary_type='THEFT'"}, \code{"date > '2024-01-01'"},
#'   \code{"ward='42'"}.
#' @param select Character or NULL. SoQL SELECT clause for column selection
#'   or aggregation. Examples: \code{"date, primary_type, description"},
#'   \code{"primary_type, count(*) as n"}.
#' @param order Character or NULL. SoQL ORDER BY clause. Examples:
#'   \code{"date DESC"}, \code{"count(*) DESC"}.
#' @param limit Integer. Maximum rows to return (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with dataset-specific columns (all character from
#'   Socrata). For the Crimes dataset (\code{"ijzp-q8t2"}), columns
#'   include: id, case_number, date, block, iucr, primary_type,
#'   description, location_description, arrest, domestic, beat, district,
#'   ward, community_area, fbi_code, x_coordinate, y_coordinate, year,
#'   updated_on, latitude, longitude.
#' @examples
#' chi_query("ijzp-q8t2", limit = 5)
#' chi_query("ijzp-q8t2", where = "primary_type='THEFT'",
#'           select = "primary_type, count(*) as n",
#'           order = "n DESC")
chi_query <- function(dataset_id, where = NULL, select = NULL,
                      order = NULL, limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order

  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .chi_base, dataset_id, query_str)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}


# == Dataset metadata ==========================================================

#' Get metadata for a specific dataset on Chicago Open Data
#'
#' Returns a single-row tibble with detailed metadata about a dataset
#' including its name, category, row count, and column count.
#'
#' @param dataset_id Character. Socrata dataset identifier (e.g.
#'   \code{"ijzp-q8t2"} for Crimes).
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{id}{character -- Dataset ID}
#'     \item{name}{character -- Dataset title}
#'     \item{description}{character -- Full description}
#'     \item{category}{character -- Category (e.g. "Public Safety")}
#'     \item{type}{character -- Display type}
#'     \item{updated_at}{POSIXct -- Last modification timestamp (UTC)}
#'     \item{row_count}{integer -- Number of rows (may be NA)}
#'     \item{column_count}{integer -- Number of columns}
#'     \item{download_count}{integer -- Number of downloads}
#'   }
#' @examples
#' chi_metadata("ijzp-q8t2")
chi_metadata <- function(dataset_id) {
  schema <- tibble(id = character(), name = character(), description = character(),
                   category = character(), type = character(),
                   updated_at = as.POSIXct(character()),
                   row_count = integer(), column_count = integer(),
                   download_count = integer())
  url <- sprintf("%s/api/views/%s.json", .chi_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$id)) return(schema)

  tibble(
    id = as.character(raw$id %||% NA_character_),
    name = as.character(raw$name %||% NA_character_),
    description = as.character(raw$description %||% NA_character_),
    category = as.character(raw$category %||% NA_character_),
    type = as.character(raw$displayType %||% NA_character_),
    updated_at = as.POSIXct(as.numeric(raw$viewLastModified %||% NA),
                            origin = "1970-01-01", tz = "UTC"),
    row_count = as.integer(raw$rowCount %||% NA_integer_),
    column_count = as.integer(length(raw$columns %||% list())),
    download_count = as.integer(raw$downloadCount %||% NA_integer_)
  )
}

#' Get row count for a dataset or filtered query
#'
#' Returns the total number of rows matching an optional SoQL WHERE clause.
#' Useful for determining dataset size before fetching or for computing
#' filtered totals without downloading data.
#'
#' @param dataset_id Character. Socrata dataset identifier (e.g.
#'   \code{"ijzp-q8t2"}).
#' @param where Character or NULL. Optional SoQL WHERE clause. Example:
#'   \code{"primary_type='THEFT' AND year='2024'"}.
#' @return Integer. Total number of matching rows, or \code{NA} on error.
#' @examples
#' chi_count("ijzp-q8t2")
#' chi_count("ijzp-q8t2", where = "primary_type='THEFT'")
chi_count <- function(dataset_id, where = NULL) {
  params <- list(`$select` = "count(*)")
  if (!is.null(where)) params[["$where"]] <- where
  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .chi_base, dataset_id, query_str)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(NA_integer_)
  as.integer(raw[[1]][[1]])
}

# == Context ===================================================================

#' Get cityofchicago.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cityofchicago_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cityofchicago_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cityofchicago.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cityofchicago.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cityofchicago.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cityofchicago.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
