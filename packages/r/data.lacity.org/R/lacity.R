# data.lacity.org.R - Self-contained data.lacity.org client



# data-lacity-org.R
# Self-contained LA City Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app tokens optional for higher rate limits)
# Rate limits: 1000 requests/hour without app token


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.lacity_base <- "https://data.lacity.org"
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

#' List available datasets on LA City Open Data
#'
#' Returns a catalog of datasets published on the City of Los Angeles Socrata
#' open data portal (data.lacity.org). Use the returned \code{id} with
#' \code{lacity_query()} or \code{lacity_metadata()} to explore further.
#'
#' @param limit Integer. Number of datasets to return (default 50, max 200).
#' @param category Optional character. Case-insensitive filter on dataset
#'   category. Common values: \code{"Public Safety"}, \code{"Transportation"},
#'   \code{"Community"}, \code{"City Infrastructure"}, \code{"Revenue"},
#'   \code{"City Performance"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 dataset identifier (e.g. \code{"2nrs-mtv8"}).}
#'     \item{name}{Character. Human-readable dataset name.}
#'     \item{description}{Character. Description of the dataset contents.}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type (e.g. \code{"table"}, \code{"map"}).}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{view_count}{Integer. Total number of views.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' lacity_datasets(limit = 10)
#' lacity_datasets(limit = 50, category = "Public Safety")
#' }
lacity_datasets <- function(limit = 50, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .lacity_base, limit)
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

#' Query a Socrata dataset on LA City Open Data
#'
#' Runs a SoQL (Socrata Query Language) query against a specific dataset on
#' data.lacity.org using the SODA 2.0 API. Returns results as a tibble whose
#' columns depend on the dataset queried. Use \code{lacity_metadata()} to
#' inspect available columns before querying.
#'
#' @param dataset_id Character. Socrata 4x4 dataset identifier. Well-known IDs:
#'   \code{"2nrs-mtv8"} (Crime Data 2020-present),
#'   \code{"2cy6-i7zn"} (MyLA311 Customer Service Requests),
#'   \code{"rwwd-gr2c"} (City Employee Payroll).
#'   Find more via \code{lacity_datasets()}.
#' @param where Optional character. SoQL WHERE clause for filtering.
#'   Examples: \code{"area_name='Hollywood'"}, \code{"date_occ > '2024-01-01'"}.
#' @param select Optional character. SoQL SELECT clause to choose columns.
#'   Example: \code{"date_occ, crm_cd_desc, area_name, lat, lon"}.
#' @param order Optional character. SoQL ORDER BY clause.
#'   Example: \code{"date_occ DESC"}.
#' @param limit Integer. Number of rows to return (default 1000, max 50000).
#' @param offset Integer. Row offset for pagination (default 0).
#' @return A tibble with columns from the queried dataset. For Crime Data
#'   (\code{"2nrs-mtv8"}), typical columns include: dr_no, date_rptd,
#'   date_occ, time_occ, area, area_name, crm_cd, crm_cd_desc, vict_age,
#'   vict_sex, vict_descent, premis_desc, status_desc, location, lat, lon,
#'   weapon_desc, cross_street (25 columns total).
#' @export
#' @examples
#' \dontrun{
#' # Recent crimes
#' lacity_query("2nrs-mtv8", limit = 5)
#'
#' # Crimes in Hollywood, most recent first
#' lacity_query("2nrs-mtv8",
#'              where = "area_name='Hollywood'",
#'              select = "date_occ, crm_cd_desc, premis_desc",
#'              order = "date_occ DESC", limit = 100)
#' }
lacity_query <- function(dataset_id, where = NULL, select = NULL,
                         order = NULL, limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order

  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .lacity_base, dataset_id, query_str)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}


# == Dataset metadata ==========================================================

#' Get metadata for a specific dataset on LA City Open Data
#'
#' Retrieves detailed metadata for a single dataset, including row count,
#' column count, and download statistics. Useful for inspecting a dataset
#' before querying it with \code{lacity_query()}.
#'
#' @param dataset_id Character. Socrata 4x4 dataset identifier (e.g.
#'   \code{"2nrs-mtv8"} for Crime Data).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. The dataset identifier.}
#'     \item{name}{Character. Human-readable dataset name.}
#'     \item{description}{Character. Full description text.}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type (e.g. \code{"table"}).}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{row_count}{Integer. Number of rows (may be NA for some datasets).}
#'     \item{column_count}{Integer. Number of columns.}
#'     \item{download_count}{Integer. Total number of downloads.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' lacity_metadata("2nrs-mtv8")
#' }
lacity_metadata <- function(dataset_id) {
  schema <- tibble(id = character(), name = character(), description = character(),
                   category = character(), type = character(),
                   updated_at = as.POSIXct(character()),
                   row_count = integer(), column_count = integer(),
                   download_count = integer())
  url <- sprintf("%s/api/views/%s.json", .lacity_base, dataset_id)
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

#' Get row count for a dataset query using SoQL
#'
#' Returns the total number of rows in a dataset, optionally filtered by a
#' SoQL WHERE clause. Useful for determining result set size before paginating
#' with \code{lacity_query()}.
#'
#' @param dataset_id Character. Socrata 4x4 dataset identifier (e.g.
#'   \code{"2nrs-mtv8"}).
#' @param where Optional character. SoQL WHERE clause to filter rows before
#'   counting. Example: \code{"area_name='Hollywood'"}.
#' @return Integer. Total number of matching rows. Returns \code{NA_integer_}
#'   on error.
#' @export
#' @examples
#' \dontrun{
#' # Total crimes in the dataset
#' lacity_count("2nrs-mtv8")
#' # => 1004894
#'
#' # Count crimes in Hollywood
#' lacity_count("2nrs-mtv8", where = "area_name='Hollywood'")
#' }
lacity_count <- function(dataset_id, where = NULL) {
  params <- list(`$select` = "count(*)")
  if (!is.null(where)) params[["$where"]] <- where
  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .lacity_base, dataset_id, query_str)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(NA_integer_)
  as.integer(raw[[1]][[1]])
}

# == Context ===================================================================

#' Get data.lacity.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
lacity_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(lacity_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.lacity.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.lacity.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.lacity.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.lacity.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
