# opendata.cityofnewyork.us.R - Self-contained opendata.cityofnewyork.us client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# opendata-cityofnewyork-us.R
# Self-contained NYC Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.cityofnewyork.us

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyc_base <- "https://data.cityofnewyork.us"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), rows_updated_at = as.POSIXct(character())
)

.schema_records <- tibble()

# == Datasets ==================================================================

#' List NYC Open Data datasets
#'
#' Returns metadata for datasets available on the NYC Open Data portal
#' (data.cityofnewyork.us). Dataset IDs from this listing can be passed
#' to \code{nycod_query()} or \code{nycod_fetch_all()} for data retrieval.
#'
#' @param limit Integer. Number of datasets to return (default 50).
#' @param query Character or NULL. Case-insensitive regex filter applied
#'   to dataset name. Examples: \code{"311"}, \code{"crime"},
#'   \code{"permit"}, \code{"taxi"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- Socrata dataset ID (e.g. "erm2-nwe9"). Pass
#'       to \code{nycod_query()}.}
#'     \item{name}{character -- Dataset title}
#'     \item{category}{character -- Category (e.g. "Transportation",
#'       "Public Safety", "Housing & Development")}
#'     \item{description}{character -- First 200 characters of description,
#'       truncated with "..." if longer}
#'     \item{rows_updated_at}{POSIXct -- Last data update timestamp}
#'   }
#' @examples
#' nycod_datasets(limit = 10)
#' nycod_datasets(limit = 50, query = "311")
nycod_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .nyc_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NYC Open Data API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id               = as.character(raw$id),
    name             = as.character(raw$name),
    category         = as.character(raw$category %||% NA_character_),
    description      = ifelse(nchar(raw$description %||% "") > 200,
                              paste0(substr(raw$description, 1, 200), "..."),
                              as.character(raw$description %||% "")),
    rows_updated_at  = as.POSIXct(as.numeric(raw$rowsUpdatedAt), origin = "1970-01-01")
  )

  if (!is.null(query)) {
    result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  }
  result
}


#' Fetch records from an NYC Open Data dataset
#'
#' Queries a specific dataset using the Socrata SODA 2.0 API with SoQL
#' (Socrata Query Language) support. Columns returned depend on the
#' dataset queried.
#'
#' @param dataset_id Character. Socrata dataset identifier. Well-known IDs:
#'   \code{"erm2-nwe9"} (311 Service Requests), \code{"qgea-i56i"} (NYPD
#'   Complaints), \code{"43nn-pn8j"} (DOB Job Applications),
#'   \code{"2bnn-yakx"} (Taxi Trip Records). Find IDs via
#'   \code{nycod_datasets()}.
#' @param where Character or NULL. SoQL WHERE clause for filtering. Examples:
#'   \code{"borough='MANHATTAN'"}, \code{"created_date > '2024-01-01'"},
#'   \code{"complaint_type='Noise'"}.
#' @param select Character or NULL. SoQL SELECT clause. Examples:
#'   \code{"borough, count(*) as n"}, \code{"complaint_type, descriptor"}.
#' @param group Character or NULL. SoQL GROUP BY clause. Example:
#'   \code{"borough"}.
#' @param order Character or NULL. SoQL ORDER BY clause. Examples:
#'   \code{"created_date DESC"}, \code{"count(*) DESC"}.
#' @param limit Integer. Maximum rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with dataset-specific columns (all character from
#'   Socrata). For the 311 dataset (\code{"erm2-nwe9"}), columns include:
#'   unique_key, created_date, agency, agency_name, complaint_type,
#'   descriptor, incident_zip, incident_address, street_name, city,
#'   status, borough, and more.
#' @examples
#' nycod_query("erm2-nwe9", limit = 5)
#' nycod_query("erm2-nwe9", where = "borough='MANHATTAN'",
#'             select = "complaint_type, count(*) as n",
#'             group = "complaint_type", order = "n DESC")
nycod_query <- function(dataset_id, where = NULL, select = NULL,
                        group = NULL, order = NULL,
                        limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .nyc_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NYC Open Data query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)

  as_tibble(raw)
}


#' Fetch all records from an NYC dataset with auto-pagination
#'
#' Repeatedly calls \code{nycod_query()} with increasing offsets until
#' all matching records are retrieved or \code{max_rows} is reached.
#' Useful for downloading complete datasets that exceed single-request
#' limits.
#'
#' @param dataset_id Character. Socrata dataset identifier (e.g.
#'   \code{"erm2-nwe9"}).
#' @param where Character or NULL. Optional SoQL WHERE clause to filter
#'   records before fetching.
#' @param max_rows Integer. Maximum total rows to fetch across all pages
#'   (default 10000). Set higher for complete downloads.
#' @param page_size Integer. Rows per API request (default 1000). The
#'   Socrata API maximum is 50000 per request.
#' @return A tibble of all matching records. Columns depend on the dataset.
#' @examples
#' nycod_fetch_all("erm2-nwe9", where = "borough='BRONX'",
#'                 max_rows = 5000)
nycod_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                            page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- nycod_query(dataset_id, where = where,
                         limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Context ===================================================================

#' Get cityofnewyork.us client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nycod_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nycod_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cityofnewyork.us.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cityofnewyork.us")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cityofnewyork.us context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cityofnewyork.us", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
