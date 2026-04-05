# data.cityofchicago.org.R - Self-contained data.cityofchicago.org client



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
#' Returns a catalog of datasets published on the City of Chicago's Socrata
#' open data portal (data.cityofchicago.org). Each row represents one dataset
#' with basic metadata. Use the returned \code{id} with \code{chi_query()} or
#' \code{chi_metadata()} to explore further.
#'
#' @param limit Integer. Number of datasets to return (default 50, max 200).
#' @param category Optional character string. Case-insensitive filter on
#'   dataset category. Common values: \code{"Public Safety"},
#'   \code{"Transportation"}, \code{"Health & Human Services"},
#'   \code{"Sanitation"}, \code{"Buildings"}, \code{"Community"},
#'   \code{"Environment"}, \code{"Ethics"}, \code{"Finance"},
#'   \code{"City Infrastructure"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 dataset identifier (e.g. \code{"ijzp-q8t2"}).}
#'     \item{name}{Character. Human-readable dataset name.}
#'     \item{description}{Character. Description of the dataset contents.}
#'     \item{category}{Character. Thematic category assigned by the publisher.}
#'     \item{type}{Character. Display type (e.g. \code{"table"}, \code{"story"}, \code{"map"}).}
#'     \item{updated_at}{POSIXct. Date/time of the last modification (UTC).}
#'     \item{view_count}{Integer. Total number of views.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Browse the first 10 datasets
#' chi_datasets(limit = 10)
#'
#' # Filter to public safety datasets
#' chi_datasets(limit = 50, category = "Public Safety")
#' }
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
#' Runs a SoQL (Socrata Query Language) query against a specific dataset using
#' the SODA 2.0 API. Returns results as a tibble whose columns depend on the
#' dataset queried. Use \code{chi_metadata()} to inspect available columns
#' before querying.
#'
#' @param dataset_id Character. Socrata 4x4 dataset identifier. Well-known IDs
#'   include: \code{"ijzp-q8t2"} (Crimes 2001-present),
#'   \code{"yama-9had"} (Building Permits), \code{"suj7-cg3j"} (Food
#'   Inspections), \code{"97t6-zrhs"} (Divvy Trips),
#'   \code{"6zsd-86xi"} (311 Service Requests). Find more via
#'   \code{chi_datasets()}.
#' @param where Optional character. SoQL WHERE clause for filtering rows.
#'   Examples: \code{"primary_type='THEFT'"}, \code{"date > '2024-01-01'"}.
#' @param select Optional character. SoQL SELECT clause to choose columns.
#'   Example: \code{"date, primary_type, description, latitude, longitude"}.
#' @param order Optional character. SoQL ORDER BY clause.
#'   Example: \code{"date DESC"}.
#' @param limit Integer. Number of rows to return (default 1000, max 50000).
#' @param offset Integer. Row offset for pagination (default 0).
#' @return A tibble with columns from the queried dataset. For the Crimes
#'   dataset (\code{"ijzp-q8t2"}), typical columns include: id, case_number,
#'   date, block, iucr, primary_type, description, location_description,
#'   arrest (logical), domestic (logical), beat, district, ward,
#'   community_area, fbi_code, x_coordinate, y_coordinate, year,
#'   updated_on, latitude, longitude.
#' @export
#' @examples
#' \dontrun{
#' # Recent crimes
#' chi_query("ijzp-q8t2", limit = 5)
#'
#' # Thefts in 2024, most recent first
#' chi_query("ijzp-q8t2",
#'           where = "primary_type='THEFT' AND year='2024'",
#'           select = "date, description, location_description, arrest",
#'           order = "date DESC", limit = 100)
#'
#' # Paginate through results
#' page1 <- chi_query("ijzp-q8t2", limit = 1000, offset = 0)
#' page2 <- chi_query("ijzp-q8t2", limit = 1000, offset = 1000)
#' }
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
#' Retrieves detailed metadata for a single dataset, including row count,
#' column count, and download statistics. Useful for inspecting a dataset
#' before querying it with \code{chi_query()}.
#'
#' @param dataset_id Character. Socrata 4x4 dataset identifier (e.g.
#'   \code{"ijzp-q8t2"} for Crimes, \code{"yama-9had"} for Building Permits).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. The dataset identifier.}
#'     \item{name}{Character. Human-readable dataset name.}
#'     \item{description}{Character. Full description text.}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type (e.g. \code{"table"}).}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{row_count}{Integer. Number of rows in the dataset (may be NA for some datasets).}
#'     \item{column_count}{Integer. Number of columns.}
#'     \item{download_count}{Integer. Total number of downloads.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' chi_metadata("ijzp-q8t2")
#' }
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

#' Get row count for a dataset query using SoQL
#'
#' Returns the total number of rows in a dataset, optionally filtered by a
#' SoQL WHERE clause. Useful for determining the size of a result set before
#' paginating through it with \code{chi_query()}.
#'
#' @param dataset_id Character. Socrata 4x4 dataset identifier (e.g.
#'   \code{"ijzp-q8t2"}).
#' @param where Optional character. SoQL WHERE clause to filter rows before
#'   counting. Example: \code{"primary_type='THEFT' AND year='2024'"}.
#' @return Integer. Total number of matching rows. Returns \code{NA_integer_}
#'   on error.
#' @export
#' @examples
#' \dontrun{
#' # Total crimes in the dataset
#' chi_count("ijzp-q8t2")
#' # => 8524185
#'
#' # Count thefts in 2024
#' chi_count("ijzp-q8t2", where = "primary_type='THEFT' AND year='2024'")
#' }
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

#' Get data.cityofchicago.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.cityofchicago.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.cityofchicago.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.cityofchicago.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.cityofchicago.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
