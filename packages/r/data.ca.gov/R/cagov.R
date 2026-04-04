# data.ca.gov.R - Self-contained data.ca.gov client



# data-ca-gov.R
# Self-contained California Open Data (CKAN) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 at data.ca.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cagov_base <- "https://data.ca.gov/api/3/action"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.ckan_result <- function(url) {
  raw <- .fetch_json(url)
  if (!isTRUE(raw$success)) {
    warning("CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(), notes = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character()
)

.schema_datastore <- tibble()

# == Datasets ==================================================================

#' Search California Open Data datasets
#'
#' Searches the California state CKAN data catalog for datasets matching
#' a keyword query. Returns dataset metadata with descriptions truncated
#' to 200 characters.
#'
#' @param query Character or \code{NULL}. Search term (e.g., \code{"water"},
#'   \code{"covid"}, \code{"wildfire"}). If \code{NULL}, returns all datasets.
#' @param rows Integer. Number of results to return (default 50).
#' @param start Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{character} -- CKAN dataset UUID}
#'     \item{name}{\code{character} -- URL-safe dataset slug (use with \code{cagov_resources()})}
#'     \item{title}{\code{character} -- Human-readable dataset title}
#'     \item{organization}{\code{character} -- Publishing organization name}
#'     \item{num_resources}{\code{integer} -- Number of downloadable resources (files)}
#'     \item{notes}{\code{character} -- Dataset description (truncated to 200 chars)}
#'   }
#' @examples
#' cagov_search("water quality")
#' cagov_search("wildfire", rows = 10)
#' @export
cagov_search <- function(query = NULL, rows = 50, start = 0) {
  params <- sprintf("rows=%d&start=%d", rows, start)
  if (!is.null(query)) {
    params <- paste0(params, "&q=", utils::URLencode(query, reserved = TRUE))
  }
  url <- paste0(.cagov_base, "/package_search?", params)

  result <- .ckan_result(url)
  if (is.null(result)) return(.schema_datasets)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_datasets)

  tibble(
    id            = vapply(datasets, function(d) d$id %||% NA_character_, character(1)),
    name          = vapply(datasets, function(d) d$name %||% NA_character_, character(1)),
    title         = vapply(datasets, function(d) d$title %||% NA_character_, character(1)),
    organization  = vapply(datasets, function(d) d$organization$title %||% NA_character_, character(1)),
    num_resources = vapply(datasets, function(d) as.integer(d$num_resources %||% 0L), integer(1)),
    notes         = vapply(datasets, function(d) {
      n <- d$notes %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


#' Get resources (downloadable files) for a California dataset
#'
#' Lists all resources (CSV, JSON, PDF, etc.) associated with a dataset.
#' Use the resource \code{id} with \code{cagov_datastore()} to query
#' tabular data directly.
#'
#' @param dataset_name Character. Dataset name slug or UUID (from the
#'   \code{name} or \code{id} column of \code{cagov_search()} results).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{character} -- Resource UUID (use with \code{cagov_datastore()})}
#'     \item{name}{\code{character} -- Resource name (e.g., "Stations", "Lab Results")}
#'     \item{format}{\code{character} -- File format (e.g., "CSV", "JSON", "PDF")}
#'     \item{url}{\code{character} -- Direct download URL}
#'     \item{description}{\code{character} -- Resource description (truncated to 200 chars)}
#'   }
#' @examples
#' cagov_resources("water-quality-data")
#' @export
cagov_resources <- function(dataset_name) {
  url <- sprintf("%s/package_show?id=%s", .cagov_base, dataset_name)
  result <- .ckan_result(url)
  if (is.null(result)) return(.schema_resources)

  resources <- result$resources
  if (length(resources) == 0) return(.schema_resources)

  tibble(
    id          = vapply(resources, function(r) r$id %||% NA_character_, character(1)),
    name        = vapply(resources, function(r) r$name %||% NA_character_, character(1)),
    format      = vapply(resources, function(r) r$format %||% NA_character_, character(1)),
    url         = vapply(resources, function(r) r$url %||% NA_character_, character(1)),
    description = vapply(resources, function(r) {
      n <- r$description %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


#' Query tabular data from the California CKAN datastore
#'
#' Fetches rows from a CKAN datastore resource, with optional filtering
#' and full-text search. Columns vary by resource. All values are returned
#' as character and may need type conversion.
#'
#' @param resource_id Character. Resource UUID (from the \code{id} column of
#'   \code{cagov_resources()} results).
#' @param filters Named list of field = value filters (e.g.,
#'   \code{list(county_name = "Los Angeles")}). Applied as exact matches.
#' @param query Character or \code{NULL}. Full-text search within the resource.
#' @param limit Integer. Maximum rows to return (default 100).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns varying by resource. All columns are
#'   \code{character} type. Example columns for water quality stations:
#'   station_id, station_name, full_station_name, station_number,
#'   station_type, latitude, longitude, county_name, sample_count,
#'   sample_date_min, sample_date_max.
#' @examples
#' # First get a resource ID
#' res <- cagov_resources("water-quality-data")
#' cagov_datastore(res$id[1], limit = 5)
#'
#' # Filter by county
#' cagov_datastore(res$id[1], filters = list(county_name = "Los Angeles"))
#' @export
cagov_datastore <- function(resource_id, filters = NULL, query = NULL,
                            limit = 100, offset = 0) {
  params <- sprintf("resource_id=%s&limit=%d&offset=%d",
                    resource_id, limit, offset)
  if (!is.null(filters)) {
    params <- paste0(params, "&filters=",
                     utils::URLencode(jsonlite::toJSON(filters, auto_unbox = TRUE)))
  }
  if (!is.null(query)) {
    params <- paste0(params, "&q=", utils::URLencode(query, reserved = TRUE))
  }

  url <- paste0(.cagov_base, "/datastore_search?", params)
  result <- .ckan_result(url)
  if (is.null(result) || is.null(result$records) || length(result$records) == 0) {
    return(.schema_datastore)
  }

  records <- result$records
  # Convert list of lists to tibble
  cols <- unique(unlist(lapply(records, names)))
  cols <- cols[cols != "_id"]

  df_list <- lapply(cols, function(col) {
    vapply(records, function(r) as.character(r[[col]] %||% NA_character_), character(1))
  })
  names(df_list) <- cols
  as_tibble(df_list)
}


#' Get detailed metadata for a single California dataset
#'
#' Returns comprehensive metadata for one dataset including author,
#' maintainer, and last-modified date.
#'
#' @param dataset_name Character. Dataset name slug or UUID.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{\code{character} -- CKAN dataset UUID}
#'     \item{name}{\code{character} -- URL-safe slug}
#'     \item{title}{\code{character} -- Human-readable title}
#'     \item{organization}{\code{character} -- Publishing organization}
#'     \item{author}{\code{character} -- Dataset author (may be NA)}
#'     \item{maintainer}{\code{character} -- Dataset maintainer (may be NA)}
#'     \item{num_resources}{\code{integer} -- Number of attached resources}
#'     \item{notes}{\code{character} -- Full description text}
#'     \item{metadata_modified}{\code{character} -- Last modification timestamp (ISO 8601)}
#'   }
#' @examples
#' cagov_dataset("water-quality-data")
#' @export
cagov_dataset <- function(dataset_name) {
  schema <- tibble(id = character(), name = character(), title = character(),
                   organization = character(), author = character(),
                   maintainer = character(), num_resources = integer(),
                   notes = character(), metadata_modified = character())
  url <- sprintf("%s/package_show?id=%s", .cagov_base, utils::URLencode(dataset_name))
  result <- tryCatch(.ckan_result(url), error = function(e) NULL)
  if (is.null(result)) return(schema)

  tibble(
    id = as.character(result$id %||% NA_character_),
    name = as.character(result$name %||% NA_character_),
    title = as.character(result$title %||% NA_character_),
    organization = as.character(result$organization$title %||% NA_character_),
    author = as.character(result$author %||% NA_character_),
    maintainer = as.character(result$maintainer %||% NA_character_),
    num_resources = as.integer(result$num_resources %||% 0L),
    notes = as.character(result$notes %||% NA_character_),
    metadata_modified = as.character(result$metadata_modified %||% NA_character_)
  )
}

#' List organizations on California Open Data
#'
#' Returns state agencies and organizations that publish datasets on
#' data.ca.gov, sorted by number of published datasets (descending).
#'
#' @param limit Integer. Maximum number of organizations to return (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{character} -- Organization UUID}
#'     \item{name}{\code{character} -- URL-safe organization slug}
#'     \item{title}{\code{character} -- Organization display name (e.g., "CAL FIRE")}
#'     \item{package_count}{\code{integer} -- Number of published datasets}
#'   }
#' @examples
#' cagov_organizations(limit = 10)
#' @export
cagov_organizations <- function(limit = 50) {
  schema <- tibble(id = character(), name = character(), title = character(),
                   package_count = integer())
  url <- sprintf("%s/organization_list?all_fields=true&limit=%d", .cagov_base, as.integer(limit))
  result <- tryCatch(.ckan_result(url), error = function(e) NULL)
  if (is.null(result) || length(result) == 0) return(schema)

  tibble(
    id = vapply(result, function(o) o$id %||% NA_character_, character(1)),
    name = vapply(result, function(o) o$name %||% NA_character_, character(1)),
    title = vapply(result, function(o) o$title %||% NA_character_, character(1)),
    package_count = vapply(result, function(o) as.integer(o$package_count %||% 0L), integer(1))
  ) |> arrange(desc(package_count))
}

# == Context ===================================================================

#' Get data.ca.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cagov_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cagov_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.ca.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.ca.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.ca.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.ca.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
