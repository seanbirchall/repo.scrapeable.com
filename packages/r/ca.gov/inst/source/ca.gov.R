# data.ca.gov.R - Self-contained data.ca.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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
#' Searches the California Open Data CKAN catalog at data.ca.gov. Returns
#' matching datasets with metadata. Supports pagination via \code{rows}
#' and \code{start}.
#'
#' @param query Character. Search term (e.g. \code{"water"}, \code{"covid"},
#'   \code{"wildfire"}). \code{NULL} lists all datasets.
#' @param rows Integer. Maximum number of results to return (default 50).
#' @param start Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. CKAN dataset UUID.}
#'     \item{name}{Character. URL-safe dataset slug.}
#'     \item{title}{Character. Human-readable dataset title.}
#'     \item{organization}{Character. Publishing organization name (e.g.
#'       \code{"California Department of Water Resources"}).}
#'     \item{num_resources}{Integer. Number of data files/resources attached.}
#'     \item{notes}{Character. Dataset description (truncated to 200 chars).}
#'   }
#' @examples
#' cagov_search("water", rows = 5)
#' cagov_search("wildfire")
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


#' Get downloadable resources for a California dataset
#'
#' Returns the list of data files (CSV, JSON, PDF, etc.) attached to a
#' specific dataset. Use the resource ID with \code{\link{cagov_datastore}}
#' to query tabular data directly.
#'
#' @param dataset_name Character. Dataset name slug or UUID. Obtain from
#'   \code{cagov_search()$name} or \code{cagov_search()$id}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Resource UUID. Pass to \code{\link{cagov_datastore}}.}
#'     \item{name}{Character. Resource name/label.}
#'     \item{format}{Character. File format (e.g. \code{"CSV"}, \code{"JSON"},
#'       \code{"PDF"}).}
#'     \item{url}{Character. Direct download URL.}
#'     \item{description}{Character. Resource description (truncated to 200 chars).}
#'   }
#' @examples
#' cagov_resources("water-plan-water-balance-data")
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
#' Queries a tabular resource stored in the CKAN datastore engine.
#' Supports field-level filters and full-text search. Not all resources
#' have datastore access -- only those stored in CKAN's DataStore backend.
#'
#' @param resource_id Character. Resource UUID from \code{cagov_resources()$id}.
#' @param filters Named list. Field-value filter pairs (e.g.
#'   \code{list(county = "Los Angeles")}). \code{NULL} for no filter.
#' @param query Character. Full-text search string within the resource.
#'   \code{NULL} for no text search.
#' @param limit Integer. Maximum rows to return (default 100).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble whose columns depend on the resource schema. All values
#'   are returned as character; apply type conversion as needed. Returns an
#'   empty tibble if the resource has no datastore records.
#' @examples
#' \dontrun{
#' res <- cagov_resources("water-plan-water-balance-data")
#' cagov_datastore(res$id[1], limit = 10)
#' }
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
#' Returns extended information about a specific dataset including author,
#' maintainer, and last modification date.
#'
#' @param dataset_name Character. Dataset name slug or UUID.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. CKAN dataset UUID.}
#'     \item{name}{Character. URL-safe dataset slug.}
#'     \item{title}{Character. Human-readable title.}
#'     \item{organization}{Character. Publishing organization name.}
#'     \item{author}{Character. Dataset author.}
#'     \item{maintainer}{Character. Dataset maintainer contact.}
#'     \item{num_resources}{Integer. Number of attached resources.}
#'     \item{notes}{Character. Full dataset description.}
#'     \item{metadata_modified}{Character. ISO 8601 timestamp of last metadata update.}
#'   }
#' @examples
#' cagov_dataset("water-plan-water-balance-data")
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

#' List organizations publishing on California Open Data
#'
#' Returns state agencies and other organizations that publish datasets
#' on data.ca.gov, sorted by number of datasets (descending).
#'
#' @param limit Integer. Maximum number of organizations to return (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Organization UUID.}
#'     \item{name}{Character. URL-safe organization slug.}
#'     \item{title}{Character. Organization display name (e.g.
#'       \code{"CAL FIRE"}).}
#'     \item{package_count}{Integer. Number of datasets published.}
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

#' Get ca.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ca.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ca.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ca.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ca.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
