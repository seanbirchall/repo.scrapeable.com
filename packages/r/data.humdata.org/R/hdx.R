# data.humdata.org.R - Self-contained data.humdata.org client



# humdata.R
# Self-contained OCHA Humanitarian Data Exchange (HDX) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: generous (CKAN API)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hdx_base <- "https://data.humdata.org/api/3/action"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(),
  metadata_created = character(), notes = character()
)

.schema_dataset <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), metadata_created = character(),
  resource_name = character(), resource_url = character(),
  resource_format = character(), resource_size = integer()
)

.schema_organizations <- tibble(
  id = character(), name = character(), title = character(),
  description = character(), package_count = integer()
)


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search HDX datasets
#'
#' Searches the OCHA Humanitarian Data Exchange (HDX) for datasets matching
#' a query. HDX hosts thousands of humanitarian datasets covering crises,
#' refugees, food security, health, and more. Use the returned \code{id}
#' or \code{name} with \code{hdx_dataset()} to get resource download URLs.
#'
#' @param query Character. Free-text search query. Examples:
#'   \code{"refugees"}, \code{"food security"}, \code{"cholera"},
#'   \code{"displacement"}, \code{"malnutrition"}, \code{"earthquake"}.
#' @param rows Integer. Number of results to return (default 10).
#' @param start Integer. Offset for pagination (default 0). Use
#'   \code{start = 10} to get the second page of 10 results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. UUID dataset identifier.}
#'     \item{name}{Character. URL-safe slug (use with \code{hdx_dataset()}).}
#'     \item{title}{Character. Human-readable dataset title.}
#'     \item{organization}{Character. Contributing organization (e.g. \code{"OCHA Sudan"}).}
#'     \item{num_resources}{Integer. Number of downloadable files/resources.}
#'     \item{metadata_created}{Character. ISO 8601 creation timestamp.}
#'     \item{notes}{Character. Description excerpt (truncated to 200 chars).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hdx_search("refugees", rows = 5)
#' hdx_search("food security", rows = 20, start = 0)
#' }
hdx_search <- function(query, rows = 10, start = 0) {
  url <- sprintf(
    "%s/package_search?q=%s&rows=%d&start=%d",
    .hdx_base, utils::URLencode(query, reserved = TRUE),
    as.integer(rows), as.integer(start)
  )
  raw <- .fetch_json(url)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0 ||
      (is.data.frame(results) && nrow(results) == 0)) return(.schema_search)

  tibble(
    id = as.character(results$id),
    name = as.character(results$name),
    title = as.character(results$title),
    organization = as.character(
      if (!is.null(results$organization)) results$organization$title
      else NA_character_
    ),
    num_resources = as.integer(results$num_resources %||% NA_integer_),
    metadata_created = as.character(results$metadata_created %||% NA_character_),
    notes = as.character(substr(results$notes %||% NA_character_, 1, 200))
  )
}

#' Get a specific HDX dataset by ID or name
#'
#' Retrieves full metadata and resource listings for a single HDX dataset.
#' Returns one row per downloadable resource (file), making it easy to find
#' CSV, Excel, or other data files to download.
#'
#' @param id Character. Dataset UUID or name slug as returned by
#'   \code{hdx_search()} (e.g. \code{"refugee-movements-in-sudan"}).
#' @return A tibble with one row per resource and columns:
#'   \describe{
#'     \item{id}{Character. UUID dataset identifier.}
#'     \item{name}{Character. URL-safe slug.}
#'     \item{title}{Character. Dataset title.}
#'     \item{organization}{Character. Contributing organization.}
#'     \item{metadata_created}{Character. ISO 8601 creation timestamp.}
#'     \item{resource_name}{Character. Name of the downloadable file.}
#'     \item{resource_url}{Character. Direct download URL for the resource.}
#'     \item{resource_format}{Character. File format (e.g. \code{"CSV"}, \code{"XLSX"}, \code{"JSON"}).}
#'     \item{resource_size}{Integer. File size in bytes (may be NA).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hdx_dataset("refugee-movements-in-sudan")
#' }
hdx_dataset <- function(id) {
  url <- sprintf("%s/package_show?id=%s", .hdx_base, id)
  raw <- .fetch_json(url)
  ds <- raw$result
  if (is.null(ds)) return(.schema_dataset)

  resources <- ds$resources
  if (is.null(resources) || length(resources) == 0) {
    return(tibble(
      id = as.character(ds$id), name = as.character(ds$name),
      title = as.character(ds$title),
      organization = as.character(ds$organization$title %||% NA_character_),
      metadata_created = as.character(ds$metadata_created %||% NA_character_),
      resource_name = NA_character_, resource_url = NA_character_,
      resource_format = NA_character_, resource_size = NA_integer_
    ))
  }

  tibble(
    id = as.character(ds$id),
    name = as.character(ds$name),
    title = as.character(ds$title),
    organization = as.character(ds$organization$title %||% NA_character_),
    metadata_created = as.character(ds$metadata_created %||% NA_character_),
    resource_name = as.character(resources$name),
    resource_url = as.character(resources$url),
    resource_format = as.character(resources$format %||% NA_character_),
    resource_size = as.integer(resources$size %||% NA_integer_)
  )
}

#' List HDX organizations
#'
#' Returns a paginated list of organizations contributing to the Humanitarian
#' Data Exchange. Each organization publishes one or more datasets (indicated
#' by \code{package_count}).
#'
#' @param limit Integer. Maximum number of organizations to return (default 20).
#' @param offset Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. UUID organization identifier.}
#'     \item{name}{Character. URL-safe slug.}
#'     \item{title}{Character. Organization display name (e.g. \code{"ACAPS"}, \code{"OCHA Sudan"}).}
#'     \item{description}{Character. Description excerpt (truncated to 200 chars).}
#'     \item{package_count}{Integer. Number of published datasets.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hdx_organizations(limit = 10)
#' hdx_organizations(limit = 50, offset = 50)
#' }
hdx_organizations <- function(limit = 20, offset = 0) {
  url <- sprintf(
    "%s/organization_list?all_fields=true&limit=%d&offset=%d",
    .hdx_base, as.integer(limit), as.integer(offset)
  )
  raw <- .fetch_json(url)
  results <- raw$result
  if (is.null(results) || length(results) == 0 ||
      (is.data.frame(results) && nrow(results) == 0)) return(.schema_organizations)

  tibble(
    id = as.character(results$id),
    name = as.character(results$name),
    title = as.character(results$title),
    description = as.character(substr(results$description %||% NA_character_, 1, 200)),
    package_count = as.integer(results$package_count %||% NA_integer_)
  )
}

# == Context ===================================================================

#' Get data.humdata.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hdx_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hdx_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.humdata.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.humdata.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.humdata.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.humdata.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
