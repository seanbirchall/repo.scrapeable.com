# humdata.R
# Self-contained OCHA Humanitarian Data Exchange (HDX) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: generous (CKAN API)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hdx_base <- "https://data.humdata.org/api/3/action"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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

# == Public functions ==========================================================

#' Search HDX humanitarian datasets
#'
#' Search the OCHA Humanitarian Data Exchange (HDX) CKAN catalog for datasets
#' related to humanitarian crises, development indicators, and emergency
#' response. Use \code{hdx_dataset()} to get full resource details for a result.
#'
#' @param query Search query (e.g. \code{"refugees"}, \code{"food security"},
#'   \code{"cholera"}, \code{"displacement"}).
#' @param rows Number of results to return (default 10).
#' @param start Offset for pagination (default 0).
#' @return A tibble with one row per dataset:
#'   \describe{
#'     \item{id}{\code{character} -- HDX dataset UUID.}
#'     \item{name}{\code{character} -- URL-safe dataset slug.}
#'     \item{title}{\code{character} -- Human-readable dataset title.}
#'     \item{organization}{\code{character} -- Publishing organization name.}
#'     \item{num_resources}{\code{integer} -- Number of downloadable resources.}
#'     \item{metadata_created}{\code{character} -- Creation timestamp.}
#'     \item{notes}{\code{character} -- Dataset description (truncated to 200 chars).}
#'   }
#' @examples
#' \dontrun{
#' hdx_search("refugees", rows = 20)
#' hdx_search("food security", rows = 5, start = 10)
#' }
#' @export
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
#' Retrieve full dataset metadata from HDX, including one row per downloadable
#' resource (CSV, Excel, GeoJSON, etc.). The resource URLs can be used to
#' download the actual data files.
#'
#' @param id Dataset UUID or URL-safe name slug. Obtain from the \code{id} or
#'   \code{name} columns of \code{hdx_search()} results.
#' @return A tibble with one row per resource:
#'   \describe{
#'     \item{id}{\code{character} -- HDX dataset UUID.}
#'     \item{name}{\code{character} -- Dataset slug.}
#'     \item{title}{\code{character} -- Dataset title.}
#'     \item{organization}{\code{character} -- Publishing organization.}
#'     \item{metadata_created}{\code{character} -- Creation timestamp.}
#'     \item{resource_name}{\code{character} -- Resource file name.}
#'     \item{resource_url}{\code{character} -- Direct download URL.}
#'     \item{resource_format}{\code{character} -- File format (e.g. \code{"CSV"}, \code{"XLSX"}).}
#'     \item{resource_size}{\code{integer} -- File size in bytes.}
#'   }
#' @examples
#' \dontrun{
#' results <- hdx_search("cholera")
#' hdx_dataset(results$name[1])
#' }
#' @export
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
#' Browse organizations publishing datasets on the Humanitarian Data Exchange.
#' Includes UN agencies, NGOs, and government bodies active in humanitarian
#' data sharing.
#'
#' @param limit Maximum number of organizations to return (default 20).
#' @param offset Offset for pagination (default 0).
#' @return A tibble with one row per organization:
#'   \describe{
#'     \item{id}{\code{character} -- Organization UUID.}
#'     \item{name}{\code{character} -- URL-safe organization slug.}
#'     \item{title}{\code{character} -- Organization display name.}
#'     \item{description}{\code{character} -- Description (truncated to 200 chars).}
#'     \item{package_count}{\code{integer} -- Number of published datasets.}
#'   }
#' @examples
#' \dontrun{
#' hdx_organizations(limit = 50)
#' }
#' @export
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

#' Get HDX client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' hdx_context()
#' }
#' @export
hdx_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hdx_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/humdata.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "humdata")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# humdata context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# humdata", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
