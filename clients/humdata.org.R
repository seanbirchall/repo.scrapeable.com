# data.humdata.org.R - Self-contained data.humdata.org client

library(httr2)
library(jsonlite)
library(tibble)


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
#' Searches the OCHA Humanitarian Data Exchange (HDX) catalog via
#' the CKAN package_search API. HDX hosts humanitarian datasets
#' covering crises, refugees, health, food security, and more.
#'
#' @param query Search query (e.g. "refugees", "food security", "cholera",
#'   "earthquake", "displacement")
#' @param rows Number of results to return (default 10)
#' @param start Offset for pagination (default 0)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Dataset UUID (character)}
#'     \item{name}{URL-safe slug name (character)}
#'     \item{title}{Human-readable dataset title (character)}
#'     \item{organization}{Publishing organization name (character)}
#'     \item{num_resources}{Number of downloadable files (integer)}
#'     \item{metadata_created}{Creation timestamp (character)}
#'     \item{notes}{First 200 chars of description (character)}
#'   }
#' @examples
#' hdx_search("refugees")
#' hdx_search("food security", rows = 5)
#' @seealso [hdx_dataset()], [hdx_organizations()], [hdx_context()]
#' @source <https://data.humdata.org/api/3/action>
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
#' Fetches full metadata and resource listing for a single HDX dataset.
#' Returns one row per downloadable resource (file) in the dataset.
#'
#' @param id Dataset UUID or URL-safe name slug (from hdx_search results)
#' @return A tibble with one row per resource and columns:
#'   \describe{
#'     \item{id}{Dataset UUID (character)}
#'     \item{name}{Dataset slug name (character)}
#'     \item{title}{Dataset title (character)}
#'     \item{organization}{Publishing organization (character)}
#'     \item{metadata_created}{Creation timestamp (character)}
#'     \item{resource_name}{File/resource name (character)}
#'     \item{resource_url}{Download URL for the resource (character)}
#'     \item{resource_format}{File format, e.g. "CSV", "XLSX" (character)}
#'     \item{resource_size}{File size in bytes (integer)}
#'   }
#' @examples
#' hdx_dataset("world-bank-indicators")
#' @seealso [hdx_search()], [hdx_organizations()], [hdx_context()]
#' @source <https://data.humdata.org/api/3/action>
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
#' Retrieves organizations that publish datasets on HDX, including
#' UN agencies, NGOs, and government bodies.
#'
#' @param limit Max results to return (default 20)
#' @param offset Offset for pagination (default 0)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Organization UUID (character)}
#'     \item{name}{URL-safe slug (character)}
#'     \item{title}{Organization display name (character)}
#'     \item{description}{First 200 chars of description (character)}
#'     \item{package_count}{Number of datasets published (integer)}
#'   }
#' @examples
#' hdx_organizations()
#' hdx_organizations(limit = 50)
#' @seealso [hdx_search()], [hdx_dataset()], [hdx_context()]
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

# == Context ===================================================================

#' Get humdata.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/humdata.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "humdata.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# humdata.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# humdata.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
