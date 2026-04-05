# data.gov.R - Self-contained data.gov client

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)


# data-gov.R
# Self-contained Data.gov (CKAN) catalog client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 Action API at catalog.data.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.datagov_base <- "https://catalog.data.gov/api/3/action"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- CKAN URL builder ----------------------------------------------------------

.ckan_url <- function(action, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params) == 0) return(paste0(.datagov_base, "/", action))
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(.datagov_base, "/", action, "?", query)
}

# -- Result extractor ----------------------------------------------------------

.ckan_result <- function(url) {
  raw <- .fetch_json(url)
  if (!isTRUE(raw$success)) {
    warning("CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(),
  metadata_created = as.POSIXct(character()), notes = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character(),
  created = as.POSIXct(character()), size = numeric()
)

.schema_organizations <- tibble(
  id = character(), name = character(), title = character(),
  package_count = integer(), description = character()
)

.schema_groups <- tibble(
  id = character(), name = character(), title = character(),
  package_count = integer(), description = character()
)

.schema_tags <- tibble(
  id = character(), name = character()
)

# == Dataset search ============================================================

#' Search Data.gov datasets
#'
#' Searches the federal open data catalog (153,000+ datasets) via the
#' CKAN package_search action at catalog.data.gov.
#'
#' @param query Character or NULL. Search term.
#'   Example: \code{"climate"}, \code{"census"}, \code{"transportation"}
#' @param organization Character or NULL. Filter by agency slug.
#'   Use \code{\link{datagov_organizations}} to find slugs.
#'   Example: \code{"noaa-gov"}, \code{"ed-gov"}, \code{"epa-gov"}
#' @param rows Integer. Number of results (default 50, max 1000).
#' @param start Integer. Offset for pagination (default 0).
#' @param sort Character. Sort field (default \code{"score desc"} for relevance).
#'   Options: \code{"score desc"}, \code{"metadata_modified desc"},
#'   \code{"name asc"}.
#' @return A tibble with 7 columns:
#'   \describe{
#'     \item{id}{Character. CKAN dataset UUID.}
#'     \item{name}{Character. URL-safe dataset slug.}
#'     \item{title}{Character. Human-readable title.}
#'     \item{organization}{Character. Agency slug (e.g. "noaa-gov").}
#'     \item{num_resources}{Integer. Number of downloadable resources.}
#'     \item{metadata_created}{POSIXct. Date metadata was created.}
#'     \item{notes}{Character. Description (truncated to 200 chars).}
#'   }
#' @examples
#' datagov_search("climate", rows = 10)
#' datagov_search(organization = "noaa-gov", rows = 5)
datagov_search <- function(query = NULL, organization = NULL,
                           rows = 50, start = 0,
                           sort = "score desc") {
  fq <- NULL
  if (!is.null(organization)) fq <- paste0("organization:", organization)

  url <- .ckan_url("package_search",
                    q = if (!is.null(query)) utils::URLencode(query, reserved = TRUE) else NULL,
                    fq = if (!is.null(fq)) utils::URLencode(fq, reserved = TRUE) else NULL,
                    rows = rows,
                    start = start,
                    sort = utils::URLencode(sort, reserved = TRUE))

  result <- .ckan_result(url)
  if (is.null(result)) return(.schema_datasets)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_datasets)

  tibble(
    id               = vapply(datasets, function(d) d$id %||% NA_character_, character(1)),
    name             = vapply(datasets, function(d) d$name %||% NA_character_, character(1)),
    title            = vapply(datasets, function(d) d$title %||% NA_character_, character(1)),
    organization     = vapply(datasets, function(d) d$organization$name %||% NA_character_, character(1)),
    num_resources    = vapply(datasets, function(d) as.integer(d$num_resources %||% 0L), integer(1)),
    metadata_created = as.POSIXct(vapply(datasets, function(d) d$metadata_created %||% NA_character_, character(1))),
    notes            = vapply(datasets, function(d) {
      n <- d$notes %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


# == Dataset details ===========================================================

#' Get full details for a Data.gov dataset
#'
#' Returns metadata for a single dataset by name slug or UUID.
#' Use \code{\link{datagov_resources}} to get downloadable file URLs.
#'
#' @param id Character. Dataset name slug or UUID. Obtain from
#'   \code{\link{datagov_search}} results.
#'   Example: a name slug from search results.
#' @return A tibble with 1 row and 7 columns (same as \code{\link{datagov_search}}):
#'   id, name, title, organization, num_resources, metadata_created, notes.
#' @examples
#' ds <- datagov_search("census", rows = 1)
#' datagov_dataset(ds$name[1])
datagov_dataset <- function(id) {
  url <- .ckan_url("package_show", id = id)
  result <- .ckan_result(url)
  if (is.null(result)) return(.schema_datasets)

  tibble(
    id               = result$id %||% NA_character_,
    name             = result$name %||% NA_character_,
    title            = result$title %||% NA_character_,
    organization     = result$organization$name %||% NA_character_,
    num_resources    = as.integer(result$num_resources %||% 0L),
    metadata_created = as.POSIXct(result$metadata_created %||% NA_character_),
    notes            = result$notes %||% NA_character_
  )
}


#' Get resources (downloadable files) for a dataset
#'
#' Each dataset contains one or more resources -- the actual data files
#' (CSV, JSON, API endpoints, etc.). Returns download URLs and metadata.
#'
#' @param id Character. Dataset name slug or UUID.
#' @return A tibble with 7 columns:
#'   \describe{
#'     \item{id}{Character. Resource UUID.}
#'     \item{name}{Character. Resource name/label.}
#'     \item{format}{Character. File format (e.g. "CSV", "JSON", "API").}
#'     \item{url}{Character. Download URL.}
#'     \item{description}{Character. Resource description (truncated to 200 chars).}
#'     \item{created}{POSIXct. Creation timestamp.}
#'     \item{size}{Numeric. File size in bytes (or NA).}
#'   }
#' @examples
#' ds <- datagov_search("census", rows = 1)
#' datagov_resources(ds$name[1])
datagov_resources <- function(id) {
  url <- .ckan_url("package_show", id = id)
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
    }, character(1)),
    created     = as.POSIXct(vapply(resources, function(r) r$created %||% NA_character_, character(1))),
    size        = vapply(resources, function(r) as.numeric(r$size %||% NA_real_), numeric(1))
  )
}


# == Organizations =============================================================

#' List Data.gov organizations (agencies)
#'
#' Returns federal agencies and their dataset counts. Use the \code{name}
#' column (slug) as the \code{organization} filter in \code{\link{datagov_search}}.
#'
#' @param query Character or NULL. Optional search term to filter organizations.
#'   Example: \code{"noaa"}, \code{"education"}
#' @param limit Integer. Max results (default 100).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with 5 columns:
#'   \describe{
#'     \item{id}{Character. Organization UUID.}
#'     \item{name}{Character. URL slug (e.g. "noaa-gov"). Use in datagov_search().}
#'     \item{title}{Character. Full agency name.}
#'     \item{package_count}{Integer. Number of datasets.}
#'     \item{description}{Character. Description (truncated to 200 chars).}
#'   }
#' @examples
#' datagov_organizations(limit = 10)
#' datagov_organizations(query = "health")
datagov_organizations <- function(query = NULL, limit = 100, offset = 0) {
  url <- .ckan_url("organization_list",
                    all_fields = "true",
                    q = if (!is.null(query)) utils::URLencode(query, reserved = TRUE) else NULL,
                    limit = limit,
                    offset = offset)

  result <- .ckan_result(url)
  if (is.null(result) || length(result) == 0) return(.schema_organizations)

  tibble(
    id            = vapply(result, function(o) o$id %||% NA_character_, character(1)),
    name          = vapply(result, function(o) o$name %||% NA_character_, character(1)),
    title         = vapply(result, function(o) o$title %||% NA_character_, character(1)),
    package_count = vapply(result, function(o) as.integer(o$package_count %||% 0L), integer(1)),
    description   = vapply(result, function(o) {
      n <- o$description %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


# == Groups (topics) ===========================================================

#' List Data.gov topic groups
#'
#' Returns topic categories used to organize datasets on Data.gov
#' (e.g. "Agriculture", "Climate", "Energy").
#'
#' @param limit Integer. Max results (default 100).
#' @return A tibble with 5 columns:
#'   \describe{
#'     \item{id}{Character. Group UUID.}
#'     \item{name}{Character. Group slug.}
#'     \item{title}{Character. Group title (e.g. "Climate").}
#'     \item{package_count}{Integer. Number of datasets in this group.}
#'     \item{description}{Character. Group description.}
#'   }
#' @examples
#' datagov_groups(limit = 10)
datagov_groups <- function(limit = 100) {
  url <- .ckan_url("group_list", all_fields = "true", limit = limit)
  result <- .ckan_result(url)
  if (is.null(result) || length(result) == 0) return(.schema_groups)

  tibble(
    id            = vapply(result, function(g) g$id %||% NA_character_, character(1)),
    name          = vapply(result, function(g) g$name %||% NA_character_, character(1)),
    title         = vapply(result, function(g) g$title %||% NA_character_, character(1)),
    package_count = vapply(result, function(g) as.integer(g$package_count %||% 0L), integer(1)),
    description   = vapply(result, function(g) {
      n <- g$description %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


# == Tags ======================================================================

#' List Data.gov tags
#'
#' Returns tags used to classify datasets. Useful for discovering
#' topics and filtering searches.
#'
#' @param query Character or NULL. Optional search term to filter tags.
#'   Example: \code{"climate"}, \code{"health"}
#' @param limit Integer. Max results (default 100).
#' @return A tibble with 2 columns:
#'   \describe{
#'     \item{id}{Character. Tag UUID (may be NA for simple list).}
#'     \item{name}{Character. Tag name (e.g. "climate-change").}
#'   }
#' @examples
#' datagov_tags(query = "climate", limit = 10)
datagov_tags <- function(query = NULL, limit = 100) {
  url <- .ckan_url("tag_list",
                    all_fields = "true",
                    query = if (!is.null(query)) utils::URLencode(query, reserved = TRUE) else NULL,
                    limit = limit)
  result <- .ckan_result(url)
  if (is.null(result) || length(result) == 0) return(.schema_tags)

  if (is.character(result)) {
    return(tibble(id = NA_character_, name = result))
  }

  tibble(
    id   = vapply(result, function(t) t$id %||% NA_character_, character(1)),
    name = vapply(result, function(t) t$name %||% NA_character_, character(1))
  )
}


# == Context ===================================================================

#' Get data.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
datagov_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(datagov_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
