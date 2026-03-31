# data-gov.R
# Self-contained Data.gov (CKAN) catalog client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 Action API at catalog.data.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.datagov_base <- "https://catalog.data.gov/api/3/action"

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
#' Searches the federal open data catalog (153,000+ datasets).
#' Uses the CKAN package_search action.
#'
#' @param query Search term (e.g. "climate", "census", "transportation")
#' @param organization Filter by agency slug (e.g. "noaa-gov", "ed-gov").
#'   Use datagov_organizations() to find slugs.
#' @param rows Number of results (default 50, max 1000)
#' @param start Offset for pagination (default 0)
#' @param sort Sort field: "score desc" (relevance, default),
#'   "metadata_modified desc" (newest), "name asc" (alphabetical)
#' @return tibble: id, name, title, organization, num_resources,
#'   metadata_created (POSIXct), notes
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
#' Returns metadata and resource list for a single dataset.
#'
#' @param id Dataset name or UUID (e.g. "annual-survey-of-manufactures")
#' @return tibble: one row with id, name, title, organization,
#'   num_resources, metadata_created, notes
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
#' Each dataset contains one or more resources — the actual data files
#' (CSV, JSON, API endpoints, etc.).
#'
#' @param id Dataset name or UUID
#' @return tibble: id, name, format, url, description, created (POSIXct), size
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
#' Returns federal agencies and their dataset counts.
#'
#' @param query Optional search term to filter organizations
#' @param limit Max results (default 100)
#' @param offset Pagination offset (default 0)
#' @return tibble: id, name (slug), title, package_count, description
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
#' Returns topic categories used to organize datasets.
#'
#' @param limit Max results (default 100)
#' @return tibble: id, name, title, package_count, description
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
#' Returns tags used to classify datasets.
#'
#' @param query Optional search term
#' @param limit Max results (default 100)
#' @return tibble: id, name
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


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the data.gov package
#'
#' @return Character string (invisibly), also printed
datagov_context <- function() {
  .build_context("data.gov", header_lines = c(
    "# data.gov - Federal Open Data Catalog Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API: CKAN v3 Action API at catalog.data.gov",
    "# All functions return tibbles with typed columns.",
    "#",
    "# 153,000+ datasets from federal agencies.",
    "#",
    "# Common organization slugs:",
    "#   noaa-gov, ed-gov, epa-gov, hhs-gov, dot-gov, usda-gov",
    "#",
    "# Workflow: datagov_search() -> datagov_resources() -> download URL"
  ))
}
