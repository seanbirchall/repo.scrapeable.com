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
#' @export
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
#' @export
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
#' Each dataset contains one or more resources --- the actual data files
#' (CSV, JSON, API endpoints, etc.).
#'
#' @param id Dataset name or UUID
#' @return tibble: id, name, format, url, description, created (POSIXct), size
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
