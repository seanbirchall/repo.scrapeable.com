# == Public functions ==========================================================

#' Search HDX datasets
#'
#' @param query Search query (e.g. "refugees", "food security", "cholera")
#' @param rows Number of results (default 10)
#' @param start Offset for pagination (default 0)
#' @return tibble: id, name, title, organization, num_resources,
#'   metadata_created, notes
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
#' @param id Dataset ID or name slug
#' @return tibble: id, name, title, organization, metadata_created,
#'   resource_name, resource_url, resource_format, resource_size (one row per resource)
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
#' @param limit Max results (default 20)
#' @param offset Offset for pagination (default 0)
#' @return tibble: id, name, title, description, package_count
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

#' Show HDX client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
hdx_context <- function() {
  .build_context(
    pkg_name = "data.humdata.org",
    header_lines = c(
      "# data.humdata.org -- OCHA Humanitarian Data Exchange Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# CKAN-based API",
      "# Example queries: refugees, food security, cholera, displacement, conflict"
    )
  )
}

