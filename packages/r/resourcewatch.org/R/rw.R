# == Dataset listing ===========================================================

#' List Resource Watch datasets
#'
#' @param page_size Number of results (default 10)
#' @param page Page number (default 1)
#' @return tibble: id, name, subtitle, provider, connectorType, published, env
#' @export
rw_datasets <- function(page_size = 10, page = 1) {
  url <- paste0(.rw_base, "/dataset?page[size]=", page_size, "&page[number]=", page)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_datasets)

  attrs <- d$attributes
  if (is.null(attrs)) return(.schema_datasets)

  tibble(
    id = as.character(d$id),
    name = as.character(attrs$name %||% NA_character_),
    subtitle = as.character(if ("subtitle" %in% names(attrs)) attrs$subtitle else NA_character_),
    provider = as.character(if ("provider" %in% names(attrs)) attrs$provider else NA_character_),
    connectorType = as.character(if ("connectorType" %in% names(attrs)) attrs$connectorType else NA_character_),
    published = as.logical(if ("published" %in% names(attrs)) attrs$published else NA),
    env = as.character(if ("env" %in% names(attrs)) attrs$env else NA_character_)
  )
}

# == Dataset detail ============================================================

#' Fetch a single Resource Watch dataset by ID
#'
#' @param id Dataset ID string
#' @return tibble: one row with id, name, subtitle, description, provider,
#'   connectorType, tableName, published
#' @export
rw_dataset <- function(id) {
  url <- paste0(.rw_base, "/dataset/", id)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || is.null(d$id)) return(.schema_dataset_detail)
  a <- d$attributes

  tibble(
    id = as.character(d$id),
    name = as.character(a$name %||% NA_character_),
    subtitle = as.character(a$subtitle %||% NA_character_),
    description = as.character(a$description %||% NA_character_),
    provider = as.character(a$provider %||% NA_character_),
    connectorType = as.character(a$connectorType %||% NA_character_),
    tableName = as.character(a$tableName %||% NA_character_),
    published = as.logical(a$published %||% NA)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Resource Watch package
#'
#' @return Character string (invisibly), also printed
#' @export
rw_context <- function() {
  .build_context("resourcewatch.org", header_lines = c(
    "# resourcewatch.org - Resource Watch API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Resource Watch provides environmental and social datasets.",
    "# Datasets can be from various providers (cartodb, gee, etc)."
  ))
}
