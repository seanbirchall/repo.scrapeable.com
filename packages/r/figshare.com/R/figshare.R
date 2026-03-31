# == Article search ============================================================

#' Search Figshare articles
#'
#' @param query Search query string (e.g. "climate", "genomics")
#' @param page_size Number of results per page (default 10, max 1000)
#' @param page Page number (default 1)
#' @return tibble: id, title, doi, url, published_date, defined_type_name
#' @export
figshare_search <- function(query, page_size = 10, page = 1) {
  body <- list(search_for = query, page_size = page_size, page = page)
  raw <- .post_json(paste0(.figshare_base, "/articles/search"), body)
  if (is.null(raw) || length(raw) == 0) return(.schema_articles)

  as_tibble(raw) |>
    transmute(
      id = as.integer(id),
      title = as.character(title),
      doi = as.character(doi),
      url = as.character(url),
      published_date = tryCatch(as.Date(published_date), error = function(e) as.Date(NA)),
      defined_type_name = as.character(if ("defined_type_name" %in% names(raw)) defined_type_name else NA_character_)
    )
}


# == Article detail ============================================================

#' Fetch a single Figshare article by ID
#'
#' @param id Figshare article ID (integer)
#' @return tibble: one row with id, title, doi, url, published_date,
#'   description, defined_type_name, license, authors, tags, citation
#' @export
figshare_article <- function(id) {
  url <- paste0(.figshare_base, "/articles/", id)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$id)) return(.schema_article_detail)

  auth_str <- if (!is.null(raw$authors) && is.data.frame(raw$authors)) {
    paste(raw$authors$full_name, collapse = "; ")
  } else NA_character_

  tag_str <- if (!is.null(raw$tags)) {
    paste(raw$tags, collapse = "; ")
  } else NA_character_

  lic_str <- if (!is.null(raw$license) && is.list(raw$license)) {
    as.character(raw$license$name %||% NA_character_)
  } else NA_character_

  tibble(
    id = as.integer(raw$id),
    title = as.character(raw$title %||% NA_character_),
    doi = as.character(raw$doi %||% NA_character_),
    url = as.character(raw$url %||% NA_character_),
    published_date = tryCatch(as.Date(raw$published_date), error = function(e) as.Date(NA)),
    description = as.character(raw$description %||% NA_character_),
    defined_type_name = as.character(raw$defined_type_name %||% NA_character_),
    license = lic_str,
    authors = auth_str,
    tags = tag_str,
    citation = as.character(raw$citation %||% NA_character_)
  )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Figshare package
#'
#' @return Character string (invisibly), also printed
#' @export
figshare_context <- function() {
  .build_context("figshare.com", header_lines = c(
    "# figshare.com - Figshare Research Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for public data",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Figshare hosts research data, figures, and datasets.",
    "# Search uses POST with JSON body. Article detail uses GET."
  ))
}
