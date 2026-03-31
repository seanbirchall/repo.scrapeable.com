# == Paper search ==============================================================

#' Search Semantic Scholar papers
#'
#' @param query Search query string (e.g. "CRISPR", "machine learning")
#' @param limit Maximum number of results (default 10, max 100)
#' @param fields Comma-separated fields to return (default "title,year,citationCount,authors")
#' @return tibble: paperId, title, year, citationCount, authors, url
#' @export
s2_papers <- function(query, limit = 10,
                      fields = "title,year,citationCount,authors,url") {
  url <- paste0(.s2_base, "/paper/search?query=", utils::URLencode(query),
                "&limit=", limit, "&fields=", fields)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_papers)

  df <- as_tibble(d)
  # Authors come as a list-column of data.frames; collapse to character

  if ("authors" %in% names(df) && is.list(df$authors)) {
    df$authors <- vapply(df$authors, function(a) {
      if (is.data.frame(a) && "name" %in% names(a)) paste(a$name, collapse = "; ")
      else if (is.character(a)) paste(a, collapse = "; ")
      else NA_character_
    }, character(1))
  }

  df |>
    transmute(
      paperId = as.character(paperId),
      title = as.character(title),
      year = as.integer(if ("year" %in% names(df)) year else NA_integer_),
      citationCount = as.integer(if ("citationCount" %in% names(df)) citationCount else NA_integer_),
      authors = as.character(if ("authors" %in% names(df)) authors else NA_character_),
      url = as.character(if ("url" %in% names(df)) url else NA_character_)
    )
}


# == Paper detail ==============================================================

#' Fetch a single Semantic Scholar paper by ID
#'
#' @param paper_id Semantic Scholar paper ID, DOI, or ArXiv ID
#' @param fields Comma-separated fields (default includes abstract, venue)
#' @return tibble: one row with paperId, title, year, citationCount,
#'   referenceCount, abstract, venue, url, authors
#' @export
s2_paper <- function(paper_id,
                     fields = "title,year,citationCount,referenceCount,abstract,venue,authors,url") {
  url <- paste0(.s2_base, "/paper/", utils::URLencode(paper_id), "?fields=", fields)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$paperId)) return(.schema_paper_detail)

  auth_str <- if (!is.null(raw$authors) && is.data.frame(raw$authors)) {
    paste(raw$authors$name, collapse = "; ")
  } else NA_character_

  tibble(
    paperId = as.character(raw$paperId),
    title = as.character(raw$title %||% NA_character_),
    year = as.integer(raw$year %||% NA_integer_),
    citationCount = as.integer(raw$citationCount %||% NA_integer_),
    referenceCount = as.integer(raw$referenceCount %||% NA_integer_),
    abstract = as.character(raw$abstract %||% NA_character_),
    venue = as.character(raw$venue %||% NA_character_),
    url = as.character(raw$url %||% NA_character_),
    authors = auth_str
  )
}


# == Author search =============================================================

#' Search Semantic Scholar authors
#'
#' @param query Author name to search
#' @param limit Maximum results (default 10, max 1000)
#' @return tibble: authorId, name, paperCount, citationCount, url
#' @export
s2_authors <- function(query, limit = 10) {
  url <- paste0(.s2_base, "/author/search?query=", utils::URLencode(query),
                "&limit=", limit)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_authors)

  as_tibble(d) |>
    transmute(
      authorId = as.character(authorId),
      name = as.character(name),
      paperCount = as.integer(if ("paperCount" %in% names(d)) paperCount else NA_integer_),
      citationCount = as.integer(if ("citationCount" %in% names(d)) citationCount else NA_integer_),
      url = as.character(if ("url" %in% names(d)) url else NA_character_)
    )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Semantic Scholar package
#'
#' @return Character string (invisibly), also printed
#' @export
s2_context <- function() {
  .build_context("api.semanticscholar.org", header_lines = c(
    "# api.semanticscholar.org - Semantic Scholar Academic API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (rate-limited without key)",
    "# Rate limit: ~100 requests per 5 minutes without API key",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common paper IDs: use DOI (e.g. '10.1038/s41586-019-1666-5'),",
    "#   ArXiv ID (e.g. 'ArXiv:2106.15928'), or S2 ID."
  ))
}
