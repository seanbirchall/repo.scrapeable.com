# == Schemas ===================================================================

.schema_publications <- tibble(
  key = character(), title = character(), venue = character(),
  year = integer(), type = character(), doi = character(),
  url = character(), authors = character()
)

.schema_authors <- tibble(
  name = character(), pid = character(), url = character(), notes = character()
)

# == Public functions ==========================================================

#' Search DBLP publications
#'
#' @param query Search query string
#' @param hits Maximum results to return (default 30, max 1000)
#' @param offset Starting position (default 0)
#' @return tibble: key, title, venue, year, type, doi, url, authors
#' @export
dblp_publications <- function(query, hits = 30, offset = 0) {
  url <- sprintf("%s/search/publ/api?q=%s&format=json&h=%d&f=%d",
                 .dblp_base, utils::URLencode(query), hits, offset)
  raw <- .fetch_json(url)
  hit_list <- raw$result$hits$hit
  if (is.null(hit_list) || length(hit_list) == 0) return(.schema_publications)

  rows <- lapply(hit_list, function(h) {
    info <- h$info
    authors_raw <- info$authors$author
    if (is.null(authors_raw)) {
      auth_str <- NA_character_
    } else if (is.list(authors_raw) && !is.null(authors_raw$text)) {
      auth_str <- authors_raw$text
    } else {
      auth_str <- paste(vapply(authors_raw, function(a) a$text %||% NA_character_, character(1)), collapse = "; ")
    }
    tibble(
      key = as.character(info$key %||% NA),
      title = as.character(info$title %||% NA),
      venue = as.character(info$venue %||% NA),
      year = as.integer(info$year %||% NA),
      type = as.character(info$type %||% NA),
      doi = as.character(info$doi %||% NA),
      url = as.character(info$url %||% NA),
      authors = auth_str
    )
  })
  bind_rows(rows)
}

#' Search DBLP authors
#'
#' @param query Author name search query
#' @param hits Maximum results (default 30)
#' @return tibble: name, pid, url, notes
#' @export
dblp_authors <- function(query, hits = 30) {
  url <- sprintf("%s/search/author/api?q=%s&format=json&h=%d",
                 .dblp_base, utils::URLencode(query), hits)
  raw <- .fetch_json(url)
  hit_list <- raw$result$hits$hit
  if (is.null(hit_list) || length(hit_list) == 0) return(.schema_authors)

  rows <- lapply(hit_list, function(h) {
    info <- h$info
    tibble(
      name = as.character(info$author %||% NA),
      pid = as.character(info$url %||% NA),
      url = as.character(h$url %||% NA),
      notes = as.character(info$notes$note$text %||% NA)
    )
  })
  bind_rows(rows)
}

#' Search DBLP venues (conferences/journals)
#'
#' @param query Venue name search query
#' @param hits Maximum results (default 30)
#' @return tibble: name, acronym, type, url
#' @export
dblp_venues <- function(query, hits = 30) {
  url <- sprintf("%s/search/venue/api?q=%s&format=json&h=%d",
                 .dblp_base, utils::URLencode(query), hits)
  raw <- .fetch_json(url)
  hit_list <- raw$result$hits$hit
  if (is.null(hit_list) || length(hit_list) == 0) {
    return(tibble(name = character(), acronym = character(),
                  type = character(), url = character()))
  }
  rows <- lapply(hit_list, function(h) {
    info <- h$info
    tibble(
      name = as.character(info$venue %||% NA),
      acronym = as.character(info$acronym %||% NA),
      type = as.character(info$type %||% NA),
      url = as.character(info$url %||% NA)
    )
  })
  bind_rows(rows)
}

#' DBLP package context for LLM integration
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
dblp_context <- function() {
  .build_context("dblp.org", header_lines = c(
    "# dblp.org - DBLP Computer Science Bibliography Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Common publication types: Conference and Workshop Papers, Journal Articles",
    "# Rate limits: be polite"
  ))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
