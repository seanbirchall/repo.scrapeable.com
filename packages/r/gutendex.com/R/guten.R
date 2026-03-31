# == Public functions ==========================================================

#' Search the Gutendex book catalog
#'
#' @param query Search term (title, author, etc.)
#' @param page Page number (default 1, 32 results per page)
#' @return tibble: id, title, authors, languages, download_count, subjects, bookshelves
#' @export
guten_search <- function(query, page = 1) {
  url <- sprintf("%s/books/?search=%s&page=%s",
                 .guten_base, utils::URLencode(query, reserved = TRUE),
                 as.integer(page))
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_books)

  as_tibble(data.frame(
    id             = as.integer(results$id),
    title          = as.character(results$title),
    authors        = vapply(results$authors, function(a) {
      if (is.null(a) || nrow(a) == 0) return(NA_character_)
      paste(a$name, collapse = "; ")
    }, character(1)),
    languages      = vapply(results$languages, function(l) paste(l, collapse = ", "), character(1)),
    download_count = as.integer(results$download_count),
    subjects       = vapply(results$subjects, function(s) {
      if (is.null(s) || length(s) == 0) return(NA_character_)
      paste(s[1:min(3, length(s))], collapse = "; ")
    }, character(1)),
    bookshelves    = vapply(results$bookshelves, function(b) {
      if (is.null(b) || length(b) == 0) return(NA_character_)
      paste(b[1:min(3, length(b))], collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  ))
}

#' Get details for a specific Gutenberg book
#'
#' @param id Gutenberg book ID (integer)
#' @return tibble with one row: id, title, authors, languages, download_count, subjects, bookshelves
#' @export
guten_book <- function(id) {
  url <- sprintf("%s/books/%s", .guten_base, as.integer(id))
  b <- .fetch_json(url)
  if (is.null(b) || is.null(b$id)) return(.schema_books)

  as_tibble(data.frame(
    id             = as.integer(b$id),
    title          = as.character(b$title),
    authors        = paste(b$authors$name, collapse = "; ") %||% NA_character_,
    languages      = paste(b$languages, collapse = ", "),
    download_count = as.integer(b$download_count),
    subjects       = paste(head(b$subjects, 3), collapse = "; ") %||% NA_character_,
    bookshelves    = paste(head(b$bookshelves, 3), collapse = "; ") %||% NA_character_,
    stringsAsFactors = FALSE
  ))
}

#' Show Gutendex package context for LLM use
#'
#' @return Invisible string with full context
#' @export
guten_context <- function() {
  .build_context("gutendex.com", header_lines = c(
    "# gutendex.com -- Project Gutenberg book catalog API",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Popular searches: shakespeare, dickens, austen, twain, darwin"
  ))
}
