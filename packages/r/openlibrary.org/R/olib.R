
# == Search ====================================================================

#' Search Open Library for books
#'
#' @param query Search query (e.g. "lord of the rings", "machine learning")
#' @param limit Number of results (default 20, max 100)
#' @return tibble: key, title, author_name, first_publish_year, publisher,
#'   isbn, subject, edition_count
#' @export
olib_search <- function(query, limit = 20) {
  url <- sprintf("%s/search.json?q=%s&limit=%d",
                 .olib_base, utils::URLencode(query), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("Open Library search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_search)

  docs <- raw$docs
  if (is.null(docs) || length(docs) == 0) return(.schema_search)

  tibble(
    key                = as.character(docs$key %||% NA_character_),
    title              = as.character(docs$title %||% NA_character_),
    author_name        = vapply(docs$author_name %||% replicate(nrow(docs), NULL), function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
    }, character(1)),
    first_publish_year = as.integer(docs$first_publish_year %||% NA_integer_),
    publisher          = vapply(docs$publisher %||% replicate(nrow(docs), NULL), function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(head(x, 3), collapse = "; ")
    }, character(1)),
    isbn               = vapply(docs$isbn %||% replicate(nrow(docs), NULL), function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else x[1]
    }, character(1)),
    subject            = vapply(docs$subject %||% replicate(nrow(docs), NULL), function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(head(x, 3), collapse = "; ")
    }, character(1)),
    edition_count      = as.integer(docs$edition_count %||% NA_integer_)
  )
}

# == Book by ISBN ==============================================================

#' Get book details by ISBN from Open Library
#'
#' @param isbn ISBN-10 or ISBN-13 (e.g. "9780140328721", "0140328726")
#' @return tibble: title, authors, publishers, publish_date,
#'   number_of_pages, isbn_13, isbn_10, subjects, cover_url
#' @export
olib_book <- function(isbn) {
  bibkey <- paste0("ISBN:", isbn)
  url <- sprintf("%s/api/books?bibkeys=%s&format=json&jscmd=data",
                 .olib_base, utils::URLencode(bibkey))
  raw <- tryCatch(
    jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE),
    error = function(e) {
      message("Open Library book fetch failed: ", e$message)
      return(NULL)
    }
  )
  if (is.null(raw) || length(raw) == 0) return(.schema_book)

  book <- raw[[1]]
  if (is.null(book)) return(.schema_book)

  authors <- if (!is.null(book$authors) && length(book$authors) > 0) {
    paste(vapply(book$authors, function(a) a$name %||% "", character(1)), collapse = "; ")
  } else NA_character_

  publishers <- if (!is.null(book$publishers) && length(book$publishers) > 0) {
    paste(vapply(book$publishers, function(p) p$name %||% "", character(1)), collapse = "; ")
  } else NA_character_

  subjects <- if (!is.null(book$subjects) && length(book$subjects) > 0) {
    paste(head(vapply(book$subjects, function(s) s$name %||% "", character(1)), 5), collapse = "; ")
  } else NA_character_

  isbn13 <- if (!is.null(book$identifiers$isbn_13) && length(book$identifiers$isbn_13) > 0) book$identifiers$isbn_13[[1]] else NA_character_
  isbn10 <- if (!is.null(book$identifiers$isbn_10) && length(book$identifiers$isbn_10) > 0) book$identifiers$isbn_10[[1]] else NA_character_

  cover <- if (!is.null(book$cover)) book$cover$large %||% book$cover$medium %||% NA_character_ else NA_character_

  tibble(
    title           = as.character(book$title %||% NA_character_),
    authors         = authors,
    publishers      = publishers,
    publish_date    = as.character(book$publish_date %||% NA_character_),
    number_of_pages = as.integer(book$number_of_pages %||% NA_integer_),
    isbn_13         = as.character(isbn13),
    isbn_10         = as.character(isbn10),
    subjects        = subjects,
    cover_url       = as.character(cover)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the openlibrary.org package
#'
#' @return Character string (invisibly), also printed
#' @export
olib_context <- function() {
  .build_context("openlibrary.org", header_lines = c(
    "# openlibrary.org - Open Library API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Endpoints: book search and ISBN lookup",
    "# All functions return tibbles with typed columns."
  ))
}
