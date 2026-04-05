# openlibrary.org.R - Self-contained openlibrary.org client



# openlibrary-org.R
# Self-contained Open Library API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.olib_base <- "https://openlibrary.org"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  key = character(), title = character(), author_name = character(),
  first_publish_year = integer(), publisher = character(),
  isbn = character(), subject = character(), edition_count = integer()
)

.schema_book <- tibble(
  title = character(), authors = character(), publishers = character(),
  publish_date = character(), number_of_pages = integer(),
  isbn_13 = character(), isbn_10 = character(),
  subjects = character(), cover_url = character()
)


# == Search ====================================================================

#' Search Open Library for books
#'
#' Full-text search across the Open Library catalog. Returns books matching
#' the query with metadata including authors, publishers, and ISBN.
#'
#' @param query Character. Search query (e.g., \code{"lord of the rings"},
#'   \code{"machine learning"}, \code{"978-0"} for ISBN prefix).
#' @param limit Integer. Number of results to return (default 20, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{character -- Open Library work key (e.g., \code{"/works/OL27448W"})}
#'     \item{title}{character -- book title}
#'     \item{author_name}{character -- semicolon-separated author names}
#'     \item{first_publish_year}{integer -- earliest known publication year}
#'     \item{publisher}{character -- semicolon-separated publisher names (up to 3)}
#'     \item{isbn}{character -- first available ISBN}
#'     \item{subject}{character -- semicolon-separated subjects (up to 3)}
#'     \item{edition_count}{integer -- number of known editions}
#'   }
#' @export
#' @examples
#' \dontrun{
#' olib_search("machine learning", limit = 5)
#' }
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
#' Fetches detailed metadata for a single book edition identified by its
#' ISBN, including page count, cover image URL, and subject headings.
#'
#' @param isbn Character. ISBN-10 or ISBN-13 (e.g., \code{"9780140328721"},
#'   \code{"0140328726"}). Hyphens are not required.
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{title}{character -- book title}
#'     \item{authors}{character -- semicolon-separated author names}
#'     \item{publishers}{character -- semicolon-separated publisher names}
#'     \item{publish_date}{character -- publication date string}
#'     \item{number_of_pages}{integer -- page count}
#'     \item{isbn_13}{character -- ISBN-13}
#'     \item{isbn_10}{character -- ISBN-10}
#'     \item{subjects}{character -- semicolon-separated subjects (up to 5)}
#'     \item{cover_url}{character -- URL to large cover image}
#'   }
#' @export
#' @examples
#' \dontrun{
#' olib_book("9780140328721")
#' }
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

#' Search Open Library by author name
#'
#' Finds works written by a specific author. Uses the Open Library search
#' API with an author filter.
#'
#' @param author Character. Author name (e.g., \code{"Tolkien"},
#'   \code{"Isaac Asimov"}, \code{"Ursula K. Le Guin"}).
#' @param limit Integer. Number of results to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{character -- Open Library work key}
#'     \item{title}{character -- book title}
#'     \item{author_name}{character -- semicolon-separated author names}
#'     \item{first_publish_year}{integer -- earliest known publication year}
#'     \item{publisher}{character -- semicolon-separated publisher names (up to 3)}
#'     \item{isbn}{character -- first available ISBN}
#'     \item{subject}{character -- semicolon-separated subjects (up to 3)}
#'     \item{edition_count}{integer -- number of known editions}
#'   }
#' @export
#' @examples
#' \dontrun{
#' olib_author("Tolkien", limit = 5)
#' }
olib_author <- function(author, limit = 20) {
  url <- sprintf("%s/search.json?author=%s&limit=%d",
                 .olib_base, utils::URLencode(author), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$docs) || length(raw$docs) == 0) return(.schema_search)

  docs <- raw$docs
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

#' Search Open Library by subject
#'
#' Browse works in a specific subject category. Uses the Open Library
#' subjects endpoint, which returns curated lists of works.
#'
#' @param subject Character. Subject name (e.g., \code{"science fiction"},
#'   \code{"history"}, \code{"cooking"}, \code{"mathematics"}).
#'   Spaces are converted to underscores automatically.
#' @param limit Integer. Number of results to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{character -- Open Library work key}
#'     \item{title}{character -- book title}
#'     \item{author_name}{character -- semicolon-separated author names}
#'     \item{first_publish_year}{integer -- earliest known publication year}
#'   }
#' @export
#' @examples
#' \dontrun{
#' olib_subject("science fiction", limit = 10)
#' }
olib_subject <- function(subject, limit = 20) {
  url <- sprintf("%s/subjects/%s.json?limit=%d",
                 .olib_base, utils::URLencode(tolower(gsub(" ", "_", subject))),
                 as.integer(limit))
  raw <- tryCatch(
    jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw)) return(tibble(key = character(), title = character(),
                                   author_name = character(), first_publish_year = integer()))

  works <- raw$works
  if (is.null(works) || length(works) == 0) {
    return(tibble(key = character(), title = character(),
                  author_name = character(), first_publish_year = integer()))
  }

  tibble(
    key = vapply(works, function(w) w$key %||% NA_character_, character(1)),
    title = vapply(works, function(w) w$title %||% NA_character_, character(1)),
    author_name = vapply(works, function(w) {
      if (!is.null(w$authors)) paste(vapply(w$authors, function(a) a$name %||% "", character(1)), collapse = "; ")
      else NA_character_
    }, character(1)),
    first_publish_year = vapply(works, function(w) {
      as.integer(w$first_publish_year %||% NA_integer_)
    }, integer(1))
  )
}

# == Context ===================================================================

#' Get openlibrary.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
olib_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(olib_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/openlibrary.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "openlibrary.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# openlibrary.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# openlibrary.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
