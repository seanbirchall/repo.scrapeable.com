# gutenberg.org.R - Self-contained gutenberg.org client




.ua <- "support@scrapeable.com"
.base <- "https://gutendex.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


#' Search Project Gutenberg books
#'
#' Queries the Gutendex API to search the Project Gutenberg catalog of
#' over 70,000 free ebooks by title, author, or subject.
#'
#' @param query Search term (e.g. "shakespeare", "pride and prejudice",
#'   "science fiction")
#' @param page Page number for pagination (default 1, 32 results per page)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Gutenberg book ID (integer)}
#'     \item{title}{Book title (character)}
#'     \item{authors}{Semicolon-separated author names (character)}
#'     \item{languages}{Comma-separated ISO language codes (character)}
#'     \item{download_count}{Total downloads from Project Gutenberg (integer)}
#'   }
#' @examples
#' guten_search("shakespeare")
#' guten_search("science fiction", page = 2)
#' @seealso [guten_book()], [guten_context()]
#' @source <https://gutendex.com>
#' @export
guten_search <- function(query, page = 1L) {
  url <- sprintf("%s/books?search=%s&page=%d", .base,
                 utils::URLencode(query, reserved = TRUE), page)
  raw <- .fetch_json(url)
  results <- raw$results
  if (length(results) == 0) return(tibble::tibble(id = integer(), title = character(), authors = character()))
  tibble::tibble(
    id = vapply(results, function(x) x$id %||% NA_integer_, integer(1)),
    title = vapply(results, function(x) x$title %||% NA_character_, character(1)),
    authors = vapply(results, function(x) {
      if (length(x$authors) > 0) paste(vapply(x$authors, function(a) a$name %||% "", character(1)), collapse = "; ")
      else NA_character_
    }, character(1)),
    languages = vapply(results, function(x) paste(unlist(x$languages), collapse = ", "), character(1)),
    download_count = vapply(results, function(x) x$download_count %||% NA_integer_, integer(1))
  )
}

#' Get a specific Gutenberg book by ID
#'
#' Fetches full metadata for a single Project Gutenberg book,
#' including subjects and download count.
#'
#' @param book_id Gutenberg book ID (integer, e.g. 84 for Frankenstein,
#'   1342 for Pride and Prejudice)
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Gutenberg book ID (integer)}
#'     \item{title}{Book title (character)}
#'     \item{authors}{Semicolon-separated author names (character)}
#'     \item{subjects}{Semicolon-separated Library of Congress subjects (character)}
#'     \item{languages}{Comma-separated ISO language codes (character)}
#'     \item{download_count}{Total downloads (integer)}
#'   }
#' @examples
#' guten_book(84)
#' guten_book(1342)
#' @seealso [guten_search()], [guten_context()]
#' @source <https://gutendex.com>
#' @export
guten_book <- function(book_id) {
  url <- sprintf("%s/books/%d", .base, as.integer(book_id))
  raw <- .fetch_json(url)
  tibble::tibble(
    id = raw$id %||% NA_integer_,
    title = raw$title %||% NA_character_,
    authors = if (length(raw$authors) > 0) paste(vapply(raw$authors, function(a) a$name, character(1)), collapse = "; ") else NA_character_,
    subjects = if (length(raw$subjects) > 0) paste(unlist(raw$subjects), collapse = "; ") else NA_character_,
    languages = paste(unlist(raw$languages), collapse = ", "),
    download_count = raw$download_count %||% NA_integer_
  )
}

# == Context ===================================================================

#' Get gutenberg.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
guten_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(guten_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gutenberg.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gutenberg.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gutenberg.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gutenberg.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
