# gutendex-com.R
# Self-contained Gutendex API client (Project Gutenberg book catalog).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.guten_base <- "https://gutendex.com"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_books <- tibble(
  id = integer(), title = character(), authors = character(),
  languages = character(), download_count = integer(),
  subjects = character(), bookshelves = character()
)

# == Public functions ==========================================================

#' Search the Gutendex book catalog
#'
#' Searches the Project Gutenberg book catalog via the Gutendex API.
#' Matches against titles, authors, and subjects. Returns 32 results
#' per page.
#'
#' @param query Character. Search term matching title, author, or subject
#'   (e.g. \code{"shakespeare"}, \code{"pride and prejudice"},
#'   \code{"science fiction"}).
#' @param page Integer. Page number (default 1). Each page returns up to
#'   32 results.
#' @return A tibble with columns: \code{id} (integer, Gutenberg book ID),
#'   \code{title} (character), \code{authors} (character, semicolon-separated),
#'   \code{languages} (character, comma-separated ISO codes e.g. \code{"en"}),
#'   \code{download_count} (integer), \code{subjects} (character, up to 3,
#'   semicolon-separated), \code{bookshelves} (character, up to 3,
#'   semicolon-separated).
#' @examples
#' \dontrun{
#' guten_search("shakespeare")
#' guten_search("science fiction", page = 2)
#' }
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
#' Retrieves full metadata for a single Project Gutenberg book by its
#' numeric ID.
#'
#' @param id Integer. Gutenberg book ID (e.g. \code{1342} for Pride and
#'   Prejudice, \code{84} for Frankenstein, \code{1513} for Romeo and Juliet).
#' @return A tibble with one row and columns: \code{id} (integer),
#'   \code{title} (character), \code{authors} (character, semicolon-separated),
#'   \code{languages} (character), \code{download_count} (integer),
#'   \code{subjects} (character, up to 3), \code{bookshelves} (character,
#'   up to 3).
#' @examples
#' \dontrun{
#' guten_book(1342)
#' guten_book(84)
#' }
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

#' Get gutendex-com client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gutendex-com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gutendex-com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gutendex-com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gutendex-com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
