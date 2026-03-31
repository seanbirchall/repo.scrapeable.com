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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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
