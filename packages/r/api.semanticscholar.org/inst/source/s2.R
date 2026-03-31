# semanticscholar.R
# Self-contained Semantic Scholar API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (rate-limited to ~100 req/5min without key)
# Rate limits: ~100 requests per 5 minutes without API key.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.s2_base <- "https://api.semanticscholar.org/graph/v1"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_papers <- tibble(
  paperId = character(), title = character(), year = integer(),
  citationCount = integer(), authors = character(), url = character()
)

.schema_paper_detail <- tibble(
  paperId = character(), title = character(), year = integer(),
  citationCount = integer(), referenceCount = integer(),
  abstract = character(), venue = character(), url = character(),
  authors = character()
)

.schema_authors <- tibble(
  authorId = character(), name = character(), paperCount = integer(),
  citationCount = integer(), url = character()
)

# == Paper search ==============================================================

#' Search Semantic Scholar papers
#'
#' @param query Search query string (e.g. "CRISPR", "machine learning")
#' @param limit Maximum number of results (default 10, max 100)
#' @param fields Comma-separated fields to return (default "title,year,citationCount,authors")
#' @return tibble: paperId, title, year, citationCount, authors, url
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
