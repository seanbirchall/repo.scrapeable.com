# semanticscholar.R
# Self-contained Semantic Scholar API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (rate-limited to ~100 req/5min without key)
# Rate limits: ~100 requests per 5 minutes without API key.


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
#' Search the Semantic Scholar academic paper database by keywords. Returns
#' paper metadata including citation counts and author lists. Rate-limited to
#' ~100 requests per 5 minutes without an API key.
#'
#' @param query Search query string (e.g. \code{"CRISPR"}, \code{"machine learning"},
#'   \code{"transformer architecture"}).
#' @param limit Maximum number of results (default 10, max 100).
#' @param fields Comma-separated API fields to return (default
#'   \code{"title,year,citationCount,authors,url"}).
#' @return A tibble with one row per paper:
#'   \describe{
#'     \item{paperId}{\code{character} -- Semantic Scholar paper ID (40-char hex).}
#'     \item{title}{\code{character} -- Paper title.}
#'     \item{year}{\code{integer} -- Publication year.}
#'     \item{citationCount}{\code{integer} -- Number of citations.}
#'     \item{authors}{\code{character} -- Author names, semicolon-separated.}
#'     \item{url}{\code{character} -- Semantic Scholar URL.}
#'   }
#' @examples
#' \dontrun{
#' s2_papers("CRISPR", limit = 20)
#' s2_papers("transformer architecture", limit = 5)
#' }
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
#' Retrieve full metadata for one paper, including abstract, venue, and
#' reference count. Accepts Semantic Scholar IDs, DOIs, and ArXiv IDs.
#'
#' @param paper_id Paper identifier. Accepts a Semantic Scholar paper ID
#'   (40-char hex), DOI (e.g. \code{"10.1038/s41586-020-2649-2"}), or ArXiv
#'   ID prefixed with \code{"ARXIV:"} (e.g. \code{"ARXIV:2301.12345"}).
#' @param fields Comma-separated API fields (default includes abstract, venue,
#'   authors, referenceCount).
#' @return A single-row tibble:
#'   \describe{
#'     \item{paperId}{\code{character} -- Semantic Scholar paper ID.}
#'     \item{title}{\code{character} -- Paper title.}
#'     \item{year}{\code{integer} -- Publication year.}
#'     \item{citationCount}{\code{integer} -- Number of citations.}
#'     \item{referenceCount}{\code{integer} -- Number of references.}
#'     \item{abstract}{\code{character} -- Paper abstract.}
#'     \item{venue}{\code{character} -- Publication venue.}
#'     \item{url}{\code{character} -- Semantic Scholar URL.}
#'     \item{authors}{\code{character} -- Author names, semicolon-separated.}
#'   }
#' @examples
#' \dontrun{
#' s2_paper("10.1038/s41586-020-2649-2")
#' s2_paper("ARXIV:2301.12345")
#' }
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
#' Search for academic authors by name. Returns paper counts and citation
#' metrics for each matching author.
#'
#' @param query Author name to search (e.g. \code{"Geoffrey Hinton"},
#'   \code{"Yoshua Bengio"}).
#' @param limit Maximum number of results (default 10, max 1000).
#' @return A tibble with one row per author:
#'   \describe{
#'     \item{authorId}{\code{character} -- Semantic Scholar author ID.}
#'     \item{name}{\code{character} -- Author name.}
#'     \item{paperCount}{\code{integer} -- Number of papers.}
#'     \item{citationCount}{\code{integer} -- Total citation count.}
#'     \item{url}{\code{character} -- Semantic Scholar profile URL.}
#'   }
#' @examples
#' \dontrun{
#' s2_authors("Geoffrey Hinton")
#' s2_authors("Bengio", limit = 5)
#' }
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

#' Get Semantic Scholar client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' s2_context()
#' }
#' @export
s2_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(s2_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/semanticscholar.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "semanticscholar")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# semanticscholar context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# semanticscholar", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
