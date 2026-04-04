# api.semanticscholar.org.R - Self-contained api.semanticscholar.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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
#' Searches the Semantic Scholar academic paper corpus by keyword. This is the
#' primary discovery function for finding research papers. Returns core
#' metadata including citation counts. Use \code{\link{s2_paper}} to get the
#' full abstract and venue for a specific paper, or \code{\link{s2_citations}}
#' and \code{\link{s2_references}} to explore the citation graph.
#'
#' @param query Character. Search query string. Supports natural language
#'   queries and boolean operators. Examples: \code{"CRISPR"},
#'   \code{"machine learning"}, \code{"deep reinforcement learning"}.
#' @param limit Integer. Maximum number of results to return (default 10,
#'   API max 100).
#' @param fields Character. Comma-separated list of fields to request from the
#'   API. Default: \code{"title,year,citationCount,authors,url"}. Other
#'   available fields include \code{"abstract"}, \code{"venue"},
#'   \code{"referenceCount"}, \code{"externalIds"}, \code{"publicationDate"}.
#'
#' @return A tibble with one row per paper and the following columns:
#' \describe{
#'   \item{paperId}{Character. Semantic Scholar paper ID (40-char hex hash).}
#'   \item{title}{Character. Paper title.}
#'   \item{year}{Integer. Publication year.}
#'   \item{citationCount}{Integer. Number of citations.}
#'   \item{authors}{Character. Author names, semicolon-separated.}
#'   \item{url}{Character. Semantic Scholar URL for the paper.}
#' }
#'
#' @examples
#' \dontrun{
#' # Search for papers on CRISPR
#' s2_papers("CRISPR gene editing", limit = 20)
#'
#' # Search for machine learning papers
#' s2_papers("transformer attention mechanism", limit = 10)
#' }
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
#' Retrieves detailed metadata for a single paper including the abstract and
#' venue, which are not included in \code{\link{s2_papers}} search results by
#' default. Accepts multiple ID formats. Use this after discovering papers via
#' \code{s2_papers()} to get the full abstract.
#'
#' @param paper_id Character. Paper identifier in any of these formats:
#'   \itemize{
#'     \item Semantic Scholar ID: \code{"649def34f8be52c8b66281af98ae884c09aef38b"}
#'     \item DOI: \code{"DOI:10.1038/nbt.3437"}
#'     \item ArXiv ID: \code{"ARXIV:2103.15348"}
#'     \item PubMed ID: \code{"PMID:12345678"}
#'     \item Corpus ID: \code{"CorpusID:12345678"}
#'   }
#' @param fields Character. Comma-separated fields to request. Default:
#'   \code{"title,year,citationCount,referenceCount,abstract,venue,authors,url"}.
#'
#' @return A tibble with one row and the following columns:
#' \describe{
#'   \item{paperId}{Character. Semantic Scholar paper ID.}
#'   \item{title}{Character. Paper title.}
#'   \item{year}{Integer. Publication year.}
#'   \item{citationCount}{Integer. Number of citations.}
#'   \item{referenceCount}{Integer. Number of references.}
#'   \item{abstract}{Character. Full paper abstract.}
#'   \item{venue}{Character. Publication venue/journal name.}
#'   \item{url}{Character. Semantic Scholar URL.}
#'   \item{authors}{Character. Author names, semicolon-separated.}
#' }
#'
#' @examples
#' \dontrun{
#' # Fetch by Semantic Scholar ID
#' s2_paper("649def34f8be52c8b66281af98ae884c09aef38b")
#'
#' # Fetch by DOI
#' s2_paper("DOI:10.1038/nbt.3437")
#'
#' # Fetch by ArXiv ID
#' s2_paper("ARXIV:2103.15348")
#' }
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
#' Searches for academic authors by name in the Semantic Scholar database.
#' Returns basic profile information including publication and citation counts.
#' Use this to find researcher profiles and their Semantic Scholar author IDs.
#'
#' @param query Character. Author name to search for. Example: \code{"Turing"},
#'   \code{"Yoshua Bengio"}, \code{"Jennifer Doudna"}.
#' @param limit Integer. Maximum number of results to return (default 10,
#'   API max 1000).
#'
#' @return A tibble with one row per author and the following columns:
#' \describe{
#'   \item{authorId}{Character. Semantic Scholar author ID.}
#'   \item{name}{Character. Author's full name.}
#'   \item{paperCount}{Integer. Total number of papers by this author.}
#'   \item{citationCount}{Integer. Total number of citations received.}
#'   \item{url}{Character. Semantic Scholar profile URL.}
#' }
#'
#' @examples
#' \dontrun{
#' # Search for an author
#' s2_authors("Yoshua Bengio", limit = 5)
#'
#' # Find all authors named "Turing"
#' s2_authors("Turing", limit = 10)
#' }
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


#' Get citations for a paper
#'
#' Returns the papers that cite a given paper. Use this to trace the impact
#' and downstream influence of a publication. Accepts the same ID formats as
#' \code{\link{s2_paper}}.
#'
#' @param paper_id Character. Paper identifier (Semantic Scholar ID, DOI,
#'   ArXiv ID, PMID, or Corpus ID). See \code{\link{s2_paper}} for format
#'   details.
#' @param limit Integer. Maximum number of citing papers to return (default 20,
#'   API max 1000).
#'
#' @return A tibble with one row per citing paper and the following columns:
#' \describe{
#'   \item{paperId}{Character. Semantic Scholar paper ID of the citing paper.}
#'   \item{title}{Character. Title of the citing paper.}
#'   \item{year}{Integer. Publication year of the citing paper.}
#'   \item{citationCount}{Integer. Citation count of the citing paper.}
#'   \item{authors}{Character. Author names, semicolon-separated.}
#'   \item{url}{Character. \code{NA} (not returned by the citations endpoint).}
#' }
#'
#' @examples
#' \dontrun{
#' # Get papers that cite the "Attention Is All You Need" paper
#' s2_citations("649def34f8be52c8b66281af98ae884c09aef38b", limit = 10)
#'
#' # Get citations for a paper by DOI
#' s2_citations("DOI:10.1038/nbt.3437", limit = 20)
#' }
s2_citations <- function(paper_id, limit = 20) {
  url <- sprintf("%s/paper/%s/citations?fields=title,year,citationCount,authors&limit=%d",
                 .s2_base, utils::URLencode(paper_id), limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(.schema_papers)
  d <- raw$data
  if (length(d) == 0) return(.schema_papers)

  citing <- if (is.data.frame(d)) {
    d$citingPaper
  } else {
    lapply(d, function(x) x$citingPaper)
  }

  if (is.data.frame(citing)) {
    if ("authors" %in% names(citing) && is.list(citing$authors)) {
      citing$authors <- vapply(citing$authors, function(a) {
        if (is.data.frame(a) && "name" %in% names(a)) paste(a$name, collapse = "; ")
        else NA_character_
      }, character(1))
    }
    as_tibble(citing) |>
      transmute(
        paperId = as.character(paperId),
        title = as.character(title),
        year = as.integer(if ("year" %in% names(citing)) year else NA_integer_),
        citationCount = as.integer(if ("citationCount" %in% names(citing)) citationCount else NA_integer_),
        authors = as.character(if ("authors" %in% names(citing)) authors else NA_character_),
        url = NA_character_
      )
  } else {
    .schema_papers
  }
}

#' Get references from a paper
#'
#' Returns the papers cited by (referenced in) a given paper. Use this to
#' explore the foundational literature and prior work behind a publication.
#' Accepts the same ID formats as \code{\link{s2_paper}}.
#'
#' @param paper_id Character. Paper identifier (Semantic Scholar ID, DOI,
#'   ArXiv ID, PMID, or Corpus ID). See \code{\link{s2_paper}} for format
#'   details.
#' @param limit Integer. Maximum number of referenced papers to return
#'   (default 20, API max 1000).
#'
#' @return A tibble with one row per referenced paper and the following columns:
#' \describe{
#'   \item{paperId}{Character. Semantic Scholar paper ID of the referenced paper.}
#'   \item{title}{Character. Title of the referenced paper.}
#'   \item{year}{Integer. Publication year of the referenced paper.}
#'   \item{citationCount}{Integer. Citation count of the referenced paper.}
#'   \item{authors}{Character. Author names, semicolon-separated.}
#'   \item{url}{Character. \code{NA} (not returned by the references endpoint).}
#' }
#'
#' @examples
#' \dontrun{
#' # Get papers referenced by a specific paper
#' s2_references("649def34f8be52c8b66281af98ae884c09aef38b", limit = 10)
#'
#' # Get references for a paper by DOI
#' s2_references("DOI:10.1038/nbt.3437", limit = 20)
#' }
s2_references <- function(paper_id, limit = 20) {
  url <- sprintf("%s/paper/%s/references?fields=title,year,citationCount,authors&limit=%d",
                 .s2_base, utils::URLencode(paper_id), limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(.schema_papers)
  d <- raw$data
  if (length(d) == 0) return(.schema_papers)

  cited <- if (is.data.frame(d)) {
    d$citedPaper
  } else {
    lapply(d, function(x) x$citedPaper)
  }

  if (is.data.frame(cited)) {
    if ("authors" %in% names(cited) && is.list(cited$authors)) {
      cited$authors <- vapply(cited$authors, function(a) {
        if (is.data.frame(a) && "name" %in% names(a)) paste(a$name, collapse = "; ")
        else NA_character_
      }, character(1))
    }
    as_tibble(cited) |>
      transmute(
        paperId = as.character(paperId),
        title = as.character(title),
        year = as.integer(if ("year" %in% names(cited)) year else NA_integer_),
        citationCount = as.integer(if ("citationCount" %in% names(cited)) citationCount else NA_integer_),
        authors = as.character(if ("authors" %in% names(cited)) authors else NA_character_),
        url = NA_character_
      )
  } else {
    .schema_papers
  }
}

# == Context ===================================================================

#' Get api.semanticscholar.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
s2_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(s2_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.semanticscholar.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.semanticscholar.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.semanticscholar.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.semanticscholar.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
