# api.semanticscholar.org.R - Self-contained api.semanticscholar.org client



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
#' Queries the Semantic Scholar Academic Graph API for papers matching a
#' free-text search string. Returns core bibliographic metadata for each hit.
#'
#' @param query Character. Search query string (e.g. \code{"CRISPR"},
#'   \code{"machine learning transformers"}).
#' @param limit Integer. Maximum number of results to return (default 10,
#'   API max 100).
#' @param fields Character. Comma-separated API field names to request
#'   (default \code{"title,year,citationCount,authors,url"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{paperId}{Character. Semantic Scholar corpus ID.}
#'     \item{title}{Character. Paper title.}
#'     \item{year}{Integer. Publication year.}
#'     \item{citationCount}{Integer. Total citations recorded by S2.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{url}{Character. Semantic Scholar URL for the paper.}
#'   }
#' @examples
#' s2_papers("CRISPR gene editing", limit = 5)
#' s2_papers("large language models", limit = 20)
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

#' Fetch detailed metadata for a single Semantic Scholar paper
#'
#' Retrieves comprehensive metadata for one paper identified by its Semantic
#' Scholar corpus ID, a DOI (prefix with \code{"DOI:"}), or an ArXiv ID
#' (prefix with \code{"ARXIV:"}).
#'
#' @param paper_id Character. Paper identifier -- a Semantic Scholar ID,
#'   a DOI (e.g. \code{"DOI:10.1126/science.aad5227"}), or an ArXiv ID
#'   (e.g. \code{"ARXIV:2301.12345"}).
#' @param fields Character. Comma-separated API field names (default includes
#'   abstract, venue, referenceCount).
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{paperId}{Character. Semantic Scholar corpus ID.}
#'     \item{title}{Character. Paper title.}
#'     \item{year}{Integer. Publication year.}
#'     \item{citationCount}{Integer. Total citation count.}
#'     \item{referenceCount}{Integer. Number of references in the paper.}
#'     \item{abstract}{Character. Full abstract text (may be \code{NA}).}
#'     \item{venue}{Character. Publication venue (journal/conference).}
#'     \item{url}{Character. Semantic Scholar URL.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'   }
#' @examples
#' s2_paper("DOI:10.1126/science.aad5227")
#' s2_paper("ARXIV:1706.03762")
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

#' Search Semantic Scholar for authors
#'
#' Searches the Semantic Scholar author index by name. Returns basic
#' author-level statistics. Note: \code{paperCount} and \code{citationCount}
#' require an API key and may return \code{NA} on the free tier.
#'
#' @param query Character. Author name to search (e.g. \code{"Geoffrey Hinton"}).
#' @param limit Integer. Maximum results to return (default 10, API max 1000).
#' @return A tibble with columns:
#'   \describe{
#'     \item{authorId}{Character. Semantic Scholar author ID.}
#'     \item{name}{Character. Author display name.}
#'     \item{paperCount}{Integer. Number of indexed papers (\code{NA} without API key).}
#'     \item{citationCount}{Integer. Total citations (\code{NA} without API key).}
#'     \item{url}{Character. Semantic Scholar author page URL (\code{NA} without API key).}
#'   }
#' @examples
#' s2_authors("Geoffrey Hinton", limit = 5)
#' s2_authors("Yoshua Bengio")
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


#' Get papers that cite a given paper
#'
#' Returns bibliographic metadata for papers that cite the specified paper.
#' Useful for forward-citation tracking and impact analysis.
#'
#' @param paper_id Character. Paper identifier -- a Semantic Scholar ID,
#'   DOI (e.g. \code{"DOI:10.1126/science.aad5227"}), or ArXiv ID.
#' @param limit Integer. Maximum number of citing papers to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{paperId}{Character. Citing paper's Semantic Scholar ID.}
#'     \item{title}{Character. Citing paper's title.}
#'     \item{year}{Integer. Publication year.}
#'     \item{citationCount}{Integer. How many citations the citing paper has.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{url}{Character. Always \code{NA} (not returned by this endpoint).}
#'   }
#' @examples
#' s2_citations("DOI:10.1126/science.aad5227", limit = 10)
#' @export
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

#' Get references cited by a given paper
#'
#' Returns bibliographic metadata for papers in the reference list of the
#' specified paper. Useful for backward-citation analysis and literature mapping.
#'
#' @param paper_id Character. Paper identifier -- a Semantic Scholar ID,
#'   DOI (e.g. \code{"DOI:10.1126/science.aad5227"}), or ArXiv ID.
#' @param limit Integer. Maximum number of references to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{paperId}{Character. Referenced paper's Semantic Scholar ID.}
#'     \item{title}{Character. Referenced paper's title.}
#'     \item{year}{Integer. Publication year.}
#'     \item{citationCount}{Integer. How many citations the referenced paper has.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{url}{Character. Always \code{NA} (not returned by this endpoint).}
#'   }
#' @examples
#' s2_references("DOI:10.1126/science.aad5227", limit = 10)
#' @export
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

#' Get semanticscholar.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/semanticscholar.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "semanticscholar.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# semanticscholar.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# semanticscholar.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
