


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
#' @param query Search query string (e.g. "CRISPR", "machine learning")
#' @param limit Maximum number of results (default 10, max 100)
#' @param fields Comma-separated fields to return (default "title,year,citationCount,authors")
#' @return tibble: paperId, title, year, citationCount, authors, url
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
#' @param paper_id Semantic Scholar paper ID, DOI, or ArXiv ID
#' @param fields Comma-separated fields (default includes abstract, venue)
#' @return tibble: one row with paperId, title, year, citationCount,
#'   referenceCount, abstract, venue, url, authors
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
#' @param query Author name to search
#' @param limit Maximum results (default 10, max 1000)
#' @return tibble: authorId, name, paperCount, citationCount, url
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


# == Context ===================================================================

#' Generate LLM-friendly context for api.semanticscholar.org
#'
#' @return Character string with full function signatures and bodies
#' @export
s2_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.semanticscholar.org.R"
  if (!file.exists(src_file)) {
    cat("# api.semanticscholar.org context - source not found\n")
    return(invisible("# api.semanticscholar.org context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

