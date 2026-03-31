#' Search Crossref for scholarly works (articles, books, etc.)
#'
#' @param query Search query string
#' @param rows Number of results. Default 10, max 1000.
#' @param offset Starting offset for pagination. Default 0.
#' @param filter Optional filter string (e.g., "type:journal-article")
#' @return tibble: doi, title, type, container_title, published_date,
#'   authors, publisher, reference_count, is_referenced_by_count, url
#' @export
cr_works <- function(query, rows = 10, offset = 0, filter = NULL) {
  url <- sprintf(
    "%s/works?query=%s&rows=%d&offset=%d",
    .cr_base, utils::URLencode(query, reserved = TRUE),
    as.integer(rows), as.integer(offset)
  )
  if (!is.null(filter)) url <- paste0(url, "&filter=", filter)

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_works)

  items <- raw$message$items
  .parse_works(items)
}

#' Search Crossref journals
#'
#' @param query Search query string (journal title)
#' @param rows Number of results. Default 10.
#' @return tibble: issn, title, publisher, subjects, total_dois
#' @export
cr_journals <- function(query, rows = 10) {
  url <- sprintf(
    "%s/journals?query=%s&rows=%d",
    .cr_base, utils::URLencode(query, reserved = TRUE), as.integer(rows)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_journals)

  items <- raw$message$items
  if (is.null(items) || length(items) == 0) return(.schema_journals)
  if (!is.data.frame(items)) return(.schema_journals)

  nms <- names(items)
  as_tibble(items) |>
    transmute(
      issn = as.character(if ("ISSN" %in% nms) sapply(ISSN, paste, collapse = "; ") else NA_character_),
      title = as.character(if ("title" %in% nms) title else NA_character_),
      publisher = as.character(if ("publisher" %in% nms) publisher else NA_character_),
      subjects = as.character(if ("subjects" %in% nms) sapply(subjects, function(x) {
        if (is.data.frame(x)) paste(x$name, collapse = "; ") else NA_character_
      }) else NA_character_),
      total_dois = as.integer(if ("counts" %in% nms) sapply(counts, function(x) x$`total-dois` %||% NA_integer_) else NA_integer_)
    )
}

#' Print Crossref context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
cr_context <- function() {
  .build_context(
    pkg_name = "api.crossref.org",
    header_lines = c(
      "# Package: api.crossref.org",
      "# Crossref REST API - scholarly metadata",
      "# Auth: none (polite pool via email in User-Agent)",
      "# Rate limits: ~50 req/sec in polite pool",
      "#",
      "# Covers: 150M+ works, 100K+ journals, DOI metadata",
      "# Filters: type:journal-article, from-pub-date:2020,",
      "#   has-full-text:true, is-referenced-by-count:>10"
    )
  )
}
