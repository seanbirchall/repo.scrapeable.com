# == Search ====================================================================

#' Search ERIC education research database
#'
#' @param query Search query (e.g. "mathematics instruction", "STEM education")
#' @param rows Number of results (default 10, max 200)
#' @param start Starting record offset for pagination (default 0)
#' @return tibble: id, title, author, source, publicationdate,
#'   description, subject, url
#' @export
eric_search <- function(query, rows = 10, start = 0) {
  url <- paste0(.eric_base, "?search=", utils::URLencode(query),
                "&rows=", rows, "&start=", start, "&format=json")
  raw <- .fetch_json(url)
  d <- raw$response$docs
  if (is.null(d) || length(d) == 0) return(.schema_search)

  # author and subject may be list columns
  .collapse_col <- function(x) {
    if (is.list(x)) vapply(x, function(v) paste(v, collapse = "; "), character(1))
    else as.character(x)
  }

  as_tibble(d) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      author = .collapse_col(if ("author" %in% names(d)) author else NA_character_),
      source = as.character(if ("source" %in% names(d)) source else NA_character_),
      publicationdate = as.character(if ("publicationdate" %in% names(d)) publicationdate else NA_character_),
      description = as.character(if ("description" %in% names(d)) description else NA_character_),
      subject = .collapse_col(if ("subject" %in% names(d)) subject else NA_character_),
      url = paste0("https://eric.ed.gov/?id=", id)
    )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the ERIC package
#'
#' @return Character string (invisibly), also printed
#' @export
eric_context <- function() {
  .build_context("eric.ed.gov", header_lines = c(
    "# eric.ed.gov - ERIC Education Research API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# ERIC: Education Resources Information Center.",
    "# Indexes journal articles, reports, and other education research.",
    "# IDs are like 'EJ1234567' (journal) or 'ED1234567' (document)."
  ))
}
