# == Search ====================================================================

#' Search CourtListener legal opinions and dockets
#'
#' @param query Search query string (e.g. "privacy", "first amendment")
#' @param type Result type: "o" = opinions, "r" = RECAP, "d" = dockets,
#'   "p" = people (default "o")
#' @param page_size Number of results (default 10)
#' @param page Page number (default 1)
#' @return tibble: id, caseName, court, dateFiled, snippet, url
#' @export
cl_search <- function(query, type = "o", page_size = 10, page = 1) {
  url <- paste0(.cl_base, "/search/?q=", utils::URLencode(query),
                "&type=", type, "&page_size=", page_size, "&page=", page)
  raw <- .fetch_json(url)
  d <- raw$results
  if (is.null(d) || length(d) == 0) return(.schema_search)

  as_tibble(d) |>
    transmute(
      id = as.integer(id),
      caseName = as.character(caseName),
      court = as.character(if ("court" %in% names(d)) court else NA_character_),
      dateFiled = tryCatch(as.Date(dateFiled), error = function(e) as.Date(NA)),
      snippet = as.character(if ("snippet" %in% names(d)) snippet else NA_character_),
      url = paste0("https://www.courtlistener.com", as.character(absolute_url))
    )
}

# == Opinion detail ============================================================

#' Fetch a CourtListener opinion cluster by ID
#'
#' @param id Opinion cluster ID (integer)
#' @return tibble: one row with id, caseName, court, dateFiled, snippet, url, plain_text
#' @export
cl_opinion <- function(id) {
  url <- paste0(.cl_base, "/clusters/", id, "/")
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$id)) return(.schema_opinion)

  tibble(
    id = as.integer(raw$id),
    caseName = as.character(raw$case_name %||% NA_character_),
    court = as.character(raw$court %||% NA_character_),
    dateFiled = tryCatch(as.Date(raw$date_filed), error = function(e) as.Date(NA)),
    snippet = as.character(raw$syllabus %||% NA_character_),
    url = paste0("https://www.courtlistener.com", as.character(raw$absolute_url %||% "")),
    plain_text = NA_character_
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the CourtListener package
#'
#' @return Character string (invisibly), also printed
#' @export
cl_context <- function() {
  .build_context("courtlistener.com", header_lines = c(
    "# courtlistener.com - CourtListener Legal API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for basic search",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Search types: o=opinions, r=RECAP, d=dockets, p=people",
    "# CourtListener provides free access to US court opinions."
  ))
}
