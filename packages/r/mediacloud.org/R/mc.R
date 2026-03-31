# == Search ====================================================================

#' Search Media Cloud for news coverage overview
#'
#' @param query Search query (e.g. "climate change", "elections")
#' @param date_range Date range as "YYYY-MM-DD..YYYY-MM-DD"
#'   (e.g. "2025-01-01..2025-03-31")
#' @return tibble: query, date_range, total, matches_per_day
#' @export
mc_search <- function(query, date_range) {
  url <- paste0(.mc_base, "/search/overview?q=", utils::URLencode(query),
                "&dt=", date_range)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_search)

  total <- as.integer(raw$total %||% raw$`relevant` %||% NA_integer_)
  mpd <- if (!is.null(raw$dailyCounts) || !is.null(raw$`matches_per_day`)) {
    counts <- raw$dailyCounts %||% raw$`matches_per_day`
    if (is.data.frame(counts)) {
      paste(apply(counts, 1, function(r) paste(r, collapse = ":")), collapse = "; ")
    } else if (is.list(counts)) {
      paste(names(counts), unlist(counts), sep = ":", collapse = "; ")
    } else as.character(counts)
  } else NA_character_

  tibble(
    query = query,
    date_range = date_range,
    total = total,
    matches_per_day = mpd
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Media Cloud package
#'
#' @return Character string (invisibly), also printed
#' @export
mc_context <- function() {
  .build_context("mediacloud.org", header_lines = c(
    "# mediacloud.org - Media Cloud News Search API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for search overview",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Media Cloud tracks news coverage across global media.",
    "# Use date_range format: 'YYYY-MM-DD..YYYY-MM-DD'"
  ))
}
