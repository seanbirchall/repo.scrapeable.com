
# == Top stories ===============================================================

#' Get top Hacker News stories
#'
#' Fetches the current top stories from Hacker News. Returns full item
#' details (title, URL, score, etc.) for the top N stories.
#'
#' @param n Number of stories to return (default 30, max 500)
#' @return tibble: id, type, title, url, by, score, descendants, time
#' @export
hn_top <- function(n = 30) {
  ids <- .fetch_json(sprintf("%s/topstories.json", .hn_base))
  if (is.null(ids) || length(ids) == 0) return(.schema_item)
  ids <- head(ids, min(n, 500))
  .fetch_items(ids)
}

# == New stories ===============================================================

#' Get newest Hacker News stories
#'
#' @param n Number of stories to return (default 30, max 500)
#' @return tibble: id, type, title, url, by, score, descendants, time
#' @export
hn_new <- function(n = 30) {
  ids <- .fetch_json(sprintf("%s/newstories.json", .hn_base))
  if (is.null(ids) || length(ids) == 0) return(.schema_item)
  ids <- head(ids, min(n, 500))
  .fetch_items(ids)
}

# == Best stories ==============================================================

#' Get best Hacker News stories
#'
#' @param n Number of stories to return (default 30, max 500)
#' @return tibble: id, type, title, url, by, score, descendants, time
#' @export
hn_best <- function(n = 30) {
  ids <- .fetch_json(sprintf("%s/beststories.json", .hn_base))
  if (is.null(ids) || length(ids) == 0) return(.schema_item)
  ids <- head(ids, min(n, 500))
  .fetch_items(ids)
}

# == Single item ===============================================================

#' Get a single Hacker News item
#'
#' Returns details for a single HN item (story, comment, poll, job).
#'
#' @param id Item ID (numeric)
#' @return tibble: id, type, title, url, by, score, descendants, time
#' @export
hn_item <- function(id) {
  .fetch_items(as.integer(id))
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the hackernews.firebaseio.com package
#'
#' @return Character string (invisibly), also printed
#' @export
hn_context <- function() {
  .build_context("hackernews.firebaseio.com", header_lines = c(
    "# hackernews.firebaseio.com - Hacker News API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API: https://hacker-news.firebaseio.com/v0/",
    "# All functions return tibbles with typed columns."
  ))
}
