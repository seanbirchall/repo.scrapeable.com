# == Schemas ===================================================================

.schema_search <- tibble(
  pageid = integer(), title = character(), size = integer(),
  wordcount = integer(), snippet = character(), timestamp = character()
)

.schema_page <- tibble(
  pageid = integer(), title = character(), extract = character()
)

# == Search ====================================================================

#' Search Wikipedia articles by keyword
#'
#' Uses the MediaWiki action API to search article text and titles.
#'
#' @param query Search term (e.g. "quantum computing", "machine learning")
#' @param limit Max results (default 10, max 500)
#' @param lang Wikipedia language code (default "en")
#' @return tibble: pageid (integer), title (character), size (integer),
#'   wordcount (integer), snippet (character), timestamp (character)
#' @export
wiki_search <- function(query, limit = 10, lang = "en") {
  url <- .wiki_url(lang,
    action = "query",
    list = "search",
    srsearch = utils::URLencode(query),
    srlimit = limit
  )
  raw <- .fetch_json(url)
  results <- raw$query$search
  if (is.null(results) || length(results) == 0) return(.schema_search)

  as_tibble(results) |>
    transmute(
      pageid    = as.integer(pageid),
      title     = as.character(title),
      size      = as.integer(size),
      wordcount = as.integer(wordcount),
      snippet   = gsub("<[^>]+>", "", as.character(snippet)),
      timestamp = as.character(timestamp)
    )
}

# == Page extract ==============================================================

#' Fetch Wikipedia page extract (intro or full)
#'
#' Returns the plain-text extract for one or more Wikipedia pages.
#'
#' @param title Page title (e.g. "Albert_Einstein"). Underscores or spaces ok.
#' @param intro If TRUE (default), return only the intro section
#' @param lang Wikipedia language code (default "en")
#' @return tibble: pageid (integer), title (character), extract (character)
#' @export
wiki_page <- function(title, intro = TRUE, lang = "en") {
  params <- list(
    action = "query",
    titles = utils::URLencode(gsub(" ", "_", title)),
    prop = "extracts",
    explaintext = "1"
  )
  if (intro) params$exintro <- "1"

  url <- do.call(.wiki_url, c(list(lang = lang), params))
  raw <- .fetch_json(url)

  pages <- raw$query$pages
  if (is.null(pages) || length(pages) == 0) return(.schema_page)

  rows <- lapply(pages, function(p) {
    tibble(
      pageid  = as.integer(p$pageid %||% NA_integer_),
      title   = as.character(p$title %||% NA_character_),
      extract = as.character(p$extract %||% NA_character_)
    )
  })
  bind_rows(rows)
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the mediawiki.org package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
#' @export
wiki_context <- function() {
  .build_context("mediawiki.org", header_lines = c(
    "# mediawiki.org - Wikipedia / MediaWiki API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: be polite, set User-Agent",
    "# All functions return tibbles with typed columns."
  ))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
