# mediawiki-org.R
# Self-contained Wikipedia / MediaWiki API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: be polite — set User-Agent, no more than 200 req/s.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wiki_base <- "https://en.wikipedia.org/w/api.php"

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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

# -- URL builder ---------------------------------------------------------------

.wiki_url <- function(lang = "en", ...) {
  base <- sprintf("https://%s.wikipedia.org/w/api.php", lang)
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  params$format <- "json"
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(base, "?", query)
}

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
