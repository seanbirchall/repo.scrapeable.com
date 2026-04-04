# mediawiki.org.R - Self-contained mediawiki.org client



# mediawiki-org.R
# Self-contained Wikipedia / MediaWiki API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: be polite — set User-Agent, no more than 200 req/s.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wiki_base <- "https://en.wikipedia.org/w/api.php"
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
#' Uses the MediaWiki action API to perform full-text search across
#' Wikipedia article titles and content. HTML tags are stripped from
#' snippet text.
#'
#' @param query Character. Search term or phrase
#'   (e.g. \code{"quantum computing"}, \code{"machine learning"},
#'   \code{"climate change"}).
#' @param limit Integer. Maximum number of results to return (default
#'   \code{10}, maximum \code{500}).
#' @param lang Character. Wikipedia language edition code (default
#'   \code{"en"}). Other examples: \code{"fr"} (French), \code{"de"}
#'   (German), \code{"es"} (Spanish), \code{"ja"} (Japanese).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pageid}{Integer. Unique Wikipedia page identifier.}
#'     \item{title}{Character. Article title.}
#'     \item{size}{Integer. Article size in bytes.}
#'     \item{wordcount}{Integer. Approximate word count.}
#'     \item{snippet}{Character. Text snippet with search-term context
#'       (HTML tags removed).}
#'     \item{timestamp}{Character. ISO-8601 timestamp of last edit.}
#'   }
#' @examples
#' wiki_search("quantum computing")
#' wiki_search("machine learning", limit = 5, lang = "en")
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
#' Returns the plain-text extract for a Wikipedia page, either the
#' introductory section only (default) or the entire article text.
#' Useful for retrieving article summaries programmatically.
#'
#' @param title Character. Wikipedia page title. Spaces or underscores
#'   are both accepted (e.g. \code{"Albert Einstein"},
#'   \code{"Albert_Einstein"}, \code{"Python_(programming_language)"}).
#' @param intro Logical. If \code{TRUE} (default), return only the
#'   lead/introductory section. If \code{FALSE}, return the full
#'   article text.
#' @param lang Character. Wikipedia language edition code (default
#'   \code{"en"}). See \code{\link{wiki_search}} for other codes.
#' @return A tibble with columns:
#'   \describe{
#'     \item{pageid}{Integer. Wikipedia page identifier.}
#'     \item{title}{Character. Canonical page title.}
#'     \item{extract}{Character. Plain-text content (intro or full).}
#'   }
#' @examples
#' wiki_page("Albert Einstein")
#' wiki_page("Python_(programming_language)", intro = FALSE)
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

#' Get mediawiki.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wiki_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wiki_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/mediawiki.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "mediawiki.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# mediawiki.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# mediawiki.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
