# mediacloud.org.R - Self-contained mediacloud.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a


# mediacloud.R
# Self-contained Media Cloud API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for search overview
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.mc_base <- "https://search.mediacloud.org/api"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  query = character(), date_range = character(),
  total = integer(), matches_per_day = character()
)

# == Search ====================================================================

#' Search Media Cloud for news coverage overview
#'
#' Queries the Media Cloud search API for an overview of news coverage
#' matching a query within a date range. Returns total article count
#' and daily breakdown.
#'
#' @param query Search query (e.g. "climate change", "elections",
#'   "artificial intelligence")
#' @param date_range Date range as "YYYY-MM-DD..YYYY-MM-DD"
#'   (e.g. "2025-01-01..2025-03-31")
#' @return A tibble with columns:
#'   \describe{
#'     \item{query}{The search query as submitted (character)}
#'     \item{date_range}{The date range as submitted (character)}
#'     \item{total}{Total number of matching articles (integer)}
#'     \item{matches_per_day}{Daily counts as "date:count" pairs, semicolon-separated (character)}
#'   }
#' @examples
#' mc_search("climate change", "2025-01-01..2025-01-31")
#' @seealso [mc_context()]
#' @source <https://search.mediacloud.org/api>
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

# == Context ===================================================================

#' Get mediacloud.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/mediacloud.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "mediacloud.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# mediacloud.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# mediacloud.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
