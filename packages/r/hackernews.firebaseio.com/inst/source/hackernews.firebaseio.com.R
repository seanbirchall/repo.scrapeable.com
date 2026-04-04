# hackernews.firebaseio.com.R - Self-contained hackernews.firebaseio.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# hackernews.R
# Self-contained Hacker News API client (Firebase).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hn_base <- "https://hacker-news.firebaseio.com/v0"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Batch fetch items --------------------------------------------------------

.fetch_items <- function(ids) {
  rows <- lapply(ids, function(id) {
    tryCatch({
      raw <- .fetch_json(sprintf("%s/item/%d.json", .hn_base, id))
      if (is.null(raw)) return(NULL)
      tibble(
        id    = as.integer(raw$id %||% NA_integer_),
        type  = as.character(raw$type %||% NA_character_),
        title = as.character(raw$title %||% NA_character_),
        url   = as.character(raw$url %||% NA_character_),
        by    = as.character(raw$by %||% NA_character_),
        score = as.integer(raw$score %||% NA_integer_),
        descendants = as.integer(raw$descendants %||% NA_integer_),
        time  = as.POSIXct(as.integer(raw$time %||% NA_integer_), origin = "1970-01-01")
      )
    }, error = function(e) NULL)
  })
  bind_rows(rows[!vapply(rows, is.null, logical(1))])
}

# == Schemas ===================================================================

.schema_item <- tibble(
  id = integer(), type = character(), title = character(),
  url = character(), by = character(), score = integer(),
  descendants = integer(), time = as.POSIXct(character())
)


# == Top stories ===============================================================

#' Get top Hacker News stories
#'
#' Fetches the current top-ranked stories from Hacker News (the front page).
#' Returns full item details including title, URL, score, and comment count
#' for the top N stories.
#'
#' @param n Integer. Number of stories to return (default 30, max 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- unique HN item identifier}
#'     \item{type}{character -- item type, typically \code{"story"}}
#'     \item{title}{character -- story headline}
#'     \item{url}{character -- link URL (NA for Ask HN / Show HN text posts)}
#'     \item{by}{character -- username of the submitter}
#'     \item{score}{integer -- upvote count}
#'     \item{descendants}{integer -- total comment count}
#'     \item{time}{POSIXct -- submission timestamp (UTC)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hn_top(5)
#' }
hn_top <- function(n = 30) {
  ids <- .fetch_json(sprintf("%s/topstories.json", .hn_base))
  if (is.null(ids) || length(ids) == 0) return(.schema_item)
  ids <- head(ids, min(n, 500))
  .fetch_items(ids)
}

# == New stories ===============================================================

#' Get newest Hacker News stories
#'
#' Fetches the most recently submitted stories on Hacker News, ordered by
#' submission time (newest first).
#'
#' @param n Integer. Number of stories to return (default 30, max 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- unique HN item identifier}
#'     \item{type}{character -- item type, typically \code{"story"}}
#'     \item{title}{character -- story headline}
#'     \item{url}{character -- link URL (NA for text posts)}
#'     \item{by}{character -- username of the submitter}
#'     \item{score}{integer -- upvote count}
#'     \item{descendants}{integer -- total comment count}
#'     \item{time}{POSIXct -- submission timestamp (UTC)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hn_new(10)
#' }
hn_new <- function(n = 30) {
  ids <- .fetch_json(sprintf("%s/newstories.json", .hn_base))
  if (is.null(ids) || length(ids) == 0) return(.schema_item)
  ids <- head(ids, min(n, 500))
  .fetch_items(ids)
}

# == Best stories ==============================================================

#' Get best Hacker News stories
#'
#' Fetches stories with the highest scores over a recent window, as curated
#' by the Hacker News ranking algorithm.
#'
#' @param n Integer. Number of stories to return (default 30, max 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- unique HN item identifier}
#'     \item{type}{character -- item type, typically \code{"story"}}
#'     \item{title}{character -- story headline}
#'     \item{url}{character -- link URL (NA for text posts)}
#'     \item{by}{character -- username of the submitter}
#'     \item{score}{integer -- upvote count}
#'     \item{descendants}{integer -- total comment count}
#'     \item{time}{POSIXct -- submission timestamp (UTC)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hn_best(20)
#' }
hn_best <- function(n = 30) {
  ids <- .fetch_json(sprintf("%s/beststories.json", .hn_base))
  if (is.null(ids) || length(ids) == 0) return(.schema_item)
  ids <- head(ids, min(n, 500))
  .fetch_items(ids)
}

# == Single item ===============================================================

#' Get a single Hacker News item by ID
#'
#' Returns details for a single HN item. Works for stories, comments,
#' polls, and job postings. The first-ever HN item is id = 1.
#'
#' @param id Numeric. The Hacker News item ID (e.g., \code{1}, \code{8863}).
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{id}{integer -- unique HN item identifier}
#'     \item{type}{character -- \code{"story"}, \code{"comment"}, \code{"poll"}, or \code{"job"}}
#'     \item{title}{character -- headline (NA for comments)}
#'     \item{url}{character -- link URL (NA for text posts and comments)}
#'     \item{by}{character -- username of the author}
#'     \item{score}{integer -- upvote count (NA for comments)}
#'     \item{descendants}{integer -- total comment count (NA for non-stories)}
#'     \item{time}{POSIXct -- creation timestamp (UTC)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hn_item(8863)
#' }
hn_item <- function(id) {
  .fetch_items(as.integer(id))
}

# == Context ===================================================================

#' Get hackernews.firebaseio.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hn_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hn_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/hackernews.firebaseio.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "hackernews.firebaseio.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# hackernews.firebaseio.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# hackernews.firebaseio.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
