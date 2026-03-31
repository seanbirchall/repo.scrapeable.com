# hackernews.R
# Self-contained Hacker News API client (Firebase).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hn_base <- "https://hacker-news.firebaseio.com/v0"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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
#' Fetches the current top stories from Hacker News. Returns full item
#' details (title, URL, score, etc.) for the top N stories.
#'
#' @param n Number of stories to return (default 30, max 500)
#' @return tibble: id, type, title, url, by, score, descendants, time
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
hn_item <- function(id) {
  .fetch_items(as.integer(id))
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the hackernews.firebaseio.com package
#'
#' @return Character string (invisibly), also printed
hn_context <- function() {
  .build_context("hackernews.firebaseio.com", header_lines = c(
    "# hackernews.firebaseio.com - Hacker News API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API: https://hacker-news.firebaseio.com/v0/",
    "# All functions return tibbles with typed columns."
  ))
}
