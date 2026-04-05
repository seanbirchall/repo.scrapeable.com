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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Fetches the current top-ranked stories from Hacker News (Y Combinator).
#' Retrieves full item details for each story individually via the Firebase
#' API. Larger values of \code{n} will be slower due to per-item requests.
#'
#' @param n Number of stories to return (default 30, max 500).
#' @return A tibble with one row per story:
#'   \describe{
#'     \item{id}{\code{integer} -- Hacker News item ID.}
#'     \item{type}{\code{character} -- Item type (usually \code{"story"}).}
#'     \item{title}{\code{character} -- Story title.}
#'     \item{url}{\code{character} -- Link URL (NA for text posts).}
#'     \item{by}{\code{character} -- Username of the submitter.}
#'     \item{score}{\code{integer} -- Current point score.}
#'     \item{descendants}{\code{integer} -- Total comment count.}
#'     \item{time}{\code{POSIXct} -- Submission timestamp (UTC).}
#'   }
#' @examples
#' \dontrun{
#' hn_top(10)
#' }
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
#' Fetches the most recently submitted stories on Hacker News, ordered by
#' submission time (newest first). These are fresh posts that may not yet have
#' votes or comments.
#'
#' @param n Number of stories to return (default 30, max 500).
#' @return A tibble with one row per story:
#'   \describe{
#'     \item{id}{\code{integer} -- Hacker News item ID.}
#'     \item{type}{\code{character} -- Item type.}
#'     \item{title}{\code{character} -- Story title.}
#'     \item{url}{\code{character} -- Link URL (NA for text posts).}
#'     \item{by}{\code{character} -- Submitter username.}
#'     \item{score}{\code{integer} -- Current point score.}
#'     \item{descendants}{\code{integer} -- Total comment count.}
#'     \item{time}{\code{POSIXct} -- Submission timestamp (UTC).}
#'   }
#' @examples
#' \dontrun{
#' hn_new(20)
#' }
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
#' Fetches the highest-voted stories on Hacker News. The "best" list is curated
#' by HN's ranking algorithm and tends to include stories with high engagement.
#'
#' @param n Number of stories to return (default 30, max 500).
#' @return A tibble with one row per story:
#'   \describe{
#'     \item{id}{\code{integer} -- Hacker News item ID.}
#'     \item{type}{\code{character} -- Item type.}
#'     \item{title}{\code{character} -- Story title.}
#'     \item{url}{\code{character} -- Link URL (NA for text posts).}
#'     \item{by}{\code{character} -- Submitter username.}
#'     \item{score}{\code{integer} -- Current point score.}
#'     \item{descendants}{\code{integer} -- Total comment count.}
#'     \item{time}{\code{POSIXct} -- Submission timestamp (UTC).}
#'   }
#' @examples
#' \dontrun{
#' hn_best(15)
#' }
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
#' Retrieve details for a single Hacker News item by its numeric ID. Works for
#' any item type: story, comment, poll, job, or poll option.
#'
#' @param id Hacker News item ID (numeric).
#' @return A single-row tibble:
#'   \describe{
#'     \item{id}{\code{integer} -- Hacker News item ID.}
#'     \item{type}{\code{character} -- Item type (\code{"story"}, \code{"comment"}, etc.).}
#'     \item{title}{\code{character} -- Title (NA for comments).}
#'     \item{url}{\code{character} -- Link URL (NA for text posts / comments).}
#'     \item{by}{\code{character} -- Author username.}
#'     \item{score}{\code{integer} -- Point score (NA for comments).}
#'     \item{descendants}{\code{integer} -- Comment count (stories only).}
#'     \item{time}{\code{POSIXct} -- Creation timestamp (UTC).}
#'   }
#' @examples
#' \dontrun{
#' hn_item(8863)
#' }
#' @export
hn_item <- function(id) {
  .fetch_items(as.integer(id))
}

# == Context (LLM injection) ==================================================

#' Get Hacker News client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' hn_context()
#' }
#' @export
hn_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hn_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/hackernews.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "hackernews")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# hackernews context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# hackernews", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
