# bsky.app.R - Self-contained bsky.app client

library(httr2)
library(jsonlite)
library(tibble)


# bsky.R
# Self-contained Bluesky (AT Protocol) public API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public endpoints
# Rate limits: generous for public API


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bsky_base <- "https://api.bsky.app/xrpc"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_posts <- tibble(
  uri = character(), cid = character(), author_handle = character(),
  author_name = character(), text = character(),
  created_at = as.POSIXct(character()), reply_count = integer(),
  repost_count = integer(), like_count = integer()
)

.schema_users <- tibble(
  did = character(), handle = character(), display_name = character(),
  description = character(), followers_count = integer(),
  follows_count = integer(), posts_count = integer(),
  created_at = as.POSIXct(character())
)

.schema_profile <- tibble(
  did = character(), handle = character(), display_name = character(),
  description = character(), followers_count = integer(),
  follows_count = integer(), posts_count = integer(),
  created_at = as.POSIXct(character())
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search Bluesky posts
#'
#' Searches public Bluesky (AT Protocol) posts via the \code{app.bsky.feed.searchPosts}
#' endpoint. No authentication is required. Results can be ranked by relevance
#' (\code{"top"}) or reverse chronological order (\code{"latest"}).
#'
#' @param query Character. Search query (e.g. \code{"rstats"}, \code{"climate data"}).
#' @param limit Integer. Maximum results to return (default 25, max 100).
#' @param sort Character. Sort order: \code{"top"} (relevance, default) or
#'   \code{"latest"} (newest first).
#' @return A tibble with one row per post and columns:
#' \describe{
#'   \item{uri}{Character. AT Protocol URI uniquely identifying the post.}
#'   \item{cid}{Character. Content identifier (CID hash).}
#'   \item{author_handle}{Character. Author's handle (e.g. \code{"alice.bsky.social"}).}
#'   \item{author_name}{Character. Author's display name, or \code{NA}.}
#'   \item{text}{Character. Full post text.}
#'   \item{created_at}{POSIXct. Timestamp when the post was created (UTC).}
#'   \item{reply_count}{Integer. Number of replies.}
#'   \item{repost_count}{Integer. Number of reposts.}
#'   \item{like_count}{Integer. Number of likes.}
#' }
#' @examples
#' \dontrun{
#' bsky_search_posts("rstats")
#' bsky_search_posts("climate data", limit = 50, sort = "latest")
#' }
#' @export
bsky_search_posts <- function(query, limit = 25, sort = "top") {
  url <- sprintf(
    "%s/app.bsky.feed.searchPosts?q=%s&limit=%d&sort=%s",
    .bsky_base, utils::URLencode(query, reserved = TRUE),
    as.integer(limit), sort
  )
  raw <- .fetch_json(url)
  posts <- raw$posts
  if (is.null(posts) || length(posts) == 0 ||
      (is.data.frame(posts) && nrow(posts) == 0)) return(.schema_posts)

  tibble(
    uri = as.character(posts$uri),
    cid = as.character(posts$cid),
    author_handle = as.character(posts$author$handle),
    author_name = as.character(posts$author$displayName %||% NA_character_),
    text = as.character(posts$record$text %||% NA_character_),
    created_at = as.POSIXct(posts$record$createdAt, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    reply_count = as.integer(posts$replyCount %||% NA_integer_),
    repost_count = as.integer(posts$repostCount %||% NA_integer_),
    like_count = as.integer(posts$likeCount %||% NA_integer_)
  )
}

#' Search Bluesky users
#'
#' Searches for Bluesky user accounts (actors) via the
#' \code{app.bsky.actor.searchActors} endpoint. No authentication required.
#' Descriptions are truncated to 200 characters.
#'
#' @param query Character. Search query (e.g. \code{"data science"},
#'   \code{"journalism"}).
#' @param limit Integer. Maximum results to return (default 25, max 100).
#' @return A tibble with one row per user and columns:
#' \describe{
#'   \item{did}{Character. Decentralised identifier (DID) for the account.}
#'   \item{handle}{Character. User handle (e.g. \code{"alice.bsky.social"}).}
#'   \item{display_name}{Character. Display name, or \code{NA}.}
#'   \item{description}{Character. Bio / description (truncated to 200 chars), or \code{NA}.}
#'   \item{followers_count}{Integer. Follower count, or \code{NA}.}
#'   \item{follows_count}{Integer. Following count, or \code{NA}.}
#'   \item{posts_count}{Integer. Total posts, or \code{NA}.}
#'   \item{created_at}{POSIXct. Account creation timestamp (UTC), or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' bsky_search_users("data science")
#' }
#' @export
bsky_search_users <- function(query, limit = 25) {
  url <- sprintf(
    "%s/app.bsky.actor.searchActors?q=%s&limit=%d",
    .bsky_base, utils::URLencode(query, reserved = TRUE), as.integer(limit)
  )
  raw <- .fetch_json(url)
  actors <- raw$actors
  if (is.null(actors) || length(actors) == 0 ||
      (is.data.frame(actors) && nrow(actors) == 0)) return(.schema_users)

  tibble(
    did = as.character(actors$did),
    handle = as.character(actors$handle),
    display_name = as.character(actors$displayName %||% NA_character_),
    description = as.character(substr(actors$description %||% NA_character_, 1, 200)),
    followers_count = as.integer(actors$followersCount %||% NA_integer_),
    follows_count = as.integer(actors$followsCount %||% NA_integer_),
    posts_count = as.integer(actors$postsCount %||% NA_integer_),
    created_at = as.POSIXct(actors$createdAt %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  )
}

#' Get a Bluesky user profile
#'
#' Fetches the full public profile for a single Bluesky account by handle
#' or DID via the \code{app.bsky.actor.getProfile} endpoint.
#'
#' @param handle Character. User handle (e.g. \code{"bsky.app"},
#'   \code{"alice.bsky.social"}) or a DID string.
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{did}{Character. Decentralised identifier.}
#'   \item{handle}{Character. Canonical handle.}
#'   \item{display_name}{Character. Display name, or \code{NA}.}
#'   \item{description}{Character. Full bio / description, or \code{NA}.}
#'   \item{followers_count}{Integer. Follower count, or \code{NA}.}
#'   \item{follows_count}{Integer. Following count, or \code{NA}.}
#'   \item{posts_count}{Integer. Total post count, or \code{NA}.}
#'   \item{created_at}{POSIXct. Account creation timestamp (UTC), or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' bsky_profile("bsky.app")
#' }
#' @export
bsky_profile <- function(handle) {
  url <- sprintf(
    "%s/app.bsky.actor.getProfile?actor=%s",
    .bsky_base, utils::URLencode(handle, reserved = TRUE)
  )
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_profile)

  tibble(
    did = as.character(raw$did),
    handle = as.character(raw$handle),
    display_name = as.character(raw$displayName %||% NA_character_),
    description = as.character(raw$description %||% NA_character_),
    followers_count = as.integer(raw$followersCount %||% NA_integer_),
    follows_count = as.integer(raw$followsCount %||% NA_integer_),
    posts_count = as.integer(raw$postsCount %||% NA_integer_),
    created_at = as.POSIXct(raw$createdAt %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  )
}

# == Context ===================================================================

#' Get bsky.app client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bsky_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bsky_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bsky.app.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bsky.app")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bsky.app context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bsky.app", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
