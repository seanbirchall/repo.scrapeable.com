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

# == Public functions ==========================================================

#' Search Bluesky posts
#'
#' Searches the Bluesky social network for public posts matching a query.
#' Supports sorting by relevance or recency. No authentication required.
#'
#' @param query Character. Search query string (e.g. \code{"R programming"},
#'   \code{"#rstats"}, \code{"climate data"}). Supports hashtags and phrases.
#' @param limit Integer. Maximum results to return (default 25, max 100).
#' @param sort Character. Sort order: \code{"top"} (relevance, default) or
#'   \code{"latest"} (chronological).
#' @return A tibble with one row per post and 9 columns:
#' \describe{
#'   \item{uri}{Character. AT Protocol URI uniquely identifying the post.}
#'   \item{cid}{Character. Content identifier (CID hash).}
#'   \item{author_handle}{Character. Author's handle (e.g. \code{"masalmon.eu"}).}
#'   \item{author_name}{Character. Author's display name, or \code{NA}.}
#'   \item{text}{Character. Full post text content.}
#'   \item{created_at}{POSIXct. Timestamp in UTC when the post was created.}
#'   \item{reply_count}{Integer. Number of replies.}
#'   \item{repost_count}{Integer. Number of reposts (boosts).}
#'   \item{like_count}{Integer. Number of likes.}
#' }
#' @examples
#' bsky_search_posts("R programming", limit = 5)
#' bsky_search_posts("#rstats", sort = "latest")
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
#' Searches for Bluesky user accounts (actors) by name, handle, or
#' description keywords. Returns profile metadata for matching users.
#' Note: follower/follow/post counts may be \code{NA} for some accounts.
#'
#' @param query Character. Search term to match against user profiles
#'   (e.g. \code{"data science"}, \code{"journalism"}).
#' @param limit Integer. Maximum results to return (default 25, max 100).
#' @return A tibble with one row per user and 8 columns:
#' \describe{
#'   \item{did}{Character. Decentralized Identifier (DID) for the user.}
#'   \item{handle}{Character. User handle (e.g. \code{"masalmon.eu"}).}
#'   \item{display_name}{Character. Display name, or \code{NA}.}
#'   \item{description}{Character. Bio text (truncated to 200 chars), or \code{NA}.}
#'   \item{followers_count}{Integer. Number of followers, or \code{NA}.}
#'   \item{follows_count}{Integer. Number of accounts followed, or \code{NA}.}
#'   \item{posts_count}{Integer. Total number of posts, or \code{NA}.}
#'   \item{created_at}{POSIXct. Account creation timestamp in UTC.}
#' }
#' @examples
#' bsky_search_users("data science", limit = 5)
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
#' Fetches the full public profile for a single Bluesky user by handle
#' or DID. Returns a one-row tibble with follower counts and metadata.
#'
#' @param handle Character. Bluesky handle (e.g. \code{"bsky.app"},
#'   \code{"masalmon.eu"}) or a DID string (e.g. \code{"did:plc:z72i7..."}).
#' @return A tibble with 1 row and 8 columns:
#' \describe{
#'   \item{did}{Character. Decentralized Identifier.}
#'   \item{handle}{Character. User handle.}
#'   \item{display_name}{Character. Display name, or \code{NA}.}
#'   \item{description}{Character. Full bio text, or \code{NA}.}
#'   \item{followers_count}{Integer. Number of followers.}
#'   \item{follows_count}{Integer. Number of accounts followed.}
#'   \item{posts_count}{Integer. Total number of posts.}
#'   \item{created_at}{POSIXct. Account creation timestamp in UTC.}
#' }
#' @examples
#' bsky_profile("bsky.app")
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

#' Get bsky client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bsky.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bsky")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bsky context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bsky", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
