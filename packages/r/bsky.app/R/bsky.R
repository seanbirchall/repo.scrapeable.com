# == Public functions ==========================================================

#' Search Bluesky posts
#'
#' @param query Search query
#' @param limit Max results (default 25, max 100)
#' @param sort Sort order: "top" or "latest" (default "top")
#' @return tibble: uri, cid, author_handle, author_name, text,
#'   created_at, reply_count, repost_count, like_count
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
#' @param query Search query
#' @param limit Max results (default 25, max 100)
#' @return tibble: did, handle, display_name, description, followers_count,
#'   follows_count, posts_count, created_at
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
#' @param handle User handle (e.g. "jay.bsky.team") or DID
#' @return tibble: one row with did, handle, display_name, description,
#'   followers_count, follows_count, posts_count, created_at
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

#' Show Bluesky client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
bsky_context <- function() {
  .build_context(
    pkg_name = "bsky.app",
    header_lines = c(
      "# bsky.app -- Bluesky Social (AT Protocol) Public API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for public endpoints",
      "# Sort options for posts: top, latest",
      "# Example handles: jay.bsky.team, bsky.app"
    )
  )
}

