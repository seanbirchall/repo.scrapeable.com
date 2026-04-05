# flickr.com.R - Self-contained Flickr public feeds client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (uses public feeds only)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.flickr_feeds_base <- "https://api.flickr.com/services/feeds"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- readLines(tmp, warn = FALSE)
  txt <- paste(raw, collapse = "\n")
  jsonlite::fromJSON(txt, simplifyVector = TRUE)
}

.parse_feed_items <- function(data) {
  items <- data$items
  if (is.null(items) || length(items) == 0 || nrow(items) == 0) {
    return(tibble(title = character(), link = character(), author = character(),
                  author_id = character(), tags = character(),
                  date_taken = character(), published = character(),
                  image_url = character()))
  }

  # Extract author name from "nobody@flickr.com (\"AuthorName\")" format
  author_clean <- gsub('.*\\("(.+)"\\)', "\\1", items$author)

  # Extract image URL from media$m
  image_url <- if (!is.null(items$media) && !is.null(items$media$m)) {
    items$media$m
  } else {
    rep(NA_character_, nrow(items))
  }

  tibble(
    title = as.character(items$title %||% NA_character_),
    link = as.character(items$link %||% NA_character_),
    author = author_clean,
    author_id = as.character(items$author_id %||% NA_character_),
    tags = as.character(items$tags %||% NA_character_),
    date_taken = as.character(items$date_taken %||% NA_character_),
    published = as.character(items$published %||% NA_character_),
    image_url = as.character(image_url)
  )
}

# == Public functions ==========================================================

#' Search Flickr public photos by tags
#'
#' Search the Flickr public photos feed by one or more tags. Returns the
#' 20 most recent matching photos (feed limit). No API key required.
#'
#' @param tags Character. Comma-separated tag string (e.g. \code{"nature,landscape"},
#'   \code{"sunset"}).
#' @param tagmode Character. Tag matching mode: \code{"any"} (OR, default)
#'   or \code{"all"} (AND -- photo must have all tags).
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Photo title.}
#'     \item{link}{Character. URL to the Flickr photo page.}
#'     \item{author}{Character. Photographer name (extracted from feed format).}
#'     \item{author_id}{Character. Flickr user ID (e.g. "12345678@N00").}
#'     \item{tags}{Character. Space-separated tags on the photo.}
#'     \item{date_taken}{Character. Date photo was taken (ISO 8601).}
#'     \item{published}{Character. Date published to Flickr (ISO 8601).}
#'     \item{image_url}{Character. URL to medium-size image (640px).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' flickr_search("sunset")
#' flickr_search("nature,landscape", tagmode = "all")
#' }
flickr_search <- function(tags, tagmode = "any") {
  url <- paste0(.flickr_feeds_base,
                "/photos_public.gne?format=json&nojsoncallback=1",
                "&tags=", utils::URLencode(tags),
                "&tagmode=", tagmode)
  data <- .fetch_json(url)
  .parse_feed_items(data)
}

#' Fetch recent public photos from a Flickr user
#'
#' Retrieve the most recent public photos from a specific Flickr user.
#' Returns up to 20 photos (feed limit). No API key required.
#'
#' @param user_id Character. Flickr user ID (e.g. \code{"12345678@N00"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Photo title.}
#'     \item{link}{Character. URL to the Flickr photo page.}
#'     \item{author}{Character. Photographer name.}
#'     \item{author_id}{Character. Flickr user ID.}
#'     \item{tags}{Character. Space-separated tags.}
#'     \item{date_taken}{Character. Date photo was taken (ISO 8601).}
#'     \item{published}{Character. Date published (ISO 8601).}
#'     \item{image_url}{Character. URL to medium-size image.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' flickr_user_photos("12345678@N00")
#' }
flickr_user_photos <- function(user_id) {
  url <- paste0(.flickr_feeds_base,
                "/photos_public.gne?format=json&nojsoncallback=1",
                "&id=", utils::URLencode(user_id))
  data <- .fetch_json(url)
  .parse_feed_items(data)
}

#' Fetch recent public photos from Flickr (no filter)
#'
#' Returns the 20 most recently uploaded public photos on Flickr.
#' No filtering, no API key required.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Photo title.}
#'     \item{link}{Character. URL to the Flickr photo page.}
#'     \item{author}{Character. Photographer name.}
#'     \item{author_id}{Character. Flickr user ID.}
#'     \item{tags}{Character. Space-separated tags.}
#'     \item{date_taken}{Character. Date photo was taken (ISO 8601).}
#'     \item{published}{Character. Date published (ISO 8601).}
#'     \item{image_url}{Character. URL to medium-size image.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' flickr_recent()
#' }
flickr_recent <- function() {
  url <- paste0(.flickr_feeds_base,
                "/photos_public.gne?format=json&nojsoncallback=1")
  data <- .fetch_json(url)
  .parse_feed_items(data)
}

#' Fetch Flickr geotagged photos feed
#'
#' Returns the most recent geotagged public photos from Flickr. Optionally
#' filter by tags. Up to 20 results (feed limit). No API key required.
#'
#' @param tags Character. Optional comma-separated tags to filter
#'   (e.g. \code{"architecture"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Photo title.}
#'     \item{link}{Character. URL to the Flickr photo page.}
#'     \item{author}{Character. Photographer name.}
#'     \item{author_id}{Character. Flickr user ID.}
#'     \item{tags}{Character. Space-separated tags.}
#'     \item{date_taken}{Character. Date photo was taken (ISO 8601).}
#'     \item{published}{Character. Date published (ISO 8601).}
#'     \item{image_url}{Character. URL to medium-size image.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' flickr_geo()
#' flickr_geo("architecture")
#' }
flickr_geo <- function(tags = NULL) {
  url <- paste0(.flickr_feeds_base,
                "/geo?format=json&nojsoncallback=1")
  if (!is.null(tags)) {
    url <- paste0(url, "&tags=", utils::URLencode(tags))
  }
  data <- .fetch_json(url)
  .parse_feed_items(data)
}

#' List available Flickr public feeds
#'
#' Returns a reference table of the public Flickr feeds accessible through
#' this client (no API key required).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Feed identifier (e.g. "photos_public", "geo").}
#'     \item{name}{Character. Human-readable feed name.}
#'     \item{description}{Character. Brief description of the feed.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' flickr_list()
#' }
flickr_list <- function() {
  tibble(
    id = c("photos_public", "geo"),
    name = c("Public Photos", "Geotagged Photos"),
    description = c(
      "Recent public photos, filterable by tags or user ID",
      "Recent geotagged public photos"
    )
  )
}

#' Get flickr.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
flickr_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(flickr_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/flickr.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "flickr.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# flickr.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# flickr.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
