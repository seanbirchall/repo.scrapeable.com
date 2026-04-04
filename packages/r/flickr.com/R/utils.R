#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# flickr.com.R - Self-contained Flickr public feeds client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (uses public feeds only)


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

