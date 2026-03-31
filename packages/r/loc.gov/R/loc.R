# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), title = character(), date = character(),
  description = character(), url = character(), format = character(),
  subject = character()
)

.schema_collections <- tibble(
  id = character(), title = character(), description = character(),
  url = character(), count = integer()
)

# == Public functions ==========================================================

#' Search the Library of Congress
#'
#' @param query Search query string
#' @param count Results per page (default 25, max 100)
#' @param page Page number (1-indexed, default 1)
#' @param format_filter Optional original_format filter (e.g. "photo", "map")
#' @return tibble: id, title, date, description, url, format, subject
#' @export
loc_search <- function(query, count = 25, page = 1, format_filter = NULL) {
  url <- sprintf("%s/search/?q=%s&fo=json&c=%d&sp=%d",
                 .loc_base, utils::URLencode(query), count, page)
  if (!is.null(format_filter)) url <- paste0(url, "&fa=original_format:", utils::URLencode(format_filter))

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_search)

  rows <- lapply(results, function(r) {
    desc <- r$description
    desc_str <- if (is.null(desc)) NA_character_ else if (is.list(desc)) paste(unlist(desc), collapse = " ") else as.character(desc)
    subj <- r$subject
    subj_str <- if (is.null(subj)) NA_character_ else if (is.list(subj)) paste(unlist(subj), collapse = "; ") else as.character(subj)
    fmt <- r$original_format
    fmt_str <- if (is.null(fmt)) NA_character_ else if (is.list(fmt)) paste(unlist(fmt), collapse = "; ") else as.character(fmt)
    tibble(
      id = as.character(r$id %||% NA),
      title = as.character(if (is.list(r$title)) r$title[[1]] else r$title %||% NA),
      date = as.character(r$date %||% NA),
      description = desc_str,
      url = as.character(r$url %||% NA),
      format = fmt_str,
      subject = subj_str
    )
  })
  bind_rows(rows)
}

#' List Library of Congress collections
#'
#' @return tibble: id, title, description, url, count
#' @export
loc_collections <- function() {
  url <- sprintf("%s/collections/?fo=json&c=100", .loc_base)
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_collections)

  rows <- lapply(results, function(r) {
    desc <- r$description
    desc_str <- if (is.null(desc)) NA_character_ else if (is.list(desc)) paste(unlist(desc), collapse = " ") else as.character(desc)
    tibble(
      id = as.character(r$id %||% NA),
      title = as.character(if (is.list(r$title)) r$title[[1]] else r$title %||% NA),
      description = desc_str,
      url = as.character(r$url %||% NA),
      count = as.integer(r$count %||% NA)
    )
  })
  bind_rows(rows)
}

#' Fetch a specific LOC item by URL path
#'
#' @param url Full LOC item URL (e.g. "https://www.loc.gov/item/2002717998/")
#' @return tibble: single row with item details
#' @export
loc_item <- function(url) {
  item_url <- paste0(gsub("/$", "", url), "/?fo=json")
  raw <- .fetch_json(item_url)
  item <- raw$item
  if (is.null(item)) return(.schema_search[0, ])

  desc <- item$description
  desc_str <- if (is.null(desc)) NA_character_ else if (is.list(desc)) paste(unlist(desc), collapse = " ") else as.character(desc)
  subj <- item$subjects
  subj_str <- if (is.null(subj)) NA_character_ else if (is.list(subj)) paste(unlist(subj), collapse = "; ") else as.character(subj)

  tibble(
    id = as.character(item$id %||% NA),
    title = as.character(item$title %||% NA),
    date = as.character(item$date %||% NA),
    description = desc_str,
    url = as.character(url),
    format = as.character(if (!is.null(item$original_format)) paste(unlist(item$original_format), collapse = "; ") else NA),
    subject = subj_str
  )
}

#' Library of Congress package context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
loc_context <- function() {
  .build_context("loc.gov", header_lines = c(
    "# loc.gov - Library of Congress API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: ~20 requests per 10 seconds recommended",
    "# Formats: photo, map, manuscript/mixed material, film/video, newspaper, book",
    "# Note: loc_item() takes a full URL like https://www.loc.gov/item/2002717998/"
  ))
}
