# loc-gov.R
# Self-contained Library of Congress API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: 20 requests per 10 seconds recommended

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.loc_base <- "https://www.loc.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

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
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

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
