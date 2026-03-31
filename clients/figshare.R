# figshare.R
# Self-contained Figshare API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: not documented, be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.figshare_base <- "https://api.figshare.com/v2"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

.post_json <- function(url, body) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

# == Schemas ===================================================================

.schema_articles <- tibble(
  id = integer(), title = character(), doi = character(),
  url = character(), published_date = as.Date(character()),
  defined_type_name = character()
)

.schema_article_detail <- tibble(
  id = integer(), title = character(), doi = character(),
  url = character(), published_date = as.Date(character()),
  description = character(), defined_type_name = character(),
  license = character(), authors = character(), tags = character(),
  citation = character()
)

# == Article search ============================================================

#' Search Figshare articles
#'
#' @param query Search query string (e.g. "climate", "genomics")
#' @param page_size Number of results per page (default 10, max 1000)
#' @param page Page number (default 1)
#' @return tibble: id, title, doi, url, published_date, defined_type_name
figshare_search <- function(query, page_size = 10, page = 1) {
  body <- list(search_for = query, page_size = page_size, page = page)
  raw <- .post_json(paste0(.figshare_base, "/articles/search"), body)
  if (is.null(raw) || length(raw) == 0) return(.schema_articles)

  as_tibble(raw) |>
    transmute(
      id = as.integer(id),
      title = as.character(title),
      doi = as.character(doi),
      url = as.character(url),
      published_date = tryCatch(as.Date(published_date), error = function(e) as.Date(NA)),
      defined_type_name = as.character(if ("defined_type_name" %in% names(raw)) defined_type_name else NA_character_)
    )
}


# == Article detail ============================================================

#' Fetch a single Figshare article by ID
#'
#' @param id Figshare article ID (integer)
#' @return tibble: one row with id, title, doi, url, published_date,
#'   description, defined_type_name, license, authors, tags, citation
figshare_article <- function(id) {
  url <- paste0(.figshare_base, "/articles/", id)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$id)) return(.schema_article_detail)

  auth_str <- if (!is.null(raw$authors) && is.data.frame(raw$authors)) {
    paste(raw$authors$full_name, collapse = "; ")
  } else NA_character_

  tag_str <- if (!is.null(raw$tags)) {
    paste(raw$tags, collapse = "; ")
  } else NA_character_

  lic_str <- if (!is.null(raw$license) && is.list(raw$license)) {
    as.character(raw$license$name %||% NA_character_)
  } else NA_character_

  tibble(
    id = as.integer(raw$id),
    title = as.character(raw$title %||% NA_character_),
    doi = as.character(raw$doi %||% NA_character_),
    url = as.character(raw$url %||% NA_character_),
    published_date = tryCatch(as.Date(raw$published_date), error = function(e) as.Date(NA)),
    description = as.character(raw$description %||% NA_character_),
    defined_type_name = as.character(raw$defined_type_name %||% NA_character_),
    license = lic_str,
    authors = auth_str,
    tags = tag_str,
    citation = as.character(raw$citation %||% NA_character_)
  )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Figshare package
#'
#' @return Character string (invisibly), also printed
figshare_context <- function() {
  .build_context("figshare.com", header_lines = c(
    "# figshare.com - Figshare Research Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for public data",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Figshare hosts research data, figures, and datasets.",
    "# Search uses POST with JSON body. Article detail uses GET."
  ))
}
