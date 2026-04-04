# figshare.com.R - Self-contained figshare.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# figshare.R
# Self-contained Figshare API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.figshare_base <- "https://api.figshare.com/v2"

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
#' Search the Figshare public repository for articles (datasets, figures,
#' filesets, journal contributions, etc.) matching a query string.
#'
#' @param query Character. Search query string (e.g. \code{"climate"},
#'   \code{"genomics"}, \code{"machine learning"}).
#' @param page_size Integer. Number of results per page (default 10, max 1000).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Figshare article ID.}
#'     \item{title}{Character. Article title.}
#'     \item{doi}{Character. DOI (e.g. "10.6084/m9.figshare.12345").}
#'     \item{url}{Character. Figshare URL.}
#'     \item{published_date}{Date. Publication date.}
#'     \item{defined_type_name}{Character. Item type: "dataset", "figure",
#'       "fileset", "journal contribution", "poster", etc.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' figshare_search("climate", page_size = 20)
#' figshare_search("genomics", page = 2)
#' }
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
#' Retrieve full metadata for a single Figshare article, including
#' description, authors, tags, license, and citation string.
#'
#' @param id Integer. Figshare article ID.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Integer. Figshare article ID.}
#'     \item{title}{Character. Article title.}
#'     \item{doi}{Character. DOI.}
#'     \item{url}{Character. Figshare URL.}
#'     \item{published_date}{Date. Publication date.}
#'     \item{description}{Character. Full description/abstract (may contain HTML).}
#'     \item{defined_type_name}{Character. Item type.}
#'     \item{license}{Character. License name (e.g. "CC BY 4.0").}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{tags}{Character. Semicolon-separated tags/keywords.}
#'     \item{citation}{Character. Formatted citation string.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' figshare_article(12345678)
#' }
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


# == Article files =============================================================

#' List files attached to a Figshare article
#'
#' Retrieve the list of files attached to a Figshare article, including
#' download URLs and checksums.
#'
#' @param id Integer. Figshare article ID.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. File ID.}
#'     \item{name}{Character. File name (e.g. "data.csv").}
#'     \item{size}{Numeric. File size in bytes.}
#'     \item{download_url}{Character. Direct download URL.}
#'     \item{computed_md5}{Character. MD5 checksum of file contents.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' figshare_files(12345678)
#' }
figshare_files <- function(id) {
  schema <- tibble(id = integer(), name = character(), size = numeric(),
                   download_url = character(), computed_md5 = character())
  url <- paste0(.figshare_base, "/articles/", id, "/files")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)
  if (!is.data.frame(raw)) return(schema)

  as_tibble(raw) |>
    transmute(
      id = as.integer(id),
      name = as.character(if ("name" %in% names(raw)) name else NA_character_),
      size = as.numeric(if ("size" %in% names(raw)) size else NA_real_),
      download_url = as.character(if ("download_url" %in% names(raw)) download_url else NA_character_),
      computed_md5 = as.character(if ("computed_md5" %in% names(raw)) computed_md5 else NA_character_)
    )
}

# == Collections ===============================================================

#' Search Figshare collections
#'
#' Search for Figshare collections, which group related articles together.
#'
#' @param query Character. Search query string.
#' @param page_size Integer. Number of results per page (default 10).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Collection ID.}
#'     \item{title}{Character. Collection title.}
#'     \item{doi}{Character. DOI.}
#'     \item{url}{Character. Figshare URL.}
#'     \item{published_date}{Date. Publication date.}
#'     \item{articles_count}{Integer. Number of articles in the collection.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' figshare_collections("genomics")
#' }
figshare_collections <- function(query, page_size = 10, page = 1) {
  schema <- tibble(id = integer(), title = character(), doi = character(),
                   url = character(), published_date = as.Date(character()),
                   articles_count = integer())
  body <- list(search_for = query, page_size = page_size, page = page)
  raw <- tryCatch(.post_json(paste0(.figshare_base, "/collections/search"), body),
                  error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)
  if (!is.data.frame(raw)) return(schema)

  as_tibble(raw) |>
    transmute(
      id = as.integer(id),
      title = as.character(if ("title" %in% names(raw)) title else NA_character_),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA_character_),
      url = as.character(if ("url" %in% names(raw)) url else NA_character_),
      published_date = tryCatch(as.Date(if ("published_date" %in% names(raw)) published_date else NA), error = function(e) as.Date(NA)),
      articles_count = as.integer(if ("articles_count" %in% names(raw)) articles_count else NA_integer_)
    )
}

# == Context ===================================================================

#' Get figshare.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
figshare_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(figshare_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/figshare.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "figshare.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# figshare.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# figshare.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
