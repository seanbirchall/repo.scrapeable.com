# data.europa.eu.R - Self-contained data.europa.eu client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# data-europa-eu.R
# Self-contained EU Open Data Portal API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.eudata_base <- "https://data.europa.eu/api/hub/search"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
  id = character(), title = character(), description = character(),
  country = character(), catalog = character(), modified = character(),
  format = character()
)

.schema_dataset <- tibble(
  id = character(), title = character(), description = character(),
  country = character(), catalog = character(), publisher = character(),
  modified = character(), issued = character(), format = character(),
  resource_url = character()
)

# == Parsers ===================================================================

.safe_text <- function(x, lang = "en") {
  if (is.null(x)) return(NA_character_)
  if (is.character(x)) return(x)
  if (is.list(x)) {
    val <- x[[lang]] %||% x[[1]] %||% NA_character_
    return(as.character(val))
  }
  as.character(x)
}


# == Public functions ==========================================================

#' Search the EU Open Data Portal
#'
#' Queries the data.europa.eu search API for open datasets published by
#' EU institutions, agencies, and member states.
#'
#' @param query Search term (e.g. "climate", "transport", "energy",
#'   "agriculture", "covid")
#' @param limit Number of results (default 10, max 100)
#' @param page Page number (default 0, zero-indexed)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Dataset identifier (character)}
#'     \item{title}{Dataset title in English when available (character)}
#'     \item{description}{Dataset description in English (character)}
#'     \item{country}{Publishing country label (character)}
#'     \item{catalog}{Source catalog title (character)}
#'     \item{modified}{Last modification date as ISO string (character)}
#'     \item{format}{Available distribution formats, comma-separated (character)}
#'   }
#' @examples
#' eudata_search("climate")
#' eudata_search("transport", limit = 5)
#' @seealso [eudata_dataset()], [eudata_context()]
#' @source <https://data.europa.eu/api/hub/search>
#' @export
eudata_search <- function(query, limit = 10, page = 0) {
  url <- sprintf("%s/search?q=%s&limit=%d&page=%d",
                 .eudata_base,
                 utils::URLencode(query, reserved = TRUE),
                 as.integer(limit), as.integer(page))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$result) || is.null(raw$result$results) ||
      length(raw$result$results) == 0) {
    return(.schema_search)
  }

  rows <- lapply(raw$result$results, function(x) {
    formats <- tryCatch({
      dists <- x$distributions
      if (!is.null(dists) && length(dists) > 0) {
        fmts <- vapply(dists, function(d) as.character(d$format$label %||% d$mediaType %||% ""), character(1))
        paste(unique(fmts[fmts != ""]), collapse = ", ")
      } else NA_character_
    }, error = function(e) NA_character_)

    tibble(
      id          = as.character(x$identifier[[1]] %||% NA),
      title       = .safe_text(x$title),
      description = .safe_text(x$description),
      country     = as.character(x$country$label %||% NA),
      catalog     = as.character(x$catalog$title[[1]] %||% NA),
      modified    = as.character(x$modified %||% NA),
      format      = formats
    )
  })
  bind_rows(rows)
}

#' Get details for a specific EU dataset
#'
#' Fetches full metadata for a single dataset from data.europa.eu,
#' including publisher, distribution format, and first resource URL.
#'
#' @param id Dataset identifier string (from eudata_search results)
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Dataset identifier (character)}
#'     \item{title}{Dataset title (character)}
#'     \item{description}{Full description (character)}
#'     \item{country}{Publishing country (character)}
#'     \item{catalog}{Source catalog title (character)}
#'     \item{publisher}{Publishing organization name (character)}
#'     \item{modified}{Last modified date (character)}
#'     \item{issued}{Date originally published (character)}
#'     \item{format}{Available formats, comma-separated (character)}
#'     \item{resource_url}{Access URL for the first distribution (character)}
#'   }
#' @examples
#' eudata_dataset("groenklimaatassen-gent")
#' @seealso [eudata_search()], [eudata_context()]
#' @source <https://data.europa.eu/api/hub/search>
#' @export
eudata_dataset <- function(id) {
  url <- sprintf("%s/datasets/%s", .eudata_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$result)) return(.schema_dataset)

  x <- raw$result
  formats <- tryCatch({
    dists <- x$distributions
    if (!is.null(dists) && length(dists) > 0) {
      fmts <- vapply(dists, function(d) as.character(d$format$label %||% d$mediaType %||% ""), character(1))
      paste(unique(fmts[fmts != ""]), collapse = ", ")
    } else NA_character_
  }, error = function(e) NA_character_)

  resource_url <- tryCatch({
    dists <- x$distributions
    if (!is.null(dists) && length(dists) > 0) {
      as.character(dists[[1]]$accessUrl %||% dists[[1]]$downloadUrl %||% NA)
    } else NA_character_
  }, error = function(e) NA_character_)

  tibble(
    id           = as.character(x$identifier[[1]] %||% NA),
    title        = .safe_text(x$title),
    description  = .safe_text(x$description),
    country      = as.character(x$country$label %||% NA),
    catalog      = as.character(x$catalog$title[[1]] %||% NA),
    publisher    = as.character(x$catalog$publisher$name %||% NA),
    modified     = as.character(x$modified %||% NA),
    issued       = as.character(x$issued %||% NA),
    format       = formats,
    resource_url = resource_url
  )
}

# == Context ===================================================================

#' Get europa.eu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
eudata_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(eudata_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/europa.eu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "europa.eu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# europa.eu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# europa.eu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
