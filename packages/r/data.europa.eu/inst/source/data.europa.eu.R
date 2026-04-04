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
#' Searches across all datasets published on the European Union Open Data
#' Portal (data.europa.eu), which aggregates open data from EU institutions
#' and member states. Returns dataset metadata including available formats.
#' Use the returned \code{id} with \code{eudata_dataset()} for full details.
#'
#' @param query Character. Free-text search term. Examples: \code{"climate"},
#'   \code{"transport"}, \code{"energy"}, \code{"agriculture"},
#'   \code{"health"}, \code{"covid"}, \code{"budget"}.
#' @param limit Integer. Number of results per page (default 10, max 100).
#' @param page Integer. Zero-indexed page number for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Dataset identifier for use with \code{eudata_dataset()}.}
#'     \item{title}{Character. Dataset title (English preferred, falls back to first available language).}
#'     \item{description}{Character. Dataset description.}
#'     \item{country}{Character. Country of origin (e.g. \code{"Belgium"}, \code{"France"}).}
#'     \item{catalog}{Character. Name of the source data catalog.}
#'     \item{modified}{Character. Date string of last modification (ISO 8601 format, may be NA).}
#'     \item{format}{Character. Comma-separated list of available formats (e.g. \code{"CSV, JSON, RDF"}).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' eudata_search("climate", limit = 5)
#' eudata_search("transport", limit = 20, page = 1)
#' }
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
#' Retrieves full metadata for a single dataset on the EU Open Data Portal,
#' including publisher, issue date, available formats, and a direct resource
#' URL for the first distribution.
#'
#' @param id Character. Dataset identifier as returned by
#'   \code{eudata_search()} (e.g. \code{"groenklimaatassen-gent"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. Dataset identifier.}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Full description.}
#'     \item{country}{Character. Country of origin.}
#'     \item{catalog}{Character. Source catalog name.}
#'     \item{publisher}{Character. Publishing organization name.}
#'     \item{modified}{Character. Last modification date (ISO 8601, may be NA).}
#'     \item{issued}{Character. Original publication date (may be NA).}
#'     \item{format}{Character. Comma-separated available formats (e.g. \code{"CSV, JSON"}).}
#'     \item{resource_url}{Character. Direct access URL for the first distribution.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' eudata_dataset("groenklimaatassen-gent")
#' }
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

#' Get data.europa.eu client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.europa.eu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.europa.eu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.europa.eu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.europa.eu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
