# api.osf.io.R - Self-contained api.osf.io client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# api-osf-io.R
# Self-contained Open Science Framework (OSF) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: none documented for read-only access


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osf_base <- "https://api.osf.io/v2"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_preprints <- tibble(
  id = character(), title = character(), description = character(),
  date_created = as.POSIXct(character()), date_published = as.POSIXct(character()),
  doi = character(), provider = character()
)

.schema_nodes <- tibble(
  id = character(), title = character(), description = character(),
  category = character(), date_created = as.POSIXct(character()),
  date_modified = as.POSIXct(character()), public = logical(),
  tags = character()
)



# == Preprints =================================================================

#' Fetch preprints from the Open Science Framework
#'
#' Returns recent preprints from the OSF v2 API, optionally filtered by
#' provider. Covers OSF Preprints and partner services (SocArXiv, PsyArXiv,
#' etc.).
#'
#' @param provider Character. Preprint provider filter (default \code{"osf"}).
#'   Other values: \code{"socarxiv"}, \code{"psyarxiv"}, \code{"engrxiv"},
#'   \code{"biohackrxiv"}.
#' @param page_size Integer. Number of results per page (default 25, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. OSF preprint identifier.}
#'     \item{title}{Character. Preprint title.}
#'     \item{description}{Character. Abstract or description text.}
#'     \item{date_created}{POSIXct. Creation timestamp (UTC).}
#'     \item{date_published}{POSIXct. Publication timestamp (UTC).}
#'     \item{doi}{Character. DOI if assigned (\code{NA} otherwise).}
#'     \item{provider}{Character. Preprint provider name.}
#'   }
#' @examples
#' osf_preprints("osf", page_size = 5)
#' osf_preprints("socarxiv", page_size = 3)
#' @export
osf_preprints <- function(provider = "osf", page_size = 25) {
  url <- sprintf("%s/preprints/?filter[provider]=%s&page[size]=%d",
                 .osf_base, provider, page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_preprints)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tibble(
      id             = as.character(item$id %||% NA),
      title          = as.character(a$title %||% NA),
      description    = as.character(a$description %||% NA),
      date_created   = as.POSIXct(a$date_created %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_published = as.POSIXct(a$date_published %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      doi            = as.character(a$doi %||% NA),
      provider       = provider
    )
  })
  bind_rows(rows)
}


# == Nodes (projects) ==========================================================

#' Search OSF nodes (projects and components)
#'
#' Searches public OSF nodes (projects, components, registrations) by title.
#' Returns metadata including category, timestamps, and tags.
#'
#' @param query Character. Title search filter (e.g. \code{"climate"},
#'   \code{"replication"}).
#' @param page_size Integer. Number of results per page (default 25, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. OSF node identifier (5-character alphanumeric).}
#'     \item{title}{Character. Project or component title.}
#'     \item{description}{Character. Project description (may be empty).}
#'     \item{category}{Character. Node category (e.g. "project", "analysis").}
#'     \item{date_created}{POSIXct. Creation timestamp (UTC).}
#'     \item{date_modified}{POSIXct. Last-modified timestamp (UTC).}
#'     \item{public}{Logical. Whether the node is publicly visible.}
#'     \item{tags}{Character. Comma-separated tag list.}
#'   }
#' @examples
#' osf_nodes("climate", page_size = 5)
#' @export
osf_nodes <- function(query, page_size = 25) {
  url <- sprintf("%s/nodes/?filter[title]=%s&page[size]=%d",
                 .osf_base, utils::URLencode(query), page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_nodes)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tags_str <- paste(unlist(a$tags %||% list()), collapse = ", ")
    tibble(
      id            = as.character(item$id %||% NA),
      title         = as.character(a$title %||% NA),
      description   = as.character(a$description %||% NA),
      category      = as.character(a$category %||% NA),
      date_created  = as.POSIXct(a$date_created %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_modified = as.POSIXct(a$date_modified %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      public        = as.logical(a$public %||% NA),
      tags          = tags_str
    )
  })
  bind_rows(rows)
}


# == Context ===================================================================

#' Get osf.io client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
osf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(osf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/osf.io.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "osf.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# osf.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# osf.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
