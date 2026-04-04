# eric.ed.gov.R - Self-contained eric.ed.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# eric.R
# Self-contained ERIC (Education Resources Information Center) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.eric_base <- "https://api.ies.ed.gov/eric/"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), title = character(), author = character(),
  source = character(), publicationdate = character(),
  description = character(), subject = character(),
  url = character()
)

# == Search ====================================================================

#' Search the ERIC education research database
#'
#' Queries the ERIC (Education Resources Information Center) API for
#' journal articles, reports, and other education research documents.
#' Supports pagination for retrieving large result sets.
#'
#' @param query Character. Search query using ERIC syntax (e.g.
#'   \code{"mathematics instruction"}, \code{"STEM education"},
#'   \code{"early childhood AND literacy"}).
#' @param rows Integer. Number of results to return (default 10, API max 200).
#' @param start Integer. Starting record offset for pagination (default 0).
#'   Use \code{start = 10} to get the second page of 10 results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ERIC document ID (e.g. \code{"EJ1234567"}).}
#'     \item{title}{Character. Document title.}
#'     \item{author}{Character. Semicolon-separated author names.}
#'     \item{source}{Character. Publication source/journal name.}
#'     \item{publicationdate}{Character. Publication date string.}
#'     \item{description}{Character. Abstract/description text.}
#'     \item{subject}{Character. Semicolon-separated subject descriptors.}
#'     \item{url}{Character. ERIC detail page URL.}
#'   }
#' @examples
#' eric_search("STEM education", rows = 5)
#' eric_search("early childhood literacy", rows = 10, start = 10)
#' @export
eric_search <- function(query, rows = 10, start = 0) {
  url <- paste0(.eric_base, "?search=", utils::URLencode(query),
                "&rows=", rows, "&start=", start, "&format=json")
  raw <- .fetch_json(url)
  d <- raw$response$docs
  if (is.null(d) || length(d) == 0) return(.schema_search)

  # author and subject may be list columns
  .collapse_col <- function(x) {
    if (is.list(x)) vapply(x, function(v) paste(v, collapse = "; "), character(1))
    else as.character(x)
  }

  as_tibble(d) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      author = .collapse_col(if ("author" %in% names(d)) author else NA_character_),
      source = as.character(if ("source" %in% names(d)) source else NA_character_),
      publicationdate = as.character(if ("publicationdate" %in% names(d)) publicationdate else NA_character_),
      description = as.character(if ("description" %in% names(d)) description else NA_character_),
      subject = .collapse_col(if ("subject" %in% names(d)) subject else NA_character_),
      url = paste0("https://eric.ed.gov/?id=", id)
    )
}

#' Get detailed metadata for a single ERIC document
#'
#' Retrieves extended metadata for one ERIC document by its ID, including
#' peer-review status and publication type not available in search results.
#'
#' @param eric_id Character. ERIC document ID (e.g. \code{"EJ1234567"}
#'   for journal articles, \code{"ED654321"} for reports/gray literature).
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{id}{Character. ERIC document ID.}
#'     \item{title}{Character. Document title.}
#'     \item{author}{Character. Semicolon-separated author names.}
#'     \item{source}{Character. Journal or publisher name.}
#'     \item{publicationdate}{Character. Publication date string.}
#'     \item{description}{Character. Abstract text.}
#'     \item{subject}{Character. Semicolon-separated subject descriptors.}
#'     \item{url}{Character. ERIC detail page URL.}
#'     \item{peerreviewed}{Character. Peer review status (\code{"T"}/\code{"F"}).}
#'     \item{publicationtype}{Character. Semicolon-separated publication types.}
#'   }
#' @examples
#' eric_document("EJ1234567")
#' @export
eric_document <- function(eric_id) {
  url <- paste0(.eric_base, "?search=id:", eric_id, "&rows=1&format=json")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_search)

  d <- raw$response$docs
  if (is.null(d) || length(d) == 0) return(.schema_search)

  .collapse_col <- function(x) {
    if (is.list(x)) vapply(x, function(v) paste(v, collapse = "; "), character(1))
    else as.character(x)
  }

  nms <- names(d)
  as_tibble(d) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      author = .collapse_col(if ("author" %in% nms) author else NA_character_),
      source = as.character(if ("source" %in% nms) source else NA_character_),
      publicationdate = as.character(if ("publicationdate" %in% nms) publicationdate else NA_character_),
      description = as.character(if ("description" %in% nms) description else NA_character_),
      subject = .collapse_col(if ("subject" %in% nms) subject else NA_character_),
      url = paste0("https://eric.ed.gov/?id=", id),
      peerreviewed = as.character(if ("peerreviewed" %in% nms) peerreviewed else NA_character_),
      publicationtype = .collapse_col(if ("publicationtype" %in% nms) publicationtype else NA_character_)
    )
}

#' Search ERIC with faceted result counts
#'
#' Performs an ERIC search and additionally returns facet (category) counts
#' for a chosen field, showing how results distribute across subjects,
#' authors, publication types, etc.
#'
#' @param query Character. Search query (same syntax as \code{eric_search()}).
#' @param rows Integer. Number of document results to return (default 10).
#' @param facet_field Character. Field to facet on. One of \code{"subject"},
#'   \code{"author"}, \code{"publicationtype"}, \code{"source"}, or
#'   \code{"educationlevel"} (default \code{"subject"}).
#' @return A named list with two elements:
#'   \describe{
#'     \item{results}{Tibble of search results (same structure as
#'       \code{eric_search()} output).}
#'     \item{facets}{Tibble with columns \code{value} (character, facet
#'       category name) and \code{count} (integer, number of matching
#'       documents).}
#'   }
#' @examples
#' res <- eric_facets("STEM education", rows = 5, facet_field = "subject")
#' res$results
#' res$facets
#' @export
eric_facets <- function(query, rows = 10, facet_field = "subject") {
  url <- paste0(.eric_base, "?search=", utils::URLencode(query),
                "&rows=", rows, "&format=json",
                "&facet=on&facet.field=", facet_field)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(list(results = .schema_search,
                                facets = tibble(value = character(), count = integer())))

  # Parse results
  results <- eric_search(query, rows = rows)

  # Parse facets
  fc <- raw$facet_counts$facet_fields
  if (is.null(fc)) {
    facets <- tibble(value = character(), count = integer())
  } else {
    fvals <- fc[[facet_field]]
    if (is.null(fvals) || length(fvals) == 0) {
      facets <- tibble(value = character(), count = integer())
    } else {
      # Facets come as alternating value, count pairs
      idx_vals <- seq(1, length(fvals), 2)
      idx_cnts <- seq(2, length(fvals), 2)
      facets <- tibble(
        value = as.character(fvals[idx_vals]),
        count = as.integer(fvals[idx_cnts])
      ) |> filter(count > 0)
    }
  }

  list(results = results, facets = facets)
}

# == Context ===================================================================

#' Get eric.ed.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
eric_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(eric_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/eric.ed.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "eric.ed.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# eric.ed.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# eric.ed.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
