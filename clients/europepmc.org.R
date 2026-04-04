# europepmc.org.R - Self-contained europepmc.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr, warn.conflicts = FALSE)


# europepmc-org.R
# Self-contained Europe PMC biomedical literature API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented, be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.epmc_base <- "https://www.ebi.ac.uk/europepmc/webservices/rest"

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
  id = character(), source = character(), pmid = character(),
  pmcid = character(), title = character(), authors = character(),
  journal = character(), pub_year = integer(), cited_by = integer(),
  is_open_access = character()
)

.schema_citations <- tibble(
  id = character(), source = character(), title = character(),
  authors = character(), journal = character(), pub_year = integer()
)

# == Public functions ==========================================================

#' Search Europe PMC articles
#'
#' Searches the Europe PMC biomedical literature database (30+ million articles
#' from PubMed, PMC, patents, and more). Supports advanced query syntax
#' including field-specific searches and Boolean operators.
#'
#' @param query Character. Search query string. Supports Europe PMC advanced
#'   syntax: \code{"malaria"}, \code{"AUTH:Smith"}, \code{"TITLE:vaccine"},
#'   \code{"SRC:MED AND malaria"}, \code{"OPEN_ACCESS:Y AND cancer"}.
#' @param page_size Integer. Results per page, 1--1000 (default 25).
#' @param cursor_mark Character. Pagination cursor (default \code{"*"} for first
#'   page). Use \code{attr(result, "next_cursor")} from the previous call to
#'   fetch the next page.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Internal Europe PMC identifier.}
#'     \item{source}{Character. Source database (e.g. "MED", "PMC", "PAT").}
#'     \item{pmid}{Character. PubMed ID.}
#'     \item{pmcid}{Character. PubMed Central ID.}
#'     \item{title}{Character. Article title.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{journal}{Character. Journal title.}
#'     \item{pub_year}{Integer. Year of publication.}
#'     \item{cited_by}{Integer. Number of citations.}
#'     \item{is_open_access}{Character. Open access flag ("Y" or "N").}
#'   }
#'   Attributes: \code{"next_cursor"} for pagination, \code{"hit_count"} for total matches.
#' @export
#' @examples
#' \dontrun{
#' epmc_search("malaria vaccine")
#' epmc_search("AUTH:Smith AND TITLE:cancer", page_size = 50)
#' }
epmc_search <- function(query, page_size = 25, cursor_mark = "*") {
  url <- sprintf("%s/search?query=%s&format=json&pageSize=%d&cursorMark=%s",
                 .epmc_base, utils::URLencode(query), page_size,
                 utils::URLencode(cursor_mark))
  raw <- .fetch_json(url)
  results <- raw$resultList$result
  if (is.null(results) || nrow(results) == 0) return(.schema_search)

  out <- as_tibble(results) |>
    transmute(
      id = as.character(id),
      source = as.character(if ("source" %in% names(results)) source else NA),
      pmid = as.character(if ("pmid" %in% names(results)) pmid else NA),
      pmcid = as.character(if ("pmcid" %in% names(results)) pmcid else NA),
      title = as.character(title),
      authors = as.character(if ("authorString" %in% names(results)) authorString else NA),
      journal = as.character(if ("journalTitle" %in% names(results)) journalTitle else NA),
      pub_year = as.integer(if ("pubYear" %in% names(results)) pubYear else NA),
      cited_by = as.integer(if ("citedByCount" %in% names(results)) citedByCount else NA),
      is_open_access = as.character(if ("isOpenAccess" %in% names(results)) isOpenAccess else NA)
    )
  attr(out, "next_cursor") <- raw$nextCursorMark
  attr(out, "hit_count") <- raw$hitCount
  out
}

#' Fetch a single article from Europe PMC
#'
#' Retrieves metadata for a specific article by its identifier. Returns a
#' single-row tibble with the same schema as \code{epmc_search()}.
#'
#' @param id Character or numeric. Article identifier: PubMed ID (e.g.
#'   \code{"33205991"}), PMC ID (e.g. \code{"PMC7685130"}), or DOI.
#' @param source Character. Source database: \code{"MED"} (PubMed, default),
#'   \code{"PMC"}, \code{"PAT"} (patents), \code{"AGR"} (Agricola),
#'   \code{"CBA"} (Chinese Biological Abstracts), \code{"ETH"} (EThOS).
#' @return A tibble (single row) with columns: id, source, pmid, pmcid, title,
#'   authors, journal, pub_year, cited_by, is_open_access.
#' @export
#' @examples
#' \dontrun{
#' epmc_article("33205991")
#' epmc_article("PMC7685130", source = "PMC")
#' }
epmc_article <- function(id, source = "MED") {
  url <- sprintf("%s/search?query=EXT_ID:%s AND SRC:%s&format=json&pageSize=1",
                 .epmc_base, utils::URLencode(as.character(id)), source)
  raw <- .fetch_json(url)
  results <- raw$resultList$result
  if (is.null(results) || nrow(results) == 0) return(.schema_search)

  as_tibble(results) |>
    transmute(
      id = as.character(id),
      source = as.character(if ("source" %in% names(results)) source else NA),
      pmid = as.character(if ("pmid" %in% names(results)) pmid else NA),
      pmcid = as.character(if ("pmcid" %in% names(results)) pmcid else NA),
      title = as.character(title),
      authors = as.character(if ("authorString" %in% names(results)) authorString else NA),
      journal = as.character(if ("journalTitle" %in% names(results)) journalTitle else NA),
      pub_year = as.integer(if ("pubYear" %in% names(results)) pubYear else NA),
      cited_by = as.integer(if ("citedByCount" %in% names(results)) citedByCount else NA),
      is_open_access = as.character(if ("isOpenAccess" %in% names(results)) isOpenAccess else NA)
    )
}

#' Fetch citations for an article
#'
#' Returns articles that cite the specified article. Useful for forward
#' citation tracking and bibliometric analysis.
#'
#' @param id Character or numeric. Article identifier (PubMed ID or PMC ID).
#' @param source Character. Source database: \code{"MED"} (default), \code{"PMC"},
#'   \code{"PAT"}, etc.
#' @param page Integer. Page number, 1-indexed (default 1).
#' @param page_size Integer. Results per page (default 25, max 1000).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Citing article identifier.}
#'     \item{source}{Character. Source database of citing article.}
#'     \item{title}{Character. Title of citing article.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{journal}{Character. Journal abbreviation.}
#'     \item{pub_year}{Integer. Year of publication.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' epmc_citations("33205991")
#' epmc_citations("33205991", page = 2, page_size = 50)
#' }
epmc_citations <- function(id, source = "MED", page = 1, page_size = 25) {
  url <- sprintf("%s/%s/%s/citations?page=%d&pageSize=%d&format=json",
                 .epmc_base, source, id, page, page_size)
  raw <- .fetch_json(url)
  results <- raw$citationList$citation
  if (is.null(results) || length(results) == 0) return(.schema_citations)
  if (is.data.frame(results)) {
    as_tibble(results) |>
      transmute(
        id = as.character(id),
        source = as.character(if ("source" %in% names(results)) source else NA),
        title = as.character(if ("title" %in% names(results)) title else NA),
        authors = as.character(if ("authorString" %in% names(results)) authorString else NA),
        journal = as.character(if ("journalAbbreviation" %in% names(results)) journalAbbreviation else NA),
        pub_year = as.integer(if ("pubYear" %in% names(results)) pubYear else NA)
      )
  } else {
    .schema_citations
  }
}

#' Get europepmc.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
epmc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(epmc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/europepmc.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "europepmc.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# europepmc.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# europepmc.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
