# ncbi.nlm.nih.gov.R - Self-contained ncbi.nlm.nih.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# ncbi-nlm-nih-gov.R
# Self-contained NCBI E-utilities client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: api_key optional (raises rate limit from 3/sec to 10/sec)
# API: https://eutils.ncbi.nlm.nih.gov/entrez/eutils


.ua <- "support@scrapeable.com"
.ncbi_base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |> httr2::req_headers(`User-Agent` = .ua) |> httr2::req_perform(path = tmp)
  tmp
}
.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.schema_search <- tibble(id = character(), count = integer())
.schema_summary <- tibble(uid = character(), name = character(), description = character())


#' Search NCBI databases via E-utilities
#'
#' Query any NCBI Entrez database (gene, pubmed, protein, nucleotide, etc.)
#' using the ESearch endpoint. Returns matching record IDs and a total hit count.
#'
#' @details
#' Wraps the NCBI ESearch E-utility
#' (\url{https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}).
#' The returned IDs can be passed to \code{\link{ncbi_summary}} to fetch
#' document summaries. Without an API key the rate limit is 3 requests per
#' second; with a key it rises to 10.
#'
#' @param db Character. Database name: \code{"gene"}, \code{"pubmed"},
#'   \code{"protein"}, \code{"nucleotide"}, \code{"snp"}, \code{"taxonomy"},
#'   etc. Full list at \url{https://eutils.ncbi.nlm.nih.gov/entrez/eutils/einfo.fcgi}.
#' @param term Character. Search term using Entrez query syntax, e.g.
#'   \code{"BRCA1"}, \code{"cancer AND 2024[pdat]"}.
#' @param retmax Integer. Maximum number of IDs to return (default 20, max 10 000).
#' @param api_key Character or \code{NULL}. Optional NCBI API key obtained from
#'   \url{https://www.ncbi.nlm.nih.gov/account/settings/}.
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. NCBI record identifier.}
#'   \item{count}{Integer. Total number of records matching the query.}
#' }
#' @export
#' @seealso \code{\link{ncbi_summary}}, \code{\link{ncbi_pubmed}}, \code{\link{ncbi_genes}}
#' @examples
#' ncbi_search("gene", "BRCA1", retmax = 5)
#' ncbi_search("pubmed", "CRISPR AND 2024[pdat]", retmax = 10)
ncbi_search <- function(db, term, retmax = 20, api_key = NULL) {
  url <- sprintf("%s/esearch.fcgi?db=%s&term=%s&retmode=json&retmax=%d",
                 .ncbi_base, db, utils::URLencode(term, reserved = TRUE), retmax)
  if (!is.null(api_key)) url <- paste0(url, "&api_key=", api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("NCBI error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_search)
  result <- raw$esearchresult
  ids <- result$idlist
  if (is.null(ids) || length(ids) == 0) return(tibble(id = character(), count = as.integer(result$count %||% 0L)))
  tibble(id = as.character(ids), count = as.integer(result$count %||% length(ids)))
}

#' Fetch document summaries for NCBI record IDs
#'
#' Retrieve brief metadata (name and description) for a set of NCBI IDs
#' returned by \code{\link{ncbi_search}}. Uses the ESummary E-utility.
#'
#' @details
#' The exact content of the \code{name} and \code{description} columns depends
#' on the database. For \code{"pubmed"} they map to article title and journal
#' name; for \code{"gene"} they map to gene symbol and full name. Batch size is
#' limited by the NCBI server (typically 500 IDs per request).
#'
#' @param db Character. Database name (must match the db used in
#'   \code{\link{ncbi_search}}).
#' @param ids Character vector. One or more NCBI record IDs.
#' @param api_key Character or \code{NULL}. Optional NCBI API key.
#' @return A tibble with columns:
#' \describe{
#'   \item{uid}{Character. NCBI record identifier.}
#'   \item{name}{Character. Record name, title, or symbol (database-dependent).}
#'   \item{description}{Character. Extended description or journal name.}
#' }
#' @export
#' @seealso \code{\link{ncbi_search}}
#' @examples
#' ids <- ncbi_search("gene", "TP53", retmax = 3)
#' ncbi_summary("gene", ids$id)
ncbi_summary <- function(db, ids, api_key = NULL) {
  id_str <- paste(ids, collapse = ",")
  url <- sprintf("%s/esummary.fcgi?db=%s&id=%s&retmode=json", .ncbi_base, db, id_str)
  if (!is.null(api_key)) url <- paste0(url, "&api_key=", api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("NCBI error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_summary)
  result <- raw$result
  uids <- result$uids
  if (is.null(uids) || length(uids) == 0) return(.schema_summary)
  entries <- lapply(uids, function(uid) {
    r <- result[[uid]]
    if (is.null(r)) return(NULL)
    tibble(uid = as.character(uid),
           name = as.character(r$name %||% r$title %||% NA_character_),
           description = as.character(r$description %||% r$fulljournalname %||% NA_character_))
  })
  bind_rows(entries)
}

#' Search PubMed for biomedical literature
#'
#' Convenience wrapper that searches PubMed and returns article summaries
#' in a single call. Combines \code{\link{ncbi_search}} and
#' \code{\link{ncbi_summary}} for the \code{"pubmed"} database.
#'
#' @details
#' PubMed indexes over 36 million citations from MEDLINE, life-science
#' journals, and online books. The query supports full Entrez syntax
#' including field tags such as \code{[pdat]} (publication date),
#' \code{[au]} (author), and Boolean operators.
#'
#' @param query Character. PubMed search query, e.g.
#'   \code{"CRISPR AND 2024[pdat]"}, \code{"cancer immunotherapy[tiab]"}.
#' @param retmax Integer. Maximum articles to return (default 20).
#' @param api_key Character or \code{NULL}. Optional NCBI API key.
#' @return A tibble with columns:
#' \describe{
#'   \item{uid}{Character. PubMed ID (PMID).}
#'   \item{name}{Character. Article title.}
#'   \item{description}{Character. Full journal name.}
#' }
#' @export
#' @seealso \code{\link{ncbi_search}}, \code{\link{ncbi_genes}}
#' @examples
#' ncbi_pubmed("CRISPR", retmax = 5)
#' ncbi_pubmed("Alzheimer AND review[pt]", retmax = 10)
ncbi_pubmed <- function(query, retmax = 20, api_key = NULL) {
  ids <- ncbi_search("pubmed", query, retmax = retmax, api_key = api_key)
  if (nrow(ids) == 0) return(.schema_summary)
  ncbi_summary("pubmed", ids$id, api_key = api_key)
}

#' Search the NCBI Gene database
#'
#' Convenience wrapper that searches the NCBI Gene database and returns
#' gene summaries. Combines \code{\link{ncbi_search}} and
#' \code{\link{ncbi_summary}} for the \code{"gene"} database.
#'
#' @details
#' NCBI Gene provides curated gene records for sequenced genomes. Each result
#' includes the official gene symbol and a brief description. For richer
#' annotations (e.g. GO terms, pathways), use the returned \code{uid} values
#' with NCBI web resources.
#'
#' @param query Character. Gene name, symbol, or keyword, e.g. \code{"BRCA1"},
#'   \code{"TP53"}, \code{"insulin receptor"}.
#' @param retmax Integer. Maximum genes to return (default 10).
#' @param api_key Character or \code{NULL}. Optional NCBI API key.
#' @return A tibble with columns:
#' \describe{
#'   \item{uid}{Character. NCBI Gene ID.}
#'   \item{name}{Character. Official gene symbol.}
#'   \item{description}{Character. Full gene name / description.}
#' }
#' @export
#' @seealso \code{\link{ncbi_search}}, \code{\link{ncbi_pubmed}}
#' @examples
#' ncbi_genes("BRCA1")
#' ncbi_genes("tumor suppressor", retmax = 5)
ncbi_genes <- function(query, retmax = 10, api_key = NULL) {
  ids <- ncbi_search("gene", query, retmax = retmax, api_key = api_key)
  if (nrow(ids) == 0) return(.schema_summary)
  ncbi_summary("gene", ids$id, api_key = api_key)
}

# == Context ===================================================================

#' Get ncbi.nlm.nih.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ncbi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ncbi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ncbi.nlm.nih.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ncbi.nlm.nih.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ncbi.nlm.nih.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ncbi.nlm.nih.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
