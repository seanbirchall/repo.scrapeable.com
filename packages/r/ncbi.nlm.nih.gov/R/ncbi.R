#' Search NCBI databases
#'
#' Search any NCBI database (gene, pubmed, protein, nucleotide, etc.)
#' Returns a list of IDs matching the query.
#'
#' @param db Database name: "gene", "pubmed", "protein", "nucleotide", etc.
#' @param term Search term (e.g. "BRCA1", "cancer AND 2024\[pdat\]")
#' @param retmax Max results (default 20, max 10000)
#' @param api_key Optional API key (raises rate limit to 10/sec)
#' @return tibble: id (character), count (integer, total matches)
#' @export
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

#' Get summaries for NCBI IDs
#'
#' @param db Database name
#' @param ids Character vector of NCBI IDs (from ncbi_search)
#' @param api_key Optional API key
#' @return tibble: uid, name, description (columns vary by database)
#' @export
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

#' Search PubMed articles
#'
#' @param query Search query (e.g. "CRISPR AND 2024\[pdat\]")
#' @param retmax Max results (default 20)
#' @param api_key Optional API key
#' @return tibble: uid, name (title), description (journal)
#' @export
ncbi_pubmed <- function(query, retmax = 20, api_key = NULL) {
  ids <- ncbi_search("pubmed", query, retmax = retmax, api_key = api_key)
  if (nrow(ids) == 0) return(.schema_summary)
  ncbi_summary("pubmed", ids$id, api_key = api_key)
}

#' Search NCBI genes
#'
#' @param query Gene name or symbol (e.g. "BRCA1", "TP53")
#' @param retmax Max results (default 10)
#' @param api_key Optional API key
#' @return tibble: uid, name, description
#' @export
ncbi_genes <- function(query, retmax = 10, api_key = NULL) {
  ids <- ncbi_search("gene", query, retmax = retmax, api_key = api_key)
  if (nrow(ids) == 0) return(.schema_summary)
  ncbi_summary("gene", ids$id, api_key = api_key)
}

#' Generate context
#' @return Character string (invisibly)
#' @export
ncbi_context <- function() {
  .build_context("ncbi.nlm.nih.gov", header_lines = c(
    "# ncbi.nlm.nih.gov - NCBI E-utilities Client for R",
    "# Auth: api_key optional (free, raises rate to 10/sec)",
    "# Databases: gene, pubmed, protein, nucleotide, snp, etc.",
    "# Rate limit: 3/sec without key, 10/sec with key"
  ))
}
