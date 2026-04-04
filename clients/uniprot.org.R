# uniprot.org.R - Self-contained uniprot.org client

library(httr2)
library(jsonlite)
library(tibble)



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://rest.uniprot.org/uniprotkb"

#' Search UniProt proteins
#'
#' Queries the UniProt REST API to search for protein entries by gene name,
#' protein name, organism, function keyword, or any free-text query.
#' Returns reviewed (Swiss-Prot) and unreviewed (TrEMBL) entries.
#'
#' @param query Character. Search term -- can be a gene name (e.g. "INS"),
#'   protein name (e.g. "insulin"), organism (e.g. "homo sapiens"),
#'   or a complex query using UniProt query syntax.
#' @param size Integer. Number of results to return (default 25).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{Character. Primary UniProt accession (e.g. "P01308").}
#'     \item{id}{Character. UniProt entry name (e.g. "INS_HUMAN").}
#'     \item{protein_name}{Character. Recommended full protein name.}
#'     \item{organism}{Character. Scientific name of the source organism.}
#'     \item{length}{Integer. Sequence length in amino acids.}
#'   }
#'
#' @examples
#' \dontrun{
#' uniprot_search("insulin")
#' uniprot_search("BRCA1", size = 10)
#' uniprot_search("organism_name:human AND gene:TP53")
#' }
#'
#' @seealso [uniprot_entry()]
#' @export
uniprot_search <- function(query, size = 25L) {
  url <- sprintf("%s/search?query=%s&size=%d&format=json", .base, URLencode(query, TRUE), size)
  raw <- .fetch_json(url)
  results <- raw$results
  if (length(results) == 0) return(tibble::tibble(accession = character(), id = character(), protein_name = character()))
  tibble::tibble(
    accession = vapply(results, function(x) x$primaryAccession %||% NA_character_, character(1)),
    id = vapply(results, function(x) x$uniProtkbId %||% NA_character_, character(1)),
    protein_name = vapply(results, function(x) tryCatch(x$proteinDescription$recommendedName$fullName$value, error = function(e) NA_character_), character(1)),
    organism = vapply(results, function(x) tryCatch(x$organism$scientificName, error = function(e) NA_character_), character(1)),
    length = vapply(results, function(x) x$sequence$length %||% NA_integer_, integer(1))
  )
}

#' Get a specific UniProt entry
#'
#' Fetches detailed information for a single UniProt protein entry
#' by its primary accession number.
#'
#' @param accession Character. UniProt primary accession number
#'   (e.g. "P01308" for human insulin, "P04637" for human p53).
#'
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{accession}{Character. Primary UniProt accession.}
#'     \item{id}{Character. UniProt entry name (e.g. "INS_HUMAN").}
#'     \item{protein_name}{Character. Recommended full protein name.}
#'     \item{organism}{Character. Scientific name of the source organism.}
#'     \item{length}{Integer. Sequence length in amino acids.}
#'   }
#'
#' @examples
#' \dontrun{
#' uniprot_entry("P01308")
#' uniprot_entry("P04637")
#' }
#'
#' @seealso [uniprot_search()]
#' @export
uniprot_entry <- function(accession) {
  url <- sprintf("%s/%s.json", .base, accession)
  raw <- .fetch_json(url)
  tibble::tibble(
    accession = raw$primaryAccession %||% NA_character_,
    id = raw$uniProtkbId %||% NA_character_,
    protein_name = tryCatch(raw$proteinDescription$recommendedName$fullName$value, error = function(e) NA_character_),
    organism = tryCatch(raw$organism$scientificName, error = function(e) NA_character_),
    length = raw$sequence$length %||% NA_integer_
  )
}

# == Context ===================================================================

#' Get uniprot.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
uniprot_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(uniprot_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/uniprot.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "uniprot.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# uniprot.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# uniprot.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
