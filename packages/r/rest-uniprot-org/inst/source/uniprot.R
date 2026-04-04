# rest-uniprot-org.R
# Self-contained UniProt protein database API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented (be respectful)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.uniprot_base <- "https://rest.uniprot.org"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_search <- tibble(
  accession = character(), id = character(), protein_name = character(),
  gene_name = character(), organism = character(), length = integer(),
  annotation_score = numeric(), entry_type = character()
)

.schema_entry <- tibble(
  accession = character(), id = character(), protein_name = character(),
  gene_name = character(), organism = character(), taxon_id = integer(),
  length = integer(), sequence = character(), annotation_score = numeric(),
  entry_type = character()
)

# == Parsers ===================================================================

.parse_search_result <- function(item) {
  protein_name <- tryCatch(
    item$proteinDescription$recommendedName$fullName$value %||% NA_character_,
    error = function(e) NA_character_
  )
  gene_name <- tryCatch({
    genes <- item$genes
    if (!is.null(genes) && length(genes) > 0) {
      genes[[1]]$geneName$value %||% NA_character_
    } else NA_character_
  }, error = function(e) NA_character_)
  organism <- tryCatch(
    item$organism$scientificName %||% NA_character_,
    error = function(e) NA_character_
  )
  seq_len <- tryCatch(
    as.integer(item$sequence$length %||% NA),
    error = function(e) NA_integer_
  )

  tibble(
    accession        = as.character(item$primaryAccession %||% NA),
    id               = as.character(item$uniProtkbId %||% NA),
    protein_name     = as.character(protein_name),
    gene_name        = as.character(gene_name),
    organism         = as.character(organism),
    length           = seq_len,
    annotation_score = as.numeric(item$annotationScore %||% NA),
    entry_type       = as.character(item$entryType %||% NA)
  )
}

# == Public functions ==========================================================

#' Search UniProt for proteins
#'
#' Queries the UniProt REST API to search for protein entries matching a
#' free-text query. Supports Boolean operators (e.g., \code{"kinase AND human"})
#' and field-specific queries (e.g., \code{"gene:BRCA1 AND organism_id:9606"}).
#'
#' @param query Character string. Search query (e.g., \code{"BRCA1"},
#'   \code{"insulin"}, \code{"kinase AND human"}).
#' @param size Integer. Number of results to return (default 25, max 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{Character. UniProt accession (e.g., \code{"P38398"}).}
#'     \item{id}{Character. UniProt entry name (e.g., \code{"BRCA1_HUMAN"}).}
#'     \item{protein_name}{Character. Recommended protein name.}
#'     \item{gene_name}{Character. Primary gene name.}
#'     \item{organism}{Character. Scientific species name.}
#'     \item{length}{Integer. Protein sequence length (amino acids).}
#'     \item{annotation_score}{Numeric. UniProt annotation score (1--5).}
#'     \item{entry_type}{Character. Entry type (\code{"UniProtKB reviewed (Swiss-Prot)"}
#'       or \code{"UniProtKB unreviewed (TrEMBL)"}).}
#'   }
#'   Returns an empty tibble if no matches are found.
#' @examples
#' uniprot_search("BRCA1", size = 5)
#' uniprot_search("kinase AND organism_id:9606", size = 10)
#' @export
uniprot_search <- function(query, size = 25) {
  url <- sprintf("%s/uniprotkb/search?query=%s&size=%d&format=json",
                 .uniprot_base,
                 utils::URLencode(query, reserved = TRUE),
                 as.integer(size))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) {
    return(.schema_search)
  }

  rows <- lapply(raw$results, .parse_search_result)
  bind_rows(rows)
}

#' Get a UniProt entry by accession
#'
#' Retrieves full metadata and amino acid sequence for a single UniProt
#' protein entry identified by its accession number. Returns richer detail
#' than \code{\link{uniprot_search}}, including the full amino acid sequence
#' and NCBI taxonomy ID.
#'
#' @param accession Character string. UniProt accession number. Examples:
#'   \itemize{
#'     \item \code{"P38398"} -- BRCA1 (human)
#'     \item \code{"P00533"} -- EGFR (human)
#'     \item \code{"P04637"} -- TP53 (human)
#'   }
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{accession}{Character. UniProt accession.}
#'     \item{id}{Character. Entry name.}
#'     \item{protein_name}{Character. Recommended protein name.}
#'     \item{gene_name}{Character. Primary gene name.}
#'     \item{organism}{Character. Scientific species name.}
#'     \item{taxon_id}{Integer. NCBI taxonomy ID.}
#'     \item{length}{Integer. Sequence length (amino acids).}
#'     \item{sequence}{Character. Full amino acid sequence.}
#'     \item{annotation_score}{Numeric. Annotation score (1--5).}
#'     \item{entry_type}{Character. Swiss-Prot (reviewed) or TrEMBL (unreviewed).}
#'   }
#' @seealso \code{\link{uniprot_search}} to find accessions by query.
#' @examples
#' uniprot_entry("P38398")
#' @export
uniprot_entry <- function(accession) {
  url <- sprintf("%s/uniprotkb/%s.json", .uniprot_base, accession)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_entry)

  protein_name <- tryCatch(
    raw$proteinDescription$recommendedName$fullName$value %||% NA_character_,
    error = function(e) NA_character_
  )
  gene_name <- tryCatch({
    genes <- raw$genes
    if (!is.null(genes) && length(genes) > 0) {
      genes[[1]]$geneName$value %||% NA_character_
    } else NA_character_
  }, error = function(e) NA_character_)

  tibble(
    accession        = as.character(raw$primaryAccession %||% NA),
    id               = as.character(raw$uniProtkbId %||% NA),
    protein_name     = as.character(protein_name),
    gene_name        = as.character(gene_name),
    organism         = as.character(raw$organism$scientificName %||% NA),
    taxon_id         = as.integer(raw$organism$taxonId %||% NA),
    length           = as.integer(raw$sequence$length %||% NA),
    sequence         = as.character(raw$sequence$value %||% NA),
    annotation_score = as.numeric(raw$annotationScore %||% NA),
    entry_type       = as.character(raw$entryType %||% NA)
  )
}

# == Context ===================================================================

#' Get rest-uniprot-org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/rest-uniprot-org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "rest-uniprot-org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# rest-uniprot-org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# rest-uniprot-org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
