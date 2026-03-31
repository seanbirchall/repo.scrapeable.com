# == Public functions ==========================================================

#' Search UniProt for proteins
#'
#' @param query Search query (e.g. "BRCA1", "insulin", "kinase AND human")
#' @param size Number of results (default 25, max 500)
#' @return tibble: accession, id, protein_name, gene_name, organism, length,
#'   annotation_score, entry_type
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
#' @param accession UniProt accession (e.g. "P38398" for BRCA1 human,
#'   "P00533" for EGFR human, "P04637" for TP53 human)
#' @return tibble with one row: accession, id, protein_name, gene_name,
#'   organism, taxon_id, length, sequence, annotation_score, entry_type
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

#' Generate LLM-friendly context for the rest.uniprot.org package
#'
#' @return Character string (invisibly), also printed
#' @export
uniprot_context <- function() {
  .build_context("rest.uniprot.org", header_lines = c(
    "# rest.uniprot.org - UniProt Protein Database Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: none documented (be respectful)",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common accessions: P38398 (BRCA1 human), P00533 (EGFR human),",
    "#   P04637 (TP53 human), P01308 (Insulin human)",
    "# Query syntax: gene:BRCA1, organism_name:human, length:[100 TO 500]"
  ))
}
