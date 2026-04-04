#' Fetch a single UniProt protein entry by accession
#'
#' @param accession UniProt accession ID (e.g. "P12345", "P38398")
#' @return tibble: one row with accession, id, gene, organism, protein_name,
#'   existence, sequence_length
#' @export
ebi_protein <- function(accession) {
  url <- sprintf("%s/proteins/%s", .ebi_base, accession)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_protein)

  tibble(
    accession       = as.character(raw$accession %||% NA_character_),
    id              = as.character(raw$id %||% NA_character_),
    gene            = tryCatch(as.character(raw$gene[[1]]$name$value), error = function(e) NA_character_),
    organism        = tryCatch(as.character(raw$organism$names[[1]]$value), error = function(e) NA_character_),
    protein_name    = tryCatch(as.character(raw$protein$recommendedName$fullName$value), error = function(e) NA_character_),
    existence       = as.character(raw$proteinExistence %||% NA_character_),
    sequence_length = as.integer(raw$sequence$length %||% NA_integer_)
  )
}

#' Search UniProt proteins
#'
#' @param query Search query string (gene name, protein name, etc.)
#' @param size Number of results (default 10, max 100)
#' @param offset Starting offset for pagination (default 0)
#' @return tibble: accession, id, gene, organism, protein_name, existence,
#'   sequence_length
#' @export
ebi_search <- function(query, size = 10, offset = 0) {
  url <- sprintf("%s/proteins?offset=%d&size=%d&gene=%s",
                 .ebi_base, offset, size, utils::URLencode(query))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_protein)

  results <- if (is.list(raw) && !is.null(raw[[1]]$accession)) raw else list(raw)

  tibble(
    accession       = vapply(results, function(x) x$accession %||% NA_character_, character(1)),
    id              = vapply(results, function(x) x$id %||% NA_character_, character(1)),
    gene            = vapply(results, function(x)
      tryCatch(as.character(x$gene[[1]]$name$value), error = function(e) NA_character_), character(1)),
    organism        = vapply(results, function(x)
      tryCatch(as.character(x$organism$names[[1]]$value), error = function(e) NA_character_), character(1)),
    protein_name    = vapply(results, function(x) {
      v <- tryCatch(x$protein$recommendedName$fullName$value, error = function(e) NULL)
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
    }, character(1)),
    existence       = vapply(results, function(x) x$proteinExistence %||% NA_character_, character(1)),
    sequence_length = vapply(results, function(x) as.integer(x$sequence$length %||% NA_integer_), integer(1))
  )
}

#' Show EBI Proteins package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
ebi_context <- function() {
  .build_context(
    pkg_name = "ebi.ac.uk",
    header_lines = c(
      "# ebi.ac.uk",
      "# UniProt Proteins API Client (via EBI)",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Rate limits: unknown (be polite)"
    )
  )
}
