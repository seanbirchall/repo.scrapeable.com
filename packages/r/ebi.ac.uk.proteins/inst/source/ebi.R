# ebi-ac-uk-proteins.R
# Self-contained UniProt Proteins API client (via EBI).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ebi_base <- "https://www.ebi.ac.uk/proteins/api"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_protein <- tibble(
  accession = character(), id = character(), gene = character(),
  organism = character(), protein_name = character(),
  existence = character(), sequence_length = integer()
)

# == Public functions ==========================================================

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
