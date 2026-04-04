# rest.ensembl.org.R - Self-contained rest.ensembl.org client

library(httr2)
library(jsonlite)
library(tibble)


# rest-ensembl-org.R
# Self-contained Ensembl REST API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://rest.ensembl.org


.ua <- "support@scrapeable.com"
.ensembl_base <- "https://rest.ensembl.org"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}
.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.schema_gene <- tibble(
  id = character(), display_name = character(), species = character(),
  biotype = character(), description = character(),
  start = integer(), end = integer(), strand = integer(),
  seq_region_name = character()
)

.schema_variant <- tibble(
  id = character(), consequence_type = character(),
  alleles = character(), minor_allele = character(),
  maf = numeric()
)


#' Look up a gene by its symbol
#'
#' Queries the Ensembl REST API to resolve a gene symbol (e.g. HGNC name)
#' to its stable Ensembl gene ID and genomic coordinates. Works for any
#' species in Ensembl (human by default).
#'
#' @param symbol Official gene symbol (e.g. \code{"BRCA2"}, \code{"TP53"},
#'   \code{"EGFR"}).
#' @param species Ensembl species name in lower-case underscore format
#'   (default \code{"homo_sapiens"}). Other examples:
#'   \code{"mus_musculus"}, \code{"danio_rerio"}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Stable Ensembl gene ID, e.g. \code{"ENSG00000139618"} (character).}
#'     \item{display_name}{Gene symbol / display name (character).}
#'     \item{species}{Species name (character).}
#'     \item{biotype}{Gene biotype, e.g. \code{"protein_coding"} (character).}
#'     \item{description}{Gene description from Ensembl (character).}
#'     \item{start}{Genomic start position in base pairs (integer).}
#'     \item{end}{Genomic end position in base pairs (integer).}
#'     \item{strand}{Strand: \code{1} (forward) or \code{-1} (reverse) (integer).}
#'     \item{seq_region_name}{Chromosome or scaffold name (character).}
#'   }
#' @export
#' @seealso \code{\link{ensembl_lookup}}, \code{\link{ensembl_sequence}},
#'   \code{\link{ensembl_variant}}
#' @examples
#' \dontrun{
#' ensembl_gene("BRCA2")
#' ensembl_gene("Trp53", species = "mus_musculus")
#' }
ensembl_gene <- function(symbol, species = "homo_sapiens") {
  url <- sprintf("%s/lookup/symbol/%s/%s?content-type=application/json",
                 .ensembl_base, species, symbol)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Ensembl error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_gene)
  tibble(
    id = raw$id %||% NA_character_, display_name = raw$display_name %||% NA_character_,
    species = raw$species %||% NA_character_, biotype = raw$biotype %||% NA_character_,
    description = raw$description %||% NA_character_,
    start = as.integer(raw$start %||% NA_integer_), end = as.integer(raw$end %||% NA_integer_),
    strand = as.integer(raw$strand %||% NA_integer_),
    seq_region_name = raw$seq_region_name %||% NA_character_
  )
}

#' Look up a gene by its stable Ensembl ID
#'
#' Retrieves genomic annotation for a gene given its stable Ensembl
#' identifier. Returns the same column set as \code{\link{ensembl_gene}}.
#'
#' @param id Stable Ensembl gene ID (e.g. \code{"ENSG00000139618"} for
#'   human BRCA2, \code{"ENSMUSG00000059552"} for mouse Trp53).
#' @return A tibble with one row and columns: \code{id}, \code{display_name},
#'   \code{species}, \code{biotype}, \code{description}, \code{start},
#'   \code{end}, \code{strand}, \code{seq_region_name}. See
#'   \code{\link{ensembl_gene}} for column details.
#' @export
#' @seealso \code{\link{ensembl_gene}}, \code{\link{ensembl_sequence}}
#' @examples
#' \dontrun{
#' ensembl_lookup("ENSG00000139618")
#' }
ensembl_lookup <- function(id) {
  url <- sprintf("%s/lookup/id/%s?content-type=application/json", .ensembl_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Ensembl error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_gene)
  tibble(
    id = raw$id %||% NA_character_, display_name = raw$display_name %||% NA_character_,
    species = raw$species %||% NA_character_, biotype = raw$biotype %||% NA_character_,
    description = raw$description %||% NA_character_,
    start = as.integer(raw$start %||% NA_integer_), end = as.integer(raw$end %||% NA_integer_),
    strand = as.integer(raw$strand %||% NA_integer_),
    seq_region_name = raw$seq_region_name %||% NA_character_
  )
}

#' Retrieve a nucleotide or protein sequence from Ensembl
#'
#' Fetches the sequence for a gene or transcript given its stable
#' Ensembl ID. The \code{type} parameter selects which sequence
#' representation to return (genomic DNA, cDNA, CDS, or protein).
#'
#' @param id Stable Ensembl gene or transcript ID (e.g.
#'   \code{"ENSG00000139618"}, \code{"ENST00000380152"}).
#' @param type Sequence type to retrieve. One of:
#'   \describe{
#'     \item{\code{"genomic"}}{Full genomic DNA including introns (default).}
#'     \item{\code{"cdna"}}{Complementary DNA (exons only, spliced).}
#'     \item{\code{"cds"}}{Coding sequence (start to stop codon).}
#'     \item{\code{"protein"}}{Translated amino-acid sequence.}
#'   }
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Ensembl ID (character).}
#'     \item{seq}{The requested sequence string (character).}
#'     \item{molecule_type}{Molecule type label, e.g. \code{"dna"} (character).}
#'   }
#' @export
#' @seealso \code{\link{ensembl_gene}}, \code{\link{ensembl_lookup}}
#' @examples
#' \dontrun{
#' ensembl_sequence("ENST00000380152", type = "cds")
#' ensembl_sequence("ENSG00000139618", type = "genomic")
#' }
ensembl_sequence <- function(id, type = "genomic") {
  url <- sprintf("%s/sequence/id/%s?type=%s&content-type=application/json",
                 .ensembl_base, id, type)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Ensembl error: ", e$message); NULL })
  if (is.null(raw)) return(tibble(id = character(), seq = character(), molecule_type = character()))
  tibble(id = raw$id %||% NA_character_, seq = raw$seq %||% NA_character_,
         molecule_type = raw$molecule_type %||% NA_character_)
}

#' Look up a genetic variant by its rsID
#'
#' Queries the Ensembl Variation API for annotation of a single-nucleotide
#' polymorphism (SNP) or other variant identified by its dbSNP rsID.
#' Returns the most severe predicted consequence, observed alleles, and
#' global minor allele frequency (MAF) when available.
#'
#' @param rsid dbSNP reference SNP ID (e.g. \code{"rs699"},
#'   \code{"rs1800497"}).
#' @param species Ensembl species name (default \code{"homo_sapiens"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{The rsID (character).}
#'     \item{consequence_type}{Most severe predicted consequence, e.g.
#'       \code{"missense_variant"} (character).}
#'     \item{alleles}{Semicolon-separated allele strings from all mappings
#'       (character).}
#'     \item{minor_allele}{Global minor allele letter (character).}
#'     \item{maf}{Global minor allele frequency (numeric, 0--1).}
#'   }
#' @export
#' @seealso \code{\link{ensembl_gene}}, \code{\link{ensembl_lookup}}
#' @examples
#' \dontrun{
#' ensembl_variant("rs699")
#' ensembl_variant("rs1800497")
#' }
ensembl_variant <- function(rsid, species = "homo_sapiens") {
  url <- sprintf("%s/variation/%s/%s?content-type=application/json",
                 .ensembl_base, species, rsid)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Ensembl error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_variant)
  tibble(
    id = raw$name %||% NA_character_,
    consequence_type = raw$most_severe_consequence %||% NA_character_,
    alleles = paste(vapply(raw$mappings %||% list(), function(m) m$allele_string %||% "", character(1)), collapse = "; "),
    minor_allele = raw$minor_allele %||% NA_character_,
    maf = as.numeric(raw$MAF %||% NA_real_)
  )
}

# == Context ===================================================================

#' Get ensembl.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ensembl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ensembl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ensembl.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ensembl.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ensembl.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ensembl.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
