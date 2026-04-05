# rest.ensembl.org.R - Self-contained rest.ensembl.org client



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


#' Look up a gene by symbol
#'
#' Queries the Ensembl REST API to retrieve annotation for a gene identified
#' by its HGNC symbol. Returns genomic coordinates, biotype, and species.
#'
#' @param symbol Character string. Gene symbol (e.g., \code{"BRCA2"},
#'   \code{"TP53"}, \code{"EGFR"}).
#' @param species Character string. Ensembl species name (default
#'   \code{"homo_sapiens"}). Use underscored binomial format
#'   (e.g., \code{"mus_musculus"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. Ensembl stable gene ID (e.g., \code{"ENSG00000139618"}).}
#'     \item{display_name}{Character. Gene symbol.}
#'     \item{species}{Character. Species name.}
#'     \item{biotype}{Character. Gene biotype (e.g., \code{"protein_coding"}).}
#'     \item{description}{Character. Gene description.}
#'     \item{start}{Integer. Genomic start position (bp).}
#'     \item{end}{Integer. Genomic end position (bp).}
#'     \item{strand}{Integer. Strand (1 = forward, -1 = reverse).}
#'     \item{seq_region_name}{Character. Chromosome or scaffold name.}
#'   }
#'   Returns an empty tibble if the symbol is not found.
#' @examples
#' ensembl_gene("BRCA2")
#' ensembl_gene("TP53")
#' @export
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

#' Look up a gene by Ensembl stable ID
#'
#' Retrieves gene annotation using an Ensembl stable identifier. Returns the
#' same columns as \code{\link{ensembl_gene}} but accepts the stable ID
#' directly instead of a gene symbol.
#'
#' @param id Character string. Ensembl stable ID (e.g.,
#'   \code{"ENSG00000139618"} for BRCA2, \code{"ENSG00000141510"} for TP53).
#' @return A tibble with one row and the same columns as
#'   \code{\link{ensembl_gene}}.
#' @seealso \code{\link{ensembl_gene}} for lookup by symbol.
#' @examples
#' ensembl_lookup("ENSG00000139618")
#' @export
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

#' Get nucleotide or protein sequence for a gene
#'
#' Fetches the sequence for an Ensembl feature (gene, transcript, or
#' protein) in the requested coordinate space.
#'
#' @param id Character string. Ensembl stable ID (e.g.,
#'   \code{"ENSG00000139618"}).
#' @param type Character string. Sequence type to retrieve. One of:
#'   \describe{
#'     \item{\code{"genomic"}}{Full genomic sequence including introns (default).}
#'     \item{\code{"cdna"}}{Spliced cDNA sequence.}
#'     \item{\code{"cds"}}{Coding sequence only.}
#'     \item{\code{"protein"}}{Translated amino acid sequence.}
#'   }
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. Ensembl stable ID.}
#'     \item{seq}{Character. The sequence string.}
#'     \item{molecule_type}{Character. Molecule type (e.g., \code{"dna"}, \code{"protein"}).}
#'   }
#' @examples
#' ensembl_sequence("ENSG00000139618", type = "cds")
#' @export
ensembl_sequence <- function(id, type = "genomic") {
  url <- sprintf("%s/sequence/id/%s?type=%s&content-type=application/json",
                 .ensembl_base, id, type)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Ensembl error: ", e$message); NULL })
  if (is.null(raw)) return(tibble(id = character(), seq = character(), molecule_type = character()))
  tibble(id = raw$id %||% NA_character_, seq = raw$seq %||% NA_character_,
         molecule_type = raw$molecule_type %||% NA_character_)
}

#' Look up a genetic variant by rsID
#'
#' Retrieves variant annotation from Ensembl for a given dbSNP reference
#' SNP ID (rsID). Returns allele information, the most severe predicted
#' consequence, minor allele, and minor allele frequency (MAF).
#'
#' @param rsid Character string. dbSNP rsID (e.g., \code{"rs699"},
#'   \code{"rs1800497"}, \code{"rs334"}).
#' @param species Character string. Ensembl species name (default
#'   \code{"homo_sapiens"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. The rsID.}
#'     \item{consequence_type}{Character. Most severe consequence
#'       (e.g., \code{"missense_variant"}).}
#'     \item{alleles}{Character. Allele string(s) separated by \code{"; "}.}
#'     \item{minor_allele}{Character. Minor allele.}
#'     \item{maf}{Numeric. Minor allele frequency (0--1).}
#'   }
#' @examples
#' ensembl_variant("rs699")
#' @export
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

#' Get rest.ensembl.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/rest.ensembl.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "rest.ensembl.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# rest.ensembl.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# rest.ensembl.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
