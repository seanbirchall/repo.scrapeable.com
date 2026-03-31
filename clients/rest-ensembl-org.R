# rest-ensembl-org.R
# Self-contained Ensembl REST API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://rest.ensembl.org

library(dplyr, warn.conflicts = FALSE)
library(tibble)

.ua <- "support@scrapeable.com"
.ensembl_base <- "https://rest.ensembl.org"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE); n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi]); if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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
#' @param symbol Gene symbol (e.g. "BRCA2", "TP53")
#' @param species Species (default "homo_sapiens")
#' @return tibble: one row with id, display_name, species, biotype,
#'   description, start, end, strand, seq_region_name
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

#' Look up a gene by Ensembl ID
#'
#' @param id Ensembl ID (e.g. "ENSG00000139618")
#' @return tibble: same columns as ensembl_gene
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

#' Get gene sequence
#'
#' @param id Ensembl ID
#' @param type "genomic" (default), "cdna", "cds", "protein"
#' @return tibble: one row with id, seq (character), molecule_type
ensembl_sequence <- function(id, type = "genomic") {
  url <- sprintf("%s/sequence/id/%s?type=%s&content-type=application/json",
                 .ensembl_base, id, type)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Ensembl error: ", e$message); NULL })
  if (is.null(raw)) return(tibble(id = character(), seq = character(), molecule_type = character()))
  tibble(id = raw$id %||% NA_character_, seq = raw$seq %||% NA_character_,
         molecule_type = raw$molecule_type %||% NA_character_)
}

#' Look up variant by rsID
#'
#' @param rsid Variant ID (e.g. "rs699")
#' @param species Species (default "homo_sapiens")
#' @return tibble: id, consequence_type, alleles, minor_allele, maf
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

#' Generate context
#' @return Character string (invisibly)
ensembl_context <- function() {
  .build_context("rest.ensembl.org", header_lines = c(
    "# rest.ensembl.org - Ensembl Genomics REST Client for R",
    "# Auth: none required. Rate limit: 15 req/sec.",
    "# Gene lookup, sequence retrieval, variant annotation.",
    "# Species: homo_sapiens, mus_musculus, etc."
  ))
}
