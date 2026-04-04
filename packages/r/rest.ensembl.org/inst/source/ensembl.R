


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
#' @param symbol Gene symbol (e.g. "BRCA2", "TP53")
#' @param species Species (default "homo_sapiens")
#' @return tibble: one row with id, display_name, species, biotype,
#'   description, start, end, strand, seq_region_name
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

#' Look up a gene by Ensembl ID
#'
#' @param id Ensembl ID (e.g. "ENSG00000139618")
#' @return tibble: same columns as ensembl_gene
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

#' Get gene sequence
#'
#' @param id Ensembl ID
#' @param type "genomic" (default), "cdna", "cds", "protein"
#' @return tibble: one row with id, seq (character), molecule_type
#' @export
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

#' Generate LLM-friendly context for rest.ensembl.org
#'
#' @return Character string with full function signatures and bodies
#' @export
ensembl_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/rest.ensembl.org.R"
  if (!file.exists(src_file)) {
    cat("# rest.ensembl.org context - source not found\n")
    return(invisible("# rest.ensembl.org context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

