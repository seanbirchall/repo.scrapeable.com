# == Public functions ==========================================================

#' Look up a gene in PharmGKB
#'
#' @param symbol Gene symbol (e.g. "CYP2D6", "VKORC1", "SLCO1B1")
#' @return tibble: id, symbol, name, chromosome, cpic_gene, pharm_var_gene
#' @export
pgkb_gene <- function(symbol) {
  url <- sprintf("%s/gene?symbol=%s", .pgkb_base,
                 utils::URLencode(symbol, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_gene)

  rows <- lapply(raw$data, function(x) {
    chr_name <- if (!is.null(x$chr)) as.character(x$chr$name %||% NA) else NA_character_
    tibble(
      id             = as.character(x$id %||% NA),
      symbol         = as.character(x$symbol %||% NA),
      name           = as.character(x$name %||% NA),
      chromosome     = chr_name,
      cpic_gene      = as.logical(x$cpicGene %||% NA),
      pharm_var_gene = as.logical(x$pharmVarGene %||% NA)
    )
  })
  bind_rows(rows)
}

#' Look up a drug/chemical in PharmGKB
#'
#' @param name Drug name (e.g. "warfarin", "clopidogrel", "simvastatin")
#' @return tibble: id, name, smiles, inchi, pediatric, type
#' @export
pgkb_drug <- function(name) {
  url <- sprintf("%s/drug?name=%s", .pgkb_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_drug)

  rows <- lapply(raw$data, function(x) {
    types <- if (!is.null(x$types) && length(x$types) > 0) {
      paste(unlist(x$types), collapse = ", ")
    } else NA_character_
    tibble(
      id        = as.character(x$id %||% NA),
      name      = as.character(x$name %||% NA),
      smiles    = as.character(x$smiles %||% NA),
      inchi     = as.character(x$inChi %||% NA),
      pediatric = as.logical(x$pediatric %||% NA),
      type      = types
    )
  })
  bind_rows(rows)
}

#' Look up a genetic variant in PharmGKB
#'
#' @param rsid Variant rsID (e.g. "rs1065852", "rs4149056", "rs9923231")
#' @return tibble: id, symbol, name, change_classification,
#'   clinical_significance, rare, type
#' @export
pgkb_variant <- function(rsid) {
  url <- sprintf("%s/variant?symbol=%s", .pgkb_base,
                 utils::URLencode(rsid, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_variant)

  rows <- lapply(raw$data, function(x) {
    tibble(
      id                    = as.character(x$id %||% NA),
      symbol                = as.character(x$symbol %||% NA),
      name                  = as.character(x$name %||% NA),
      change_classification = as.character(x$changeClassification %||% NA),
      clinical_significance = as.character(x$clinicalSignificance %||% NA),
      rare                  = as.logical(x$rare %||% NA),
      type                  = as.character(x$type %||% NA)
    )
  })
  bind_rows(rows)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the api.pharmgkb.org package
#'
#' @return Character string (invisibly), also printed
#' @export
pgkb_context <- function() {
  .build_context("api.pharmgkb.org", header_lines = c(
    "# api.pharmgkb.org - PharmGKB Pharmacogenomics Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: none documented",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common gene symbols: CYP2D6, CYP2C19, VKORC1, SLCO1B1, CYP3A5",
    "# Common drugs: warfarin, clopidogrel, simvastatin, codeine, tamoxifen",
    "# Common variants: rs1065852, rs4149056, rs9923231, rs4244285"
  ))
}
