# api.pharmgkb.org.R - Self-contained api.pharmgkb.org client



# api-pharmgkb-org.R
# Self-contained PharmGKB pharmacogenomics API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pgkb_base <- "https://api.pharmgkb.org/v1/data"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_gene <- tibble(
  id = character(), symbol = character(), name = character(),
  chromosome = character(), cpic_gene = logical(),
  pharm_var_gene = logical()
)

.schema_drug <- tibble(
  id = character(), name = character(), smiles = character(),
  inchi = character(), pediatric = logical(), type = character()
)

.schema_variant <- tibble(
  id = character(), symbol = character(), name = character(),
  change_classification = character(), clinical_significance = character(),
  rare = logical(), type = character()
)


# == Public functions ==========================================================

#' Look up a pharmacogene in PharmGKB
#'
#' Returns metadata for a gene from the Pharmacogenomics Knowledge Base,
#' which curates genes relevant to drug response and metabolism. Indicates
#' whether the gene is included in CPIC (Clinical Pharmacogenetics
#' Implementation Consortium) guidelines and the PharmVar database.
#' Pair with \code{pgkb_drug()} to explore gene-drug interactions or
#' \code{pgkb_variant()} to look up specific polymorphisms.
#'
#' @param symbol Character. HGNC gene symbol, e.g. \code{"CYP2D6"}
#'   (cytochrome P450 2D6), \code{"VKORC1"} (vitamin K epoxide reductase),
#'   \code{"SLCO1B1"} (statin transporter), \code{"TPMT"} (thiopurine
#'   methyltransferase).
#' @return A tibble with one row per matching gene:
#'   \describe{
#'     \item{id}{\code{character} -- PharmGKB accession ID (e.g. "PA128")}
#'     \item{symbol}{\code{character} -- HGNC gene symbol (e.g. "CYP2D6")}
#'     \item{name}{\code{character} -- Full gene name (e.g. "cytochrome P450 family 2 subfamily D member 6")}
#'     \item{chromosome}{\code{character} -- Chromosomal location (e.g. "chr22")}
#'     \item{cpic_gene}{\code{logical} -- TRUE if the gene has CPIC clinical guidelines}
#'     \item{pharm_var_gene}{\code{logical} -- TRUE if the gene is in the PharmVar database}
#'   }
#' @examples
#' \dontrun{
#' # Look up CYP2D6, a major drug-metabolizing enzyme
#' pgkb_gene("CYP2D6")
#'
#' # Check if VKORC1 has CPIC guidelines
#' pgkb_gene("VKORC1")$cpic_gene
#'
#' # Statin transporter gene
#' pgkb_gene("SLCO1B1")
#' }
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

#' Look up a drug or chemical in PharmGKB
#'
#' Returns pharmacogenomic metadata for a drug, including its chemical
#' structure (SMILES, InChI), pediatric relevance, and type classification.
#' PharmGKB curates drug-gene relationships central to precision medicine
#' and pharmacogenomics. Use alongside \code{pgkb_gene()} and
#' \code{pgkb_variant()} for complete gene-drug-variant lookups.
#'
#' @param name Character. Drug name (case-insensitive), e.g.
#'   \code{"warfarin"}, \code{"clopidogrel"}, \code{"simvastatin"},
#'   \code{"codeine"}, \code{"tamoxifen"}.
#' @return A tibble with one row per matching drug:
#'   \describe{
#'     \item{id}{\code{character} -- PharmGKB accession ID (e.g. "PA451906")}
#'     \item{name}{\code{character} -- Drug name (e.g. "warfarin")}
#'     \item{smiles}{\code{character} -- SMILES chemical structure string}
#'     \item{inchi}{\code{character} -- InChI chemical identifier}
#'     \item{pediatric}{\code{logical} -- TRUE if the drug has pediatric pharmacogenomic relevance}
#'     \item{type}{\code{character} -- Classification: "Drug", "Biological", "Ion, Element", etc.}
#'   }
#' @examples
#' \dontrun{
#' # Look up warfarin (anticoagulant with strong PGx evidence)
#' pgkb_drug("warfarin")
#'
#' # Check if clopidogrel has pediatric PGx data
#' pgkb_drug("clopidogrel")$pediatric
#'
#' # Statin lookup
#' pgkb_drug("simvastatin")
#' }
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
#' Returns pharmacogenomic annotation for a genetic variant (SNP)
#' by its dbSNP rsID. Includes clinical significance for drug response,
#' change classification (coding, intronic, etc.), and rarity. Useful
#' for understanding how a specific polymorphism affects drug metabolism
#' or response. Pair with \code{pgkb_gene()} for the parent gene and
#' \code{pgkb_drug()} for associated medications.
#'
#' @param rsid Character. dbSNP reference SNP ID, e.g.
#'   \code{"rs1065852"} (CYP2D6 variant), \code{"rs4149056"}
#'   (SLCO1B1*5, statin myopathy risk), \code{"rs9923231"} (VKORC1
#'   warfarin sensitivity).
#' @return A tibble with one row per matching variant:
#'   \describe{
#'     \item{id}{\code{character} -- PharmGKB variant accession (e.g. "PA166156050")}
#'     \item{symbol}{\code{character} -- dbSNP rsID (e.g. "rs1065852")}
#'     \item{name}{\code{character} -- Variant name (same as symbol for most SNPs)}
#'     \item{change_classification}{\code{character} -- Genomic location type: "Intronic", "Missense", "Synonymous", etc.}
#'     \item{clinical_significance}{\code{character} -- PGx significance: "drug-response", "pathogenic", etc.}
#'     \item{rare}{\code{logical} -- TRUE if the variant is rare (MAF < 1\%)}
#'     \item{type}{\code{character} -- Variant type: "SNP", "Indel", etc.}
#'   }
#' @examples
#' \dontrun{
#' # CYP2D6 variant affecting drug metabolism
#' pgkb_variant("rs1065852")
#'
#' # SLCO1B1*5 variant (statin myopathy risk)
#' pgkb_variant("rs4149056")
#'
#' # VKORC1 variant affecting warfarin dose
#' pgkb_variant("rs9923231")
#' }
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

#' Get api.pharmgkb.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pgkb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pgkb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.pharmgkb.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.pharmgkb.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.pharmgkb.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.pharmgkb.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
