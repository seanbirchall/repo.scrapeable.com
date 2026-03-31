# api-pharmgkb-org.R
# Self-contained PharmGKB pharmacogenomics API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pgkb_base <- "https://api.pharmgkb.org/v1/data"

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
