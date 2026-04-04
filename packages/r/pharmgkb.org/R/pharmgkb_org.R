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

#' Look up a gene in PharmGKB
#'
#' Queries the PharmGKB pharmacogenomics knowledge base for gene
#' information relevant to drug metabolism and response.
#'
#' @param symbol Gene symbol (e.g. "CYP2D6", "VKORC1", "SLCO1B1",
#'   "TPMT", "DPYD")
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{PharmGKB accession ID, e.g. "PA128" (character)}
#'     \item{symbol}{Official gene symbol (character)}
#'     \item{name}{Full gene name (character)}
#'     \item{chromosome}{Chromosome location (character)}
#'     \item{cpic_gene}{Whether gene has CPIC guidelines (logical)}
#'     \item{pharm_var_gene}{Whether gene is in PharmVar (logical)}
#'   }
#' @examples
#' pgkb_gene("CYP2D6")
#' pgkb_gene("VKORC1")
#' @seealso [pgkb_drug()], [pgkb_variant()], [pgkb_context()]
#' @source <https://api.pharmgkb.org>
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
#' Queries PharmGKB for drug/chemical metadata including structural
#' identifiers and pharmacogenomic classification.
#'
#' @param name Drug name (e.g. "warfarin", "clopidogrel", "simvastatin",
#'   "codeine", "ibuprofen")
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{PharmGKB accession ID (character)}
#'     \item{name}{Drug/chemical name (character)}
#'     \item{smiles}{SMILES structural notation (character)}
#'     \item{inchi}{InChI structural identifier (character)}
#'     \item{pediatric}{Whether pediatric info is available (logical)}
#'     \item{type}{Comma-separated drug types (character)}
#'   }
#' @examples
#' pgkb_drug("warfarin")
#' pgkb_drug("clopidogrel")
#' @seealso [pgkb_gene()], [pgkb_variant()], [pgkb_context()]
#' @source <https://api.pharmgkb.org>
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
#' Queries PharmGKB for pharmacogenomic variant data by dbSNP rsID,
#' including clinical significance and functional classification.
#'
#' @param rsid Variant rsID from dbSNP (e.g. "rs1065852", "rs4149056",
#'   "rs9923231")
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{PharmGKB variant accession (character)}
#'     \item{symbol}{Variant symbol/rsID (character)}
#'     \item{name}{Full variant name (character)}
#'     \item{change_classification}{Functional effect classification (character)}
#'     \item{clinical_significance}{Clinical significance level (character)}
#'     \item{rare}{Whether variant is rare (logical)}
#'     \item{type}{Variant type (character)}
#'   }
#' @examples
#' pgkb_variant("rs1065852")
#' pgkb_variant("rs9923231")
#' @seealso [pgkb_gene()], [pgkb_drug()], [pgkb_context()]
#' @source <https://api.pharmgkb.org>
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

#' Get pharmgkb.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/pharmgkb.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "pharmgkb.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# pharmgkb.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# pharmgkb.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
