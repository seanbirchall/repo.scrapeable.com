# ebi.ac.uk.R
# Self-contained EBI client covering UniProt Proteins API and ChEMBL API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.ebi_base <- "https://www.ebi.ac.uk/proteins/api"
.chembl_base <- "https://www.ebi.ac.uk/chembl/api/data"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
.fetch_json_simple <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)


# == Schemas ===================================================================

.schema_protein <- tibble(
  accession = character(), id = character(), gene = character(),
  organism = character(), protein_name = character(),
  existence = character(), sequence_length = integer()
)

.schema_molecule <- tibble(
  molecule_chembl_id = character(), pref_name = character(),
  molecule_type = character(), max_phase = character(),
  formula = character(), mw = numeric(), smiles = character()
)

.schema_target <- tibble(
  target_chembl_id = character(), pref_name = character(),
  target_type = character(), organism = character()
)

.schema_activity <- tibble(
  activity_id = integer(), molecule_chembl_id = character(),
  target_chembl_id = character(), standard_type = character(),
  standard_value = numeric(), standard_units = character(),
  pchembl_value = numeric()
)


# == UniProt Proteins ==========================================================

#' Fetch a single UniProt protein entry by accession
#'
#' Retrieves full protein metadata from the EBI Proteins API for one
#' UniProt accession. Returns gene name, organism, recommended protein
#' name, evidence level, and sequence length.
#'
#' @param accession Character. UniProt accession ID (e.g. \code{"P12345"},
#'   \code{"P38398"} for BRCA1, \code{"P04637"} for TP53).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{accession}{character -- UniProt accession (e.g. \code{"P38398"})}
#'     \item{id}{character -- UniProt entry name (e.g. \code{"BRCA1_HUMAN"})}
#'     \item{gene}{character -- primary gene symbol (e.g. \code{"BRCA1"})}
#'     \item{organism}{character -- species name (e.g. \code{"Homo sapiens"})}
#'     \item{protein_name}{character -- recommended protein name}
#'     \item{existence}{character -- protein existence level (e.g. \code{"Evidence at protein level"})}
#'     \item{sequence_length}{integer -- amino acid count}
#'   }
#' @examples
#' ebi_protein("P38398")
#' ebi_protein("P04637")
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

#' Search UniProt proteins by gene name
#'
#' Queries the EBI Proteins API by gene name and returns matching
#' protein entries with metadata. Supports pagination via \code{offset}.
#'
#' @param query Character. Gene name to search for (e.g. \code{"BRCA1"},
#'   \code{"TP53"}, \code{"insulin"}).
#' @param size Integer. Number of results to return (default 10, max 100).
#' @param offset Integer. Starting offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{character -- UniProt accession}
#'     \item{id}{character -- UniProt entry name}
#'     \item{gene}{character -- primary gene symbol}
#'     \item{organism}{character -- species name}
#'     \item{protein_name}{character -- recommended protein name (may be \code{NA})}
#'     \item{existence}{character -- evidence level (\code{"Evidence at protein level"}, \code{"Predicted"}, etc.)}
#'     \item{sequence_length}{integer -- amino acid count}
#'   }
#' @examples
#' ebi_search("BRCA1", size = 5)
#' ebi_search("insulin", size = 10, offset = 0)
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

#' Get protein features and annotations from UniProt
#'
#' Returns structural and functional annotations for a protein, including
#' domains, binding sites, active sites, signal peptides, and more.
#'
#' @param accession Character. UniProt accession ID (e.g. \code{"P38398"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{character -- the queried accession}
#'     \item{type}{character -- annotation type (e.g. \code{"CHAIN"}, \code{"DOMAIN"}, \code{"BINDING"}, \code{"MOD_RES"})}
#'     \item{description}{character -- annotation description (e.g. \code{"BRCT 1"})}
#'     \item{begin}{integer -- start residue position}
#'     \item{end}{integer -- end residue position}
#'     \item{evidence}{character -- semicolon-separated evidence codes (e.g. \code{"ECO:0000255"})}
#'   }
#' @examples
#' ebi_features("P38398")
#' @export
ebi_features <- function(accession) {
  url <- sprintf("%s/features/%s", .ebi_base, accession)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(accession = character(), type = character(),
                                   description = character(), begin = integer(), end = integer()))

  features <- raw$features
  if (is.null(features) || length(features) == 0) {
    return(tibble(accession = character(), type = character(),
                  description = character(), begin = integer(), end = integer()))
  }

  tibble(
    accession = accession,
    type = vapply(features, function(f) f$type %||% NA_character_, character(1)),
    description = vapply(features, function(f) f$description %||% NA_character_, character(1)),
    begin = vapply(features, function(f) {
      tryCatch(as.integer(f$begin %||% f$location$start$value), error = function(e) NA_integer_)
    }, integer(1)),
    end = vapply(features, function(f) {
      tryCatch(as.integer(f$end %||% f$location$end$value), error = function(e) NA_integer_)
    }, integer(1)),
    evidence = vapply(features, function(f) {
      if (!is.null(f$evidences)) paste(vapply(f$evidences, function(e) e$code %||% "", character(1)), collapse = "; ")
      else NA_character_
    }, character(1))
  )
}

#' Get protein amino acid sequence
#'
#' Retrieves the full amino acid sequence for a UniProt protein entry.
#'
#' @param accession Character. UniProt accession ID (e.g. \code{"P38398"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{accession}{character -- the queried accession}
#'     \item{length}{integer -- sequence length in amino acids}
#'     \item{sequence}{character -- full amino acid sequence (one-letter code)}
#'     \item{checksum}{character -- CRC64 checksum of the sequence}
#'   }
#' @examples
#' ebi_sequence("P38398")
#' @export
ebi_sequence <- function(accession) {
  url <- sprintf("%s/proteins/%s", .ebi_base, accession)
  full <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(full)) return(tibble(accession = character(), length = integer(),
                                    sequence = character(), checksum = character()))

  tibble(
    accession = accession,
    length = as.integer(full$sequence$length %||% NA_integer_),
    sequence = as.character(full$sequence$sequence %||% NA_character_),
    checksum = as.character(full$sequence$crc64 %||% NA_character_)
  )
}


# == ChEMBL ===================================================================

#' Fetch a single ChEMBL molecule by ID
#'
#' Retrieves compound metadata from the ChEMBL database including name,
#' type, clinical phase, molecular formula, weight, and canonical SMILES.
#'
#' @param id Character. ChEMBL molecule ID (e.g. \code{"CHEMBL25"} for aspirin,
#'   \code{"CHEMBL1642"} for ibuprofen).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{molecule_chembl_id}{character -- ChEMBL molecule ID}
#'     \item{pref_name}{character -- preferred compound name (e.g. \code{"ASPIRIN"})}
#'     \item{molecule_type}{character -- type (e.g. \code{"Small molecule"}, \code{"Protein"})}
#'     \item{max_phase}{character -- max clinical phase (\code{"4.0"} = approved)}
#'     \item{formula}{character -- molecular formula (e.g. \code{"C9H8O4"})}
#'     \item{mw}{numeric -- molecular weight in daltons}
#'     \item{smiles}{character -- canonical SMILES string}
#'   }
#' @examples
#' chembl_molecule("CHEMBL25")
#' chembl_molecule("CHEMBL1642")
#' @export
chembl_molecule <- function(id) {
  url <- sprintf("%s/molecule/%s.json", .chembl_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_molecule)

  props <- raw$molecule_properties
  structs <- raw$molecule_structures

  tibble(
    molecule_chembl_id = as.character(raw$molecule_chembl_id %||% NA_character_),
    pref_name          = as.character(raw$pref_name %||% NA_character_),
    molecule_type      = as.character(raw$molecule_type %||% NA_character_),
    max_phase          = as.character(raw$max_phase %||% NA_character_),
    formula            = as.character(props$full_molformula %||% NA_character_),
    mw                 = as.numeric(props$full_mwt %||% NA_real_),
    smiles             = as.character(structs$canonical_smiles %||% NA_character_)
  )
}

#' Search ChEMBL molecules by name
#'
#' Free-text search across the ChEMBL compound database. Returns matching
#' molecules with structural and clinical metadata.
#'
#' @param query Character. Search query (e.g. \code{"aspirin"}, \code{"ibuprofen"},
#'   \code{"caffeine"}).
#' @param limit Integer. Max results to return (default 10).
#' @return A tibble with columns:
#'   \describe{
#'     \item{molecule_chembl_id}{character -- ChEMBL molecule ID}
#'     \item{pref_name}{character -- preferred compound name}
#'     \item{molecule_type}{character -- compound type}
#'     \item{max_phase}{character -- max clinical phase}
#'     \item{formula}{character -- molecular formula}
#'     \item{mw}{numeric -- molecular weight in daltons}
#'     \item{smiles}{character -- canonical SMILES string}
#'   }
#' @examples
#' chembl_search("aspirin", limit = 5)
#' chembl_search("caffeine")
#' @export
chembl_search <- function(query, limit = 10) {
  url <- sprintf("%s/molecule/search.json?q=%s&limit=%d",
                 .chembl_base, utils::URLencode(query), limit)
  raw <- tryCatch(.fetch_json_simple(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$molecules)) return(.schema_molecule)

  mols <- raw$molecules
  if (length(mols) == 0 || nrow(mols) == 0) return(.schema_molecule)

  tibble(
    molecule_chembl_id = as.character(mols$molecule_chembl_id),
    pref_name          = as.character(mols$pref_name),
    molecule_type      = as.character(mols$molecule_type),
    max_phase          = as.character(mols$max_phase),
    formula            = as.character(mols$molecule_properties$full_molformula),
    mw                 = as.numeric(mols$molecule_properties$full_mwt),
    smiles             = as.character(mols$molecule_structures$canonical_smiles)
  )
}

#' Fetch a single ChEMBL target by ID
#'
#' Retrieves target (protein/gene) metadata from ChEMBL.
#'
#' @param id Character. ChEMBL target ID (e.g. \code{"CHEMBL240"},
#'   \code{"CHEMBL203"} for EGFR).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{target_chembl_id}{character -- ChEMBL target ID}
#'     \item{pref_name}{character -- target name (e.g. \code{"Voltage-gated inwardly rectifying potassium channel KCNH2"})}
#'     \item{target_type}{character -- type (e.g. \code{"SINGLE PROTEIN"}, \code{"PROTEIN COMPLEX"})}
#'     \item{organism}{character -- species (e.g. \code{"Homo sapiens"})}
#'   }
#' @examples
#' chembl_target("CHEMBL240")
#' @export
chembl_target <- function(id) {
  url <- sprintf("%s/target/%s.json", .chembl_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_target)

  tibble(
    target_chembl_id = as.character(raw$target_chembl_id %||% NA_character_),
    pref_name        = as.character(raw$pref_name %||% NA_character_),
    target_type      = as.character(raw$target_type %||% NA_character_),
    organism         = as.character(raw$organism %||% NA_character_)
  )
}

#' Fetch ChEMBL bioactivity data for a molecule
#'
#' Returns bioassay activity measurements for a given compound, including
#' IC50, Ki, EC50, and other pharmacological endpoints.
#'
#' @param molecule_id Character. ChEMBL molecule ID (e.g. \code{"CHEMBL25"}).
#' @param limit Integer. Max results to return (default 10).
#' @return A tibble with columns:
#'   \describe{
#'     \item{activity_id}{integer -- unique activity record ID}
#'     \item{molecule_chembl_id}{character -- the queried molecule}
#'     \item{target_chembl_id}{character -- target tested against}
#'     \item{standard_type}{character -- measurement type (e.g. \code{"IC50"}, \code{"Ki"}, \code{"Log K'"})}
#'     \item{standard_value}{numeric -- measured value}
#'     \item{standard_units}{character -- units (e.g. \code{"nM"}, \code{"uM kg-1"})}
#'     \item{pchembl_value}{numeric -- negative log molar activity (-log10), comparable across assays}
#'   }
#' @examples
#' chembl_activities("CHEMBL25", limit = 5)
#' @export
chembl_activities <- function(molecule_id, limit = 10) {
  url <- sprintf("%s/activity.json?molecule_chembl_id=%s&limit=%d",
                 .chembl_base, molecule_id, limit)
  raw <- tryCatch(.fetch_json_simple(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$activities)) return(.schema_activity)

  acts <- raw$activities
  if (length(acts) == 0 || nrow(acts) == 0) return(.schema_activity)

  tibble(
    activity_id        = as.integer(acts$activity_id),
    molecule_chembl_id = as.character(acts$molecule_chembl_id),
    target_chembl_id   = as.character(acts$target_chembl_id),
    standard_type      = as.character(acts$standard_type),
    standard_value     = as.numeric(acts$standard_value),
    standard_units     = as.character(acts$standard_units),
    pchembl_value      = as.numeric(acts$pchembl_value)
  )
}


# == Context ===================================================================

#' Get ebi.ac.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ebi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ebi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ebi.ac.uk.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ebi.ac.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ebi.ac.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ebi.ac.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
