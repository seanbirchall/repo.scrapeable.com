# ebi-ac-uk-chembl.R
# Self-contained ChEMBL API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.chembl_base <- "https://www.ebi.ac.uk/chembl/api/data"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

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

# == Public functions ==========================================================

#' Fetch a single ChEMBL molecule by ID
#'
#' Retrieves detailed information for a single molecule from the ChEMBL
#' database at EMBL-EBI, including its molecular formula, molecular weight,
#' canonical SMILES, and maximum clinical trial phase reached.
#'
#' @param id Character. ChEMBL molecule identifier (e.g. \code{"CHEMBL25"}
#'   for aspirin, \code{"CHEMBL1642"} for ibuprofen).
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{molecule_chembl_id}{Character. ChEMBL identifier.}
#'   \item{pref_name}{Character. Preferred name (e.g. \code{"ASPIRIN"}), or \code{NA}.}
#'   \item{molecule_type}{Character. Type (e.g. \code{"Small molecule"}, \code{"Antibody"}), or \code{NA}.}
#'   \item{max_phase}{Character. Highest clinical trial phase (\code{"4"} = approved), or \code{NA}.}
#'   \item{formula}{Character. Molecular formula, or \code{NA}.}
#'   \item{mw}{Numeric. Molecular weight (g/mol), or \code{NA}.}
#'   \item{smiles}{Character. Canonical SMILES string, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' chembl_molecule("CHEMBL25")
#' }
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
#' Performs a free-text search across the ChEMBL molecule database. Returns
#' molecules whose names or synonyms match the query, with the same column
#' schema as \code{\link{chembl_molecule}}.
#'
#' @param query Character. Search query (e.g. \code{"aspirin"},
#'   \code{"ibuprofen"}, \code{"kinase inhibitor"}).
#' @param limit Integer. Maximum results to return (default 10).
#' @return A tibble with the same columns as \code{\link{chembl_molecule}}:
#'   molecule_chembl_id, pref_name, molecule_type, max_phase, formula, mw,
#'   smiles.
#' @examples
#' \dontrun{
#' chembl_search("aspirin")
#' chembl_search("kinase inhibitor", limit = 20)
#' }
#' @export
chembl_search <- function(query, limit = 10) {
  url <- sprintf("%s/molecule/search.json?q=%s&limit=%d",
                 .chembl_base, utils::URLencode(query), limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
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
#' Retrieves metadata for a single biological target from ChEMBL. Targets
#' are typically proteins, protein families, or other molecular entities
#' against which compounds are tested.
#'
#' @param id Character. ChEMBL target identifier (e.g. \code{"CHEMBL240"}).
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{target_chembl_id}{Character. ChEMBL target identifier.}
#'   \item{pref_name}{Character. Preferred target name, or \code{NA}.}
#'   \item{target_type}{Character. Target type (e.g. \code{"SINGLE PROTEIN"}), or \code{NA}.}
#'   \item{organism}{Character. Source organism (e.g. \code{"Homo sapiens"}), or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' chembl_target("CHEMBL240")
#' }
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
#' Retrieves bioactivity measurements recorded in ChEMBL for a given
#' molecule. Each row represents a single assay result linking the molecule
#' to a biological target with a standardised activity value.
#' The \code{pchembl_value} is a negative log-transformed potency measure
#' comparable across assay types.
#'
#' @param molecule_id Character. ChEMBL molecule identifier
#'   (e.g. \code{"CHEMBL25"} for aspirin).
#' @param limit Integer. Maximum results to return (default 10).
#' @return A tibble with one row per activity record and columns:
#' \describe{
#'   \item{activity_id}{Integer. Unique activity record identifier.}
#'   \item{molecule_chembl_id}{Character. Molecule ChEMBL ID.}
#'   \item{target_chembl_id}{Character. Target ChEMBL ID.}
#'   \item{standard_type}{Character. Activity type (e.g. \code{"IC50"}, \code{"Ki"}).}
#'   \item{standard_value}{Numeric. Standardised activity value, or \code{NA}.}
#'   \item{standard_units}{Character. Units (e.g. \code{"nM"}), or \code{NA}.}
#'   \item{pchembl_value}{Numeric. pChEMBL value (-log potency), or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' chembl_activities("CHEMBL25")
#' chembl_activities("CHEMBL25", limit = 50)
#' }
#' @export
chembl_activities <- function(molecule_id, limit = 10) {
  url <- sprintf("%s/activity.json?molecule_chembl_id=%s&limit=%d",
                 .chembl_base, molecule_id, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
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

#' Get ebi-ac-uk-chembl client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
chembl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(chembl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ebi-ac-uk-chembl.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ebi-ac-uk-chembl")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ebi-ac-uk-chembl context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ebi-ac-uk-chembl", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
