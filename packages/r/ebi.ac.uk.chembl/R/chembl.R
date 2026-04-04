#' Fetch a single ChEMBL molecule by ID
#'
#' @param id ChEMBL molecule ID (e.g. "CHEMBL25" for aspirin)
#' @return tibble: one row with molecule_chembl_id, pref_name, molecule_type,
#'   max_phase, formula, mw, smiles
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
#' @param query Search query (e.g. "aspirin", "ibuprofen")
#' @param limit Max results to return (default 10)
#' @return tibble: molecule_chembl_id, pref_name, molecule_type, max_phase,
#'   formula, mw, smiles
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
#' @param id ChEMBL target ID (e.g. "CHEMBL240")
#' @return tibble: one row with target_chembl_id, pref_name, target_type, organism
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
#' @param molecule_id ChEMBL molecule ID (e.g. "CHEMBL25")
#' @param limit Max results (default 10)
#' @return tibble: activity_id, molecule_chembl_id, target_chembl_id,
#'   standard_type, standard_value, standard_units, pchembl_value
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

#' Show ChEMBL package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
chembl_context <- function() {
  .build_context(
    pkg_name = "chembl.ebi.ac.uk",
    header_lines = c(
      "# chembl.ebi.ac.uk",
      "# ChEMBL API Client (drug/compound database)",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Rate limits: unknown (be polite)"
    )
  )
}
