# ebi-ac-uk-chembl.R
# Self-contained ChEMBL API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
