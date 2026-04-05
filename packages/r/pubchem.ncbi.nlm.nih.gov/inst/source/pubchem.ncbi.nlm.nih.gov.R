# pubchem.ncbi.nlm.nih.gov.R - Self-contained pubchem.ncbi.nlm.nih.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)


# pubchem-ncbi-nlm-nih-gov.R
# Self-contained PubChem PUG REST API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: 5 requests per second


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pubchem_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_compound <- tibble(
  cid = integer(), iupac_name = character(), molecular_formula = character(),
  molecular_weight = numeric(), inchi = character(),
  canonical_smiles = character()
)

.schema_properties <- tibble(
  cid = integer(), property = character(), value = character()
)

.schema_synonyms <- tibble(
  cid = integer(), synonym = character()
)

# == Public functions ==========================================================


#' Look up a compound by name in PubChem
#'
#' Queries the PubChem PUG REST API to retrieve core molecular properties for
#' a compound identified by its common or systematic name. Returns a single-row
#' tibble with the PubChem compound ID (CID), IUPAC name, molecular formula and
#' weight, InChI identifier, and canonical SMILES string.
#'
#' @param name Character string. Compound name to look up. Accepts common names
#'   (e.g., \code{"aspirin"}), brand names (e.g., \code{"tylenol"}), or
#'   systematic IUPAC names (e.g., \code{"2-acetoxybenzoic acid"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cid}{Integer. PubChem Compound ID.}
#'     \item{iupac_name}{Character. IUPAC systematic name.}
#'     \item{molecular_formula}{Character. Molecular formula (e.g., \code{"C9H8O4"}).}
#'     \item{molecular_weight}{Numeric. Molecular weight in g/mol.}
#'     \item{inchi}{Character. IUPAC International Chemical Identifier.}
#'     \item{canonical_smiles}{Character. Canonical SMILES representation.}
#'   }
#'   Returns an empty (zero-row) tibble on lookup failure.
#' @examples
#' pubchem_compound("aspirin")
#' pubchem_compound("caffeine")
#' @export
pubchem_compound <- function(name) {
  url <- sprintf(
    "%s/compound/name/%s/property/IUPACName,MolecularFormula,MolecularWeight,InChI,CanonicalSMILES/JSON",
    .pubchem_base, utils::URLencode(name, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_compound)

  props <- raw$PropertyTable$Properties
  if (is.null(props) || nrow(props) == 0) return(.schema_compound)

  nms <- names(props)
  as_tibble(props) |>
    transmute(
      cid = as.integer(CID),
      iupac_name = as.character(if ("IUPACName" %in% nms) IUPACName else NA_character_),
      molecular_formula = as.character(if ("MolecularFormula" %in% nms) MolecularFormula else NA_character_),
      molecular_weight = as.numeric(if ("MolecularWeight" %in% nms) MolecularWeight else NA_real_),
      inchi = as.character(if ("InChI" %in% nms) InChI else NA_character_),
      canonical_smiles = as.character(if ("CanonicalSMILES" %in% nms) CanonicalSMILES else NA_character_)
    )
}

#' Get specific physicochemical properties for a compound
#'
#' Retrieves one or more computed molecular properties for a named compound
#' from PubChem and returns them in long (tidy) format. Useful for comparing
#' drug-likeness descriptors such as XLogP, TPSA, hydrogen-bond counts, and
#' rotatable bond counts.
#'
#' @param name Character string. Compound name (e.g., \code{"aspirin"}).
#' @param properties Character string. Comma-separated PubChem property names.
#'   Defaults to a standard drug-likeness panel: MolecularFormula,
#'   MolecularWeight, XLogP, ExactMass, MonoisotopicMass, TPSA, Complexity,
#'   HBondDonorCount, HBondAcceptorCount, RotatableBondCount. See
#'   \url{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest} for the full list.
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{cid}{Integer. PubChem Compound ID.}
#'     \item{property}{Character. Property name (e.g., \code{"XLogP"}).}
#'     \item{value}{Character. Property value (as text for uniform typing).}
#'   }
#'   Returns an empty tibble on failure.
#' @examples
#' pubchem_properties("ibuprofen")
#' pubchem_properties("glucose", properties = "MolecularFormula,MolecularWeight")
#' @export
pubchem_properties <- function(name,
                               properties = "MolecularFormula,MolecularWeight,XLogP,ExactMass,MonoisotopicMass,TPSA,Complexity,HBondDonorCount,HBondAcceptorCount,RotatableBondCount") {
  url <- sprintf(
    "%s/compound/name/%s/property/%s/JSON",
    .pubchem_base, utils::URLencode(name, reserved = TRUE), properties
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_properties)

  props <- raw$PropertyTable$Properties
  if (is.null(props) || nrow(props) == 0) return(.schema_properties)

  # Pivot to long format
  cid <- props$CID[1]
  prop_names <- setdiff(names(props), "CID")
  rows <- lapply(prop_names, function(p) {
    tibble(
      cid = as.integer(cid),
      property = p,
      value = as.character(props[[p]][1])
    )
  })
  bind_rows(rows)
}

#' Get all known synonyms for a compound
#'
#' Returns every synonym registered in PubChem for a given compound, including
#' trade names, IUPAC names, CAS numbers, and other identifiers. Useful for
#' mapping between naming conventions.
#'
#' @param name Character string. Compound name (e.g., \code{"aspirin"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cid}{Integer. PubChem Compound ID.}
#'     \item{synonym}{Character. One synonym per row.}
#'   }
#'   Returns an empty tibble if the compound is not found.
#' @examples
#' pubchem_synonyms("aspirin")
#' @export
pubchem_synonyms <- function(name) {
  url <- sprintf(
    "%s/compound/name/%s/synonyms/JSON",
    .pubchem_base, utils::URLencode(name, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_synonyms)

  info <- raw$InformationList$Information
  if (is.null(info) || length(info) == 0) return(.schema_synonyms)

  # info may be a data.frame or a list depending on result count
  if (is.data.frame(info)) {
    cid <- info$CID[1]
    syns <- info$Synonym[[1]]
  } else {
    cid <- info[[1]]$CID %||% NA_integer_
    syns <- info[[1]]$Synonym
  }
  if (is.null(syns)) return(.schema_synonyms)

  tibble(
    cid = as.integer(cid),
    synonym = as.character(syns)
  )
}

#' Search PubChem compounds by text query
#'
#' Resolves a compound name to one or more PubChem Compound IDs (CIDs) and
#' fetches basic molecular properties for each match. Useful for discovering
#' compounds when you have a partial or ambiguous name.
#'
#' @param query Character string. Search text (e.g., \code{"aspirin"},
#'   \code{"caffeine"}, \code{"anti-inflammatory"}).
#' @param max_results Integer. Maximum number of compounds to return
#'   (default 10).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cid}{Integer. PubChem Compound ID.}
#'     \item{iupac_name}{Character. IUPAC systematic name.}
#'     \item{molecular_formula}{Character. Molecular formula.}
#'     \item{molecular_weight}{Numeric. Molecular weight in g/mol.}
#'   }
#'   Returns an empty tibble if no matches are found.
#' @examples
#' pubchem_search("caffeine")
#' pubchem_search("ibuprofen", max_results = 5)
#' @export
pubchem_search <- function(query, max_results = 10) {
  # Step 1: get CIDs from autocomplete/search
  url <- sprintf(
    "%s/compound/name/%s/cids/JSON",
    .pubchem_base, utils::URLencode(query, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_compound[, c("cid", "iupac_name", "molecular_formula", "molecular_weight")])

  cids <- raw$IdentifierList$CID
  if (is.null(cids) || length(cids) == 0) return(.schema_compound[, c("cid", "iupac_name", "molecular_formula", "molecular_weight")])
  cids <- head(cids, max_results)

  # Step 2: get properties for those CIDs
  cid_str <- paste(cids, collapse = ",")
  prop_url <- sprintf(
    "%s/compound/cid/%s/property/IUPACName,MolecularFormula,MolecularWeight/JSON",
    .pubchem_base, cid_str
  )
  prop_raw <- tryCatch(.fetch_json(prop_url), error = function(e) NULL)
  if (is.null(prop_raw)) return(tibble(cid = as.integer(cids), iupac_name = NA_character_,
                                        molecular_formula = NA_character_, molecular_weight = NA_real_))

  props <- prop_raw$PropertyTable$Properties
  if (is.null(props) || nrow(props) == 0) return(tibble(cid = as.integer(cids)))

  nms <- names(props)
  as_tibble(props) |>
    transmute(
      cid = as.integer(CID),
      iupac_name = as.character(if ("IUPACName" %in% nms) IUPACName else NA_character_),
      molecular_formula = as.character(if ("MolecularFormula" %in% nms) MolecularFormula else NA_character_),
      molecular_weight = as.numeric(if ("MolecularWeight" %in% nms) MolecularWeight else NA_real_)
    )
}

#' Get bioactivity assay data for a compound
#'
#' Retrieves the assay summary for a compound from the PubChem BioAssay
#' database. Returns assay IDs, names, activity outcomes (active/inactive),
#' and biological target names. Useful for exploring the pharmacological
#' profile of a compound.
#'
#' @param cid Integer. PubChem Compound ID (e.g., \code{2244} for aspirin,
#'   \code{2519} for caffeine). Obtain CIDs from \code{pubchem_compound()} or
#'   \code{pubchem_search()}.
#' @param max_results Integer. Maximum number of assays to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{aid}{Integer. PubChem Assay ID.}
#'     \item{assay_name}{Character. Name of the bioassay.}
#'     \item{activity_outcome}{Character. Outcome (\code{"Active"},
#'       \code{"Inactive"}, \code{"Inconclusive"}).}
#'     \item{target_name}{Character. Biological target name.}
#'   }
#'   Returns an empty tibble if no assay data is available.
#' @examples
#' pubchem_assays(2244, max_results = 5)
#' @export
pubchem_assays <- function(cid, max_results = 20) {
  schema <- tibble(aid = integer(), assay_name = character(),
                   activity_outcome = character(), target_name = character())

  # Get assay IDs linked to this compound
  url <- sprintf("%s/compound/cid/%d/assaysummary/JSON", .pubchem_base, as.integer(cid))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)

  tbl <- raw$Table
  if (is.null(tbl)) return(schema)

  cols <- tbl$Columns$Column
  rows <- tbl$Row
  if (is.null(rows) || length(rows) == 0) return(schema)

  # Parse tabular response
  col_names <- if (is.data.frame(cols)) cols$Heading else if (is.character(cols)) cols else vapply(cols, function(c) c$Heading %||% "", character(1))

  aid_idx <- which(col_names == "AID")[1]
  name_idx <- which(col_names == "Assay Name")[1]
  outcome_idx <- which(col_names == "Activity Outcome")[1]
  target_idx <- which(col_names == "Target Name")[1]

  # rows may be a data.frame with a Cell list-column or a plain list
  if (is.data.frame(rows)) {
    cell_list <- rows$Cell
    n_rows <- min(nrow(rows), max_results)
  } else {
    cell_list <- lapply(rows, function(r) r$Cell)
    n_rows <- min(length(cell_list), max_results)
  }

  parsed <- lapply(seq_len(n_rows), function(i) {
    cells <- cell_list[[i]]
    get_cell <- function(idx) {
      if (is.na(idx) || idx > length(cells)) return(NA_character_)
      val <- cells[idx]
      if (is.null(val) || identical(val, "")) NA_character_ else val
    }
    tibble(
      aid = suppressWarnings(as.integer(get_cell(aid_idx))),
      assay_name = as.character(get_cell(name_idx)),
      activity_outcome = as.character(get_cell(outcome_idx)),
      target_name = as.character(get_cell(target_idx))
    )
  })
  bind_rows(parsed)
}

# == Context ===================================================================

#' Get pubchem.ncbi.nlm.nih.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pubchem_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pubchem_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/pubchem.ncbi.nlm.nih.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "pubchem.ncbi.nlm.nih.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# pubchem.ncbi.nlm.nih.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# pubchem.ncbi.nlm.nih.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
