#' Look up a compound by name in PubChem
#'
#' Returns key properties for a compound identified by name.
#'
#' @param name Compound name (e.g., "aspirin", "caffeine", "glucose")
#' @return tibble: cid, iupac_name, molecular_formula, molecular_weight,
#'   inchi, canonical_smiles
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

#' Get specific properties for a compound
#'
#' @param name Compound name (e.g., "aspirin")
#' @param properties Comma-separated property names. Default includes common
#'   properties. See PubChem docs for full list.
#' @return tibble: cid, property, value (long format)
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

#' Get synonyms for a compound
#'
#' @param name Compound name (e.g., "aspirin")
#' @return tibble: cid, synonym
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

  cid <- info[[1]]$CID %||% info$CID[1]
  syns <- info[[1]]$Synonym %||% info$Synonym[[1]]
  if (is.null(syns)) return(.schema_synonyms)

  tibble(
    cid = as.integer(cid),
    synonym = as.character(syns)
  )
}

#' Print PubChem context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
pubchem_context <- function() {
  .build_context(
    pkg_name = "pubchem.ncbi.nlm.nih.gov",
    header_lines = c(
      "# Package: pubchem.ncbi.nlm.nih.gov",
      "# PubChem PUG REST API - chemical compound data",
      "# Auth: none",
      "# Rate limits: 5 requests per second",
      "#",
      "# Look up compounds by name: aspirin, caffeine, glucose, etc.",
      "# Properties: MolecularFormula, MolecularWeight, XLogP, TPSA,",
      "#   Complexity, HBondDonorCount, HBondAcceptorCount, InChI, SMILES"
    )
  )
}
