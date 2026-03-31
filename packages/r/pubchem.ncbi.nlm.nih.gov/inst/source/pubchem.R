# pubchem-ncbi-nlm-nih-gov.R
# Self-contained PubChem PUG REST API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: 5 requests per second

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pubchem_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

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
