# == Drug lookup ===============================================================

#' Look up RxCUI by drug name
#'
#' Returns the RxNorm Concept Unique Identifier (RxCUI) for a drug name.
#'
#' @param name Drug name (e.g. "aspirin", "metformin")
#' @return tibble: rxcui (character)
#' @export
rxnorm_rxcui <- function(name) {
  url <- sprintf("%s/rxcui.json?name=%s", .rx_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_rxcui)

  ids <- raw$idGroup$rxnormId
  if (is.null(ids) || length(ids) == 0) return(.schema_rxcui)
  tibble(rxcui = as.character(ids))
}


#' Search drugs by name
#'
#' Returns all RxNorm concepts matching a drug name, grouped by term type.
#'
#' @param name Drug name (e.g. "metformin")
#' @return tibble: rxcui, name, tty (term type), synonym
#' @export
rxnorm_drugs <- function(name) {
  url <- sprintf("%s/drugs.json?name=%s", .rx_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_concepts)

  groups <- raw$drugGroup$conceptGroup
  if (is.null(groups) || length(groups) == 0) return(.schema_concepts)

  results <- lapply(groups, function(g) {
    props <- g$conceptProperties
    if (is.null(props) || length(props) == 0) return(NULL)
    tibble(
      rxcui   = vapply(props, function(p) p$rxcui %||% NA_character_, character(1)),
      name    = vapply(props, function(p) p$name %||% NA_character_, character(1)),
      tty     = vapply(props, function(p) p$tty %||% NA_character_, character(1)),
      synonym = vapply(props, function(p) p$synonym %||% NA_character_, character(1))
    )
  })
  bind_rows(results)
}


#' Get drug properties by RxCUI
#'
#' @param rxcui RxNorm concept ID
#' @return tibble: one row with rxcui, name, tty, synonym
#' @export
rxnorm_properties <- function(rxcui) {
  url <- sprintf("%s/rxcui/%s/properties.json", .rx_base, rxcui)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw) || is.null(raw$properties)) return(.schema_concepts)

  p <- raw$properties
  tibble(
    rxcui   = p$rxcui %||% NA_character_,
    name    = p$name %||% NA_character_,
    tty     = p$tty %||% NA_character_,
    synonym = p$synonym %||% NA_character_
  )
}


#' Get all related concepts for a drug
#'
#' Returns all brand names, clinical drugs, dose forms, etc.
#'
#' @param rxcui RxNorm concept ID
#' @param tty Optional term type filter (e.g. "BN" for brand names,
#'   "SCD" for clinical drugs). Can be a vector.
#' @return tibble: rxcui, name, tty, synonym
#' @export
rxnorm_related <- function(rxcui, tty = NULL) {
  if (!is.null(tty)) {
    tty_str <- paste(tty, collapse = "+")
    url <- sprintf("%s/rxcui/%s/related.json?tty=%s", .rx_base, rxcui, tty_str)
  } else {
    url <- sprintf("%s/rxcui/%s/allrelated.json", .rx_base, rxcui)
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_concepts)

  groups <- raw$allRelatedGroup$conceptGroup %||% raw$relatedGroup$conceptGroup
  if (is.null(groups) || length(groups) == 0) return(.schema_concepts)

  results <- lapply(groups, function(g) {
    props <- g$conceptProperties
    if (is.null(props) || length(props) == 0) return(NULL)
    tibble(
      rxcui   = vapply(props, function(p) p$rxcui %||% NA_character_, character(1)),
      name    = vapply(props, function(p) p$name %||% NA_character_, character(1)),
      tty     = vapply(props, function(p) p$tty %||% NA_character_, character(1)),
      synonym = vapply(props, function(p) p$synonym %||% NA_character_, character(1))
    )
  })
  bind_rows(results)
}


#' Get spelling suggestions
#'
#' @param name Misspelled drug name
#' @return tibble: suggestion (character)
#' @export
rxnorm_spelling <- function(name) {
  url <- sprintf("%s/spellingsuggestions.json?name=%s", .rx_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_suggestions)

  suggestions <- raw$suggestionGroup$suggestionList$suggestion
  if (is.null(suggestions) || length(suggestions) == 0) return(.schema_suggestions)
  tibble(suggestion = as.character(suggestions))
}


#' Get drug classes for an RxCUI
#'
#' Returns ATC, VA, and other classification codes.
#'
#' @param rxcui RxNorm concept ID
#' @param source Classification source: "ATC", "VA", "MESH", "MEDRT".
#'   Default "ATC".
#' @return tibble: class_id, class_name, class_type, rxcui, drug_name
#' @export
rxnorm_class <- function(rxcui, source = "ATC") {
  url <- sprintf("%s/rxclass/class/byRxcui.json?rxcui=%s&relaSource=%s",
                 .rx_base, rxcui, source)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("RxNorm API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_classes)

  items <- raw$rxclassDrugInfoList$rxclassDrugInfo
  if (is.null(items) || length(items) == 0) return(.schema_classes)

  tibble(
    class_id   = vapply(items, function(i) i$rxclassMinConceptItem$classId %||% NA_character_, character(1)),
    class_name = vapply(items, function(i) i$rxclassMinConceptItem$className %||% NA_character_, character(1)),
    class_type = vapply(items, function(i) i$rxclassMinConceptItem$classType %||% NA_character_, character(1)),
    rxcui      = vapply(items, function(i) i$minConcept$rxcui %||% NA_character_, character(1)),
    drug_name  = vapply(items, function(i) i$minConcept$name %||% NA_character_, character(1))
  )
}


# == Context ===================================================================

#' Generate context for the RxNorm package
#' @return Character string (invisibly)
#' @export
rxnorm_context <- function() {
  .build_context("rxnav.nlm.nih.gov", header_lines = c(
    "# rxnav.nlm.nih.gov - RxNorm Drug Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# TTY codes: IN=Ingredient, BN=Brand Name, SCD=Clinical Drug,",
    "#   SBD=Branded Drug, DF=Dose Form, MIN=Multi-Ingredient",
    "#",
    "# Workflow: rxnorm_rxcui('aspirin') -> rxnorm_related(rxcui) -> rxnorm_class(rxcui)"
  ))
}
