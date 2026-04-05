# rxnav.nlm.nih.gov.R - Self-contained rxnav.nlm.nih.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# rxnav-nlm-nih-gov.R
# Self-contained RxNorm drug data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://rxnav.nlm.nih.gov/REST


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rx_base <- "https://rxnav.nlm.nih.gov/REST"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_concepts <- tibble(
  rxcui = character(), name = character(), tty = character(),
  synonym = character()
)

.schema_rxcui <- tibble(
  rxcui = character()
)

.schema_suggestions <- tibble(
  suggestion = character()
)

.schema_classes <- tibble(
  class_id = character(), class_name = character(), class_type = character(),
  rxcui = character(), drug_name = character()
)

# == Drug lookup ===============================================================

#' Look up RxCUI by drug name
#'
#' Returns the RxNorm Concept Unique Identifier (RxCUI) for a drug name.
#' The RxCUI is the primary key for all other RxNorm lookups.
#'
#' @param name Character. Drug name to look up (e.g. \code{"aspirin"},
#'   \code{"metformin"}, \code{"acetaminophen"}). Must be an exact or
#'   near-exact match; use \code{rxnorm_spelling()} for misspellings.
#' @return A tibble with one column:
#'   \describe{
#'     \item{rxcui}{Character. RxNorm Concept Unique Identifier (e.g. \code{"1191"}).}
#'   }
#' @examples
#' rxnorm_rxcui("aspirin")
#' rxnorm_rxcui("metformin")
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
#' Returns all RxNorm concepts matching a drug name, including branded
#' products, clinical drugs, ingredients, and dose forms. Results span
#' multiple term types (TTY).
#'
#' @param name Character. Drug name to search (e.g. \code{"metformin"},
#'   \code{"ibuprofen"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{rxcui}{Character. RxNorm Concept Unique Identifier.}
#'     \item{name}{Character. Full concept name (e.g. \code{"metformin hydrochloride 500 MG Oral Tablet"}).}
#'     \item{tty}{Character. Term type code: \code{"SBD"} (Semantic Branded Drug), \code{"SCD"} (Semantic Clinical Drug), \code{"IN"} (Ingredient), \code{"BN"} (Brand Name), \code{"SBDF"}, \code{"SCDF"}, etc.}
#'     \item{synonym}{Character. Brand name synonym if available, otherwise empty string.}
#'   }
#' @examples
#' rxnorm_drugs("metformin")
#' rxnorm_drugs("ibuprofen")
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
#' Retrieves the basic properties (name, term type, synonym) for a single
#' RxNorm concept identified by its RxCUI.
#'
#' @param rxcui Character or numeric. RxNorm Concept Unique Identifier
#'   (e.g. \code{"161"} for acetaminophen, \code{"1191"} for aspirin).
#'   Obtain from \code{rxnorm_rxcui()} or \code{rxnorm_drugs()}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{rxcui}{Character. The queried RxCUI.}
#'     \item{name}{Character. Preferred concept name (e.g. \code{"acetaminophen"}).}
#'     \item{tty}{Character. Term type code (e.g. \code{"IN"} for Ingredient).}
#'     \item{synonym}{Character. Synonym or empty string.}
#'   }
#' @examples
#' rxnorm_properties("161")
#' rxnorm_properties("1191")
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
#' Returns brand names, clinical drugs, dose forms, and other concepts
#' related to a given RxCUI. Optionally filter by term type.
#'
#' @param rxcui Character or numeric. RxNorm Concept Unique Identifier
#'   (e.g. \code{"161"} for acetaminophen).
#' @param tty Character vector or NULL. Term type filter(s). Common values:
#'   \code{"BN"} (Brand Name), \code{"SCD"} (Semantic Clinical Drug),
#'   \code{"SBD"} (Semantic Branded Drug), \code{"SBDF"} (Semantic Branded
#'   Dose Form), \code{"IN"} (Ingredient), \code{"DF"} (Dose Form).
#'   Pass multiple as \code{c("BN", "SCD")}. NULL (default) returns all
#'   related types.
#' @return A tibble with columns:
#'   \describe{
#'     \item{rxcui}{Character. RxCUI of the related concept.}
#'     \item{name}{Character. Full concept name (e.g. \code{"Tylenol"}).}
#'     \item{tty}{Character. Term type code of the related concept.}
#'     \item{synonym}{Character. Synonym or empty string.}
#'   }
#' @examples
#' rxnorm_related("161", tty = "BN")
#' rxnorm_related("1191")
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


#' Get spelling suggestions for a drug name
#'
#' Returns candidate corrections for a potentially misspelled drug name.
#' Useful as a fallback when \code{rxnorm_rxcui()} returns no results.
#'
#' @param name Character. Possibly misspelled drug name
#'   (e.g. \code{"asprin"}, \code{"ibuprophen"}).
#' @return A tibble with one column:
#'   \describe{
#'     \item{suggestion}{Character. Suggested corrected drug name.}
#'   }
#' @examples
#' rxnorm_spelling("asprin")
#' rxnorm_spelling("ibuprophen")
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


#' Get drug classification for an RxCUI
#'
#' Returns pharmacological classification codes (ATC, VA, MeSH, or MED-RT)
#' for a drug identified by its RxCUI. Useful for grouping drugs by
#' therapeutic category.
#'
#' @param rxcui Character or numeric. RxNorm Concept Unique Identifier
#'   (e.g. \code{"161"} for acetaminophen).
#' @param source Character. Classification system to query. One of:
#'   \code{"ATC"} (Anatomical Therapeutic Chemical, default),
#'   \code{"VA"} (Veterans Affairs Drug Classes),
#'   \code{"MESH"} (Medical Subject Headings),
#'   \code{"MEDRT"} (Medication Reference Terminology).
#' @return A tibble with columns:
#'   \describe{
#'     \item{class_id}{Character. Classification code (e.g. \code{"N02BE"} for ATC).}
#'     \item{class_name}{Character. Human-readable class name (e.g. \code{"Anilides"}).}
#'     \item{class_type}{Character. Classification level (e.g. \code{"ATC1-4"}).}
#'     \item{rxcui}{Character. RxCUI of the classified drug.}
#'     \item{drug_name}{Character. Drug name associated with the classification.}
#'   }
#' @examples
#' rxnorm_class("161", source = "ATC")
#' rxnorm_class("1191", source = "VA")
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

#' Get rxnav.nlm.nih.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' rxnorm_context()
#' @export
rxnorm_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(rxnorm_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/rxnav.nlm.nih.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "rxnav.nlm.nih.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# rxnav.nlm.nih.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# rxnav.nlm.nih.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
