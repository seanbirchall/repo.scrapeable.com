


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

#' Generate LLM-friendly context for rxnav.nlm.nih.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
rxnorm_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/rxnav.nlm.nih.gov.R"
  if (!file.exists(src_file)) {
    cat("# rxnav.nlm.nih.gov context - source not found\n")
    return(invisible("# rxnav.nlm.nih.gov context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

