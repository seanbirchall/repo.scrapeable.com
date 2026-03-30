# == Drug adverse events =======================================================

#' Search FDA drug adverse event reports (FAERS)
#'
#' @param search openFDA search query. Examples:
#'   "patient.drug.openfda.brand_name:aspirin",
#'   "patient.reaction.reactionmeddrapt:nausea",
#'   "receivedate:\[20230101+TO+20231231\]"
#' @param api_key Optional API key
#' @param max_results Max results (default 100, max 26000 via skip)
#' @return tibble of adverse event reports
#' @export
fda_drug_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/event.json", search, api_key = api_key, max_results = max_results)
}

#' Count drug adverse events by field
#'
#' Returns top values and counts for a field across all matching events.
#'
#' @param field Field to count (e.g. "patient.reaction.reactionmeddrapt.exact",
#'   "patient.drug.openfda.brand_name.exact", "serious")
#' @param search Optional search filter
#' @param limit Number of top values (default 10, max 1000)
#' @param api_key Optional API key
#' @return tibble: term, count
#' @export
fda_drug_event_count <- function(field, search = NULL, limit = 10,
                                 api_key = NULL) {
  raw <- .fda_get("drug/event.json", search = search, count = field,
                  limit = limit, api_key = api_key)
  results <- raw$results
  if (is.null(results) || length(results) == 0)
    return(tibble(term = character(), count = integer()))
  bind_rows(lapply(results, function(r) {
    tibble(term = r$term %||% NA_character_, count = as.integer(r$count %||% NA))
  }))
}


# == Drug labels (SPL) =========================================================

#' Search FDA drug labeling (package inserts)
#'
#' @param search Search query. Examples:
#'   "openfda.brand_name:lipitor",
#'   "indications_and_usage:diabetes",
#'   "openfda.generic_name:metformin"
#' @param api_key Optional API key
#' @param max_results Max results (default 10 — labels are large documents)
#' @return tibble of drug label records
#' @export
fda_drug_label <- function(search = NULL, api_key = NULL, max_results = 10) {
  .fda_fetch_all("drug/label.json", search, api_key = api_key, max_results = max_results)
}


# == Drug NDC directory ========================================================

#' Search the National Drug Code (NDC) directory
#'
#' @param search Search query. Examples:
#'   "brand_name:tylenol", "generic_name:acetaminophen",
#'   "product_ndc:0002-4112"
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble: product_ndc, brand_name, generic_name, dosage_form,
#'   route, marketing_category, ...
#' @export
fda_drug_ndc <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/ndc.json", search, api_key = api_key, max_results = max_results)
}


# == Drug recalls (enforcement) ================================================

#' Search FDA drug recall/enforcement actions
#'
#' @param search Search query. Examples:
#'   "classification:Class+I" (most serious),
#'   "state:CA", "report_date:\[20240101+TO+20241231\]"
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble: recall_number, classification, reason_for_recall,
#'   product_description, city, state, ...
#' @export
fda_drug_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/enforcement.json", search, api_key = api_key, max_results = max_results)
}


# == Orange Book (drugsfda) ====================================================

#' Search FDA approved drug applications (Orange Book)
#'
#' The Orange Book contains approved drug products with therapeutic
#' equivalence evaluations. Includes application numbers, sponsors,
#' products, therapeutic equivalence (TE) codes, and submission history.
#'
#' @param search Search query. Examples:
#'   "openfda.brand_name:lipitor",
#'   "products.brand_name:metformin",
#'   "sponsor_name:pfizer",
#'   "products.te_code:AB"
#' @param api_key Optional API key
#' @param max_results Max results (default 50)
#' @return tibble: application_number, sponsor_name, plus nested products
#'   and submissions data (flattened)
#' @export
fda_orange_book <- function(search = NULL, api_key = NULL, max_results = 50) {
  raw <- .fda_get("drug/drugsfda.json", search = search,
                  limit = min(max_results, 100), api_key = api_key)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  bind_rows(lapply(results, function(r) {
    # Flatten products
    products <- r$products
    if (is.null(products) || length(products) == 0) {
      return(tibble(
        application_number = r$application_number %||% NA_character_,
        sponsor_name = r$sponsor_name %||% NA_character_
      ))
    }

    bind_rows(lapply(products, function(p) {
      tibble(
        application_number = r$application_number %||% NA_character_,
        sponsor_name       = r$sponsor_name %||% NA_character_,
        product_number     = p$product_number %||% NA_character_,
        brand_name         = p$brand_name %||% NA_character_,
        dosage_form        = p$dosage_form %||% NA_character_,
        route              = p$route %||% NA_character_,
        marketing_status   = p$marketing_status %||% NA_character_,
        te_code            = p$te_code %||% NA_character_,
        reference_drug     = p$reference_drug %||% NA_character_,
        active_ingredients = paste(vapply(
          p$active_ingredients %||% list(),
          function(ai) paste(ai$name %||% "", ai$strength %||% "", sep = " "),
          character(1)
        ), collapse = "; ")
      )
    }))
  }))
}


# == Device adverse events =====================================================

#' Search FDA device adverse event reports (MAUDE)
#'
#' @param search Search query. Examples:
#'   "device.generic_name:pacemaker",
#'   "date_received:\[20240101+TO+20241231\]"
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble of device event reports
#' @export
fda_device_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("device/event.json", search, api_key = api_key, max_results = max_results)
}

#' Search FDA device recalls
#'
#' @param search Search query
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble of device recall records
#' @export
fda_device_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("device/enforcement.json", search, api_key = api_key, max_results = max_results)
}

#' Search FDA device classifications (510k, PMA, etc.)
#'
#' @param search Search query. Examples:
#'   "device_name:catheter", "medical_specialty:CV"
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble of device classification records
#' @export
fda_device_classification <- function(search = NULL, api_key = NULL,
                                      max_results = 100) {
  .fda_fetch_all("device/classification.json", search, api_key = api_key, max_results = max_results)
}


# == Food ======================================================================

#' Search FDA food recall/enforcement actions
#'
#' @param search Search query
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble of food recall records
#' @export
fda_food_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("food/enforcement.json", search, api_key = api_key, max_results = max_results)
}

#' Search FDA food adverse event reports (CAERS)
#'
#' @param search Search query
#' @param api_key Optional API key
#' @param max_results Max results (default 100)
#' @return tibble of food adverse event reports
#' @export
fda_food_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("food/event.json", search, api_key = api_key, max_results = max_results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the api.fda.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
fda_context <- function() {
  .build_context("api.fda.gov", header_lines = c(
    "# api.fda.gov - FDA openFDA API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: optional API key (api_key param). 240 req/min without.",
    "# All functions return tibbles.",
    "#",
    "# Search syntax: field:value, field:[start+TO+end] for dates",
    "#   brand_name, generic_name, manufacturer_name, ndc, reaction, etc.",
    "#   Use .exact suffix for exact match: brand_name.exact:LIPITOR",
    "#",
    "# Endpoints covered:",
    "#   Drug: adverse events, labels, NDC, recalls, Orange Book (drugsfda)",
    "#   Device: adverse events, recalls, classifications",
    "#   Food: adverse events, recalls"
  ))
}
