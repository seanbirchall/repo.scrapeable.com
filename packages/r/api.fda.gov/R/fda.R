# api.fda.gov.R
# Self-contained api.fda.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


# fda-gov.R
# Self-contained FDA openFDA API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional API key (query param api_key). Without: 240 req/min, 120K/day.
#   With: 240 req/min, 120K/day (same limits, key prevents throttling).
#   Register at https://open.fda.gov/apis/authentication/
# Docs: https://open.fda.gov/apis/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fda_base <- "https://api.fda.gov"
# -- Core fetch engine ---------------------------------------------------------

.fda_get <- function(endpoint, search = NULL, count = NULL, sort = NULL,
                     limit = 100, skip = 0, api_key = NULL) {
  params <- list()
  if (!is.null(search))  params$search <- search
  if (!is.null(count))   params$count <- count
  if (!is.null(sort))    params$sort <- sort
  if (!is.null(api_key)) params$api_key <- api_key
  params$limit <- min(limit, 1000)
  if (skip > 0) params$skip <- skip

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.fda_base, "/", endpoint, "?", query)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# Paginated fetch returning tibble
.fda_fetch_all <- function(endpoint, search = NULL, sort = NULL,
                           api_key = NULL, max_results = 100) {
  all_results <- list()
  skip <- 0
  page_size <- min(max_results, 100)

  repeat {
    raw <- .fda_get(endpoint, search = search, sort = sort,
                    limit = page_size, skip = skip, api_key = api_key)
    results <- raw$results
    if (is.null(results) || length(results) == 0) break

    all_results <- c(all_results, results)
    skip <- skip + length(results)

    if (skip >= (raw$meta$results$total %||% 0)) break
    if (length(all_results) >= max_results) break
    if (length(results) < page_size) break
  }

  if (length(all_results) == 0) return(tibble())
  if (length(all_results) > max_results) all_results <- all_results[seq_len(max_results)]

  # Flatten list-of-lists to tibble
  bind_rows(lapply(all_results, function(r) {
    flat <- lapply(r, function(v) {
      if (is.null(v)) NA_character_
      else if (is.list(v) || length(v) > 1) paste(v, collapse = "; ")
      else as.character(v)
    })
    as_tibble(flat)
  }))
}



# == Drug adverse events =======================================================

#' @title Search FDA drug adverse event reports (FAERS)
#'
#' Queries the FDA Adverse Event Reporting System (FAERS) for drug safety
#' reports. Each row represents one safety report containing patient info,
#' drug details, and adverse reactions. Use \code{fda_drug_event_count()} for
#' aggregate counts instead of individual reports.
#'
#' @param search openFDA search query string. Uses openFDA query syntax.
#'   Examples: \code{"patient.drug.openfda.brand_name:aspirin"},
#'   \code{"patient.reaction.reactionmeddrapt:nausea"},
#'   \code{"receivedate:[20230101+TO+20231231]"}.
#'   \code{NULL} returns unfiltered results. Default \code{NULL}.
#' @param api_key Optional FDA API key for higher rate limits. Register at
#'   \url{https://open.fda.gov/apis/authentication/}. Default \code{NULL}.
#' @param max_results Maximum number of reports to return. Default \code{100}.
#'   Maximum \code{26000} (API limit via skip pagination).
#' @return A tibble with columns (all character, flattened from nested JSON):
#'   \itemize{
#'     \item \code{safetyreportid} (character): Unique report identifier
#'     \item \code{receivedate} (character): Date FDA received the report (YYYYMMDD)
#'     \item \code{serious} (character): Whether the event was serious (\code{"1"} = yes)
#'     \item \code{seriousnesshospitalization} (character): Hospitalization flag
#'     \item \code{primarysourcecountry} (character): Country of the primary reporter
#'     \item \code{occurcountry} (character): Country where the event occurred
#'     \item \code{reporttype} (character): Report type code
#'     \item \code{patient} (character): Flattened patient/drug/reaction data
#'     \item \code{companynumb} (character): Company report number
#'   }
#'   Plus additional fields depending on report completeness.
#' @examples
#' \dontrun{
#' # Adverse events for aspirin
#' fda_drug_event(search = "patient.drug.openfda.brand_name:aspirin", max_results = 50)
#'
#' # Nausea reactions from 2023
#' fda_drug_event(search = "patient.reaction.reactionmeddrapt:nausea+AND+receivedate:[20230101+TO+20231231]")
#' }
fda_drug_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/event.json", search, api_key = api_key, max_results = max_results)
}

#' @title Count drug adverse events by field
#'
#' Returns the top values and their counts for a specific field across all
#' matching FAERS reports. Useful for finding the most common adverse
#' reactions, most-reported drugs, or serious event distributions without
#' downloading individual reports.
#'
#' @param field Field to count. Must use \code{.exact} suffix for text fields.
#'   Examples: \code{"patient.reaction.reactionmeddrapt.exact"} (top reactions),
#'   \code{"patient.drug.openfda.brand_name.exact"} (top drugs),
#'   \code{"serious"} (serious vs non-serious).
#' @param search Optional search filter to narrow the events counted. Uses
#'   openFDA query syntax. Example:
#'   \code{"patient.drug.openfda.brand_name:aspirin"}.
#'   Default \code{NULL} (count across all events).
#' @param limit Number of top values to return. Default \code{10}. Maximum \code{1000}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{term} (character): Field value, e.g. \code{"DRUG INEFFECTIVE"}, \code{"DEATH"}, \code{"NAUSEA"}
#'     \item \code{count} (integer): Number of reports with this value
#'   }
#' @examples
#' \dontrun{
#' # Top 10 most common adverse reactions
#' fda_drug_event_count("patient.reaction.reactionmeddrapt.exact")
#'
#' # Top adverse reactions for aspirin
#' fda_drug_event_count("patient.reaction.reactionmeddrapt.exact",
#'                      search = "patient.drug.openfda.brand_name:aspirin")
#'
#' # Top 20 most-reported drugs
#' fda_drug_event_count("patient.drug.openfda.brand_name.exact", limit = 20)
#' }
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

#' @title Search FDA drug labeling (package inserts)
#'
#' Returns Structured Product Labeling (SPL) data for drug package inserts.
#' Labels are large documents with many sections (indications, warnings,
#' dosage, etc.), so keep \code{max_results} low. Each row is one label
#' with sections flattened to character columns.
#'
#' @param search openFDA search query. Examples:
#'   \code{"openfda.brand_name:lipitor"} (by brand name),
#'   \code{"indications_and_usage:diabetes"} (by indication text),
#'   \code{"openfda.generic_name:metformin"} (by generic name).
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of labels to return. Default \code{10}.
#'   Keep low as labels are large documents.
#' @return A tibble with many columns representing label sections (all character),
#'   including indications_and_usage, warnings, dosage_and_administration,
#'   adverse_reactions, drug_interactions, plus openfda metadata fields.
#' @examples
#' \dontrun{
#' # Look up Lipitor label
#' fda_drug_label(search = "openfda.brand_name:lipitor", max_results = 1)
#'
#' # Find labels mentioning diabetes
#' fda_drug_label(search = "indications_and_usage:diabetes", max_results = 5)
#' }
fda_drug_label <- function(search = NULL, api_key = NULL, max_results = 10) {
  .fda_fetch_all("drug/label.json", search, api_key = api_key, max_results = max_results)
}


# == Drug NDC directory ========================================================

#' @title Search the National Drug Code (NDC) directory
#'
#' Queries the FDA NDC directory for drug product listings. Each row is one
#' product with NDC code, brand/generic names, dosage form, route, and
#' labeler information. Useful for looking up specific drug products.
#'
#' @param search openFDA search query. Examples:
#'   \code{"brand_name:tylenol"}, \code{"generic_name:acetaminophen"},
#'   \code{"product_ndc:0002-4112"}. Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of products to return. Default \code{100}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{product_ndc} (character): National Drug Code
#'     \item \code{brand_name} (character): Brand name, e.g. \code{"TYLENOL"}
#'     \item \code{generic_name} (character): Generic name, e.g. \code{"ACETAMINOPHEN"}
#'     \item \code{labeler_name} (character): Manufacturer/labeler
#'     \item \code{dosage_form} (character): Form, e.g. \code{"TABLET"}, \code{"CAPSULE"}
#'     \item \code{route} (character): Route of administration, e.g. \code{"ORAL"}
#'     \item \code{marketing_category} (character): Regulatory category
#'     \item \code{product_type} (character): Product type classification
#'     \item \code{active_ingredients} (character): Active ingredients (flattened)
#'     \item \code{application_number} (character): NDA/ANDA number
#'   }
#'   Plus additional fields like packaging, spl_id, openfda metadata.
#' @examples
#' \dontrun{
#' # Search for Tylenol products
#' fda_drug_ndc(search = "brand_name:tylenol")
#'
#' # Search by generic name
#' fda_drug_ndc(search = "generic_name:acetaminophen", max_results = 50)
#' }
fda_drug_ndc <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/ndc.json", search, api_key = api_key, max_results = max_results)
}


# == Drug recalls (enforcement) ================================================

#' @title Search FDA drug recall/enforcement actions
#'
#' Returns FDA drug enforcement actions including recalls, market withdrawals,
#' and safety alerts. Each row is one recall event with classification
#' (Class I = most serious), recalling firm, reason, and distribution details.
#'
#' @param search openFDA search query. Examples:
#'   \code{"classification:Class+I"} (most serious recalls),
#'   \code{"state:CA"} (California recalls),
#'   \code{"report_date:[20240101+TO+20241231]"} (2024 recalls).
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of records. Default \code{100}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{recall_number} (character): Unique recall identifier
#'     \item \code{classification} (character): Severity: \code{"Class I"}, \code{"Class II"}, \code{"Class III"}
#'     \item \code{reason_for_recall} (character): Description of why the recall was initiated
#'     \item \code{product_description} (character): Description of the recalled product
#'     \item \code{recalling_firm} (character): Company initiating the recall
#'     \item \code{city} (character): City of the recalling firm
#'     \item \code{state} (character): State of the recalling firm
#'     \item \code{status} (character): Recall status (e.g. \code{"Ongoing"}, \code{"Terminated"})
#'     \item \code{voluntary_mandated} (character): Whether voluntary or mandated
#'     \item \code{recall_initiation_date} (character): Date recall was initiated
#'     \item \code{distribution_pattern} (character): Geographic distribution of product
#'   }
#' @examples
#' \dontrun{
#' # Recent drug recalls
#' fda_drug_recall(max_results = 50)
#'
#' # Most serious (Class I) recalls
#' fda_drug_recall(search = "classification:Class+I")
#'
#' # California recalls in 2024
#' fda_drug_recall(search = "state:CA+AND+report_date:[20240101+TO+20241231]")
#' }
fda_drug_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/enforcement.json", search, api_key = api_key, max_results = max_results)
}


# == Orange Book (drugsfda) ====================================================

#' @title Search FDA approved drug applications (Orange Book)
#'
#' Queries the FDA Orange Book database of approved drug products with
#' therapeutic equivalence evaluations. Results are flattened so each row
#' is one product within an application, with active ingredients, TE codes,
#' and marketing status. Useful for finding generic equivalents and
#' reference listed drugs.
#'
#' @param search openFDA search query. Examples:
#'   \code{"openfda.brand_name:lipitor"} (by brand name),
#'   \code{"sponsor_name:pfizer"} (by sponsor),
#'   \code{"products.te_code:AB"} (therapeutically equivalent products).
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of results. Default \code{50}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{application_number} (character): NDA/ANDA number, e.g. \code{"NDA020702"}
#'     \item \code{sponsor_name} (character): Company holding the approval
#'     \item \code{product_number} (character): Product number within the application
#'     \item \code{brand_name} (character): Brand name of the product
#'     \item \code{dosage_form} (character): Dosage form, e.g. \code{"TABLET"}, \code{"CAPSULE"}
#'     \item \code{route} (character): Route of administration, e.g. \code{"ORAL"}
#'     \item \code{marketing_status} (character): Status, e.g. \code{"Prescription"}, \code{"Discontinued"}
#'     \item \code{te_code} (character): Therapeutic equivalence code, e.g. \code{"AB"}, \code{"BX"}
#'     \item \code{reference_drug} (character): Whether this is the reference listed drug
#'     \item \code{active_ingredients} (character): Active ingredients with strengths, semicolon-separated
#'   }
#' @examples
#' \dontrun{
#' # Look up Lipitor and its generics
#' fda_orange_book(search = "openfda.brand_name:lipitor")
#'
#' # Find all Pfizer approved drugs
#' fda_orange_book(search = "sponsor_name:pfizer", max_results = 100)
#'
#' # Find therapeutically equivalent products
#' fda_orange_book(search = "products.te_code:AB", max_results = 20)
#' }
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

#' @title Search FDA device adverse event reports (MAUDE)
#'
#' Queries the FDA Manufacturer and User Facility Device Experience (MAUDE)
#' database for medical device adverse event reports. Each row is one event
#' report with manufacturer details, device information, and patient outcomes.
#' Reports contain many fields (80+) due to detailed manufacturer data.
#'
#' @param search openFDA search query. Examples:
#'   \code{"device.generic_name:pacemaker"},
#'   \code{"date_received:[20240101+TO+20241231]"}.
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of reports. Default \code{100}.
#' @return A tibble with 80+ columns (all character) including:
#'   \itemize{
#'     \item \code{report_number} (character): MDR report number
#'     \item \code{event_type} (character): Type of event
#'     \item \code{date_received} (character): Date FDA received the report
#'     \item \code{date_of_event} (character): Date the event occurred
#'     \item \code{manufacturer_name} (character): Device manufacturer
#'     \item \code{device} (character): Flattened device information
#'     \item \code{patient} (character): Flattened patient outcome data
#'     \item \code{mdr_text} (character): Narrative text descriptions
#'     \item \code{product_problem_flag} (character): Whether a product problem was reported
#'     \item \code{adverse_event_flag} (character): Whether an adverse event occurred
#'   }
#' @examples
#' \dontrun{
#' # Pacemaker adverse events
#' fda_device_event(search = "device.generic_name:pacemaker", max_results = 50)
#'
#' # Recent device events
#' fda_device_event(search = "date_received:[20240101+TO+20241231]")
#' }
fda_device_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("device/event.json", search, api_key = api_key, max_results = max_results)
}

#' @title Search FDA device recalls
#'
#' Returns FDA medical device recall/enforcement actions. Same structure as
#' \code{fda_drug_recall()} but for devices. Each row is one recall with
#' classification, reason, product description, and distribution details.
#'
#' @param search openFDA search query. Examples:
#'   \code{"classification:Class+I"}, \code{"recalling_firm:medtronic"}.
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of records. Default \code{100}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{recall_number} (character): Unique recall identifier
#'     \item \code{classification} (character): Severity class (I, II, or III)
#'     \item \code{reason_for_recall} (character): Recall reason
#'     \item \code{product_description} (character): Device description
#'     \item \code{recalling_firm} (character): Company name
#'     \item \code{city} (character): City of recalling firm
#'     \item \code{state} (character): State of recalling firm
#'     \item \code{status} (character): Recall status
#'     \item \code{recall_initiation_date} (character): Date initiated
#'     \item \code{distribution_pattern} (character): Distribution area
#'   }
#' @examples
#' \dontrun{
#' # Recent device recalls
#' fda_device_recall(max_results = 50)
#'
#' # Class I device recalls
#' fda_device_recall(search = "classification:Class+I")
#' }
fda_device_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("device/enforcement.json", search, api_key = api_key, max_results = max_results)
}

#' @title Search FDA device classifications
#'
#' Queries the FDA device classification database. Each row is one device
#' classification with product code, device class (I, II, III), review
#' panel, medical specialty, and regulatory pathway information. Useful
#' for understanding what class a device falls into and its regulatory
#' requirements.
#'
#' @param search openFDA search query. Examples:
#'   \code{"device_name:catheter"}, \code{"medical_specialty:CV"} (cardiovascular),
#'   \code{"device_class:3"} (Class III devices).
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of records. Default \code{100}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{device_name} (character): Device name
#'     \item \code{device_class} (character): FDA device class (\code{"1"}, \code{"2"}, \code{"3"})
#'     \item \code{product_code} (character): Three-letter product code
#'     \item \code{medical_specialty} (character): Medical specialty code
#'     \item \code{medical_specialty_description} (character): Full specialty name
#'     \item \code{review_panel} (character): Review panel code
#'     \item \code{regulation_number} (character): CFR regulation number
#'     \item \code{definition} (character): Device definition text
#'     \item \code{implant_flag} (character): Whether the device is an implant
#'     \item \code{life_sustain_support_flag} (character): Life-sustaining flag
#'     \item \code{gmp_exempt_flag} (character): GMP exemption status
#'   }
#' @examples
#' \dontrun{
#' # Search for catheter classifications
#' fda_device_classification(search = "device_name:catheter")
#'
#' # Cardiovascular devices
#' fda_device_classification(search = "medical_specialty:CV", max_results = 50)
#' }
fda_device_classification <- function(search = NULL, api_key = NULL,
                                      max_results = 100) {
  .fda_fetch_all("device/classification.json", search, api_key = api_key, max_results = max_results)
}


# == Food ======================================================================

#' @title Search FDA food recall/enforcement actions
#'
#' Returns FDA food enforcement actions including recalls, market withdrawals,
#' and safety alerts. Each row is one recall event with the same structure
#' as drug and device recalls (classification, reason, firm, distribution).
#'
#' @param search openFDA search query. Examples:
#'   \code{"classification:Class+I"}, \code{"reason_for_recall:salmonella"},
#'   \code{"state:CA"}. Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of records. Default \code{100}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{recall_number} (character): Unique recall identifier
#'     \item \code{classification} (character): Severity class (I, II, or III)
#'     \item \code{reason_for_recall} (character): Recall reason (e.g. undeclared allergen)
#'     \item \code{product_description} (character): Food product description
#'     \item \code{recalling_firm} (character): Company name
#'     \item \code{city} (character): City of recalling firm
#'     \item \code{state} (character): State of recalling firm
#'     \item \code{status} (character): Recall status
#'     \item \code{distribution_pattern} (character): Distribution area
#'     \item \code{recall_initiation_date} (character): Date initiated
#'   }
#' @examples
#' \dontrun{
#' # Recent food recalls
#' fda_food_recall(max_results = 50)
#'
#' # Salmonella-related recalls
#' fda_food_recall(search = "reason_for_recall:salmonella")
#' }
fda_food_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("food/enforcement.json", search, api_key = api_key, max_results = max_results)
}

#' @title Search FDA food adverse event reports (CAERS)
#'
#' Queries the FDA Center for Food Safety and Applied Nutrition Adverse
#' Event Reporting System (CAERS) for food and dietary supplement adverse
#' event reports. Each row is one report with product information and
#' adverse outcomes.
#'
#' @param search openFDA search query. Examples:
#'   \code{"products.industry_name:dietary+supplement"},
#'   \code{"reactions:nausea"}.
#'   Default \code{NULL}.
#' @param api_key Optional FDA API key. Default \code{NULL}.
#' @param max_results Maximum number of reports. Default \code{100}.
#' @return A tibble with columns (all character) including report number,
#'   date information, product details, reactions, and outcomes. Column
#'   names vary by report completeness.
#' @examples
#' \dontrun{
#' # Recent food adverse events
#' fda_food_event(max_results = 50)
#'
#' # Dietary supplement adverse events
#' fda_food_event(search = "products.industry_name:dietary+supplement")
#' }
fda_food_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("food/event.json", search, api_key = api_key, max_results = max_results)
}


# == Context ===================================================================

#' Get api.fda.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fda_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fda_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.fda.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.fda.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.fda.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.fda.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
