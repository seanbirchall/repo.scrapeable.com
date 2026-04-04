# api.fda.gov.R
# Self-contained api.fda.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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

#' Search FDA drug adverse event reports (FAERS)
#'
#' Queries the FDA Adverse Event Reporting System (FAERS) database,
#' which contains reports of adverse drug reactions submitted by
#' healthcare professionals, consumers, and manufacturers.
#'
#' @param search Character or NULL. openFDA search query. Examples:
#'   \code{"patient.drug.openfda.brand_name:aspirin"},
#'   \code{"patient.reaction.reactionmeddrapt:nausea"},
#'   \code{"receivedate:[20230101+TO+20231231]"}.
#'   NULL returns unfiltered results.
#' @param api_key Character or NULL. Optional API key from
#'   \url{https://open.fda.gov/apis/authentication/}. Without a key:
#'   240 requests/minute, 120K/day.
#' @param max_results Integer. Maximum results to return (default 100,
#'   max ~26000 via pagination).
#' @return A tibble with columns including: safetyreportid, receivedate,
#'   serious, seriousnessdeath, patient (flattened), and more (varies by report).
#' @export
#' @examples
#' \dontrun{
#' fda_drug_event("patient.drug.openfda.brand_name:aspirin", max_results = 10)
#' }
fda_drug_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/event.json", search, api_key = api_key, max_results = max_results)
}

#' Count drug adverse events by field
#'
#' Returns the top values and their counts for a specified field across all
#' matching adverse event reports. Useful for finding the most common
#' reactions, drugs, or outcomes.
#'
#' @param field Character. Field to count. Common fields:
#'   \code{"patient.reaction.reactionmeddrapt.exact"} (reactions),
#'   \code{"patient.drug.openfda.brand_name.exact"} (brand names),
#'   \code{"patient.drug.openfda.generic_name.exact"} (generic names),
#'   \code{"serious"} (seriousness flag).
#' @param search Character or NULL. Optional search filter to narrow
#'   the events before counting.
#' @param limit Integer. Number of top values to return (default 10, max 1000).
#' @param api_key Character or NULL. Optional API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{term}{Character. Field value (e.g. "NAUSEA", "DEATH").}
#'     \item{count}{Integer. Number of reports with this value.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fda_drug_event_count("patient.reaction.reactionmeddrapt.exact", limit = 10)
#' fda_drug_event_count("patient.drug.openfda.brand_name.exact",
#'                      search = "patient.reaction.reactionmeddrapt:nausea")
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

#' Search FDA drug labeling (package inserts)
#'
#' Queries Structured Product Labeling (SPL) data, which includes the full
#' text of drug package inserts: indications, dosage, warnings,
#' contraindications, and more. Results are large documents.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"openfda.brand_name:lipitor"},
#'   \code{"indications_and_usage:diabetes"},
#'   \code{"openfda.generic_name:metformin"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 10; labels are large).
#' @return A tibble with columns including: effective_time, indications_and_usage,
#'   dosage_and_administration, warnings, adverse_reactions, openfda (flattened).
#' @export
#' @examples
#' \dontrun{
#' fda_drug_label("openfda.brand_name:lipitor", max_results = 2)
#' }
fda_drug_label <- function(search = NULL, api_key = NULL, max_results = 10) {
  .fda_fetch_all("drug/label.json", search, api_key = api_key, max_results = max_results)
}


# == Drug NDC directory ========================================================

#' Search the National Drug Code (NDC) directory
#'
#' Queries the FDA's NDC directory, a registry of all drugs marketed in
#' the US. Each record includes product codes, names, dosage forms,
#' routes of administration, and active ingredients.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"brand_name:tylenol"}, \code{"generic_name:acetaminophen"},
#'   \code{"product_ndc:0002-4112"}, \code{"dosage_form:TABLET"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: product_ndc, brand_name,
#'   generic_name, labeler_name, dosage_form, route, marketing_category,
#'   active_ingredients, packaging, and more (~18 columns).
#' @export
#' @examples
#' \dontrun{
#' fda_drug_ndc("brand_name:tylenol")
#' fda_drug_ndc("generic_name:metformin", max_results = 20)
#' }
fda_drug_ndc <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/ndc.json", search, api_key = api_key, max_results = max_results)
}


# == Drug recalls (enforcement) ================================================

#' Search FDA drug recall/enforcement actions
#'
#' Queries FDA drug enforcement reports, including recalls, market
#' withdrawals, and safety alerts. Classification indicates severity:
#' Class I (most serious) to Class III.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"classification:Class+I"} (most serious recalls),
#'   \code{"state:CA"}, \code{"report_date:[20240101+TO+20241231]"},
#'   \code{"reason_for_recall:contamination"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: recall_number, classification,
#'   reason_for_recall, product_description, recalling_firm, city, state,
#'   report_date, voluntary_mandated, status, and more.
#' @export
#' @examples
#' \dontrun{
#' fda_drug_recall("classification:Class+I", max_results = 20)
#' }
fda_drug_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("drug/enforcement.json", search, api_key = api_key, max_results = max_results)
}


# == Orange Book (drugsfda) ====================================================

#' Search FDA approved drug applications (Orange Book)
#'
#' The Orange Book contains approved drug products with therapeutic
#' equivalence evaluations. Products are flattened so each row represents
#' one product within an application.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"openfda.brand_name:lipitor"},
#'   \code{"sponsor_name:pfizer"},
#'   \code{"products.te_code:AB"} (therapeutically equivalent).
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{application_number}{Character. NDA/ANDA number.}
#'     \item{sponsor_name}{Character. Sponsoring company.}
#'     \item{product_number}{Character. Product number within the application.}
#'     \item{brand_name}{Character. Brand name.}
#'     \item{dosage_form}{Character. Dosage form (e.g. "TABLET").}
#'     \item{route}{Character. Route of administration (e.g. "ORAL").}
#'     \item{marketing_status}{Character. Marketing status.}
#'     \item{te_code}{Character. Therapeutic equivalence code (e.g. "AB").}
#'     \item{reference_drug}{Character. Whether this is a reference drug ("Yes"/"No").}
#'     \item{active_ingredients}{Character. Semicolon-separated ingredients with strengths.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fda_orange_book("openfda.brand_name:lipitor")
#' fda_orange_book("sponsor_name:pfizer", max_results = 20)
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

#' Search FDA device adverse event reports (MAUDE)
#'
#' Queries the Manufacturer and User Facility Device Experience (MAUDE)
#' database, which contains medical device adverse event reports.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"device.generic_name:pacemaker"},
#'   \code{"date_received:[20240101+TO+20241231]"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: report_number, date_received,
#'   device (flattened), event_type, mdr_text, and more.
#' @export
#' @examples
#' \dontrun{
#' fda_device_event("device.generic_name:pacemaker", max_results = 10)
#' }
fda_device_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("device/event.json", search, api_key = api_key, max_results = max_results)
}

#' Search FDA device recalls
#'
#' Queries FDA medical device enforcement/recall reports. Includes
#' classification, recalling firm, product description, and distribution
#' patterns.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"classification:Class+I"}, \code{"recalling_firm:medtronic"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: recall_number, classification,
#'   reason_for_recall, product_description, recalling_firm, and more.
#' @export
#' @examples
#' \dontrun{
#' fda_device_recall("classification:Class+I", max_results = 20)
#' }
fda_device_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("device/enforcement.json", search, api_key = api_key, max_results = max_results)
}

#' Search FDA device classifications (510k, PMA, etc.)
#'
#' Queries the FDA device classification database, which assigns each
#' device type a regulatory class (I, II, or III) and product code.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"device_name:catheter"}, \code{"medical_specialty:CV"}
#'   (cardiovascular), \code{"regulation_number:870"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: device_name, device_class,
#'   product_code, medical_specialty, regulation_number, and more.
#' @export
#' @examples
#' \dontrun{
#' fda_device_classification("device_name:catheter", max_results = 20)
#' }
fda_device_classification <- function(search = NULL, api_key = NULL,
                                      max_results = 100) {
  .fda_fetch_all("device/classification.json", search, api_key = api_key, max_results = max_results)
}


# == Food ======================================================================

#' Search FDA food recall/enforcement actions
#'
#' Queries FDA food enforcement reports, including recalls for
#' contamination, allergen mislabeling, and other safety concerns.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"classification:Class+I"}, \code{"reason_for_recall:salmonella"},
#'   \code{"state:CA"}, \code{"report_date:[20240101+TO+20241231]"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: recall_number, classification,
#'   reason_for_recall, product_description, recalling_firm, city, state,
#'   distribution_pattern, and more.
#' @export
#' @examples
#' \dontrun{
#' fda_food_recall("reason_for_recall:salmonella", max_results = 20)
#' }
fda_food_recall <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("food/enforcement.json", search, api_key = api_key, max_results = max_results)
}

#' Search FDA food adverse event reports (CAERS)
#'
#' Queries the Center for Food Safety and Applied Nutrition Adverse Event
#' Reporting System (CAERS), covering dietary supplements, cosmetics,
#' and food products.
#'
#' @param search Character or NULL. Search query. Examples:
#'   \code{"products.name_brand:monster+energy"},
#'   \code{"reactions:hospitalization"}.
#' @param api_key Character or NULL. Optional API key.
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including: report_number, date_started,
#'   outcomes, products (flattened), reactions, consumer (flattened).
#' @export
#' @examples
#' \dontrun{
#' fda_food_event(max_results = 10)
#' }
fda_food_event <- function(search = NULL, api_key = NULL, max_results = 100) {
  .fda_fetch_all("food/event.json", search, api_key = api_key, max_results = max_results)
}


# == Context ===================================================================

#' Get fda.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fda.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fda.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fda.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fda.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
