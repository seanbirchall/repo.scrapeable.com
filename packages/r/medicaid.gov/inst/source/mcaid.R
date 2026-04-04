# medicaid.gov.R - Self-contained Medicaid.gov client
# Fetches drug pricing, enrollment, eligibility, and managed care data
# from download.medicaid.gov CSV endpoints.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Prefix: mcaid_

library(httr2)
library(dplyr)
library(tibble)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(300) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url, limit = NULL) {
  f <- .fetch(url, ".csv")
  df <- tryCatch(
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      message("Error reading CSV: ", conditionMessage(e))
      data.frame()
    }
  )
  if (!is.null(limit)) df <- utils::head(df, limit)
  tibble::as_tibble(df)
}

.safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub("[,$]", "", x)))
}

.safe_date <- function(x, formats = c("%m/%d/%Y", "%Y-%m-%d")) {
  out <- rep(as.Date(NA), length(x))
  for (fmt in formats) {
    missing <- is.na(out)
    if (!any(missing)) break
    out[missing] <- as.Date(x[missing], format = fmt)
  }
  out
}

.safe_integer <- function(x) {
  suppressWarnings(as.integer(gsub("[,]", "", x)))
}

# == Catalog ===================================================================

.mcaid_catalog <- tibble::tibble(
  name = c(
    "NADAC (National Average Drug Acquisition Cost)",
    "NADAC Comparison",
    "First Time NADAC Rates",
    "Drug Products (Rebate Program)",
    "Drug AMP Reporting Quarterly",
    "Medicaid/CHIP Enrollment",
    "Eligibility Renewals",
    "CHIP Enrollment (CAA Reporting)",
    "ACA Federal Upper Limits",
    "Managed Care Programs by State",
    "FMAP Expenditure",
    "Newly Reported Drugs"
  ),
  description = c(
    "Weekly national average drug acquisition cost per unit for pharmacy-dispensed drugs",
    "Weekly NADAC rate changes showing old vs new prices and percent change",
    "First-time NADAC rates for newly surveyed drugs",
    "Active drug products in the Medicaid Drug Rebate Program with NDC and labeler info",
    "Average Manufacturer Price quarterly reporting data",
    "State-level Medicaid and CHIP enrollment, applications, and determinations",
    "Beneficiary renewal and eligibility processing data by state",
    "Separate CHIP enrollment by state and coverage month",
    "ACA Federal Upper Limit amounts for generic and biosimilar drugs",
    "Managed care program characteristics and populations by state",
    "Federal matching (FFCRA increased FMAP) expenditure data by state",
    "Drugs newly reported to the Medicaid Drug Rebate Program"
  ),
  category = c(
    "Drug Pricing", "Drug Pricing", "Drug Pricing",
    "Drug Rebate Program", "Drug Pricing",
    "Enrollment", "Enrollment", "Enrollment",
    "Drug Pricing", "Managed Care",
    "Expenditure", "Drug Rebate Program"
  ),
  url = c(
    "https://download.medicaid.gov/data/nadac-national-average-drug-acquisition-cost-04-01-2026.csv",
    "https://download.medicaid.gov/data/nadac-comparison-04-01-2026.csv",
    "https://download.medicaid.gov/data/first-time-nadac-rates-03232026.csv",
    "https://download.medicaid.gov/data/drugproducts4q_2025Updated02122026.csv",
    "https://download.medicaid.gov/data/DrugAMPReportingQuarterly4Q2025.csv",
    "https://download.medicaid.gov/data/pi-dataset-march-2026release.csv",
    "https://download.medicaid.gov/data/renewal-dataset-march-2026-release.csv",
    "https://download.medicaid.gov/data/current-chip-caa-reporting-metrics-march-2026-release.csv",
    "https://download.medicaid.gov/data/aca-federal-upper-limits-03302026.csv",
    "https://download.medicaid.gov/data/managed-care-program-by-state-2023.csv",
    "https://download.medicaid.gov/data/Medicaid-CMS-64-FFCRA-Increased-FMAP-Expenditure-03182026.csv",
    "https://download.medicaid.gov/data/mdrp-newly-rprt-drug-03232026-to-03292026.csv"
  ),
  format = rep("CSV", 12),
  update_frequency = c(
    "Weekly", "Weekly", "Weekly",
    "Quarterly", "Quarterly",
    "Monthly", "Monthly", "Monthly",
    "Weekly", "Annual",
    "Quarterly", "Weekly"
  )
)

# == Schemas ===================================================================

.schema_nadac <- tibble::tibble(
  ndc_description = character(),
  ndc = character(),
  nadac_per_unit = numeric(),
  effective_date = as.Date(character()),
  pricing_unit = character(),
  pharmacy_type = character(),
  otc = character(),
  explanation_code = character(),
  classification = character(),
  as_of_date = as.Date(character())
)

.schema_nadac_comparison <- tibble::tibble(
  ndc_description = character(),
  ndc = character(),
  old_nadac_per_unit = numeric(),
  new_nadac_per_unit = numeric(),
  classification = character(),
  percent_change = numeric(),
  primary_reason = character(),
  start_date = as.Date(character()),
  end_date = as.Date(character()),
  effective_date = as.Date(character())
)

.schema_drug_products <- tibble::tibble(
  year = integer(),
  quarter = integer(),
  labeler_name = character(),
  ndc = character(),
  drug_category = character(),
  drug_type_indicator = character(),
  unit_type = character(),
  fda_product_name = character(),
  fda_approval_date = as.Date(character()),
  market_date = as.Date(character())
)

.schema_enrollment <- tibble::tibble(
  state_abbreviation = character(),
  state_name = character(),
  reporting_period = character(),
  state_expanded_medicaid = character(),
  preliminary_or_updated = character(),
  final_report = character()
)

.schema_chip_enrollment <- tibble::tibble(
  state_abbreviation = character(),
  coverage_month = character(),
  tmsis_data_through_month = character(),
  schip_enrollment = integer(),
  data_notes = character()
)

.schema_ful <- tibble::tibble(
  product_group = character(),
  ingredient = character(),
  strength = character(),
  dosage = character(),
  route = character(),
  unit_type = character(),
  weighted_avg_amp = numeric(),
  aca_ful = numeric(),
  package_size = character(),
  ndc = character(),
  a_rated = character(),
  year = integer(),
  month = integer()
)

.schema_managed_care <- tibble::tibble(
  features = character(),
  program_type = character(),
  statewide_or_regional = character(),
  federal_authority = character(),
  program_start_date = character()
)

# == Public functions ==========================================================

#' List all available Medicaid datasets
#'
#' Returns a catalog tibble of datasets available via download.medicaid.gov.
#' @return tibble with columns: name, description, category, url, format, update_frequency
mcaid_list <- function() {
  .mcaid_catalog
}

#' Search available Medicaid datasets
#'
#' @param query Character string to search for (case-insensitive grep)
#' @return tibble of matching datasets
mcaid_search <- function(query) {
  pattern <- tolower(query)
  matches <- grepl(pattern, tolower(.mcaid_catalog$name)) |
    grepl(pattern, tolower(.mcaid_catalog$description)) |
    grepl(pattern, tolower(.mcaid_catalog$category))
  .mcaid_catalog[matches, ]
}

#' NADAC (National Average Drug Acquisition Cost)
#'
#' Weekly drug acquisition cost per unit for pharmacy-dispensed covered drugs.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with drug name, NDC, NADAC per unit, effective date, pricing unit, etc.
mcaid_nadac <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/nadac-national-average-drug-acquisition-cost-04-01-2026.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_nadac)
  result <- tibble::tibble(
    ndc_description     = as.character(df[["NDC Description"]]),
    ndc                 = as.character(df[["NDC"]]),
    nadac_per_unit      = .safe_numeric(df[["NADAC Per Unit"]]),
    effective_date      = .safe_date(df[["Effective Date"]]),
    pricing_unit        = as.character(df[["Pricing Unit"]]),
    pharmacy_type       = as.character(df[["Pharmacy Type Indicator"]]),
    otc                 = as.character(df[["OTC"]]),
    explanation_code    = as.character(df[["Explanation Code"]]),
    classification      = as.character(df[["Classification for Rate Setting"]]),
    as_of_date          = .safe_date(df[["As of Date"]])
  )
  result
}

#' NADAC Weekly Comparison
#'
#' Shows weekly changes in NADAC rates with old/new prices and percent change.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with drug name, NDC, old/new prices, percent change, reason, dates
mcaid_nadac_comparison <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/nadac-comparison-04-01-2026.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_nadac_comparison)
  tibble::tibble(
    ndc_description     = as.character(df[["NDC Description"]]),
    ndc                 = as.character(df[["NDC"]]),
    old_nadac_per_unit  = .safe_numeric(df[["Old NADAC Per Unit"]]),
    new_nadac_per_unit  = .safe_numeric(df[["New NADAC Per Unit"]]),
    classification      = as.character(df[["Classification for Rate Setting"]]),
    percent_change      = .safe_numeric(df[["Percent Change"]]),
    primary_reason      = as.character(df[["Primary Reason"]]),
    start_date          = .safe_date(df[["Start Date"]]),
    end_date            = .safe_date(df[["End Date"]]),
    effective_date      = .safe_date(df[["Effective Date"]])
  )
}

#' Drug Products in the Medicaid Drug Rebate Program
#'
#' Active drug products with NDC, labeler, category, and FDA info.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with year, quarter, labeler, NDC, drug category, FDA product name, etc.
mcaid_drug_products <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/drugproducts4q_2025Updated02122026.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_drug_products)
  tibble::tibble(
    year                = .safe_integer(df[["Year"]]),
    quarter             = .safe_integer(df[["Quarter"]]),
    labeler_name        = as.character(df[["Labeler Name"]]),
    ndc                 = as.character(df[["NDC"]]),
    labeler_code        = as.character(df[["Labeler Code"]]),
    product_code        = as.character(df[["Product Code"]]),
    package_size_code   = as.character(df[["Package Size Code"]]),
    drug_category       = as.character(df[["Drug Category"]]),
    drug_type_indicator = as.character(df[["Drug Type Indicator"]]),
    termination_date    = as.character(df[["Termination Date"]]),
    unit_type           = as.character(df[["Unit Type"]]),
    units_per_pkg       = .safe_numeric(df[["Units Per Pkg Size"]]),
    fda_approval_date   = .safe_date(df[["FDA Approval Date"]]),
    market_date         = .safe_date(df[["Market Date"]]),
    fda_te_code         = as.character(df[["FDA Therapeutic Equivalence Code"]]),
    fda_product_name    = as.character(df[["FDA Product Name"]]),
    clotting_factor     = as.character(df[["Clotting Factor Indicator"]]),
    pediatric           = as.character(df[["Pediatric Indicator"]])
  )
}

#' Medicaid/CHIP Enrollment and Applications
#'
#' State-level enrollment, applications, and determinations data.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with state, reporting period, enrollment figures
mcaid_enrollment <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/pi-dataset-march-2026release.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_enrollment)
  # This dataset has many columns including footnotes; keep the key ones
  cols_keep <- names(df)[!grepl("footnot", names(df), ignore.case = TRUE)]
  result <- df[, cols_keep, drop = FALSE]
  # Clean column names
  names(result) <- gsub("\\s+", "_", tolower(names(result)))
  tibble::as_tibble(result)
}

#' Medicaid Eligibility Renewals
#'
#' Beneficiary renewal and eligibility processing data by state.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with state, reporting period, renewal counts
mcaid_eligibility <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/renewal-dataset-march-2026-release.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(tibble::tibble())
  cols_keep <- names(df)[!grepl("footnot", names(df), ignore.case = TRUE)]
  result <- df[, cols_keep, drop = FALSE]
  names(result) <- gsub("\\s+", "_", tolower(names(result)))
  tibble::as_tibble(result)
}

#' CHIP Enrollment by State
#'
#' Separate CHIP enrollment counts by state and coverage month.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with state, coverage month, CHIP enrollment count
mcaid_chip_enrollment <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/current-chip-caa-reporting-metrics-march-2026-release.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_chip_enrollment)
  tibble::tibble(
    state_abbreviation    = as.character(df[["State Abbreviation"]]),
    coverage_month        = as.character(df[["Coverage Month"]]),
    tmsis_data_through    = as.character(df[["T-MSIS Data Through Month"]]),
    schip_enrollment      = .safe_integer(df[["S-CHIP Enrollment"]]),
    data_notes            = as.character(df[["Data Notes"]])
  )
}

#' ACA Federal Upper Limits
#'
#' Federal Upper Limit amounts for generic and biosimilar drugs.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with ingredient, strength, dosage, ACA FUL, NDC, etc.
mcaid_ful <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/aca-federal-upper-limits-03302026.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_ful)
  tibble::tibble(
    product_group       = as.character(df[["Product Group"]]),
    ingredient          = as.character(df[["Ingredient"]]),
    strength            = as.character(df[["Strength"]]),
    dosage              = as.character(df[["Dosage"]]),
    route               = as.character(df[["Route"]]),
    unit_type           = as.character(df[["MDR Unit Type"]]),
    weighted_avg_amp    = .safe_numeric(df[["Weighted Average of AMPs"]]),
    aca_ful             = .safe_numeric(df[["ACA FUL"]]),
    package_size        = as.character(df[["Package Size"]]),
    ndc                 = as.character(df[["NDC"]]),
    a_rated             = as.character(df[["A-Rated"]]),
    multiplier_gt_175   = as.character(df[["Multiplier Greater Than 175 Percent of Weighted Avg of AMPs"]]),
    year                = .safe_integer(df[["Year"]]),
    month               = .safe_integer(df[["Month"]])
  )
}

#' Managed Care Programs by State
#'
#' Managed care program characteristics, populations, and services by state.
#' @param limit Maximum number of rows to return (NULL for all)
#' @return tibble with program features, type, authority, populations
mcaid_managed_care <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/managed-care-program-by-state-2023.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_managed_care)
  names(df) <- gsub("\\s+", "_", tolower(names(df)))
  tibble::as_tibble(df)
}

#' Return the source of this client for LLM context
#'
#' Reads its own source file and returns function signatures and bodies.
#' @return Character string with full source code
mcaid_context <- function() {
  src_file <- tryCatch(
    {
      env <- environment(mcaid_context)
      src <- attr(env, "srcfile")
      if (!is.null(src)) src$filename
      else NULL
    },
    error = function(e) NULL
  )
  if (is.null(src_file) || !file.exists(src_file)) {
    src_file <- sys.frame(sys.nframe())$ofile
  }
  if (is.null(src_file) || !file.exists(src_file)) {
    src_file <- "clients/medicaid.gov.R"
  }
  if (file.exists(src_file)) {
    paste(readLines(src_file, warn = FALSE), collapse = "\n")
  } else {
    "Source file not found."
  }
}
