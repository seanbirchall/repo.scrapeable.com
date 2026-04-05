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
#' Returns a catalog of 12 datasets available for download from
#' download.medicaid.gov. Use this to discover what data is available
#' before calling domain-specific functions like \code{mcaid_nadac()} or
#' \code{mcaid_enrollment()}. Each row includes a direct CSV download URL.
#'
#' @return A tibble with columns:
#'   - name (character): Dataset title (e.g. "NADAC (National Average Drug Acquisition Cost)")
#'   - description (character): Brief description of dataset contents
#'   - category (character): One of "Drug Pricing", "Drug Rebate Program",
#'     "Enrollment", "Managed Care", "Expenditure"
#'   - url (character): Direct download URL for the CSV file
#'   - format (character): Always "CSV"
#'   - update_frequency (character): "Weekly", "Monthly", "Quarterly", or "Annual"
#' @examples
#' \dontrun{
#' # Browse all available datasets
#' mcaid_list()
#'
#' # Filter to drug pricing datasets
#' mcaid_list() |> dplyr::filter(category == "Drug Pricing")
#' }
#' @export
mcaid_list <- function() {
  .mcaid_catalog
}

#' Search available Medicaid datasets by keyword
#'
#' Searches the Medicaid dataset catalog by name, description, and category.
#' Use this when you know roughly what data you need but not the exact
#' function name. Returns the same columns as \code{mcaid_list()}.
#'
#' @param query Character string to search for (case-insensitive). Matched
#'   against name, description, and category fields. Examples: "drug",
#'   "enrollment", "managed care", "NADAC", "CHIP".
#' @return A tibble of matching datasets with columns: name, description,
#'   category, url, format, update_frequency. Empty tibble if no matches.
#' @examples
#' \dontrun{
#' # Find drug pricing datasets
#' mcaid_search("drug")
#'
#' # Find enrollment data
#' mcaid_search("enrollment")
#' }
#' @export
mcaid_search <- function(query) {
  pattern <- tolower(query)
  matches <- grepl(pattern, tolower(.mcaid_catalog$name)) |
    grepl(pattern, tolower(.mcaid_catalog$description)) |
    grepl(pattern, tolower(.mcaid_catalog$category))
  .mcaid_catalog[matches, ]
}

#' Fetch NADAC drug acquisition cost data
#'
#' Returns weekly National Average Drug Acquisition Cost (NADAC) data
#' for retail community pharmacies. Includes per-unit pricing by NDC,
#' pharmacy type, and effective date. Updated weekly by CMS. The full
#' dataset contains ~30,000 rows covering all surveyed drug products.
#'
#' @param limit Maximum rows to return. NULL for all rows (warning: ~30k rows).
#'   Default NULL.
#' @return A tibble with columns:
#'   - ndc_description (character): Drug product name and strength
#'   - ndc (character): 11-digit National Drug Code
#'   - nadac_per_unit (numeric): Average acquisition cost per unit in USD
#'   - effective_date (Date): Date this price became effective
#'   - pricing_unit (character): Unit of measure ("EA", "ML", "GM")
#'   - pharmacy_type (character): Pharmacy type indicator ("C/I" = chain/independent)
#'   - otc (character): Over-the-counter flag ("Y"/"N")
#'   - explanation_code (character): Reason code for pricing
#'   - classification (character): "G" (generic) or "B" (brand)
#'   - as_of_date (Date): Survey reference date
#' @examples
#' \dontrun{
#' # Get latest NADAC prices (first 100)
#' prices <- mcaid_nadac(limit = 100)
#'
#' # Find prices for metformin
#' prices |> dplyr::filter(grepl("metformin", ndc_description, ignore.case = TRUE))
#'
#' # Compare generic vs brand pricing
#' mcaid_nadac(limit = 500) |>
#'   dplyr::group_by(classification) |>
#'   dplyr::summarise(avg_cost = mean(nadac_per_unit, na.rm = TRUE))
#' }
#' @export
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

#' Fetch NADAC weekly price change comparison
#'
#' Returns weekly NADAC rate changes showing old vs new prices and percent
#' change. Use this to track which drugs had price increases or decreases
#' in the most recent survey. Complements \code{mcaid_nadac()} which shows
#' current prices only.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with columns:
#'   - ndc_description (character): Drug product name and strength
#'   - ndc (character): 11-digit National Drug Code
#'   - old_nadac_per_unit (numeric): Previous NADAC price per unit in USD
#'   - new_nadac_per_unit (numeric): Updated NADAC price per unit in USD
#'   - classification (character): "G" (generic) or "B" (brand)
#'   - percent_change (numeric): Percentage change from old to new price
#'   - primary_reason (character): Reason for price change
#'   - start_date (Date): Start of comparison period
#'   - end_date (Date): End of comparison period
#'   - effective_date (Date): Date new price takes effect
#' @examples
#' \dontrun{
#' # Get all price changes this week
#' changes <- mcaid_nadac_comparison()
#'
#' # Find largest price increases
#' changes |> dplyr::arrange(desc(percent_change)) |> head(20)
#'
#' # Find drugs with >50% price change
#' changes |> dplyr::filter(abs(percent_change) > 50)
#' }
#' @export
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

#' Fetch Medicaid Drug Rebate Program product listing
#'
#' Returns active drug products registered in the Medicaid Drug Rebate
#' Program (MDRP). Includes manufacturer (labeler) information, NDC codes,
#' FDA approval data, and drug classification. Updated quarterly. The full
#' dataset contains ~100k+ product records.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with columns:
#'   - year (integer): Reporting year
#'   - quarter (integer): Reporting quarter (1-4)
#'   - labeler_name (character): Drug manufacturer/labeler name
#'   - ndc (character): 11-digit National Drug Code
#'   - labeler_code (character): First 5 digits of NDC identifying manufacturer
#'   - product_code (character): Middle 4 digits of NDC identifying product
#'   - package_size_code (character): Last 2 digits of NDC identifying package
#'   - drug_category (character): "S" (single source), "I" (innovator multiple source),
#'     "N" (non-innovator multiple source)
#'   - drug_type_indicator (character): Drug type classification
#'   - termination_date (character): Date product was terminated from program, if applicable
#'   - unit_type (character): Unit of measure ("TAB", "CAP", "ML", "GM", "EA")
#'   - units_per_pkg (numeric): Number of units per package
#'   - fda_approval_date (Date): FDA approval date
#'   - market_date (Date): Date product entered market
#'   - fda_te_code (character): FDA therapeutic equivalence code
#'   - fda_product_name (character): FDA-registered product name
#'   - clotting_factor (character): Clotting factor indicator ("Y"/"N")
#'   - pediatric (character): Pediatric indicator ("Y"/"N")
#' @examples
#' \dontrun{
#' # Get first 200 drug products
#' drugs <- mcaid_drug_products(limit = 200)
#'
#' # Find products from a specific manufacturer
#' drugs |> dplyr::filter(grepl("PFIZER", labeler_name, ignore.case = TRUE))
#'
#' # Count products by drug category
#' mcaid_drug_products(limit = 1000) |> dplyr::count(drug_category)
#' }
#' @export
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

#' Fetch Medicaid/CHIP enrollment, applications, and determinations
#'
#' Returns state-level Performance Indicator data including total Medicaid
#' and CHIP enrollment, new applications, eligibility determinations, and
#' call center metrics. Updated monthly. Footnote columns are automatically
#' removed and column names are cleaned to snake_case.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with snake_case column names. Key columns include:
#'   - state_abbreviation (character): Two-letter state code
#'   - state_name (character): Full state name
#'   - reporting_period (character): Month/year of report
#'   - state_expanded_medicaid (character): Whether state expanded Medicaid ("Y"/"N")
#'   - preliminary_or_updated (character): Data freshness indicator
#'   - final_report (character): Whether this is the final report for the period
#'   Additional columns vary but typically include enrollment counts,
#'   application counts, and determination metrics.
#' @examples
#' \dontrun{
#' # Get enrollment data
#' enrollment <- mcaid_enrollment(limit = 200)
#'
#' # Compare expansion vs non-expansion states
#' enrollment |>
#'   dplyr::group_by(state_expanded_medicaid) |>
#'   dplyr::summarise(n_states = dplyr::n_distinct(state_abbreviation))
#' }
#' @export
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

#' Fetch Medicaid eligibility renewal processing data
#'
#' Returns state-level data on beneficiary eligibility renewals including
#' total renewals due, completed, and outcomes (renewed vs terminated).
#' Tracks Medicaid unwinding progress post-COVID continuous enrollment.
#' Updated monthly. Footnote columns are automatically removed and column
#' names are cleaned to snake_case.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with snake_case column names including state, reporting
#'   period, renewal counts, and outcome breakdowns. Exact columns vary
#'   by reporting period.
#' @examples
#' \dontrun{
#' # Get renewal data
#' renewals <- mcaid_eligibility(limit = 200)
#'
#' # View available columns
#' names(renewals)
#' }
#' @export
mcaid_eligibility <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/renewal-dataset-march-2026-release.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(tibble::tibble())
  cols_keep <- names(df)[!grepl("footnot", names(df), ignore.case = TRUE)]
  result <- df[, cols_keep, drop = FALSE]
  names(result) <- gsub("\\s+", "_", tolower(names(result)))
  tibble::as_tibble(result)
}

#' Fetch separate CHIP enrollment by state and month
#'
#' Returns Children's Health Insurance Program (CHIP) enrollment counts
#' by state and coverage month, reported under the CAA (Consolidated
#' Appropriations Act) requirements. Use this for CHIP-specific enrollment
#' trends; for combined Medicaid/CHIP data use \code{mcaid_enrollment()}.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with columns:
#'   - state_abbreviation (character): Two-letter state code
#'   - coverage_month (character): Month of coverage (e.g. "January 2026")
#'   - tmsis_data_through (character): T-MSIS data through month
#'   - schip_enrollment (integer): Separate CHIP enrollment count
#'   - data_notes (character): State-specific data notes or caveats
#' @examples
#' \dontrun{
#' # Get CHIP enrollment
#' chip <- mcaid_chip_enrollment(limit = 200)
#'
#' # Total CHIP enrollment by state
#' chip |>
#'   dplyr::group_by(state_abbreviation) |>
#'   dplyr::summarise(latest_enrollment = dplyr::last(schip_enrollment))
#' }
#' @export
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

#' Fetch ACA Federal Upper Limit (FUL) drug prices
#'
#' Returns ACA Federal Upper Limit amounts for generic and biosimilar drugs.
#' FUL sets the maximum federal reimbursement for multi-source drugs.
#' Based on 175% of the weighted average of AMP (Average Manufacturer Price).
#' Updated weekly. Use this for Medicaid pharmacy reimbursement analysis.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with columns:
#'   - product_group (character): Therapeutic product group
#'   - ingredient (character): Active ingredient name
#'   - strength (character): Drug strength (e.g. "500 MG")
#'   - dosage (character): Dosage form (e.g. "TABLET", "CAPSULE")
#'   - route (character): Route of administration ("ORAL", "TOPICAL", etc.)
#'   - unit_type (character): MDR unit type ("TAB", "CAP", "ML", "GM", "EA")
#'   - weighted_avg_amp (numeric): Weighted average AMP in USD
#'   - aca_ful (numeric): ACA Federal Upper Limit per unit in USD
#'   - package_size (character): Package size description
#'   - ndc (character): 11-digit National Drug Code
#'   - a_rated (character): FDA A-rated therapeutic equivalent ("Y"/"N")
#'   - multiplier_gt_175 (character): Whether FUL exceeds 175% of weighted avg AMP
#'   - year (integer): FUL effective year
#'   - month (integer): FUL effective month (1-12)
#' @examples
#' \dontrun{
#' # Get FUL prices
#' ful <- mcaid_ful(limit = 200)
#'
#' # Find FUL for metformin tablets
#' ful |> dplyr::filter(grepl("METFORMIN", ingredient))
#'
#' # Compare ACA FUL to weighted avg AMP
#' ful |> dplyr::mutate(markup = aca_ful / weighted_avg_amp)
#' }
#' @export
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

#' Fetch Medicaid managed care programs by state
#'
#' Returns managed care program characteristics including program type,
#' covered populations, service areas, and federal authority. Provides
#' a snapshot of how each state structures its Medicaid managed care
#' delivery system. Updated annually. Column names are cleaned to
#' snake_case automatically.
#'
#' @param limit Maximum rows to return. NULL for all rows. Default NULL.
#' @return A tibble with snake_case column names. Key columns include:
#'   - features (character): Program feature description
#'   - program_type (character): Type of managed care program
#'     ("Comprehensive MCO", "PCCM", "PIHP", "PAHP", etc.)
#'   - statewide_or_regional (character): Geographic scope
#'   - federal_authority (character): Federal authority type
#'     ("1915(b)", "1115", "State Plan", etc.)
#'   - program_start_date (character): When the program began
#' @examples
#' \dontrun{
#' # Get all managed care programs
#' mc <- mcaid_managed_care()
#'
#' # Count programs by type
#' mc |> dplyr::count(program_type, sort = TRUE)
#' }
#' @export
mcaid_managed_care <- function(limit = NULL) {
  url <- "https://download.medicaid.gov/data/managed-care-program-by-state-2023.csv"
  df <- .fetch_csv(url, limit)
  if (nrow(df) == 0) return(.schema_managed_care)
  names(df) <- gsub("\\s+", "_", tolower(names(df)))
  tibble::as_tibble(df)
}

#' Get medicaid.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mcaid_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mcaid_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/medicaid.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "medicaid.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# medicaid.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# medicaid.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
