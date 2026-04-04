# healthit.gov.R
# Self-contained ONC Health IT client (CHPL API + static CSV datasets).
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: Public API key included (registered, free tier).
# Docs: https://chpl.healthit.gov/#/resources/chpl_api

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

.ua <- "support@scrapeable.com"
.chpl_base <- "https://chpl.healthit.gov/rest"
.chpl_key  <- "12909a978483dfb8ecd0596c98ae9094"

# -- Core fetch: JSON ---------------------------------------------------------

.hit_fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(`api_key` = .chpl_key) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  if (file.info(tmp)$size == 0) return(list())
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# -- Core fetch: CSV to temp file, then read locally --------------------------

.hit_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  status <- httr2::resp_status(resp)
  if (status >= 400 || file.info(tmp)$size == 0) return(tibble())
  ct <- httr2::resp_content_type(resp) %||% ""
  if (grepl("html", ct, ignore.case = TRUE)) return(tibble())
  read.csv(tmp, stringsAsFactors = FALSE, na.strings = c("", "NA", "."))
}

# -- Parse CHPL search results to tibble --------------------------------------

.parse_chpl_results <- function(results) {
  if (length(results) == 0) return(.schema_chpl_products)
  bind_rows(lapply(results, function(r) {
    tibble(
      id               = r$id %||% NA_integer_,
      chpl_id          = r$chplProductNumber %||% NA_character_,
      developer        = r$developer$name %||% NA_character_,
      product          = r$product$name %||% NA_character_,
      version          = r$version$name %||% NA_character_,
      certification_date = as.Date(r$certificationDate %||% NA_character_),
      certification_status = r$certificationStatus$name %||% NA_character_,
      certification_body   = r$certificationBody$name %||% NA_character_,
      edition          = if (!is.null(r$edition)) r$edition$name else NA_character_,
      decertification_date = as.Date(r$decertificationDate %||% NA_character_),
      surveillance_count   = r$surveillanceCount %||% 0L,
      open_surveillance     = r$openSurveillanceCount %||% 0L,
      criteria_count   = length(r$criteriaMet %||% list()),
      mandatory_disclosures = r$mandatoryDisclosures %||% NA_character_
    )
  }))
}

# -- Parse developer list to tibble -------------------------------------------

.parse_chpl_developers <- function(devs) {
  if (length(devs) == 0) return(.schema_chpl_developers)
  bind_rows(lapply(devs, function(d) {
    status_val <- NA_character_
    if (!is.null(d$statuses) && length(d$statuses) > 0) {
      last <- d$statuses[[length(d$statuses)]]
      status_val <- last$status$name %||% NA_character_
    }
    tibble(
      id       = d$id %||% NA_integer_,
      name     = d$name %||% NA_character_,
      code     = d$developerCode %||% NA_character_,
      website  = d$website %||% NA_character_,
      self_developer = d$selfDeveloper %||% NA,
      status   = status_val
    )
  }))
}

# -- Schemas -------------------------------------------------------------------

.schema_chpl_products <- tibble(
  id = integer(), chpl_id = character(), developer = character(),
  product = character(), version = character(),
  certification_date = as.Date(character()),
  certification_status = character(), certification_body = character(),
  edition = character(), decertification_date = as.Date(character()),
  surveillance_count = integer(), open_surveillance = integer(),
  criteria_count = integer(), mandatory_disclosures = character()
)

.schema_chpl_developers <- tibble(
  id = integer(), name = character(), code = character(),
  website = character(), self_developer = logical(), status = character()
)

# == Public functions ==========================================================

#' List available Health IT datasets
#'
#' Returns a tibble describing datasets available through this client:
#' CHPL API endpoints and static ONC CSV files.
#' @return tibble
#' @export
hit_list <- function() {
  tibble(
    dataset = c(
      "chpl_products", "chpl_developers",
      "ehr_vendors", "hitech_grantees", "erx_state",
      "rec_kpi_grantee", "rec_kpi_state", "rec_kpi_county",
      "mu_report", "hospital_ph_reporting",
      "nehrs_physicians", "aha_hospitals",
      "policy_levers", "workforce_trained",
      "providers_database", "erx_county",
      "budget_performance", "cms_incentive",
      "privacy_consent", "strategic_plan_goals",
      "market_readiness", "hitech_crosswalk",
      "sharp_outputs", "mu_lit_review",
      "college_consortia"
    ),
    description = c(
      "Certified Health IT Product List (CHPL) - live API search",
      "Health IT developers/vendors - live API",
      "EHR developers reported by providers in federal programs",
      "ONC HITECH grantee list",
      "Electronic prescribing adoption by state",
      "Regional Extension Center KPIs by grantee",
      "Regional Extension Center KPIs by state",
      "Regional Extension Center KPIs by county",
      "EHR products used for Meaningful Use attestation",
      "Hospital public health measures reporting",
      "Office-based physician EHR adoption (NEHRS)",
      "Non-federal acute care hospital Health IT adoption",
      "State Health IT policy levers activities catalog",
      "Students trained through ONC workforce programs",
      "Office-based health care providers database",
      "Electronic prescribing adoption by county",
      "ONC budget performance measure data",
      "CMS EHR Incentive Program measures",
      "State Health IT privacy and consent laws",
      "Federal Health IT Strategic Plan 2015-2020 goals",
      "2015 Edition market readiness data",
      "HITECH grantee crosswalk",
      "SHARP program output inventory",
      "Meaningful Use lit review appendix",
      "Community College Consortia KPIs"
    ),
    source = c(
      "CHPL API", "CHPL API",
      rep("ONC CSV", 23)
    )
  )
}

#' Search certified Health IT products (CHPL)
#'
#' Search the Certified Health IT Product List by keyword, developer, status, etc.
#' @param query Free-text search term (matches product name, developer, CHPL ID).
#' @param developer Developer/vendor name filter.
#' @param status Certification status filter: "Active", "Retired", "Withdrawn by Developer",
#'   "Withdrawn by ONC-ACB", "Suspended by ONC-ACB", "Suspended by ONC".
#' @param page_size Results per page (max 100). Default 20.
#' @param max_pages Maximum pages to fetch. Default 1.
#' @return tibble of certified products
#' @export
hit_search <- function(query = NULL, developer = NULL, status = "Active",
                       page_size = 20, max_pages = 1) {
  page_size <- min(page_size, 100)
  all_results <- list()

  for (pg in seq_len(max_pages)) {
    url <- paste0(.chpl_base, "/search/v3")
    req <- httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_url_query(
        api_key    = .chpl_key,
        pageSize   = page_size,
        pageNumber = pg - 1L
      )
    if (!is.null(query))     req <- req |> httr2::req_url_query(searchTerm = query)
    if (!is.null(developer)) req <- req |> httr2::req_url_query(developer = developer)
    if (!is.null(status))    req <- req |> httr2::req_url_query(certificationStatuses = status)

    tmp <- tempfile(fileext = ".json")
    req |> httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
    unlink(tmp)

    results <- raw$results %||% list()
    if (length(results) == 0) break
    all_results <- c(all_results, results)

    total <- raw$recordCount %||% 0
    if (length(all_results) >= total) break
  }

  .parse_chpl_results(all_results)
}

#' List Health IT developers/vendors
#'
#' Returns all developers registered in the CHPL system.
#' @return tibble with developer id, name, code, website, status
#' @export
hit_developers <- function() {
  raw <- .hit_fetch_json(paste0(.chpl_base, "/developers"))
  devs <- raw$developers %||% list()
  .parse_chpl_developers(devs)
}

#' Get certified product details by CHPL ID
#'
#' @param chpl_id The CHPL product number (e.g., "15.07.07.1447.BE02.01.00.1.160815").
#' @return tibble (single row) with product details
#' @export
hit_product <- function(chpl_id) {
  # Use search with the CHPL ID as search term
  hit_search(query = chpl_id, status = NULL, page_size = 10, max_pages = 1) |>
    filter(.data$chpl_id == .env$chpl_id | grepl(.env$chpl_id, .data$chpl_id, fixed = TRUE))
}

#' Get ONC dataset by name
#'
#' Fetches one of the static ONC CSV datasets.
#' @param dataset Dataset name from hit_list() (e.g., "ehr_vendors", "nehrs_physicians").
#' @return tibble
#' @export
hit_dataset <- function(dataset) {
  urls <- c(
    ehr_vendors       = "https://www.healthit.gov/sites/default/files/data/datasets/EHR-vendors-count-dataset.csv",
    hitech_grantees   = "https://www.healthit.gov/sites/default/files/data/datasets/ONC_HITECH_Grantee_List.csv",
    erx_state         = "https://www.healthit.gov/sites/default/files/data/datasets/Surescripts_04-2014_State.csv",
    rec_kpi_grantee   = "https://www.healthit.gov/sites/default/files/data/datasets/REC_KPI_Masterfile.csv",
    rec_kpi_state     = "https://www.healthit.gov/sites/default/files/data/datasets/REC_KPI_State.csv",
    rec_kpi_county    = "https://www.healthit.gov/sites/default/files/data/datasets/REC_KPI_County.csv",
    mu_report         = "https://www.healthit.gov/sites/default/files/data/datasets/MU_REPORT.csv",
    hospital_ph_reporting = "https://www.healthit.gov/sites/default/files/data/datasets/hospital-mu-public-health-measures.csv",
    nehrs_physicians  = "https://www.healthit.gov/sites/default/files/data/datasets/nehrs.csv",
    aha_hospitals     = "https://www.healthit.gov/sites/default/files/data/datasets/aha.csv",
    policy_levers     = "https://www.healthit.gov/sites/default/files/data/datasets/policy-levers-activities-catalog.csv",
    workforce_trained = "https://www.healthit.gov/sites/default/files/data/datasets/workforce-programs-trained.csv",
    providers_database = "https://www.healthit.gov/sites/default/files/data/datasets/SKA_State_County_Data_2011-2013.csv",
    erx_county        = "https://www.healthit.gov/sites/default/files/data/datasets/Surescripts_County_04-2014.csv",
    budget_performance = "https://www.healthit.gov/sites/default/files/data/datasets/performance-measures.csv",
    cms_incentive     = "https://www.healthit.gov/sites/default/files/data/datasets/Meaningful-Use-Acceleration-Scorecard.csv",
    privacy_consent   = "https://www.healthit.gov/sites/default/files/data/datasets/state-health-it-privacy-consent-law-policies.csv",
    strategic_plan_goals = "https://www.healthit.gov/sites/default/files/data/datasets/federal-health-it-strategic-plan-2015-2020-goals.csv",
    market_readiness  = "https://www.healthit.gov/sites/default/files/data/datasets/2015-edition-market-readiness-hospitals-clinicians-data.csv",
    hitech_crosswalk  = "https://www.healthit.gov/sites/default/files/data/datasets/HealthIT_Dashboard_AreaType_Crosswalk.csv",
    sharp_outputs     = "https://www.healthit.gov/sites/default/files/data/datasets/sharp-projects-outputs.csv",
    mu_lit_review     = "https://www.healthit.gov/sites/default/files/data/datasets/systematic-lit-review-appendix.csv",
    college_consortia = "https://www.healthit.gov/sites/default/files/data/datasets/Comm_College_Consortia_KPI.csv"
  )

  if (!dataset %in% names(urls)) {
    stop("Unknown dataset: '", dataset, "'. Use hit_list() to see available datasets.",
         call. = FALSE)
  }

  df <- .hit_fetch_csv(urls[[dataset]])
  if (nrow(df) == 0) {
    message("Note: Dataset '", dataset, "' returned no data (URL may have moved).")
  }
  as_tibble(df)
}

#' Self-reading context function
#'
#' Returns the full source code of this client file.
#' @return character vector (one element per line)
#' @export
hit_context <- function() {
  src <- tryCatch(
    readLines(sys.frame(1)$ofile %||%
      file.path(system.file(package = "healthit.gov"), "source", "healthit.R") %||%
      "clients/healthit.gov.R"),
    error = function(e) {
      f <- system.file("source", "healthit.R", package = "healthit.gov")
      if (nzchar(f)) readLines(f) else character()
    }
  )
  src
}
