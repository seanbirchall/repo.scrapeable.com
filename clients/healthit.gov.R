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
#' Returns a catalog of all datasets available through this client,
#' including live CHPL API endpoints and static ONC CSV files.
#'
#' @return A tibble with columns: \code{dataset} (character, key to use with
#'   \code{\link{hit_dataset}}), \code{description} (character),
#'   \code{source} (character, \code{"CHPL API"} or \code{"ONC CSV"}).
#' @examples
#' \dontrun{
#' hit_list()
#' }
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
#' Search the Certified Health IT Product List by keyword, developer,
#' certification status, or any combination. The CHPL is maintained by ONC
#' and lists all certified health IT modules.
#'
#' @param query Character or NULL. Free-text search term matching product name,
#'   developer, or CHPL ID (e.g. \code{"epic"}, \code{"cerner"}, \code{"allscripts"}).
#' @param developer Character or NULL. Developer/vendor name filter
#'   (e.g. \code{"Epic Systems Corporation"}).
#' @param status Character or NULL. Certification status filter. Valid values:
#'   \code{"Active"}, \code{"Retired"}, \code{"Withdrawn by Developer"},
#'   \code{"Withdrawn by ONC-ACB"}, \code{"Suspended by ONC-ACB"},
#'   \code{"Suspended by ONC"}. Default: \code{"Active"}.
#' @param page_size Integer. Results per page, max 100 (default 20).
#' @param max_pages Integer. Maximum pages to fetch (default 1).
#' @return A tibble with columns: \code{id} (integer), \code{chpl_id}
#'   (character, CHPL product number), \code{developer} (character),
#'   \code{product} (character), \code{version} (character),
#'   \code{certification_date} (Date), \code{certification_status} (character),
#'   \code{certification_body} (character), \code{edition} (character),
#'   \code{decertification_date} (Date), \code{surveillance_count} (integer),
#'   \code{open_surveillance} (integer), \code{criteria_count} (integer),
#'   \code{mandatory_disclosures} (character, URL).
#' @examples
#' \dontrun{
#' hit_search(query = "epic")
#' hit_search(status = "Retired", page_size = 50)
#' hit_search(developer = "Cerner Corporation")
#' }
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
#' Returns all developers (vendors) registered in the CHPL system.
#' These are the companies that develop and certify health IT products.
#'
#' @return A tibble with columns: \code{id} (integer), \code{name}
#'   (character, developer/company name), \code{code} (character,
#'   developer code), \code{website} (character, URL or NA),
#'   \code{self_developer} (logical), \code{status} (character or NA).
#' @examples
#' \dontrun{
#' hit_developers()
#' }
#' @export
hit_developers <- function() {
  raw <- .hit_fetch_json(paste0(.chpl_base, "/developers"))
  devs <- raw$developers %||% list()
  .parse_chpl_developers(devs)
}

#' Get certified product details by CHPL ID
#'
#' Looks up a specific certified health IT product by its CHPL product
#' number. Searches and filters to find the exact match.
#'
#' @param chpl_id Character. The CHPL product number (e.g.
#'   \code{"15.07.07.1447.BE02.01.00.1.160815"}).
#' @return A tibble (single row) with the same columns as
#'   \code{\link{hit_search}}.
#' @examples
#' \dontrun{
#' hit_product("15.07.07.1447.BE02.01.00.1.160815")
#' }
#' @export
hit_product <- function(chpl_id) {
  # Use search with the CHPL ID as search term
  hit_search(query = chpl_id, status = NULL, page_size = 10, max_pages = 1) |>
    filter(.data$chpl_id == .env$chpl_id | grepl(.env$chpl_id, .data$chpl_id, fixed = TRUE))
}

#' Get ONC dataset by name
#'
#' Downloads and parses one of the static ONC CSV datasets hosted at
#' healthit.gov. Use \code{\link{hit_list}} to see all available datasets.
#'
#' @param dataset Character. Dataset name from \code{hit_list()}. Valid values
#'   include: \code{"ehr_vendors"}, \code{"hitech_grantees"},
#'   \code{"erx_state"}, \code{"rec_kpi_grantee"}, \code{"rec_kpi_state"},
#'   \code{"rec_kpi_county"}, \code{"mu_report"},
#'   \code{"hospital_ph_reporting"}, \code{"nehrs_physicians"},
#'   \code{"aha_hospitals"}, \code{"policy_levers"},
#'   \code{"workforce_trained"}, \code{"providers_database"},
#'   \code{"erx_county"}, \code{"budget_performance"},
#'   \code{"cms_incentive"}, \code{"privacy_consent"},
#'   \code{"strategic_plan_goals"}, \code{"market_readiness"},
#'   \code{"hitech_crosswalk"}, \code{"sharp_outputs"},
#'   \code{"mu_lit_review"}, \code{"college_consortia"}.
#' @return A tibble with dataset-specific columns. Returns empty tibble
#'   with a message if the URL has moved or data is unavailable.
#' @examples
#' \dontrun{
#' hit_dataset("ehr_vendors")
#' hit_dataset("nehrs_physicians")
#' }
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

#' Get healthit.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hit_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hit_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/healthit.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "healthit.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# healthit.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# healthit.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
