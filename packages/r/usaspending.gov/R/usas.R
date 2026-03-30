# == Reference data ============================================================

#' List all top-tier federal agencies
#'
#' @return tibble: agency_id, toptier_code, agency_name, active_fy, active_fq,
#'   budget_authority, obligated_amount, outlay_amount
#' @export
usas_agencies <- function() {
  raw <- .fetch_json(paste0(.usas_base, "/references/toptier_agencies/"))
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_agencies)
  as_tibble(results) |>
    transmute(
      agency_id = as.integer(agency_id),
      toptier_code = as.character(toptier_code),
      agency_name = as.character(agency_name),
      active_fy = as.character(active_fy),
      active_fq = as.character(active_fq),
      budget_authority = as.numeric(budget_authority_amount),
      obligated_amount = as.numeric(obligated_amount),
      outlay_amount = as.numeric(outlay_amount)
    )
}

#' Fetch agency overview details
#'
#' @param toptier_code Agency toptier code (e.g. "012" for USDA, "097" for DOD).
#'   Use usas_agencies() to find codes.
#' @param fiscal_year Fiscal year (default: current)
#' @return tibble: one row with agency details
#' @export
usas_agency <- function(toptier_code, fiscal_year = NULL) {
  url <- sprintf("%s/agency/%s/", .usas_base, toptier_code)
  if (!is.null(fiscal_year)) url <- paste0(url, "?fiscal_year=", fiscal_year)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(tibble())
  as_tibble(as.list(raw))
}

#' List all NAICS codes (top-tier)
#'
#' @return tibble: naics_code, naics_description
#' @export
usas_naics <- function() {
  raw <- .fetch_json(paste0(.usas_base, "/references/naics/"))
  results <- raw$results
  if (is.null(results) || length(results) == 0)
    return(tibble(naics_code = character(), naics_description = character()))
  as_tibble(results)
}

#' List available submission periods
#'
#' @return tibble: submission_fiscal_year, submission_fiscal_month, etc.
#' @export
usas_periods <- function() {
  raw <- .fetch_json(paste0(.usas_base, "/references/submission_periods/"))
  results <- raw$available_periods
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results) |>
    mutate(across(any_of(c("submission_fiscal_year", "submission_fiscal_month",
                            "submission_fiscal_quarter")), as.integer))
}

#' Fetch total budgetary resources by fiscal year
#'
#' @return tibble: fiscal_year, total_budgetary_resources
#' @export
usas_budget <- function() {
  raw <- .fetch_json(paste0(.usas_base, "/references/total_budgetary_resources/"))
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results) |>
    mutate(
      fiscal_year = as.integer(fiscal_year),
      total_budgetary_resources = as.numeric(total_budgetary_resources)
    )
}

#' List all recipient states with award amounts
#'
#' @return tibble: fips, code, name, type, amount, count
#' @export
usas_states <- function() {
  raw <- .fetch_json(paste0(.usas_base, "/recipient/state/"))
  if (is.null(raw) || length(raw) == 0) return(.schema_states)
  as_tibble(raw) |>
    transmute(
      fips = as.character(fips),
      code = as.character(code),
      name = as.character(name),
      type = as.character(type),
      amount = as.numeric(amount),
      count = as.integer(count)
    )
}


# == Award search ==============================================================

#' Search awards (contracts, grants, loans, etc.)
#'
#' The main search endpoint. Supports extensive filtering.
#'
#' @param keywords Character vector of keywords to search
#' @param award_type Character vector of award types. Options:
#'   "A", "B", "C", "D" (contracts); "02", "03", "04", "05" (grants/assistance);
#'   "06", "07", "08", "09", "10", "11" (loans/other)
#' @param agency Agency name (awarding agency filter)
#' @param recipient Recipient name
#' @param place_of_performance State code (e.g. "CA", "NY")
#' @param date_start Start date for award date range
#' @param date_end End date for award date range
#' @param naics NAICS code filter
#' @param psc Product/Service code filter
#' @param max_results Maximum results (default 100)
#' @return tibble of award summaries
#' @export
usas_awards <- function(keywords = NULL, award_type = NULL, agency = NULL,
                        recipient = NULL, place_of_performance = NULL,
                        date_start = NULL, date_end = NULL,
                        naics = NULL, psc = NULL, max_results = 100) {
  # award_type_codes is required and must be from ONE group:
  # contracts: A, B, C, D  |  grants: 02, 03, 04, 05  |  loans: 07, 08
  # direct_payments: 06, 10  |  other: 09, 11  |  idvs: IDV_A, IDV_B, etc.
  if (is.null(award_type)) award_type <- c("A", "B", "C", "D")

  filters <- list(award_type_codes = as.list(award_type))
  if (!is.null(keywords)) filters$keywords <- as.list(keywords)
  if (!is.null(place_of_performance)) filters$place_of_performance_locations <- list(list(state = place_of_performance))
  if (!is.null(naics))                filters$naics_codes <- list(list(code = naics))
  if (!is.null(psc))                  filters$psc_codes <- as.list(psc)
  if (!is.null(date_start) || !is.null(date_end)) {
    filters$time_period <- list(list(
      start_date = as.character(date_start %||% "2000-01-01"),
      end_date = as.character(date_end %||% format(Sys.Date(), "%Y-%m-%d"))
    ))
  }
  if (!is.null(agency)) {
    filters$agencies <- list(list(type = "awarding", tier = "toptier", name = agency))
  }
  if (!is.null(recipient)) {
    filters$recipient_search_text <- recipient
  }

  body <- list(
    filters = filters,
    fields = c("Award ID", "Recipient Name", "Start Date", "End Date",
               "Award Amount", "Total Outlays", "Description",
               "def_codes", "COVID-19 Obligations", "COVID-19 Outlays",
               "Infrastructure Obligations", "Infrastructure Outlays",
               "Awarding Agency", "Awarding Sub Agency",
               "Contract Award Type", "Award Type",
               "Funding Agency", "Funding Sub Agency"),
    sort = "Award Amount",
    order = "desc"
  )

  url <- paste0(.usas_base, "/search/spending_by_award/")
  page_size <- min(max_results, 100)
  max_pages <- ceiling(max_results / page_size)

  df <- .usas_fetch_all(url, body, max_pages = max_pages, page_size = page_size)
  if (nrow(df) == 0) return(.schema_awards)

  # Standardize column names
  if (ncol(df) > 0) names(df) <- gsub(" ", "_", tolower(names(df)))

  # Type casting
  df |>
    mutate(
      across(any_of(c("award_amount", "total_outlays")), as.numeric),
      across(any_of(c("start_date", "end_date")), as.Date)
    ) |>
    head(max_results)
}

#' Fetch a single award by ID
#'
#' @param award_id The generated unique award ID or PIID/FAIN
#' @return tibble: one row with full award details
#' @export
usas_award <- function(award_id) {
  url <- sprintf("%s/awards/%s/", .usas_base, award_id)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(tibble())
  # Flatten the nested response
  flat <- unlist(raw, recursive = TRUE)
  as_tibble(t(flat)) |>
    mutate(across(everything(), as.character))
}


# == Spending aggregations =====================================================

#' Spending over time
#'
#' Aggregated spending amounts grouped by time period.
#'
#' @param group Time grouping: "fiscal_year", "quarter", "month"
#' @param keywords Keyword filter
#' @param award_type Award type codes
#' @param agency Awarding agency name
#' @param fiscal_year Fiscal year filter
#' @return tibble: time_period, aggregated_amount
#' @export
usas_spending_time <- function(group = "fiscal_year", keywords = NULL,
                               award_type = c("A", "B", "C", "D"),
                               agency = NULL, fiscal_year = NULL) {
  filters <- list(award_type_codes = as.list(award_type))
  if (!is.null(keywords)) filters$keywords <- as.list(keywords)
  if (!is.null(agency))     filters$agencies <- list(list(type = "awarding", tier = "toptier", name = agency))
  if (!is.null(fiscal_year)) {
    filters$time_period <- list(list(
      start_date = paste0(fiscal_year - 1, "-10-01"),
      end_date = paste0(fiscal_year, "-09-30")
    ))
  }

  body <- list(filters = filters, group = group)
  raw <- .post_json(paste0(.usas_base, "/search/spending_over_time/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_spending_time)

  as_tibble(results) |>
    mutate(
      time_period = if ("time_period" %in% names(results)) {
        if (is.list(time_period)) {
          vapply(time_period, function(tp) paste(tp, collapse = "-"), character(1))
        } else as.character(time_period)
      } else paste(fiscal_year, quarter, sep = "Q"),
      aggregated_amount = as.numeric(aggregated_amount)
    ) |>
    select(time_period, aggregated_amount)
}

#' Spending by geography
#'
#' Award spending aggregated by state, county, or congressional district.
#'
#' @param scope "place_of_performance" (default) or "recipient_location"
#' @param geo_layer "state" (default), "county", or "district"
#' @param keywords Keyword filter
#' @param award_type Award type codes
#' @param fiscal_year Fiscal year
#' @return tibble: shape_code, display_name, aggregated_amount, population, per_capita
#' @export
usas_spending_geo <- function(scope = "place_of_performance",
                              geo_layer = "state",
                              keywords = NULL,
                              award_type = c("A", "B", "C", "D"),
                              fiscal_year = NULL) {
  filters <- list(award_type_codes = as.list(award_type))
  if (!is.null(keywords)) filters$keywords <- as.list(keywords)
  if (!is.null(fiscal_year)) {
    filters$time_period <- list(list(
      start_date = paste0(fiscal_year - 1, "-10-01"),
      end_date = paste0(fiscal_year, "-09-30")
    ))
  }

  body <- list(filters = filters, scope = scope, geo_layer = geo_layer)
  raw <- .post_json(paste0(.usas_base, "/search/spending_by_geography/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_spending_geo)

  as_tibble(results) |>
    mutate(
      shape_code = as.character(shape_code),
      display_name = as.character(display_name),
      aggregated_amount = as.numeric(aggregated_amount),
      population = as.integer(population %||% NA_integer_),
      per_capita = as.numeric(per_capita %||% NA_real_)
    )
}

#' Spending by category
#'
#' Spending grouped by a specific dimension.
#'
#' @param category One of: "awarding_agency", "awarding_subagency",
#'   "funding_agency", "funding_subagency", "recipient", "cfda",
#'   "naics", "psc", "country", "state_territory", "county", "district",
#'   "federal_account"
#' @param keywords Keyword filter
#' @param award_type Award type codes
#' @param fiscal_year Fiscal year
#' @param max_results Max results (default 100)
#' @return tibble: name, amount, count (columns vary by category)
#' @export
usas_spending_category <- function(category = "awarding_agency",
                                   keywords = NULL,
                                   award_type = c("A", "B", "C", "D"),
                                   fiscal_year = NULL, max_results = 100) {
  filters <- list(award_type_codes = as.list(award_type))
  if (!is.null(keywords)) filters$keywords <- as.list(keywords)
  if (!is.null(fiscal_year)) {
    filters$time_period <- list(list(
      start_date = paste0(fiscal_year - 1, "-10-01"),
      end_date = paste0(fiscal_year, "-09-30")
    ))
  }

  body <- list(filters = filters, category = category, limit = max_results)
  url <- paste0(.usas_base, "/search/spending_by_category/", category, "/")
  raw <- .post_json(url, body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  as_tibble(results) |>
    mutate(across(any_of(c("amount", "aggregated_amount")), as.numeric),
           across(any_of("count"), as.integer))
}


# == Recipient search ==========================================================

#' Search recipients by name
#'
#' @param query Recipient name search text
#' @param award_type Award type filter
#' @param max_results Max results (default 50)
#' @return tibble: name, uei, duns, recipient_id, recipient_level, amount
#' @export
usas_recipients <- function(query = NULL, award_type = NULL, max_results = 50) {
  body <- list(limit = max_results)
  if (!is.null(query))      body$keyword <- query
  if (!is.null(award_type)) body$award_type <- award_type

  raw <- .post_json(paste0(.usas_base, "/recipient/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_recipients)

  as_tibble(results) |>
    mutate(amount = as.numeric(amount))
}

#' Fetch recipient details by hash ID
#'
#' @param recipient_id Recipient hash ID (from usas_recipients)
#' @param fiscal_year Optional fiscal year
#' @return tibble: one row with recipient overview
#' @export
usas_recipient <- function(recipient_id, fiscal_year = NULL) {
  url <- sprintf("%s/recipient/%s/", .usas_base, recipient_id)
  if (!is.null(fiscal_year)) url <- paste0(url, "?fiscal_year=", fiscal_year)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(tibble())
  flat <- unlist(raw, recursive = TRUE)
  as_tibble(t(flat)) |> mutate(across(everything(), as.character))
}


# == Federal accounts ==========================================================

#' Search federal accounts
#'
#' @param keywords Keyword filter
#' @param agency_id Agency identifier
#' @param max_results Max results (default 50)
#' @return tibble of federal accounts with obligation/outlay totals
#' @export
usas_accounts <- function(keywords = NULL, agency_id = NULL, max_results = 50) {
  body <- list(limit = max_results)
  if (!is.null(keywords))  body$filters <- list(agency_identifier = agency_id)
  if (!is.null(keywords))  body$keyword <- keywords

  raw <- .post_json(paste0(.usas_base, "/federal_accounts/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}


# == Autocomplete ==============================================================

#' Autocomplete agency names
#'
#' @param query Search text
#' @param limit Max results (default 10)
#' @return tibble: id, toptier_flag, subtier_agency_name, toptier_agency_name
#' @export
usas_autocomplete_agency <- function(query, limit = 10) {
  body <- list(search_text = query, limit = limit)
  raw <- .post_json(paste0(.usas_base, "/autocomplete/awarding_agency/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}

#' Autocomplete recipient names
#'
#' @param query Search text
#' @param limit Max results (default 10)
#' @return tibble: recipient_name, uei, duns
#' @export
usas_autocomplete_recipient <- function(query, limit = 10) {
  body <- list(search_text = query, limit = limit)
  raw <- .post_json(paste0(.usas_base, "/autocomplete/recipient/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}

#' Autocomplete NAICS codes
#'
#' @param query Search text
#' @param limit Max results (default 10)
#' @return tibble: naics_code, naics_description
#' @export
usas_autocomplete_naics <- function(query, limit = 10) {
  body <- list(search_text = query, limit = limit)
  raw <- .post_json(paste0(.usas_base, "/autocomplete/naics/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the usaspending.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
usas_context <- function() {
  .build_context("usaspending.gov", header_lines = c(
    "# usaspending.gov - Federal Spending Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API)",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Award type codes:",
    "#   Contracts: A (BPA Call), B (Purchase Order), C (Delivery Order), D (Definitive Contract)",
    "#   Grants: 02 (Block Grant), 03 (Formula Grant), 04 (Project Grant), 05 (Cooperative Agreement)",
    "#   Loans: 07 (Direct Loan), 08 (Guaranteed/Insured Loan)",
    "#   Other: 06 (Direct Payment), 09 (Insurance), 10 (Other Assistance), 11 (Other)"
  ))
}
