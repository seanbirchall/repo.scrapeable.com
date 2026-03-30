# == Candidates ================================================================

#' Search FEC candidates
#'
#' @param query Name search string
#' @param office Filter: "P" (president), "S" (senate), "H" (house)
#' @param state 2-letter state code
#' @param party Party code: "DEM", "REP", "LIB", "GRE", etc.
#' @param election_year Election year (e.g. 2024)
#' @param api_key FEC API key (default: DEMO_KEY)
#' @param max_results Max results (default 100)
#' @return tibble: candidate_id, name, party, state, office, district, ...
#' @export
fec_candidates <- function(query = NULL, office = NULL, state = NULL,
                           party = NULL, election_year = NULL,
                           api_key = "DEMO_KEY", max_results = 100) {
  params <- list()
  if (!is.null(query))         params$q <- query
  if (!is.null(office))        params$office <- office
  if (!is.null(state))         params$state <- state
  if (!is.null(party))         params$party <- party
  if (!is.null(election_year)) params$election_year <- election_year
  .fec_get("candidates/", params, api_key, max_results)
}

#' Candidate financial totals
#'
#' Aggregated financial data: receipts, disbursements, cash on hand.
#'
#' @param office "P", "S", or "H"
#' @param election_year Election year
#' @param candidate_id Specific candidate ID filter
#' @param sort Sort field (default: "-receipts" for top fundraisers)
#' @param api_key FEC API key
#' @param max_results Max results (default 50)
#' @return tibble: candidate_id, name, party, office, receipts,
#'   disbursements, cash_on_hand_end_period, ...
#' @export
fec_candidate_totals <- function(office = NULL, election_year = NULL,
                                 candidate_id = NULL, sort = "-receipts",
                                 api_key = "DEMO_KEY", max_results = 50) {
  params <- list(sort = sort)
  if (!is.null(office))        params$office <- office
  if (!is.null(election_year)) params$election_year <- election_year
  if (!is.null(candidate_id))  params$candidate_id <- candidate_id
  .fec_get("candidates/totals/", params, api_key, max_results)
}


# == Committees ================================================================

#' Search FEC committees (PACs, party committees, campaign committees)
#'
#' @param query Name search string
#' @param committee_type "P" (presidential), "H" (house), "S" (senate),
#'   "N" (PAC - nonqualified), "Q" (PAC - qualified), "W" (PAC - super),
#'   "X" (party - nonqualified), "Y" (party - qualified)
#' @param designation "A" (authorized by candidate), "B" (lobbyist/registrant),
#'   "D" (leadership), "J" (joint fundraiser), "P" (principal campaign),
#'   "U" (unauthorized)
#' @param api_key FEC API key
#' @param max_results Max results
#' @return tibble: committee_id, name, designation, committee_type, party, ...
#' @export
fec_committees <- function(query = NULL, committee_type = NULL,
                           designation = NULL, api_key = "DEMO_KEY",
                           max_results = 100) {
  params <- list()
  if (!is.null(query))          params$q <- query
  if (!is.null(committee_type)) params$committee_type <- committee_type
  if (!is.null(designation))    params$designation <- designation
  .fec_get("committees/", params, api_key, max_results)
}

#' Committee financial totals
#'
#' @param committee_id Committee ID (e.g. "C00401224" for ActBlue)
#' @param cycle Election cycle (e.g. 2024)
#' @param api_key FEC API key
#' @return tibble: one row per cycle with financial totals
#' @export
fec_committee_totals <- function(committee_id, cycle = NULL,
                                 api_key = "DEMO_KEY") {
  params <- list()
  if (!is.null(cycle)) params$cycle <- cycle
  .fec_get(paste0("committee/", committee_id, "/totals/"), params, api_key)
}


# == Filings ===================================================================

#' Search FEC filings
#'
#' @param committee_id Committee ID filter
#' @param candidate_id Candidate ID filter
#' @param form_type Form type: "F1" (registration), "F2" (candidate auth),
#'   "F3" (house/senate), "F3P" (presidential), "F3X" (PAC), "F24" (24hr IE)
#' @param min_receipt_date Start date
#' @param max_receipt_date End date
#' @param api_key FEC API key
#' @param max_results Max results
#' @return tibble of filing records
#' @export
fec_filings <- function(committee_id = NULL, candidate_id = NULL,
                        form_type = NULL, min_receipt_date = NULL,
                        max_receipt_date = NULL, api_key = "DEMO_KEY",
                        max_results = 100) {
  params <- list(sort = "-receipt_date")
  if (!is.null(committee_id))     params$committee_id <- committee_id
  if (!is.null(candidate_id))     params$candidate_id <- candidate_id
  if (!is.null(form_type))        params$form_type <- form_type
  if (!is.null(min_receipt_date)) params$min_receipt_date <- min_receipt_date
  if (!is.null(max_receipt_date)) params$max_receipt_date <- max_receipt_date
  .fec_get("filings/", params, api_key, max_results)
}


# == Receipts (contributions) ==================================================

#' Individual contributions (Schedule A)
#'
#' @param committee_id Committee ID
#' @param contributor_name Contributor name search
#' @param contributor_state State filter
#' @param contributor_zip ZIP code filter
#' @param min_date Start date
#' @param max_date End date
#' @param min_amount Minimum contribution amount
#' @param max_amount Maximum contribution amount
#' @param two_year_transaction_period Election cycle (e.g. 2024)
#' @param api_key FEC API key
#' @param max_results Max results
#' @return tibble of individual contribution records
#' @export
fec_receipts <- function(committee_id = NULL, contributor_name = NULL,
                         contributor_state = NULL, contributor_zip = NULL,
                         min_date = NULL, max_date = NULL,
                         min_amount = NULL, max_amount = NULL,
                         two_year_transaction_period = NULL,
                         api_key = "DEMO_KEY", max_results = 100) {
  params <- list(sort = "-contribution_receipt_date")
  if (!is.null(committee_id))               params$committee_id <- committee_id
  if (!is.null(contributor_name))            params$contributor_name <- contributor_name
  if (!is.null(contributor_state))           params$contributor_state <- contributor_state
  if (!is.null(contributor_zip))             params$contributor_zip <- contributor_zip
  if (!is.null(min_date))                    params$min_date <- min_date
  if (!is.null(max_date))                    params$max_date <- max_date
  if (!is.null(min_amount))                  params$min_amount <- min_amount
  if (!is.null(max_amount))                  params$max_amount <- max_amount
  if (!is.null(two_year_transaction_period)) params$two_year_transaction_period <- two_year_transaction_period
  .fec_get("schedules/schedule_a/", params, api_key, max_results)
}


# == Disbursements =============================================================

#' Committee disbursements (Schedule B)
#'
#' @param committee_id Committee ID
#' @param recipient_name Recipient name search
#' @param min_date Start date
#' @param max_date End date
#' @param min_amount Minimum amount
#' @param two_year_transaction_period Election cycle
#' @param api_key FEC API key
#' @param max_results Max results
#' @return tibble of disbursement records
#' @export
fec_disbursements <- function(committee_id = NULL, recipient_name = NULL,
                              min_date = NULL, max_date = NULL,
                              min_amount = NULL,
                              two_year_transaction_period = NULL,
                              api_key = "DEMO_KEY", max_results = 100) {
  params <- list(sort = "-disbursement_date")
  if (!is.null(committee_id))               params$committee_id <- committee_id
  if (!is.null(recipient_name))             params$recipient_name <- recipient_name
  if (!is.null(min_date))                   params$min_date <- min_date
  if (!is.null(max_date))                   params$max_date <- max_date
  if (!is.null(min_amount))                 params$min_amount <- min_amount
  if (!is.null(two_year_transaction_period)) params$two_year_transaction_period <- two_year_transaction_period
  .fec_get("schedules/schedule_b/", params, api_key, max_results)
}


# == Independent expenditures ==================================================

#' Independent expenditures (Schedule E)
#'
#' Money spent independently advocating for/against candidates.
#'
#' @param candidate_id Candidate targeted
#' @param committee_id Committee making the expenditure
#' @param support_oppose "S" (support) or "O" (oppose)
#' @param min_date Start date
#' @param max_date End date
#' @param min_amount Minimum amount
#' @param api_key FEC API key
#' @param max_results Max results
#' @return tibble of independent expenditure records
#' @export
fec_independent_expenditures <- function(candidate_id = NULL,
                                         committee_id = NULL,
                                         support_oppose = NULL,
                                         min_date = NULL, max_date = NULL,
                                         min_amount = NULL,
                                         api_key = "DEMO_KEY",
                                         max_results = 100) {
  params <- list(sort = "-expenditure_date")
  if (!is.null(candidate_id))  params$candidate_id <- candidate_id
  if (!is.null(committee_id))  params$committee_id <- committee_id
  if (!is.null(support_oppose)) params$support_oppose_indicator <- support_oppose
  if (!is.null(min_date))      params$min_date <- min_date
  if (!is.null(max_date))      params$max_date <- max_date
  if (!is.null(min_amount))    params$min_amount <- min_amount
  .fec_get("schedules/schedule_e/", params, api_key, max_results)
}


# == Elections =================================================================

#' Election results and candidates by year/office/state/district
#'
#' @param office "president", "senate", or "house"
#' @param cycle Election cycle year
#' @param state 2-letter state code (required for senate/house)
#' @param district District number (required for house)
#' @param api_key FEC API key
#' @return tibble of election results
#' @export
fec_elections <- function(office, cycle, state = NULL, district = NULL,
                          api_key = "DEMO_KEY") {
  params <- list(office = office, cycle = cycle)
  if (!is.null(state))    params$state <- state
  if (!is.null(district)) params$district <- district
  .fec_get("elections/", params, api_key)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the fec.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
fec_context <- function() {
  .build_context("fec.gov", header_lines = c(
    "# fec.gov - Federal Election Commission API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: API key required (api_key param). DEMO_KEY for testing.",
    "#   Get a real key at https://api.data.gov/signup/",
    "# Rate limit: DEMO_KEY = 30/hr, real key = 1000/hr",
    "# All functions return tibbles.",
    "#",
    "# Office codes: P (president), S (senate), H (house)",
    "# Party codes: DEM, REP, LIB, GRE, IND",
    "# Committee types: P/H/S (campaign), N/Q (PAC), W (super PAC), X/Y (party)"
  ))
}
