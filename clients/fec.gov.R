# fec.gov.R
# Self-contained fec.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# fec-gov.R
# Self-contained Federal Election Commission API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. DEMO_KEY works for testing (low rate limit).
#   Get a key at https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 30 req/hour. Real key = 1000 req/hour.
# Docs: https://api.open.fec.gov/developers/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fec_base <- "https://api.open.fec.gov/v1"
# -- Fetch helpers -------------------------------------------------------------

.fec_get <- function(endpoint, params = list(), api_key = "DEMO_KEY",
                     max_results = NULL) {
  params$api_key <- api_key
  params$per_page <- min(max_results %||% 100, 100)
  params$page <- 1

  all_results <- list()
  repeat {
    query <- paste(names(params),
                   vapply(params, function(v) utils::URLencode(as.character(v), reserved = FALSE),
                          character(1)),
                   sep = "=", collapse = "&")
    url <- paste0(.fec_base, "/", endpoint, "?", query)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_retry(max_tries = 2, backoff = function(...) 10) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    results <- raw$results
    if (is.null(results) || length(results) == 0) break
    df <- as_tibble(results)
    all_results[[length(all_results) + 1]] <- df

    n_so_far <- sum(vapply(all_results, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break

    pages <- raw$pagination$pages %||% 1
    if (params$page >= pages) break
    params$page <- params$page + 1
  }

  if (length(all_results) == 0) return(tibble())
  result <- bind_rows(all_results)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Auto-type date columns (defensive — only pure YYYY-MM-DD strings)
  for (col in names(result)) {
    if (grepl("date", col) && is.character(result[[col]])) {
      vals <- result[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike) > length(vals) * 0.5) {
        result[[col]] <- suppressWarnings(as.Date(substr(vals, 1, 10)))
      }
    }
  }
  result
}


# == Schemas ===================================================================

.schema_candidates <- tibble(
  candidate_id = character(), name = character(), party = character(),
  party_full = character(), state = character(), office = character(),
  office_full = character(), district = character(),
  incumbent_challenge = character(), election_years = list()
)

.schema_committees <- tibble(
  committee_id = character(), name = character(), designation = character(),
  committee_type = character(), party = character(), state = character(),
  treasurer_name = character()
)

.schema_totals <- tibble(
  candidate_id = character(), name = character(), party = character(),
  office = character(), state = character(), election_year = integer(),
  receipts = numeric(), disbursements = numeric(),
  cash_on_hand_end_period = numeric()
)



# == Candidates ================================================================

#' Search FEC candidates
#'
#' Query the Federal Election Commission candidate database by name, office,
#' state, party, or election year. Returns biographical and filing metadata
#' for each matching candidate.
#'
#' @param query Character. Name search string (e.g. "Biden", "Harris").
#' @param office Character. Filter by office: \code{"P"} (president),
#'   \code{"S"} (senate), \code{"H"} (house).
#' @param state Character. Two-letter state abbreviation (e.g. \code{"CA"}).
#' @param party Character. Party code: \code{"DEM"}, \code{"REP"},
#'   \code{"LIB"}, \code{"GRE"}, etc.
#' @param election_year Integer. Election year (e.g. 2024).
#' @param api_key Character. FEC API key. \code{"DEMO_KEY"} works for testing
#'   (30 req/hour). Register at \url{https://api.data.gov/signup/}.
#' @param max_results Integer. Maximum number of results (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{candidate_id}{Character. FEC candidate ID (e.g. "P80000722").}
#'     \item{name}{Character. Candidate name (e.g. "BIDEN, JOSEPH R JR").}
#'     \item{party}{Character. Party code (e.g. "DEM").}
#'     \item{party_full}{Character. Full party name (e.g. "DEMOCRATIC PARTY").}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{office}{Character. Office code: "P", "S", or "H".}
#'     \item{office_full}{Character. Full office name.}
#'     \item{district}{Character. Congressional district (house only).}
#'     \item{district_number}{Integer. District number.}
#'     \item{incumbent_challenge}{Character. Status code: "I", "C", or "O".}
#'     \item{incumbent_challenge_full}{Character. Full status label.}
#'     \item{candidate_status}{Character. "C" (candidate) or "N".}
#'     \item{election_years}{List. Election years the candidate has run.}
#'     \item{cycles}{List. FEC cycles with data.}
#'     \item{first_file_date}{Date. Date of first filing.}
#'     \item{last_file_date}{Date. Date of most recent filing.}
#'     \item{last_f2_date}{Date. Most recent Form 2 date.}
#'     \item{has_raised_funds}{Logical. Whether candidate has raised funds.}
#'     \item{federal_funds_flag}{Logical. Whether using federal funds.}
#'     \item{active_through}{Integer. Last active election year.}
#'     \item{candidate_inactive}{Logical. Whether currently inactive.}
#'     \item{load_date}{Date. Date loaded into FEC database.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fec_candidates("Harris", office = "P", election_year = 2024)
#' fec_candidates(state = "CA", office = "S")
#' }
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
#' Aggregated financial data for candidates: total receipts, disbursements,
#' cash on hand, and itemized contribution breakdowns. Useful for comparing
#' fundraising performance across candidates in a given race.
#'
#' @param office Character. \code{"P"} (president), \code{"S"} (senate),
#'   or \code{"H"} (house).
#' @param election_year Integer. Election year (e.g. 2024).
#' @param candidate_id Character. Specific FEC candidate ID to filter by.
#' @param sort Character. Sort field (default \code{"-receipts"} for top
#'   fundraisers first). Prefix with \code{"-"} for descending.
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results (default 50).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{candidate_id}{Character. FEC candidate ID.}
#'     \item{name}{Character. Candidate name.}
#'     \item{party}{Character. Party code.}
#'     \item{party_full}{Character. Full party name.}
#'     \item{office}{Character. Office code.}
#'     \item{office_full}{Character. Full office name.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{election_year}{Integer. Election year.}
#'     \item{cycle}{Integer. FEC reporting cycle.}
#'     \item{receipts}{Numeric. Total receipts in dollars.}
#'     \item{disbursements}{Numeric. Total disbursements in dollars.}
#'     \item{cash_on_hand_end_period}{Character. Cash on hand at period end.}
#'     \item{individual_itemized_contributions}{Numeric. Itemized individual contributions.}
#'     \item{other_political_committee_contributions}{Numeric. PAC contributions.}
#'     \item{transfers_from_other_authorized_committee}{Numeric. Transfers in.}
#'     \item{debts_owed_by_committee}{Character. Outstanding debts.}
#'     \item{coverage_start_date}{Date. Reporting period start.}
#'     \item{coverage_end_date}{Date. Reporting period end.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fec_candidate_totals(office = "P", election_year = 2024)
#' fec_candidate_totals(candidate_id = "P80000722")
#' }
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
#' Search for political committees registered with the FEC, including PACs,
#' super PACs, campaign committees, party committees, and joint fundraisers.
#'
#' @param query Character. Name search string (e.g. "ActBlue", "WinRed").
#' @param committee_type Character. Committee type code:
#'   \code{"P"} (presidential), \code{"H"} (house), \code{"S"} (senate),
#'   \code{"N"} (PAC - nonqualified), \code{"Q"} (PAC - qualified),
#'   \code{"V"} (PAC with non-contribution account), \code{"W"} (PAC - super),
#'   \code{"X"} (party - nonqualified), \code{"Y"} (party - qualified).
#' @param designation Character. Committee designation:
#'   \code{"A"} (authorized by candidate), \code{"B"} (lobbyist/registrant),
#'   \code{"D"} (leadership), \code{"J"} (joint fundraiser),
#'   \code{"P"} (principal campaign), \code{"U"} (unauthorized).
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{committee_id}{Character. FEC committee ID (e.g. "C00401224").}
#'     \item{name}{Character. Committee name.}
#'     \item{committee_type}{Character. Type code.}
#'     \item{committee_type_full}{Character. Full type description.}
#'     \item{designation}{Character. Designation code.}
#'     \item{designation_full}{Character. Full designation description.}
#'     \item{party}{Character/Logical. Party code (if applicable).}
#'     \item{party_full}{Character/Logical. Full party name.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{treasurer_name}{Character. Name of committee treasurer.}
#'     \item{filing_frequency}{Character. Filing frequency code.}
#'     \item{organization_type}{Character. Organization type.}
#'     \item{first_file_date}{Date. Date of first filing.}
#'     \item{last_file_date}{Date. Date of most recent filing.}
#'     \item{cycles}{List. FEC cycles with data.}
#'     \item{candidate_ids}{List. Associated candidate IDs.}
#'     \item{affiliated_committee_name}{Character. Affiliated committee.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fec_committees("ActBlue")
#' fec_committees(committee_type = "W")
#' }
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
#' Retrieve aggregated financial totals for a specific committee, broken
#' down by election cycle. Includes receipts, disbursements, cash on hand,
#' and detailed contribution/expenditure breakdowns.
#'
#' @param committee_id Character. FEC committee ID (e.g. \code{"C00401224"}
#'   for ActBlue).
#' @param cycle Integer. Election cycle year (e.g. 2024). If \code{NULL},
#'   returns all available cycles.
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @return A tibble with one row per cycle. Columns vary by committee type
#'   but typically include financial totals such as receipts, disbursements,
#'   individual contributions, and cash on hand.
#' @export
#' @examples
#' \dontrun{
#' fec_committee_totals("C00401224")
#' fec_committee_totals("C00401224", cycle = 2024)
#' }
fec_committee_totals <- function(committee_id, cycle = NULL,
                                 api_key = "DEMO_KEY") {
  params <- list()
  if (!is.null(cycle)) params$cycle <- cycle
  .fec_get(paste0("committee/", committee_id, "/totals/"), params, api_key)
}


# == Filings ===================================================================

#' Search FEC filings
#'
#' Search for electronic and paper filings submitted to the FEC. Each filing
#' is a specific report (e.g. quarterly, year-end, 48-hour notice) submitted
#' by a committee or candidate.
#'
#' @param committee_id Character. Committee ID to filter by.
#' @param candidate_id Character. Candidate ID to filter by.
#' @param form_type Character. FEC form type code:
#'   \code{"F1"} (registration), \code{"F2"} (candidate authorization),
#'   \code{"F3"} (house/senate report), \code{"F3P"} (presidential report),
#'   \code{"F3X"} (PAC/party report), \code{"F24"} (24-hour IE notice).
#' @param min_receipt_date Character. Start date filter (YYYY-MM-DD).
#' @param max_receipt_date Character. End date filter (YYYY-MM-DD).
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{committee_id}{Character. Committee ID.}
#'     \item{committee_name}{Character. Committee name.}
#'     \item{form_type}{Character. FEC form type.}
#'     \item{report_type}{Character. Report type code.}
#'     \item{report_type_full}{Character. Full report type name.}
#'     \item{receipt_date}{Date. Date filed.}
#'     \item{coverage_start_date}{Date. Reporting period start.}
#'     \item{coverage_end_date}{Date. Reporting period end.}
#'     \item{total_receipts}{Numeric. Total receipts in filing.}
#'     \item{total_disbursements}{Numeric. Total disbursements.}
#'     \item{total_individual_contributions}{Numeric. Individual contributions.}
#'     \item{cash_on_hand_beginning_period}{Numeric. Starting cash.}
#'     \item{cash_on_hand_end_period}{Numeric. Ending cash.}
#'     \item{pdf_url}{Character. URL to PDF of filing.}
#'     \item{csv_url}{Character. URL to CSV of filing.}
#'     \item{fec_url}{Character. URL on FEC website.}
#'     \item{file_number}{Integer. FEC file number.}
#'     \item{amendment_indicator}{Character. "N" (new), "A" (amendment).}
#'     \item{is_amended}{Logical. Whether this filing has been amended.}
#'     \item{most_recent}{Logical. Whether this is the most recent version.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fec_filings(committee_id = "C00401224", max_results = 10)
#' fec_filings(form_type = "F3P", max_results = 5)
#' }
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
#' Search itemized individual contributions reported to the FEC. Each record
#' represents a single contribution from a person to a committee. Includes
#' contributor name, employer, occupation, amount, and date.
#'
#' @param committee_id Character. Receiving committee ID.
#' @param contributor_name Character. Contributor name search string.
#' @param contributor_state Character. Two-letter state code filter.
#' @param contributor_zip Character. ZIP code filter.
#' @param min_date Character. Start date (YYYY-MM-DD).
#' @param max_date Character. End date (YYYY-MM-DD).
#' @param min_amount Numeric. Minimum contribution amount in dollars.
#' @param max_amount Numeric. Maximum contribution amount in dollars.
#' @param two_year_transaction_period Integer. Election cycle (e.g. 2024).
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including contributor_name,
#'   contributor_state, contributor_city, contributor_zip,
#'   contributor_employer, contributor_occupation, committee_id,
#'   committee_name, contribution_receipt_amount,
#'   contribution_receipt_date, receipt_type, and memo_text.
#' @export
#' @examples
#' \dontrun{
#' fec_receipts(committee_id = "C00401224", max_results = 10)
#' fec_receipts(contributor_name = "Smith", contributor_state = "NY")
#' }
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
#' Search itemized disbursements (spending) reported by committees to the FEC.
#' Each record represents a payment from a committee to a vendor or recipient.
#'
#' @param committee_id Character. Committee ID making the disbursement.
#' @param recipient_name Character. Recipient/vendor name search string.
#' @param min_date Character. Start date (YYYY-MM-DD).
#' @param max_date Character. End date (YYYY-MM-DD).
#' @param min_amount Numeric. Minimum disbursement amount in dollars.
#' @param two_year_transaction_period Integer. Election cycle (e.g. 2024).
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including committee_id, committee_name,
#'   recipient_name, recipient_city, recipient_state, disbursement_amount,
#'   disbursement_date, disbursement_description, and memo_text.
#' @export
#' @examples
#' \dontrun{
#' fec_disbursements(committee_id = "C00401224", max_results = 10)
#' fec_disbursements(recipient_name = "Google", min_amount = 10000)
#' }
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
#' Search independent expenditures -- money spent by committees independently
#' advocating for or against specific candidates. These are not contributions
#' to candidates and are not coordinated with campaigns.
#'
#' @param candidate_id Character. FEC candidate ID being targeted.
#' @param committee_id Character. Committee ID making the expenditure.
#' @param support_oppose Character. \code{"S"} (support) or \code{"O"} (oppose).
#' @param min_date Character. Start date (YYYY-MM-DD).
#' @param max_date Character. End date (YYYY-MM-DD).
#' @param min_amount Numeric. Minimum expenditure amount in dollars.
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results (default 100).
#' @return A tibble with columns including committee_id, committee_name,
#'   candidate_id, candidate_name, support_oppose_indicator, expenditure_amount,
#'   expenditure_date, expenditure_description, payee_name, and office_total_ytd.
#' @export
#' @examples
#' \dontrun{
#' fec_independent_expenditures(max_results = 10)
#' fec_independent_expenditures(candidate_id = "P80000722", support_oppose = "O")
#' }
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
#' Retrieve candidates and their fundraising totals for a specific election.
#' Useful for comparing all candidates in a given race.
#'
#' @param office Character. Office type: \code{"president"}, \code{"senate"},
#'   or \code{"house"}.
#' @param cycle Integer. Election cycle year (e.g. 2024).
#' @param state Character. Two-letter state code (required for senate/house).
#' @param district Integer. District number (required for house races).
#' @param api_key Character. FEC API key (default \code{"DEMO_KEY"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{candidate_id}{Character. FEC candidate ID.}
#'     \item{candidate_name}{Character. Candidate name.}
#'     \item{candidate_pcc_id}{Character. Principal campaign committee ID.}
#'     \item{candidate_pcc_name}{Character. Principal campaign committee name.}
#'     \item{party_full}{Character. Full party name.}
#'     \item{incumbent_challenge_full}{Character. Incumbent/challenger/open.}
#'     \item{total_receipts}{Numeric. Total receipts in dollars.}
#'     \item{total_disbursements}{Numeric. Total disbursements in dollars.}
#'     \item{cash_on_hand_end_period}{Numeric. Cash on hand.}
#'     \item{coverage_end_date}{Character. Last reporting date.}
#'     \item{candidate_election_year}{Integer. Election year.}
#'     \item{committee_ids}{List. All associated committee IDs.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fec_elections("president", 2024)
#' fec_elections("senate", 2024, state = "AZ")
#' fec_elections("house", 2024, state = "CA", district = 12)
#' }
fec_elections <- function(office, cycle, state = NULL, district = NULL,
                          api_key = "DEMO_KEY") {
  params <- list(office = office, cycle = cycle)
  if (!is.null(state))    params$state <- state
  if (!is.null(district)) params$district <- district
  .fec_get("elections/", params, api_key)
}


# == Context ===================================================================

#' Get fec.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fec_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fec_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fec.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fec.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fec.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fec.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
