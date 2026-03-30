# == Core: generic dataset fetch ===============================================

#' Fetch any Treasury Fiscal Data dataset
#'
#' Universal engine for the Fiscal Data API. Pass the endpoint path
#' and optional filters/fields.
#'
#' @param endpoint API endpoint path (e.g. "v2/accounting/od/debt_to_penny")
#' @param fields Character vector of field names to return. NULL = all.
#' @param filter Filter string using API syntax
#'   (e.g. "record_date:gte:2024-01-01", "country:eq:Canada")
#' @param sort Sort field with optional "-" prefix for descending.
#'   Default: "-record_date"
#' @param max_results Maximum records to return. NULL = all.
#' @return tibble with auto-typed columns (dates, numbers from API metadata)
#' @export
treas_get <- function(endpoint, fields = NULL, filter = NULL,
                      sort = "-record_date", max_results = NULL) {
  params <- list(sort = sort)
  if (!is.null(fields)) params[["fields"]] <- paste(fields, collapse = ",")
  if (!is.null(filter)) params[["filter"]] <- filter
  .treas_get(endpoint, params, max_results)
}


# == Debt ======================================================================

#' Total public debt outstanding (Debt to the Penny)
#'
#' Daily total public debt, debt held by public, and intragovernmental holdings.
#'
#' @param start Start date (Date or string). NULL = all.
#' @param end End date. NULL = today.
#' @param max_results Max records (default 365 = ~1 year of daily data)
#' @return tibble: record_date, debt_held_public_amt, intragov_hold_amt,
#'   tot_pub_debt_out_amt
#' @export
treas_debt <- function(start = NULL, end = NULL, max_results = 365) {
  filter <- NULL
  parts <- character()
  if (!is.null(start)) parts <- c(parts, paste0("record_date:gte:", start))
  if (!is.null(end))   parts <- c(parts, paste0("record_date:lte:", end))
  if (length(parts) > 0) filter <- paste(parts, collapse = ",")

  treas_get("v2/accounting/od/debt_to_penny",
            fields = c("record_date", "debt_held_public_amt",
                       "intragov_hold_amt", "tot_pub_debt_out_amt"),
            filter = filter, max_results = max_results)
}

#' Federal debt held by foreign and international investors
#'
#' @param max_results Max records (default 100)
#' @return tibble with country-level holdings
#' @export
treas_debt_foreign <- function(max_results = 100) {
  treas_get("v2/accounting/od/statement_net_cost",
            max_results = max_results)
}


# == Interest rates ============================================================

#' Average interest rates on Treasury securities
#'
#' Monthly average rates by security type (Bills, Notes, Bonds, TIPS, etc.)
#'
#' @param start Start date filter
#' @param end End date filter
#' @param security Optional security type filter
#'   (e.g. "Treasury Bills", "Treasury Notes", "Treasury Bonds")
#' @param max_results Max records (default 500)
#' @return tibble: record_date, security_type_desc, security_desc,
#'   avg_interest_rate_amt
#' @export
treas_rates <- function(start = NULL, end = NULL, security = NULL,
                        max_results = 500) {
  parts <- character()
  if (!is.null(start))    parts <- c(parts, paste0("record_date:gte:", start))
  if (!is.null(end))      parts <- c(parts, paste0("record_date:lte:", end))
  if (!is.null(security)) parts <- c(parts, paste0("security_desc:eq:", security))
  filter <- if (length(parts) > 0) paste(parts, collapse = ",") else NULL

  treas_get("v2/accounting/od/avg_interest_rates", filter = filter,
            max_results = max_results)
}

#' Treasury yield curve rates (daily)
#'
#' @param start Start date
#' @param end End date
#' @param max_results Max records (default 250 = ~1 year of trading days)
#' @return tibble: record_date with columns for each maturity
#'   (1mo, 2mo, 3mo, 4mo, 6mo, 1yr, 2yr, 3yr, 5yr, 7yr, 10yr, 20yr, 30yr)
#' @export
treas_yield_curve <- function(start = NULL, end = NULL, max_results = 250) {
  parts <- character()
  if (!is.null(start)) parts <- c(parts, paste0("record_date:gte:", start))
  if (!is.null(end))   parts <- c(parts, paste0("record_date:lte:", end))
  filter <- if (length(parts) > 0) paste(parts, collapse = ",") else NULL

  treas_get("v2/accounting/od/treasury_offset_program", filter = filter,
            max_results = max_results)
}


# == Exchange rates ============================================================

#' Treasury exchange rates (quarterly)
#'
#' Official exchange rates used by the US government.
#'
#' @param country Filter by country name (e.g. "Canada", "Japan")
#' @param start Start date
#' @param end End date
#' @param max_results Max records (default 500)
#' @return tibble: record_date, country, currency, country_currency_desc,
#'   exchange_rate, effective_date
#' @export
treas_exchange <- function(country = NULL, start = NULL, end = NULL,
                           max_results = 500) {
  parts <- character()
  if (!is.null(country)) parts <- c(parts, paste0("country:eq:", country))
  if (!is.null(start))   parts <- c(parts, paste0("record_date:gte:", start))
  if (!is.null(end))     parts <- c(parts, paste0("record_date:lte:", end))
  filter <- if (length(parts) > 0) paste(parts, collapse = ",") else NULL

  treas_get("v1/accounting/od/rates_of_exchange", filter = filter,
            max_results = max_results)
}


# == Revenue & spending ========================================================

#' Monthly Treasury Statement - receipts (revenue)
#'
#' Federal government receipts by source.
#'
#' @param fiscal_year Fiscal year filter
#' @param max_results Max records (default 200)
#' @return tibble with revenue categories and amounts
#' @export
treas_revenue <- function(fiscal_year = NULL, max_results = 200) {
  filter <- if (!is.null(fiscal_year))
    paste0("record_fiscal_year:eq:", fiscal_year) else NULL
  treas_get("v1/accounting/mts/mts_table_4", filter = filter,
            max_results = max_results)
}

#' Monthly Treasury Statement - outlays (spending)
#'
#' Federal government outlays by department/agency.
#'
#' @param fiscal_year Fiscal year filter
#' @param max_results Max records (default 200)
#' @return tibble with spending categories and amounts
#' @export
treas_spending <- function(fiscal_year = NULL, max_results = 200) {
  filter <- if (!is.null(fiscal_year))
    paste0("record_fiscal_year:eq:", fiscal_year) else NULL
  treas_get("v1/accounting/mts/mts_table_5", filter = filter,
            max_results = max_results)
}

#' Daily Treasury Statement - operating cash balance
#'
#' @param start Start date
#' @param end End date
#' @param max_results Max records (default 90)
#' @return tibble: record_date, account_type, open_today_bal,
#'   close_today_bal
#' @export
treas_cash_balance <- function(start = NULL, end = NULL, max_results = 90) {
  parts <- character()
  if (!is.null(start)) parts <- c(parts, paste0("record_date:gte:", start))
  if (!is.null(end))   parts <- c(parts, paste0("record_date:lte:", end))
  filter <- if (length(parts) > 0) paste(parts, collapse = ",") else NULL

  treas_get("v1/accounting/dts/operating_cash_balance", filter = filter,
            max_results = max_results)
}


# == Treasury securities =======================================================

#' Treasury securities outstanding (Monthly Statement of Public Debt)
#'
#' @param max_results Max records (default 100)
#' @return tibble with security types and amounts outstanding
#' @export
treas_securities <- function(max_results = 100) {
  treas_get("v1/debt/mspd/mspd_table_1", max_results = max_results)
}

#' Treasury auction results
#'
#' @param security_type Filter: "Bill", "Note", "Bond", "TIPS", "FRN"
#' @param start Start date
#' @param end End date
#' @param max_results Max records (default 100)
#' @return tibble with auction details
#' @export
treas_auctions <- function(security_type = NULL, start = NULL, end = NULL,
                           max_results = 100) {
  parts <- character()
  if (!is.null(security_type)) parts <- c(parts, paste0("security_type:eq:", security_type))
  if (!is.null(start)) parts <- c(parts, paste0("record_date:gte:", start))
  if (!is.null(end))   parts <- c(parts, paste0("record_date:lte:", end))
  filter <- if (length(parts) > 0) paste(parts, collapse = ",") else NULL

  treas_get("v1/accounting/od/auctions_query", filter = filter,
            max_results = max_results)
}


# == Savings bonds =============================================================

#' Savings bond redemption values
#'
#' @param max_results Max records (default 100)
#' @return tibble with bond series and redemption values
#' @export
treas_savings_bonds <- function(max_results = 100) {
  treas_get("v1/accounting/od/savings_bonds_report", max_results = max_results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the treasury.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
treas_context <- function() {
  .build_context("treasury.gov", header_lines = c(
    "# treasury.gov - US Treasury Fiscal Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API)",
    "# All functions return tibbles. Columns auto-typed from API metadata.",
    "#",
    "# Filter syntax: 'field:operator:value' (e.g. 'record_date:gte:2024-01-01')",
    "# Operators: eq, lt, lte, gt, gte, in, contains",
    "#",
    "# Key datasets:",
    "#   treas_debt()         = Total public debt (daily)",
    "#   treas_rates()        = Avg interest rates by security type (monthly)",
    "#   treas_exchange()     = Government exchange rates (quarterly)",
    "#   treas_revenue()      = Federal receipts by source",
    "#   treas_spending()     = Federal outlays by agency",
    "#   treas_cash_balance() = Daily Treasury operating cash",
    "#   treas_securities()   = Securities outstanding",
    "#   treas_auctions()     = Treasury auction results"
  ))
}
