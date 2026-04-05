# treasury.gov.R
# Self-contained treasury.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


# treasury-gov.R
# Self-contained US Treasury Fiscal Data API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://fiscaldata.treasury.gov/api-documentation/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.treas_base <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service"
# -- Fetch + type casting ------------------------------------------------------

.treas_get <- function(endpoint, params = list(), max_results = NULL) {
  # Default sort by record_date desc
  if (is.null(params[["sort"]])) params[["sort"]] <- "-record_date"

  # Pagination
  page_size <- min(max_results %||% 10000, 10000)
  params[["page[size]"]] <- page_size
  params[["page[number]"]] <- 1

  all_data <- list()
  total <- NULL

  repeat {
    query <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(.treas_base, "/", endpoint, "?", query)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    if (is.null(total)) {
      total <- raw$meta$`total-count`
      if (!is.null(total) && total > page_size)
        message(sprintf("Fetching %d records...", min(total, max_results %||% total)))
    }

    df <- as_tibble(raw$data)
    if (nrow(df) == 0) break
    all_data[[length(all_data) + 1]] <- df

    # Check if we have enough
    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break

    total_pages <- raw$meta$`total-pages`
    if (is.null(total_pages) || params[["page[number]"]] >= total_pages) break
    params[["page[number]"]] <- params[["page[number]"]] + 1
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Auto-type using meta datatypes
  types <- raw$meta$dataTypes
  if (!is.null(types)) {
    for (col in names(types)) {
      if (!col %in% names(result)) next
      t <- types[[col]]
      if (t == "DATE") result[[col]] <- as.Date(result[[col]])
      else if (t %in% c("NUMBER", "CURRENCY", "PERCENTAGE"))
        result[[col]] <- suppressWarnings(as.numeric(result[[col]]))
    }
  }
  result
}


# == Schemas ===================================================================

.schema_debt <- tibble(
  record_date = as.Date(character()), debt_held_public_amt = numeric(),
  intragov_hold_amt = numeric(), tot_pub_debt_out_amt = numeric()
)

.schema_rates <- tibble(
  record_date = as.Date(character()), security_desc = character(),
  avg_interest_rate_amt = numeric()
)

.schema_exchange <- tibble(
  record_date = as.Date(character()), country_currency_desc = character(),
  exchange_rate = numeric()
)



# == Core: generic dataset fetch ===============================================

#' Fetch any Treasury Fiscal Data dataset
#'
#' Universal query engine for the US Treasury Fiscal Data API
#' (\url{https://fiscaldata.treasury.gov}). Supports any endpoint with
#' field selection, filtering, sorting, and auto-pagination. Columns are
#' automatically typed (dates, numbers, currencies) using API metadata.
#'
#' @param endpoint Character. API endpoint path relative to the base URL
#'   (e.g. \code{"v2/accounting/od/debt_to_penny"},
#'   \code{"v1/accounting/od/rates_of_exchange"}).
#'   See \url{https://fiscaldata.treasury.gov/api-documentation/} for all endpoints.
#' @param fields Character vector or \code{NULL}. Field names to return
#'   (e.g. \code{c("record_date", "exchange_rate")}). \code{NULL} returns all.
#' @param filter Character or \code{NULL}. Filter string using Treasury API
#'   syntax with colon-separated field:operator:value triplets. Multiple
#'   filters are comma-separated. Examples:
#'   \code{"record_date:gte:2024-01-01"},
#'   \code{"country:eq:Canada"},
#'   \code{"record_date:gte:2024-01-01,country:eq:Japan"}.
#' @param sort Character. Sort field with optional \code{"-"} prefix for
#'   descending order. Default \code{"-record_date"} (newest first).
#' @param max_results Integer or \code{NULL}. Maximum records to return.
#'   \code{NULL} returns all available records (auto-paginated).
#' @return A tibble with auto-typed columns. Date fields become \code{Date},
#'   numeric/currency/percentage fields become \code{numeric}, others remain
#'   \code{character}.
#' @examples
#' \dontrun{
#' treas_get("v2/accounting/od/debt_to_penny", max_results = 10)
#' treas_get("v1/accounting/od/rates_of_exchange",
#'           filter = "country:eq:Japan", max_results = 5)
#' }
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
#' Returns daily US total public debt from the Treasury's "Debt to the
#' Penny" dataset. Includes debt held by the public, intragovernmental
#' holdings, and the total. Values are in dollars.
#'
#' @param start Date or character or \code{NULL}. Start date for filtering
#'   (e.g. \code{"2024-01-01"} or \code{as.Date("2024-01-01")}). Default
#'   \code{NULL} returns all available dates.
#' @param end Date or character or \code{NULL}. End date for filtering.
#'   Default \code{NULL}.
#' @param max_results Integer. Maximum records to return (default 365,
#'   approximately 1 year of business days).
#' @return A tibble with 4 columns:
#' \describe{
#'   \item{record_date}{Date. Business day the debt was recorded.}
#'   \item{debt_held_public_amt}{Numeric. Debt held by the public (dollars).}
#'   \item{intragov_hold_amt}{Numeric. Intragovernmental holdings (dollars).}
#'   \item{tot_pub_debt_out_amt}{Numeric. Total public debt outstanding (dollars).}
#' }
#' @examples
#' \dontrun{
#' treas_debt(max_results = 30)
#' treas_debt(start = "2024-01-01", end = "2024-12-31")
#' }
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

#' Federal statement of net cost
#'
#' Returns records from the Statement of Net Cost dataset, which reports
#' federal government net costs by department and agency.
#'
#' @param max_results Integer. Maximum records to return (default 100).
#' @return A tibble with auto-typed columns from the Statement of Net Cost
#'   dataset.
#' @examples
#' \dontrun{
#' treas_debt_foreign(max_results = 20)
#' }
#' @export
treas_debt_foreign <- function(max_results = 100) {
  treas_get("v2/accounting/od/statement_net_cost",
            max_results = max_results)
}


# == Interest rates ============================================================

#' Average interest rates on Treasury securities
#'
#' Returns monthly average interest rates for US Treasury securities by
#' type (Bills, Notes, Bonds, TIPS, FRN, etc.). Useful for tracking the
#' government's cost of borrowing over time.
#'
#' @param start Date or character or \code{NULL}. Start date filter.
#' @param end Date or character or \code{NULL}. End date filter.
#' @param security Character or \code{NULL}. Filter by security description
#'   (e.g. \code{"Treasury Bills"}, \code{"Treasury Notes"},
#'   \code{"Treasury Bonds"}, \code{"Treasury Inflation-Protected Securities"}).
#' @param max_results Integer. Maximum records to return (default 500).
#' @return A tibble with auto-typed columns including:
#' \describe{
#'   \item{record_date}{Date. Month-end date.}
#'   \item{security_type_desc}{Character. Broad type (e.g. "Marketable", "Non-marketable").}
#'   \item{security_desc}{Character. Specific security name.}
#'   \item{avg_interest_rate_amt}{Numeric. Average interest rate (percentage).}
#' }
#' @examples
#' \dontrun{
#' treas_rates(max_results = 20)
#' treas_rates(security = "Treasury Bonds", start = "2023-01-01")
#' }
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

#' Treasury offset program data
#'
#' Returns records from the Treasury Offset Program dataset, which tracks
#' federal and state payment offsets (garnishments applied to federal payments).
#'
#' @param start Date or character or \code{NULL}. Start date filter.
#' @param end Date or character or \code{NULL}. End date filter.
#' @param max_results Integer. Maximum records (default 250).
#' @return A tibble with auto-typed columns from the Treasury Offset Program.
#' @examples
#' \dontrun{
#' treas_yield_curve(max_results = 10)
#' }
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
#' Returns official quarterly exchange rates used by the US government
#' for foreign currency conversions. Rates represent the number of
#' foreign currency units per US dollar (or vice versa, depending on
#' the currency convention).
#'
#' @param country Character or \code{NULL}. Filter by country name
#'   (e.g. \code{"Canada"}, \code{"Japan"}, \code{"United Kingdom"}).
#'   Case-sensitive. Default \code{NULL} returns all countries.
#' @param start Date or character or \code{NULL}. Start date filter.
#' @param end Date or character or \code{NULL}. End date filter.
#' @param max_results Integer. Maximum records to return (default 500).
#' @return A tibble with auto-typed columns including:
#' \describe{
#'   \item{record_date}{Date. Quarter-end date.}
#'   \item{country}{Character. Country name.}
#'   \item{currency}{Character. Currency name.}
#'   \item{country_currency_desc}{Character. Combined country-currency label.}
#'   \item{exchange_rate}{Numeric. Exchange rate value.}
#'   \item{effective_date}{Date. Date the rate became effective.}
#' }
#' @examples
#' \dontrun{
#' treas_exchange(country = "Canada", max_results = 10)
#' treas_exchange(start = "2024-01-01")
#' }
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
#' Returns federal government receipts (revenue) by source from the
#' Monthly Treasury Statement (MTS Table 4). Sources include individual
#' income taxes, corporate income taxes, social insurance, excise taxes,
#' customs duties, and other receipts.
#'
#' @param fiscal_year Integer or character or \code{NULL}. Filter by
#'   federal fiscal year (Oct-Sep, e.g. \code{2024}). Default \code{NULL}
#'   returns all years.
#' @param max_results Integer. Maximum records (default 200).
#' @return A tibble with auto-typed columns including record_date,
#'   classification descriptions, and dollar amounts.
#' @examples
#' \dontrun{
#' treas_revenue(fiscal_year = 2024)
#' treas_revenue(max_results = 50)
#' }
#' @seealso \code{\link{treas_spending}} for federal outlays.
#' @export
treas_revenue <- function(fiscal_year = NULL, max_results = 200) {
  filter <- if (!is.null(fiscal_year))
    paste0("record_fiscal_year:eq:", fiscal_year) else NULL
  treas_get("v1/accounting/mts/mts_table_4", filter = filter,
            max_results = max_results)
}

#' Monthly Treasury Statement - outlays (spending)
#'
#' Returns federal government outlays (spending) by department and agency
#' from the Monthly Treasury Statement (MTS Table 5). Includes Defense,
#' Health and Human Services, Social Security, Treasury, and other
#' departments.
#'
#' @param fiscal_year Integer or character or \code{NULL}. Filter by
#'   federal fiscal year (e.g. \code{2024}). Default \code{NULL}.
#' @param max_results Integer. Maximum records (default 200).
#' @return A tibble with auto-typed columns including record_date,
#'   classification descriptions, and dollar amounts.
#' @examples
#' \dontrun{
#' treas_spending(fiscal_year = 2024)
#' }
#' @seealso \code{\link{treas_revenue}} for federal receipts.
#' @export
treas_spending <- function(fiscal_year = NULL, max_results = 200) {
  filter <- if (!is.null(fiscal_year))
    paste0("record_fiscal_year:eq:", fiscal_year) else NULL
  treas_get("v1/accounting/mts/mts_table_5", filter = filter,
            max_results = max_results)
}

#' Daily Treasury Statement - operating cash balance
#'
#' Returns the US Treasury's daily operating cash balance from the Daily
#' Treasury Statement (DTS). Shows opening and closing balances for the
#' Treasury's general account and supplementary financing accounts.
#'
#' @param start Date or character or \code{NULL}. Start date filter.
#' @param end Date or character or \code{NULL}. End date filter.
#' @param max_results Integer. Maximum records (default 90, approximately
#'   3 months of business days).
#' @return A tibble with auto-typed columns including:
#' \describe{
#'   \item{record_date}{Date. Business day.}
#'   \item{account_type}{Character. Account description.}
#'   \item{open_today_bal}{Numeric. Opening balance (dollars).}
#'   \item{close_today_bal}{Numeric. Closing balance (dollars).}
#' }
#' @examples
#' \dontrun{
#' treas_cash_balance(max_results = 30)
#' treas_cash_balance(start = "2024-06-01", end = "2024-06-30")
#' }
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
#' Returns monthly data from the Monthly Statement of Public Debt (MSPD
#' Table 1), showing amounts outstanding by security type (bills, notes,
#' bonds, TIPS, FRN, savings bonds, etc.).
#'
#' @param max_results Integer. Maximum records (default 100).
#' @return A tibble with auto-typed columns including record_date,
#'   security descriptions, and amounts outstanding in dollars.
#' @examples
#' \dontrun{
#' treas_securities(max_results = 20)
#' }
#' @export
treas_securities <- function(max_results = 100) {
  treas_get("v1/debt/mspd/mspd_table_1", max_results = max_results)
}

#' Treasury auction results
#'
#' Returns results from Treasury security auctions including bid-to-cover
#' ratios, high yields, allotment percentages, and auction dates.
#'
#' @param security_type Character or \code{NULL}. Filter by security type:
#'   \code{"Bill"}, \code{"Note"}, \code{"Bond"}, \code{"TIPS"}, or
#'   \code{"FRN"}. Default \code{NULL} returns all types.
#' @param start Date or character or \code{NULL}. Start date filter.
#' @param end Date or character or \code{NULL}. End date filter.
#' @param max_results Integer. Maximum records (default 100).
#' @return A tibble with auto-typed columns including record_date,
#'   security type, auction date, high yield, and amounts.
#' @examples
#' \dontrun{
#' treas_auctions(security_type = "Bond", max_results = 20)
#' treas_auctions(start = "2024-01-01")
#' }
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
#' Returns savings bond data from the Treasury's Savings Bonds Report,
#' including series information and redemption values.
#'
#' @param max_results Integer. Maximum records (default 100).
#' @return A tibble with auto-typed columns including record_date,
#'   bond series, and value information.
#' @examples
#' \dontrun{
#' treas_savings_bonds(max_results = 20)
#' }
#' @export
treas_savings_bonds <- function(max_results = 100) {
  treas_get("v1/accounting/od/savings_bonds_report", max_results = max_results)
}


# == Context ===================================================================

#' Get treasury.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
treas_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(treas_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/treasury.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "treasury.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# treasury.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# treasury.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
