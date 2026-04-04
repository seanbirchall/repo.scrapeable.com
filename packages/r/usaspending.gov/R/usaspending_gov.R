# usaspending.gov.R
# Self-contained usaspending.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


# usaspending-gov.R
# Self-contained USASpending.gov API v2 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Rate limits: undocumented, be reasonable
# Docs: https://api.usaspending.gov/docs/endpoints


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.usas_base <- "https://api.usaspending.gov/api/v2"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# POST with JSON body — many USASpending endpoints are POST
.post_json <- function(url, body) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

# -- Pagination for POST endpoints --------------------------------------------

.usas_fetch_all <- function(url, body, results_key = "results",
                            max_pages = 50, page_size = 100) {
  body$limit <- page_size
  body$page <- 1
  all_results <- list()

  for (i in seq_len(max_pages)) {
    raw <- .post_json(url, body)
    results <- raw[[results_key]]
    if (is.null(results) || length(results) == 0) break

    df <- if (is.data.frame(results)) results else as_tibble(results)
    all_results[[i]] <- df

    # Check if more pages
    has_next <- raw$page_metadata$hasNext %||% FALSE
    if (!has_next) break
    body$page <- body$page + 1

    if (i %% 10 == 0) message(sprintf("  ...fetched %d pages", i))
  }

  if (length(all_results) == 0) return(tibble())
  bind_rows(all_results)
}


# == Schemas ===================================================================

.schema_agencies <- tibble(
  agency_id = integer(), toptier_code = character(), agency_name = character(),
  active_fy = character(), active_fq = character(),
  budget_authority = numeric(), obligated_amount = numeric(),
  outlay_amount = numeric()
)

.schema_awards <- tibble(
  award_id = character(), generated_internal_id = character(),
  type = character(), type_description = character(),
  description = character(), piid = character(), fain = character(),
  uri = character(), total_obligation = numeric(),
  date_signed = as.Date(character()), start_date = as.Date(character()),
  end_date = as.Date(character()),
  awarding_agency_name = character(), funding_agency_name = character(),
  recipient_name = character(), recipient_uei = character(),
  place_of_performance_state = character()
)

.schema_spending_time <- tibble(
  time_period = character(), aggregated_amount = numeric()
)

.schema_spending_geo <- tibble(
  shape_code = character(), display_name = character(),
  aggregated_amount = numeric(), population = integer(),
  per_capita = numeric()
)

.schema_recipients <- tibble(
  name = character(), uei = character(), duns = character(),
  recipient_id = character(), recipient_level = character(),
  amount = numeric()
)

.schema_states <- tibble(
  fips = character(), code = character(), name = character(),
  type = character(), amount = numeric(), count = integer()
)



# == Reference data ============================================================

#' List all top-tier federal agencies
#'
#' Returns all top-tier federal agencies with their current fiscal year
#' budget authority, obligated amounts, and outlay amounts.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{agency_id}{Integer unique agency identifier}
#'     \item{toptier_code}{Character agency toptier code (e.g., "012" for USDA)}
#'     \item{agency_name}{Agency name}
#'     \item{active_fy}{Active fiscal year}
#'     \item{active_fq}{Active fiscal quarter}
#'     \item{budget_authority}{Numeric budget authority amount}
#'     \item{obligated_amount}{Numeric obligated amount}
#'     \item{outlay_amount}{Numeric outlay amount}
#'   }
#' @export
#' @family USASpending reference functions
#' @seealso \code{\link{usas_agency}} for detailed info on a single agency,
#'   \code{\link{usas_autocomplete_agency}} to search by name
#' @examples
#' \dontrun{
#' usas_agencies()
#' }
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
#' @param toptier_code Character agency toptier code (e.g., \code{"012"}
#'   for USDA, \code{"097"} for DOD). Use \code{\link{usas_agencies}} to
#'   find codes.
#' @param fiscal_year Optional integer fiscal year (default: current).
#' @return A tibble with one row containing detailed agency information.
#' @export
#' @family USASpending reference functions
#' @seealso \code{\link{usas_agencies}} to list all agencies
usas_agency <- function(toptier_code, fiscal_year = NULL) {
  url <- sprintf("%s/agency/%s/", .usas_base, toptier_code)
  if (!is.null(fiscal_year)) url <- paste0(url, "?fiscal_year=", fiscal_year)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(tibble())
  as_tibble(as.list(raw))
}

#' List all NAICS codes (top-tier)
#'
#' @return A tibble with columns naics_code and naics_description.
#' @export
#' @family USASpending reference functions
usas_naics <- function() {
  raw <- .fetch_json(paste0(.usas_base, "/references/naics/"))
  results <- raw$results
  if (is.null(results) || length(results) == 0)
    return(tibble(naics_code = character(), naics_description = character()))
  as_tibble(results)
}

#' List available submission periods
#'
#' @return A tibble with submission_fiscal_year, submission_fiscal_month,
#'   and submission_fiscal_quarter columns.
#' @export
#' @family USASpending reference functions
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
#' @return A tibble with columns fiscal_year (integer) and
#'   total_budgetary_resources (numeric).
#' @export
#' @family USASpending reference functions
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
#' @return A tibble with columns:
#'   \describe{
#'     \item{fips}{FIPS state code}
#'     \item{code}{Two-letter state code}
#'     \item{name}{State name}
#'     \item{type}{Location type (e.g., "state")}
#'     \item{amount}{Numeric total award amount}
#'     \item{count}{Integer number of awards}
#'   }
#' @export
#' @family USASpending reference functions
#' @examples
#' \dontrun{
#' usas_states()
#' }
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

#' Search federal awards (contracts, grants, loans, etc.)
#'
#' The main search endpoint for USASpending.gov. Supports extensive
#' filtering by keyword, award type, agency, recipient, location,
#' date range, NAICS code, and product/service code. Results are
#' auto-paginated and sorted by award amount descending.
#'
#' @param keywords Optional character vector of keywords to search.
#' @param award_type Optional character vector of award type codes.
#'   Contracts: \code{"A"}, \code{"B"}, \code{"C"}, \code{"D"};
#'   Grants: \code{"02"}, \code{"03"}, \code{"04"}, \code{"05"};
#'   Loans: \code{"07"}, \code{"08"}. Defaults to contracts.
#' @param agency Optional character awarding agency name.
#' @param recipient Optional character recipient name.
#' @param place_of_performance Optional character state code
#'   (e.g., \code{"CA"}, \code{"NY"}).
#' @param date_start Optional start date for award date range.
#' @param date_end Optional end date for award date range.
#' @param naics Optional NAICS code filter.
#' @param psc Optional product/service code filter.
#' @param max_results Integer maximum results (default 100).
#' @return A tibble of award summaries with auto-generated column names.
#' @export
#' @family USASpending search functions
#' @seealso \code{\link{usas_award}} for details on a single award,
#'   \code{\link{usas_spending_time}} for time-series aggregations
#' @examples
#' \dontrun{
#' # Search for cybersecurity contracts
#' usas_awards(keywords = "cybersecurity", max_results = 50)
#'
#' # Grants in California
#' usas_awards(award_type = c("02", "03", "04", "05"),
#'             place_of_performance = "CA", max_results = 20)
#' }
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
#' @param award_id Character generated unique award ID or PIID/FAIN.
#' @return A tibble with one row containing flattened award details.
#' @export
#' @family USASpending search functions
#' @seealso \code{\link{usas_awards}} to search for awards
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
#' Aggregated spending amounts grouped by time period. Useful for
#' trend analysis of federal spending.
#'
#' @param group Character time grouping: \code{"fiscal_year"} (default),
#'   \code{"quarter"}, or \code{"month"}.
#' @param keywords Optional character vector of keyword filters.
#' @param award_type Character vector of award type codes (default: contracts).
#' @param agency Optional character awarding agency name.
#' @param fiscal_year Optional integer fiscal year filter.
#' @return A tibble with columns:
#'   \describe{
#'     \item{time_period}{Character time period label}
#'     \item{aggregated_amount}{Numeric total spending amount}
#'   }
#' @export
#' @family USASpending aggregation functions
#' @seealso \code{\link{usas_spending_geo}} for geographic breakdown,
#'   \code{\link{usas_spending_category}} for category breakdown
#' @examples
#' \dontrun{
#' usas_spending_time(group = "fiscal_year")
#' }
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
#' @param scope Character: \code{"place_of_performance"} (default) or
#'   \code{"recipient_location"}.
#' @param geo_layer Character: \code{"state"} (default), \code{"county"},
#'   or \code{"district"}.
#' @param keywords Optional character vector of keyword filters.
#' @param award_type Character vector of award type codes (default: contracts).
#' @param fiscal_year Optional integer fiscal year.
#' @return A tibble with columns:
#'   \describe{
#'     \item{shape_code}{Geographic identifier (FIPS code)}
#'     \item{display_name}{Human-readable location name}
#'     \item{aggregated_amount}{Numeric total spending}
#'     \item{population}{Integer population (if available)}
#'     \item{per_capita}{Numeric per-capita spending}
#'   }
#' @export
#' @family USASpending aggregation functions
#' @seealso \code{\link{usas_spending_time}} for time-series breakdown
#' @examples
#' \dontrun{
#' usas_spending_geo(geo_layer = "state")
#' }
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
#' Spending grouped by a specific dimension such as agency, recipient,
#' NAICS code, or geographic area.
#'
#' @param category Character category dimension. One of:
#'   \code{"awarding_agency"}, \code{"awarding_subagency"},
#'   \code{"funding_agency"}, \code{"funding_subagency"},
#'   \code{"recipient"}, \code{"cfda"}, \code{"naics"}, \code{"psc"},
#'   \code{"country"}, \code{"state_territory"}, \code{"county"},
#'   \code{"district"}, \code{"federal_account"}.
#' @param keywords Optional character vector of keyword filters.
#' @param award_type Character vector of award type codes (default: contracts).
#' @param fiscal_year Optional integer fiscal year.
#' @param max_results Integer max results (default 100).
#' @return A tibble with columns varying by category (typically name,
#'   amount, count).
#' @export
#' @family USASpending aggregation functions
#' @examples
#' \dontrun{
#' usas_spending_category("awarding_agency")
#' usas_spending_category("naics", keywords = "cybersecurity")
#' }
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
#' @param query Optional character recipient name search text.
#' @param award_type Optional award type filter.
#' @param max_results Integer max results (default 50).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Recipient name}
#'     \item{uei}{Unique Entity Identifier}
#'     \item{duns}{DUNS number (legacy)}
#'     \item{recipient_id}{Internal recipient hash ID}
#'     \item{recipient_level}{Recipient aggregation level}
#'     \item{amount}{Numeric total award amount}
#'   }
#' @export
#' @family USASpending recipient functions
#' @seealso \code{\link{usas_recipient}} for detailed recipient info
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
#' @param recipient_id Character recipient hash ID (from
#'   \code{\link{usas_recipients}}).
#' @param fiscal_year Optional integer fiscal year.
#' @return A tibble with one row of flattened recipient details.
#' @export
#' @family USASpending recipient functions
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
#' @param keywords Optional character keyword filter.
#' @param agency_id Optional character agency identifier.
#' @param max_results Integer max results (default 50).
#' @return A tibble of federal accounts with obligation/outlay totals.
#' @export
#' @family USASpending reference functions
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
#' @param query Character search text.
#' @param limit Integer max results (default 10).
#' @return A tibble with id, toptier_flag, subtier_agency_name,
#'   toptier_agency_name columns.
#' @export
#' @family USASpending autocomplete functions
usas_autocomplete_agency <- function(query, limit = 10) {
  body <- list(search_text = query, limit = limit)
  raw <- .post_json(paste0(.usas_base, "/autocomplete/awarding_agency/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}

#' Autocomplete recipient names
#'
#' @param query Character search text.
#' @param limit Integer max results (default 10).
#' @return A tibble with recipient_name, uei, duns columns.
#' @export
#' @family USASpending autocomplete functions
usas_autocomplete_recipient <- function(query, limit = 10) {
  body <- list(search_text = query, limit = limit)
  raw <- .post_json(paste0(.usas_base, "/autocomplete/recipient/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}

#' Autocomplete NAICS codes
#'
#' @param query Character search text.
#' @param limit Integer max results (default 10).
#' @return A tibble with naics_code and naics_description columns.
#' @export
#' @family USASpending autocomplete functions
usas_autocomplete_naics <- function(query, limit = 10) {
  body <- list(search_text = query, limit = limit)
  raw <- .post_json(paste0(.usas_base, "/autocomplete/naics/"), body)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())
  as_tibble(results)
}


# == Context ===================================================================

#' Get usaspending.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
usas_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(usas_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/usaspending.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "usaspending.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# usaspending.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# usaspending.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
