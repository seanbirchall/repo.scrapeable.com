# wa.gov.R
# Washington State Open Data client (data.wa.gov — Socrata/SODA 2.1).
# Discovery + access for 630+ Socrata datasets covering politics, health,
# education, wildlife, labor, cannabis/liquor, data breaches, and more.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data.


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.wa_domain <- "data.wa.gov"
.wa_disco  <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_   else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_      else as.double(x)

# -- JSON fetch ----------------------------------------------------------------

.wa_fetch <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

.wa_fetch_list <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE, flatten = FALSE)
}

# -- SODA query helper --------------------------------------------------------

._soda_query <- function(view_id, select = NULL, where = NULL,
                         order = NULL, group = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  url <- sprintf("https://%s/resource/%s.json", .wa_domain, view_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(order))  params[["$order"]]   <- order
  if (!is.null(group))  params[["$group"]]   <- group
  if (!is.null(q))      params[["$q"]]       <- q

  raw <- .wa_fetch(url, params)
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  .type_cols(df)
}

# -- Auto-type columns --------------------------------------------------------

.type_cols <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) == 0) next
      # numeric?
      if (all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
      # date?
      if (grepl("date|Date|updated|created|At$", col) &&
          sum(grepl("^\\d{4}-\\d{2}-\\d{2}", vals)) > length(vals) * 0.5) {
        df[[col]] <- as.Date(substr(df[[col]], 1, 10))
      }
    }
  }
  df
}

# -- Paginated fetch -----------------------------------------------------------

._soda_all <- function(view_id, select = NULL, where = NULL,
                       order = NULL, max_rows = 50000, page_size = 5000) {
  all_data <- list()
  offset <- 0
  repeat {
    remaining <- max_rows - offset
    if (remaining <= 0) break
    lim <- min(page_size, remaining)
    chunk <- ._soda_query(view_id, select = select, where = where,
                          order = order, limit = lim, offset = offset)
    if (nrow(chunk) == 0) break
    all_data[[length(all_data) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(all_data) == 0) return(tibble())
  bind_rows(all_data)
}


# == Discovery =================================================================

#' List all datasets on data.wa.gov
#'
#' Uses the Socrata Discovery API to list datasets on Washington State's
#' open data portal, sorted by popularity (page views). Covers 630+
#' datasets on politics, health, wildlife, labor, cannabis, and more.
#'
#' @param limit Integer max results (default 100, max 10000).
#' @param offset Integer pagination offset.
#' @param only Character resource type: \code{"datasets"} (default),
#'   \code{"maps"}, \code{"files"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Socrata 4x4 view ID (use with \code{wag_view})}
#'     \item{name}{Dataset name}
#'     \item{description}{Description (truncated to 200 chars)}
#'     \item{type}{Resource type}
#'     \item{updatedAt}{Date of last update}
#'     \item{page_views}{Integer total page views}
#'     \item{category}{Domain category}
#'   }
#' @export
#' @family WA discovery functions
#' @seealso \code{\link{wag_search}} for keyword search,
#'   \code{\link{wag_view}} to query any dataset
#' @examples
#' \dontrun{
#' wag_list(limit = 20)
#' }
wag_list <- function(limit = 100, offset = 0, only = "datasets") {
  params <- list(domains = .wa_domain, limit = limit, offset = offset,
                 order = "page_views_total")
  if (!is.null(only)) params$only <- only

  raw <- .wa_fetch_list(.wa_disco, params)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  tibble(
    id          = vapply(results, function(r) .safe_chr(r$resource$id), character(1)),
    name        = vapply(results, function(r) .safe_chr(r$resource$name), character(1)),
    description = vapply(results, function(r) {
      d <- .safe_chr(r$resource$description)
      if (is.na(d)) d else substr(d, 1, 200)
    }, character(1)),
    type        = vapply(results, function(r) .safe_chr(r$resource$type), character(1)),
    updatedAt   = as.Date(vapply(results, function(r)
      substr(.safe_chr(r$resource$updatedAt), 1, 10), character(1))),
    page_views  = vapply(results, function(r)
      .safe_int(r$resource$page_views$page_views_total), integer(1)),
    category    = vapply(results, function(r)
      .safe_chr(r$classification$domain_category), character(1))
  )
}


#' Search datasets on data.wa.gov by keyword
#'
#' Full-text search across dataset names, descriptions, and column names.
#'
#' @param query Character search term (e.g., \code{"salmon"},
#'   \code{"cannabis"}, \code{"campaign"}).
#' @param limit Integer max results (default 50).
#' @param offset Integer pagination offset.
#' @return A tibble with the same columns as \code{\link{wag_list}}
#'   (minus category).
#' @export
#' @family WA discovery functions
#' @seealso \code{\link{wag_list}} for browsing by popularity,
#'   \code{\link{wag_view}} to query a found dataset
#' @examples
#' \dontrun{
#' wag_search("salmon")
#' wag_search("campaign contributions")
#' }
wag_search <- function(query, limit = 50, offset = 0) {
  params <- list(domains = .wa_domain, q = query, limit = limit,
                 offset = offset, only = "datasets")
  raw <- .wa_fetch_list(.wa_disco, params)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  tibble(
    id          = vapply(results, function(r) .safe_chr(r$resource$id), character(1)),
    name        = vapply(results, function(r) .safe_chr(r$resource$name), character(1)),
    description = vapply(results, function(r) {
      d <- .safe_chr(r$resource$description)
      if (is.na(d)) d else substr(d, 1, 200)
    }, character(1)),
    type        = vapply(results, function(r) .safe_chr(r$resource$type), character(1)),
    updatedAt   = as.Date(vapply(results, function(r)
      substr(.safe_chr(r$resource$updatedAt), 1, 10), character(1))),
    page_views  = vapply(results, function(r)
      .safe_int(r$resource$page_views$page_views_total), integer(1))
  )
}


#' Query any WA dataset by Socrata view ID
#'
#' Generic SODA 2.1 query against any dataset on data.wa.gov.
#' Columns are auto-typed (numeric strings become numeric, date-like
#' columns become Date).
#'
#' @param view_id Character Socrata 4x4 view ID.
#' @param select Optional SoQL SELECT clause.
#' @param where Optional SoQL WHERE clause.
#' @param order Optional SoQL ORDER BY clause.
#' @param q Optional full-text search within the dataset.
#' @param limit Integer max rows (default 1000).
#' @param offset Integer row offset.
#' @return A tibble with auto-typed columns.
#' @export
#' @family WA discovery functions
#' @seealso \code{\link{wag_list}} to find view IDs
#' @examples
#' \dontrun{
#' wag_view("qxh8-f4bd", limit = 10)
#' }
wag_view <- function(view_id, select = NULL, where = NULL,
                     order = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query(view_id, select = select, where = where,
               order = order, q = q, limit = limit, offset = offset)
}


# == Political Finance =========================================================

#' Campaign contributions to candidates and political committees
#'
#' Washington Public Disclosure Commission (PDC) data on contributions.
#' Ordered by receipt date descending.
#'
#' @param filer_name Optional character filter by candidate/committee
#'   name (case-insensitive partial match).
#' @param year Optional election year filter.
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of contribution records.
#' @export
#' @family WA political finance functions
#' @seealso \code{\link{wag_expenditures}} for campaign spending,
#'   \code{\link{wag_campaign_summary}} for aggregated totals
#' @examples
#' \dontrun{
#' wag_contributions(filer_name = "Inslee", year = "2020", limit = 50)
#' }
wag_contributions <- function(filer_name = NULL, year = NULL,
                              where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(filer_name)) clauses <- c(clauses,
    sprintf("upper(filer_name) LIKE upper('%%%s%%')", filer_name))
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("election_year='%s'", year))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("kv7h-kjye", where = w, order = "receipt_date DESC", limit = limit)
}


#' Campaign expenditures by candidates and political committees
#'
#' PDC expenditure records ordered by expenditure date descending.
#'
#' @param filer_name Optional character filter by candidate/committee name.
#' @param year Optional election year.
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of expenditure records.
#' @export
#' @family WA political finance functions
#' @seealso \code{\link{wag_contributions}} for contributions received
#' @examples
#' \dontrun{
#' wag_expenditures(year = "2024", limit = 50)
#' }
wag_expenditures <- function(filer_name = NULL, year = NULL,
                             where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(filer_name)) clauses <- c(clauses,
    sprintf("upper(filer_name) LIKE upper('%%%s%%')", filer_name))
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("election_year='%s'", year))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tijg-9zyp", where = w, order = "expenditure_date DESC", limit = limit)
}


#' PDC enforcement cases
#'
#' @param case_status Optional filter: \code{"Open"}, \code{"Closed"}, etc.
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of enforcement case records.
#' @export
#' @family WA political finance functions
wag_enforcement_cases <- function(case_status = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(case_status)) clauses <- c(clauses,
    sprintf("case_status='%s'", case_status))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("a4ma-dq6s", where = w, limit = limit)
}


#' Campaign finance summary
#'
#' @param filer_name Optional character filter by name.
#' @param year Optional election year.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of campaign finance summary records.
#' @export
#' @family WA political finance functions
wag_campaign_summary <- function(filer_name = NULL, year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(filer_name)) clauses <- c(clauses,
    sprintf("upper(filer_name) LIKE upper('%%%s%%')", filer_name))
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("election_year='%s'", year))
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("3h9x-7bvm", where = w, limit = limit)
}


# == Lobbyist Data =============================================================

#' Lobbyist employment registrations
#'
#' @param year Optional registration year.
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of lobbyist registration records.
#' @export
#' @family WA political finance functions
wag_lobbyist_registrations <- function(year = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("employment_year='%s'", year))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("xhn7-64im", where = w, limit = limit)
}


#' Lobbyist compensation and expenses by source
#'
#' @param year Optional filter year.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of lobbyist compensation records.
#' @export
#' @family WA political finance functions
wag_lobbyist_compensation <- function(year = NULL, limit = 1000) {
  w <- if (!is.null(year)) sprintf("reporting_period_year='%s'", year) else NULL
  ._soda_query("9nnw-c693", where = w, limit = limit)
}


# == Health ====================================================================

#' Health care provider credentials
#'
#' Washington Department of Health professional credential records.
#'
#' @param credential_type Optional filter by type (e.g., \code{"Physician"}).
#' @param last_name Optional filter by last name (partial match).
#' @param status Optional filter: \code{"Active"}, \code{"Expired"}, etc.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of provider credential records.
#' @export
#' @family WA health functions
#' @examples
#' \dontrun{
#' wag_health_providers(credential_type = "Physician", limit = 50)
#' }
wag_health_providers <- function(credential_type = NULL, last_name = NULL,
                                 status = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(credential_type)) clauses <- c(clauses,
    sprintf("upper(credentialtype) LIKE upper('%%%s%%')", credential_type))
  if (!is.null(last_name)) clauses <- c(clauses,
    sprintf("upper(lastname) LIKE upper('%%%s%%')", last_name))
  if (!is.null(status)) clauses <- c(clauses,
    sprintf("status='%s'", status))
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("qxh8-f4bd", where = w, limit = limit)
}


# == Data Breaches =============================================================

#' Data breach notifications affecting WA residents
#'
#' Attorney General's office data breach notification database.
#'
#' @param year Optional filter by year.
#' @param industry_type Optional filter by industry.
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of data breach notification records.
#' @export
#' @family WA consumer protection functions
#' @seealso \code{\link{wag_consumer_complaints}} for consumer complaints
wag_data_breaches <- function(year = NULL, industry_type = NULL,
                              where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year)) clauses <- c(clauses, sprintf("year='%s'", year))
  if (!is.null(industry_type)) clauses <- c(clauses,
    sprintf("industrytype='%s'", industry_type))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("sb4j-ca4h", where = w, order = "datesubmitted DESC", limit = limit)
}


#' Consumer complaints filed with the Attorney General
#'
#' @param business_name Optional character filter by business name (partial match).
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of consumer complaint records.
#' @export
#' @family WA consumer protection functions
#' @seealso \code{\link{wag_data_breaches}} for data breach notifications
wag_consumer_complaints <- function(business_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(businessname) LIKE upper('%%%s%%')", business_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("gpri-47xz", where = w, limit = limit)
}


# == Education =================================================================

#' Graduation pathway completion rates
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of graduation rate records.
#' @export
#' @family WA education functions
#' @seealso \code{\link{wag_school_improvement}} for school improvement data
wag_graduation_rates <- function(where = NULL, limit = 1000) {
  ._soda_query("vjrh-5urx", where = where, limit = limit)
}


#' Washington School Improvement Framework (WSIF) data
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of school improvement records.
#' @export
#' @family WA education functions
wag_school_improvement <- function(where = NULL, limit = 1000) {
  ._soda_query("u25x-vdun", where = where, limit = limit)
}


# == Wildlife & Fish (WDFW) ====================================================

#' WDFW salmonid population indicators
#'
#' @param species Optional character filter by species name (partial match).
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of salmonid population indicator records.
#' @export
#' @family WA wildlife functions
#' @seealso \code{\link{wag_salmon_metrics}} for spawner abundance,
#'   \code{\link{wag_hatcheries}} for hatchery data
#' @examples
#' \dontrun{
#' wag_salmon_populations(species = "Chinook")
#' }
wag_salmon_populations <- function(species = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(species)) clauses <- c(clauses,
    sprintf("upper(species) LIKE upper('%%%s%%')", species))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("ncqh-ypvf", where = w, limit = limit)
}


#' WDFW salmonid spawner abundance metrics
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of spawner abundance metric records.
#' @export
#' @family WA wildlife functions
wag_salmon_metrics <- function(where = NULL, limit = 1000) {
  ._soda_query("x25s-cxg8", where = where, limit = limit)
}


#' WDFW hatchery information
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of hatchery information records.
#' @export
#' @family WA wildlife functions
wag_hatcheries <- function(where = NULL, limit = 1000) {
  ._soda_query("hjdc-v2n4", where = where, limit = limit)
}


#' WDFW spawning ground surveys
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of spawning ground survey records.
#' @export
#' @family WA wildlife functions
wag_spawning_surveys <- function(where = NULL, limit = 1000) {
  ._soda_query("97am-y7xm", where = where, limit = limit)
}


#' WDFW fish releases/plants
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of fish release/plant records.
#' @export
#' @family WA wildlife functions
wag_fish_plants <- function(where = NULL, limit = 1000) {
  ._soda_query("6fex-3r7d", where = where, limit = limit)
}


# == Labor & Industries ========================================================

#' L&I contractor license data
#'
#' @param business_name Optional character filter by business name (partial match).
#' @param where Optional additional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of contractor license records.
#' @export
#' @family WA labor functions
wag_contractor_licenses <- function(business_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(businessname) LIKE upper('%%%s%%')", business_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("m8qx-ubtq", where = w, limit = limit)
}


#' L&I public works intent projects
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of public works intent records.
#' @export
#' @family WA labor functions
wag_public_works_intents <- function(where = NULL, limit = 1000) {
  ._soda_query("t9je-9qwa", where = where, limit = limit)
}


#' L&I apprentice utilization on public works projects
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of apprentice utilization records.
#' @export
#' @family WA labor functions
wag_apprentice_utilization <- function(where = NULL, limit = 1000) {
  ._soda_query("ijvn-uemp", where = where, limit = limit)
}


# == Liquor & Cannabis =========================================================

#' LCB cannabis license renewals
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of cannabis license renewal records.
#' @export
#' @family WA licensing functions
#' @seealso \code{\link{wag_liquor_renewals}} for liquor licenses
wag_cannabis_renewals <- function(where = NULL, limit = 1000) {
  ._soda_query("brpd-b6zd", where = where, limit = limit)
}


#' LCB liquor license renewals
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of liquor license renewal records.
#' @export
#' @family WA licensing functions
#' @seealso \code{\link{wag_cannabis_renewals}} for cannabis licenses
wag_liquor_renewals <- function(where = NULL, limit = 1000) {
  ._soda_query("9dee-kzm5", where = where, limit = limit)
}


# == Environment ===============================================================

#' SEPA (State Environmental Policy Act) register
#'
#' @param where Optional SoQL WHERE clause.
#' @param q Optional full-text search query.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of SEPA register records.
#' @export
#' @family WA environment functions
#' @seealso \code{\link{wag_greenhouse_gas}} for emissions data
wag_sepa <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("mmcb-z6jf", where = where, q = q, limit = limit)
}


#' Greenhouse gas emissions facility data (2017)
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of greenhouse gas emissions records.
#' @export
#' @family WA environment functions
wag_greenhouse_gas <- function(where = NULL, limit = 1000) {
  ._soda_query("533j-4nbp", where = where, limit = limit)
}


# == Criminal Justice ==========================================================

#' Criminal justice training commission officer certification cases
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of officer certification case records.
#' @export
#' @family WA criminal justice functions
#' @seealso \code{\link{wag_crime_reports}} for crime statistics
wag_officer_certifications <- function(where = NULL, limit = 1000) {
  ._soda_query("r5ki-dmfz", where = where, limit = limit)
}


#' Criminal justice data book
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of criminal justice data book records.
#' @export
#' @family WA criminal justice functions
wag_crime_reports <- function(where = NULL, limit = 1000) {
  ._soda_query("v2gc-rgep", where = where, limit = limit)
}


# == Government Finance ========================================================

#' State agency contracts (current fiscal year)
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of state agency contract records.
#' @export
#' @family WA government finance functions
wag_agency_contracts <- function(where = NULL, limit = 1000) {
  ._soda_query("6fx9-ncas", where = where, limit = limit)
}


#' Statewide master contract sales data
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of contract sales records.
#' @export
#' @family WA government finance functions
wag_contract_sales <- function(where = NULL, limit = 1000) {
  ._soda_query("n8q6-4twj", where = where, limit = limit)
}


#' IT spend by technology towers
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of IT spending records by technology tower.
#' @export
#' @family WA government finance functions
wag_it_spend <- function(where = NULL, limit = 1000) {
  ._soda_query("vuts-wzbq", where = where, limit = limit)
}


# == Licensing =================================================================

#' Professional license transactions (DOL)
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of professional license transaction records.
#' @export
#' @family WA licensing functions
wag_professional_licenses <- function(where = NULL, limit = 1000) {
  ._soda_query("ixni-jq78", where = where, limit = limit)
}


#' Vehicle registrations by class and county
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of vehicle registration records.
#' @export
#' @family WA licensing functions
wag_vehicle_registrations <- function(where = NULL, limit = 1000) {
  ._soda_query("hmzg-s6q4", where = where, limit = limit)
}


#' Washington State Certified Public Accountants
#'
#' @param where Optional SoQL WHERE clause.
#' @param limit Integer max rows (default 1000).
#' @return A tibble of CPA firm records.
#' @export
#' @family WA licensing functions
wag_cpa_firms <- function(where = NULL, limit = 1000) {
  ._soda_query("pzcu-jpab", where = where, limit = limit)
}


# == Context ===================================================================

#' Get wa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wag_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wag_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/wa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "wa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# wa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# wa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
