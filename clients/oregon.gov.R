# oregon.gov.R
# Oregon State Open Data client (data.oregon.gov — Socrata/SODA 2.1).
# Discovery + access for 420+ Socrata datasets covering business, health,
# elections, workers' comp, housing, licensing, education, environment, and more.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.or_domain <- "data.oregon.gov"
.or_disco  <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_   else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_      else as.double(x)

# -- JSON fetch ----------------------------------------------------------------

.or_fetch <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

.or_fetch_list <- function(url, params = list()) {
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
  url <- sprintf("https://%s/resource/%s.json", .or_domain, view_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(order))  params[["$order"]]   <- order
  if (!is.null(group))  params[["$group"]]   <- group
  if (!is.null(q))      params[["$q"]]       <- q

  raw <- .or_fetch(url, params)
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
      if (all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
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

#' List all datasets on data.oregon.gov
#'
#' Uses the Socrata Discovery API to list datasets from Oregon's open data
#' portal, sorted by total page views (most popular first). Returns dataset
#' metadata including four-by-four IDs that can be passed to
#' \code{\link{oreg_view}} or any domain-specific function.
#'
#' @param limit Integer. Maximum results to return (default 100, max 10000).
#' @param offset Integer. Pagination offset for paging through the catalog.
#' @param only Character. Resource type filter: \code{"datasets"} (default),
#'   \code{"maps"}, \code{"files"}, or \code{NULL} for all types.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{id}{Character. Socrata four-by-four dataset identifier.}
#'   \item{name}{Character. Dataset title.}
#'   \item{description}{Character. Brief description (truncated to 200 chars).}
#'   \item{type}{Character. Resource type (dataset, map, file, etc.).}
#'   \item{updatedAt}{Date. Date of last update.}
#'   \item{page_views}{Integer. Total page views (popularity metric).}
#'   \item{category}{Character. Domain category (e.g. "business", "health").}
#' }
#'
#' @examples
#' \dontrun{
#' oreg_list(limit = 20)
#' oreg_list(only = "maps", limit = 10)
#' }
#'
#' @seealso \code{\link{oreg_search}} for keyword search,
#'   \code{\link{oreg_view}} for querying a dataset by ID.
#' @export
oreg_list <- function(limit = 100, offset = 0, only = "datasets") {
  params <- list(domains = .or_domain, limit = limit, offset = offset,
                 order = "page_views_total")
  if (!is.null(only)) params$only <- only

  raw <- .or_fetch_list(.or_disco, params)
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


#' Search datasets on data.oregon.gov
#'
#' Full-text search across dataset names, descriptions, and column names on
#' Oregon's open data portal. Uses the Socrata Discovery API.
#'
#' @param query Character. Search term (e.g. \code{"business"}, \code{"covid"}).
#' @param limit Integer. Maximum results to return (default 50).
#' @param offset Integer. Pagination offset.
#'
#' @return A tibble with columns \code{id}, \code{name}, \code{description},
#'   \code{type}, \code{updatedAt}, and \code{page_views}.
#'
#' @examples
#' \dontrun{
#' oreg_search("housing")
#' oreg_search("voter registration", limit = 10)
#' }
#'
#' @seealso \code{\link{oreg_list}} for browsing by popularity.
#' @export
oreg_search <- function(query, limit = 50, offset = 0) {
  params <- list(domains = .or_domain, q = query, limit = limit,
                 offset = offset, only = "datasets")
  raw <- .or_fetch_list(.or_disco, params)
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


#' Query any Oregon dataset by view ID (generic SODA access)
#'
#' Generic accessor for any Oregon Socrata dataset. Accepts SoQL clauses
#' for server-side filtering, column selection, ordering, and full-text
#' search. Numeric and date columns are automatically typed.
#'
#' @param view_id Character. Socrata four-by-four view identifier
#'   (e.g. \code{"tckn-sxa6"} for active businesses).
#' @param select Character. SoQL SELECT clause to choose columns
#'   (e.g. \code{"business_name, city, registry_date"}).
#' @param where Character. SoQL WHERE clause for filtering
#'   (e.g. \code{"city='Portland'"}).
#' @param order Character. SoQL ORDER BY clause
#'   (e.g. \code{"registry_date DESC"}).
#' @param q Character. Full-text search query within the dataset.
#' @param limit Integer. Maximum rows to return (default 1000).
#' @param offset Integer. Row offset for pagination.
#'
#' @return A tibble with auto-typed columns from the dataset.
#'
#' @examples
#' \dontrun{
#' oreg_view("tckn-sxa6", where = "city='Portland'", limit = 10)
#' }
#'
#' @seealso \code{\link{oreg_list}}, \code{\link{oreg_search}}
#' @export
oreg_view <- function(view_id, select = NULL, where = NULL,
                      order = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query(view_id, select = select, where = where,
               order = order, q = q, limit = limit, offset = offset)
}


# == Business ==================================================================

#' Active businesses in Oregon (all)
#'
#' Queries the complete registry of active businesses from the Oregon
#' Secretary of State (dataset \code{tckn-sxa6}). Supports filtering by
#' business name (partial match), entity type, and city. This is Oregon's
#' most popular dataset with 450k+ page views.
#'
#' @param business_name Character. Filter by business name (case-insensitive
#'   partial match using SoQL LIKE).
#' @param entity_type Character. Filter by entity type, e.g.
#'   \code{"DOMESTIC BUSINESS CORPORATION"}, \code{"DOMESTIC LIMITED LIABILITY COMPANY"}.
#' @param city Character. Filter by city (case-insensitive partial match).
#' @param where Character. Additional SoQL WHERE clause for custom filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble with columns including \code{registry_number},
#'   \code{business_name}, \code{entity_type}, \code{registry_date},
#'   \code{city}, \code{state}, \code{zip}, and \code{jurisdiction}.
#'
#' @examples
#' \dontrun{
#' oreg_businesses(city = "Portland", limit = 10)
#' oreg_businesses(entity_type = "DOMESTIC LIMITED LIABILITY COMPANY", limit = 20)
#' }
#'
#' @export
oreg_businesses <- function(business_name = NULL, entity_type = NULL,
                            city = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(business_name) LIKE upper('%%%s%%')", business_name))
  if (!is.null(entity_type)) clauses <- c(clauses,
    sprintf("entity_type='%s'", entity_type))
  if (!is.null(city)) clauses <- c(clauses,
    sprintf("upper(city) LIKE upper('%%%s%%')", city))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tckn-sxa6", where = w, limit = limit)
}


#' Active businesses by county
#'
#' Queries Oregon active businesses grouped/filtered by county
#' (dataset \code{6g49-bcrm}).
#'
#' @param county Character. County name to filter (case-insensitive partial match).
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of business records with county-level columns.
#'
#' @examples
#' \dontrun{
#' oreg_businesses_county(county = "Multnomah", limit = 20)
#' }
#'
#' @export
oreg_businesses_county <- function(county = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses,
    sprintf("upper(county) LIKE upper('%%%s%%')", county))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("6g49-bcrm", where = w, limit = limit)
}


#' Active nonprofit corporations
#'
#' Returns active nonprofit corporations registered in Oregon
#' (dataset \code{8kyv-b2kw}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of nonprofit corporation records.
#'
#' @examples
#' \dontrun{
#' oreg_nonprofits(limit = 50)
#' }
#'
#' @export
oreg_nonprofits <- function(where = NULL, limit = 1000) {
  ._soda_query("8kyv-b2kw", where = where, limit = limit)
}


#' Active trademark registrations
#'
#' Returns active trademark registrations filed with the Oregon Secretary of
#' State (dataset \code{ny3n-dx3v}). Supports full-text search across
#' trademark descriptions.
#'
#' @param q Character. Full-text search query across trademark descriptions.
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of trademark registration records.
#'
#' @examples
#' \dontrun{
#' oreg_trademarks(q = "coffee", limit = 20)
#' }
#'
#' @export
oreg_trademarks <- function(q = NULL, where = NULL, limit = 1000) {
  ._soda_query("ny3n-dx3v", where = where, q = q, limit = limit)
}


#' UCC (Uniform Commercial Code) filings entered last month
#'
#' Returns UCC filings entered in the most recent month from the Oregon
#' Secretary of State (dataset \code{snfi-f79b}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of UCC filing records.
#'
#' @examples
#' \dontrun{
#' oreg_ucc_filings(limit = 50)
#' }
#'
#' @export
oreg_ucc_filings <- function(where = NULL, limit = 1000) {
  ._soda_query("snfi-f79b", where = where, limit = limit)
}


# == Elections =================================================================

#' Voter registration data
#'
#' Returns Oregon voter registration counts broken down by county, party
#' affiliation, and district (dataset \code{8h6y-5uec}). Supports filtering
#' by county and party name.
#'
#' @param county Character. County name filter (case-insensitive partial match).
#' @param party Character. Party name filter (case-insensitive partial match,
#'   e.g. \code{"Democrat"}, \code{"Republican"}).
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of voter registration records with county, party, and
#'   registration count columns.
#'
#' @examples
#' \dontrun{
#' oreg_voter_registration(county = "Multnomah", limit = 50)
#' }
#'
#' @export
oreg_voter_registration <- function(county = NULL, party = NULL,
                                    where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses,
    sprintf("upper(county) LIKE upper('%%%s%%')", county))
  if (!is.null(party)) clauses <- c(clauses,
    sprintf("upper(party) LIKE upper('%%%s%%')", party))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("8h6y-5uec", where = w, limit = limit)
}


#' Campaign finance penalty notices
#'
#' Returns campaign finance penalty notices issued in Oregon
#' (dataset \code{fku5-vh2b}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of penalty notice records.
#'
#' @examples
#' \dontrun{
#' oreg_campaign_penalties(limit = 50)
#' }
#'
#' @export
oreg_campaign_penalties <- function(where = NULL, limit = 1000) {
  ._soda_query("fku5-vh2b", where = where, limit = limit)
}


#' Voting districts by precinct
#'
#' Returns Oregon voting district assignments by precinct
#' (dataset \code{r7vb-b9k4}).
#'
#' @param county Character. County name filter (case-insensitive partial match).
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of voting district records.
#'
#' @examples
#' \dontrun{
#' oreg_voting_districts(county = "Lane", limit = 50)
#' }
#'
#' @export
oreg_voting_districts <- function(county = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses,
    sprintf("upper(county) LIKE upper('%%%s%%')", county))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("r7vb-b9k4", where = w, limit = limit)
}


# == Workers' Compensation ====================================================

#' Active workers' compensation employer database
#'
#' Returns the Oregon workers' compensation employer database
#' (dataset \code{q9zj-c8r2}). Includes employer names, addresses,
#' carrier information, and coverage status.
#'
#' @param business_name Character. Employer name filter (case-insensitive
#'   partial match on \code{legal_business_name}).
#' @param county_code Character. County code filter.
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of employer workers' compensation records.
#'
#' @examples
#' \dontrun{
#' oreg_workers_comp_employers(business_name = "Nike", limit = 20)
#' }
#'
#' @export
oreg_workers_comp_employers <- function(business_name = NULL, county_code = NULL,
                                        where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(legal_business_name) LIKE upper('%%%s%%')", business_name))
  if (!is.null(county_code)) clauses <- c(clauses,
    sprintf("county_code='%s'", county_code))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("q9zj-c8r2", where = w, limit = limit)
}


#' Workers' compensation record level claims
#'
#' Returns individual workers' compensation claim records from Oregon
#' (dataset \code{t9t7-8a2y}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of claim-level records.
#'
#' @examples
#' \dontrun{
#' oreg_workers_comp_claims(limit = 50)
#' }
#'
#' @export
oreg_workers_comp_claims <- function(where = NULL, limit = 1000) {
  ._soda_query("t9t7-8a2y", where = where, limit = limit)
}


#' WARN (Worker Adjustment and Retraining Notification) notices
#'
#' Returns WARN Act notices filed in Oregon (dataset \code{ijbz-jpx8}).
#' These notices are filed by employers planning mass layoffs or plant
#' closures.
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across all fields.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of WARN notice records.
#'
#' @examples
#' \dontrun{
#' oreg_warn_notices(limit = 50)
#' oreg_warn_notices(q = "Intel")
#' }
#'
#' @export
oreg_warn_notices <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("ijbz-jpx8", where = where, q = q, limit = limit)
}


# == Health ====================================================================

#' Weekly reportable disease data
#'
#' Returns weekly counts of reportable diseases in Oregon
#' (dataset \code{2idf-8ewz}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of weekly disease surveillance records.
#'
#' @examples
#' \dontrun{
#' oreg_disease_data(limit = 50)
#' }
#'
#' @export
oreg_disease_data <- function(where = NULL, limit = 1000) {
  ._soda_query("2idf-8ewz", where = where, limit = limit)
}


#' Oregon Medical Board license type and status counts
#'
#' Returns license type and status counts from the Oregon Medical Board
#' (dataset \code{ifun-evx5}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of license type/status summary records.
#'
#' @examples
#' \dontrun{
#' oreg_medical_licenses()
#' }
#'
#' @export
oreg_medical_licenses <- function(where = NULL, limit = 1000) {
  ._soda_query("ifun-evx5", where = where, limit = limit)
}


#' Oregon Medicaid prioritized list funding line
#'
#' Returns the Oregon Health Plan prioritized list of health services with
#' funding line positions (dataset \code{2mv8-fp67}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of prioritized health service records.
#'
#' @examples
#' \dontrun{
#' oreg_medicaid_funding(limit = 50)
#' }
#'
#' @export
oreg_medicaid_funding <- function(where = NULL, limit = 1000) {
  ._soda_query("2mv8-fp67", where = where, limit = limit)
}


# == Housing ===================================================================

#' Affordable housing inventory
#'
#' Returns affordable housing inventory data from Oregon Housing and Community
#' Services (dataset \code{p9yn-ftai}). Supports filtering by city.
#'
#' @param city Character. City name filter (case-insensitive partial match).
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of affordable housing property records.
#'
#' @examples
#' \dontrun{
#' oreg_affordable_housing(city = "Portland", limit = 50)
#' }
#'
#' @export
oreg_affordable_housing <- function(city = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(city)) clauses <- c(clauses,
    sprintf("upper(city) LIKE upper('%%%s%%')", city))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("p9yn-ftai", where = w, limit = limit)
}


#' Affordable rental housing partner database
#'
#' Returns affordable rental housing partners from OHCS
#' (dataset \code{97sx-jz4n}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of housing partner records.
#'
#' @examples
#' \dontrun{
#' oreg_rental_housing_partners(limit = 50)
#' }
#'
#' @export
oreg_rental_housing_partners <- function(where = NULL, limit = 1000) {
  ._soda_query("97sx-jz4n", where = where, limit = limit)
}


#' OHCS housing grant opportunities
#'
#' Returns Oregon Housing and Community Services grant opportunity records
#' (dataset \code{8e87-r9yi}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of housing grant opportunity records.
#'
#' @examples
#' \dontrun{
#' oreg_housing_grants()
#' }
#'
#' @export
oreg_housing_grants <- function(where = NULL, limit = 1000) {
  ._soda_query("8e87-r9yi", where = where, limit = limit)
}


# == Licensing =================================================================

#' Notary public directory
#'
#' Returns the Oregon notary public directory from the Secretary of State
#' (dataset \code{m934-wp3t}). Supports filtering by last name.
#'
#' @param last_name Character. Last name filter (case-insensitive partial match).
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of notary public records.
#'
#' @examples
#' \dontrun{
#' oreg_notaries(last_name = "Smith", limit = 20)
#' }
#'
#' @export
oreg_notaries <- function(last_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(last_name)) clauses <- c(clauses,
    sprintf("upper(last_name) LIKE upper('%%%s%%')", last_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("m934-wp3t", where = w, limit = limit)
}


#' Building Codes Division - Active contractor/individual licenses
#'
#' Returns active contractor and individual licenses from the Oregon Building
#' Codes Division (dataset \code{vhbr-cuaq}).
#'
#' @param business_name Character. Business name filter (case-insensitive
#'   partial match).
#' @param where Character. Additional SoQL WHERE clause.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of contractor license records.
#'
#' @examples
#' \dontrun{
#' oreg_contractor_licenses(business_name = "Electric", limit = 20)
#' }
#'
#' @export
oreg_contractor_licenses <- function(business_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(business_name) LIKE upper('%%%s%%')", business_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("vhbr-cuaq", where = w, limit = limit)
}


#' CCB (Construction Contractors Board) active licenses
#'
#' Returns active Construction Contractors Board licenses in Oregon
#' (dataset \code{g77e-6bhs}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across all fields.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of CCB license records.
#'
#' @examples
#' \dontrun{
#' oreg_ccb_licenses(q = "plumbing", limit = 20)
#' }
#'
#' @export
oreg_ccb_licenses <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("g77e-6bhs", where = where, q = q, limit = limit)
}


# == Education =================================================================

#' Community college course listing
#'
#' Returns community college course listings from Oregon
#' (dataset \code{4yhv-tzjd}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across course titles and descriptions.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of community college course records.
#'
#' @examples
#' \dontrun{
#' oreg_cc_courses(q = "nursing", limit = 20)
#' }
#'
#' @export
oreg_cc_courses <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("4yhv-tzjd", where = where, q = q, limit = limit)
}


#' Eligible training provider list
#'
#' Returns eligible training providers approved by Oregon Employment Department
#' (dataset \code{dhnh-39zs}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across provider names and programs.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of training provider records.
#'
#' @examples
#' \dontrun{
#' oreg_training_providers(q = "tech", limit = 20)
#' }
#'
#' @export
oreg_training_providers <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("dhnh-39zs", where = where, q = q, limit = limit)
}


# == Environment & Agriculture =================================================

#' Farm products master list
#'
#' Returns the Oregon farm products master list from the Department of
#' Agriculture (dataset \code{3qaz-7u98}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across product names and descriptions.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of farm product records.
#'
#' @examples
#' \dontrun{
#' oreg_farm_products(q = "wine", limit = 20)
#' }
#'
#' @export
oreg_farm_products <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("3qaz-7u98", where = where, q = q, limit = limit)
}


#' Hazardous materials storage sites
#'
#' Returns hazardous materials storage site records from the Oregon
#' State Fire Marshal (dataset \code{ae2j-x3ks}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across all fields.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of hazmat site records.
#'
#' @examples
#' \dontrun{
#' oreg_hazmat_sites(q = "Portland", limit = 20)
#' }
#'
#' @export
oreg_hazmat_sites <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("ae2j-x3ks", where = where, q = q, limit = limit)
}


#' Fertilizer program stop sales
#'
#' Returns fertilizer program stop sale records from the Oregon Department
#' of Agriculture (dataset \code{svge-u3j9}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of fertilizer stop sale records.
#'
#' @examples
#' \dontrun{
#' oreg_fertilizer_stops(limit = 50)
#' }
#'
#' @export
oreg_fertilizer_stops <- function(where = NULL, limit = 1000) {
  ._soda_query("svge-u3j9", where = where, limit = limit)
}


# == Finance ===================================================================

#' Budgeted revenue
#'
#' Returns Oregon state budgeted revenue data (dataset \code{mwsa-rpk9}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of budgeted revenue records.
#'
#' @examples
#' \dontrun{
#' oreg_budgeted_revenue(limit = 50)
#' }
#'
#' @export
oreg_budgeted_revenue <- function(where = NULL, limit = 1000) {
  ._soda_query("mwsa-rpk9", where = where, limit = limit)
}


#' Secretary of State financial accounting (current biennium)
#'
#' Returns financial accounting data from the Oregon Secretary of State
#' for the current biennium (dataset \code{vsgy-hcp4}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of accounting records.
#'
#' @examples
#' \dontrun{
#' oreg_sos_accounting(limit = 50)
#' }
#'
#' @export
oreg_sos_accounting <- function(where = NULL, limit = 1000) {
  ._soda_query("vsgy-hcp4", where = where, limit = limit)
}


#' Secretary of State payroll (current biennium)
#'
#' Returns payroll data from the Oregon Secretary of State for the current
#' biennium (dataset \code{fqkk-e5mr}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of payroll records.
#'
#' @examples
#' \dontrun{
#' oreg_sos_payroll(limit = 50)
#' }
#'
#' @export
oreg_sos_payroll <- function(where = NULL, limit = 1000) {
  ._soda_query("fqkk-e5mr", where = where, limit = limit)
}


# == Libraries =================================================================

#' Oregon library directory
#'
#' Returns a directory of libraries in Oregon (dataset \code{6x9d-idz4}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across library names and locations.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of library directory records.
#'
#' @examples
#' \dontrun{
#' oreg_libraries(q = "Eugene", limit = 20)
#' }
#'
#' @export
oreg_libraries <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("6x9d-idz4", where = where, q = q, limit = limit)
}


#' Oregon public library statistics
#'
#' Returns annual statistics for Oregon public libraries including
#' circulation, visits, and collection data (dataset \code{8zw7-zgjw}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of library statistics records.
#'
#' @examples
#' \dontrun{
#' oreg_library_stats(limit = 50)
#' }
#'
#' @export
oreg_library_stats <- function(where = NULL, limit = 1000) {
  ._soda_query("8zw7-zgjw", where = where, limit = limit)
}


# == Government ================================================================

#' Oregon agencies, boards, and commissions
#'
#' Returns a directory of Oregon state agencies, boards, and commissions
#' (dataset \code{wu8n-jqum}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across agency names and descriptions.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of agency/board/commission records.
#'
#' @examples
#' \dontrun{
#' oreg_agencies(q = "environment", limit = 20)
#' }
#'
#' @export
oreg_agencies <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("wu8n-jqum", where = where, q = q, limit = limit)
}


#' State agency data inventory
#'
#' Returns the Oregon state agency data inventory listing datasets
#' maintained by each agency (dataset \code{yp9j-pm7w}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param q Character. Full-text search across inventory entries.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of data inventory records.
#'
#' @examples
#' \dontrun{
#' oreg_data_inventory(q = "transportation", limit = 20)
#' }
#'
#' @export
oreg_data_inventory <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("yp9j-pm7w", where = where, q = q, limit = limit)
}


#' OEM emergency managers (local and tribal)
#'
#' Returns the directory of local and tribal emergency managers from the
#' Oregon Office of Emergency Management (dataset \code{vcvj-awba}).
#'
#' @param where Character. SoQL WHERE clause for filtering.
#' @param limit Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of emergency manager contact records.
#'
#' @examples
#' \dontrun{
#' oreg_emergency_managers()
#' }
#'
#' @export
oreg_emergency_managers <- function(where = NULL, limit = 1000) {
  ._soda_query("vcvj-awba", where = where, limit = limit)
}


# == Context ===================================================================

#' Get oregon.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
oreg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(oreg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/oregon.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "oregon.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# oregon.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# oregon.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
