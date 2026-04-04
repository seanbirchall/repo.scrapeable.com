# -- Discovery -----------------------------------------------------------------

#' List available FCC Open Data datasets
#'
#' Queries the Socrata catalog API for datasets hosted on opendata.fcc.gov.
#'
#' @param limit Maximum number of datasets to return (default 100)
#' @return tibble: id, name, type, description, updated_at
#' @export
fcc_list <- function(limit = 100) {
  url <- sprintf("https://api.us.socrata.com/api/catalog/v1?domains=opendata.fcc.gov&limit=%d",
                 limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("FCC catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_datasets)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_datasets)

  res <- results$resource
  tibble(
    id          = as.character(res$id %||% NA_character_),
    name        = as.character(res$name %||% NA_character_),
    type        = as.character(res$type %||% NA_character_),
    description = vapply(res$description %||% rep(NA_character_, nrow(res)),
                         function(x) {
                           x <- as.character(x %||% "")
                           if (nchar(x) > 200) paste0(substr(x, 1, 200), "...") else x
                         }, character(1)),
    updated_at  = as.character(res$updatedAt %||% NA_character_)
  )
}


#' Search FCC Open Data datasets by keyword
#'
#' Full-text search across dataset names and descriptions.
#'
#' @param query Search term (e.g. "broadband", "complaints", "spectrum")
#' @param limit Maximum results (default 50)
#' @return tibble: id, name, type, description, updated_at
#' @export
fcc_search <- function(query, limit = 50) {
  url <- sprintf(
    "https://api.us.socrata.com/api/catalog/v1?domains=opendata.fcc.gov&q=%s&limit=%d",
    utils::URLencode(query, reserved = TRUE), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("FCC search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_datasets)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_datasets)

  res <- results$resource
  tibble(
    id          = as.character(res$id %||% NA_character_),
    name        = as.character(res$name %||% NA_character_),
    type        = as.character(res$type %||% NA_character_),
    description = vapply(res$description %||% rep(NA_character_, nrow(res)),
                         function(x) {
                           x <- as.character(x %||% "")
                           if (nchar(x) > 200) paste0(substr(x, 1, 200), "...") else x
                         }, character(1)),
    updated_at  = as.character(res$updatedAt %||% NA_character_)
  )
}


#' Get known FCC view IDs
#'
#' Returns a tibble of pre-catalogued FCC dataset view IDs with short names.
#' Use these IDs with fcc_view() or the named convenience functions.
#'
#' @return tibble: name, view_id
#' @export
fcc_views <- function() {
  tibble(
    name    = names(.fcc_views),
    view_id = unlist(.fcc_views, use.names = FALSE)
  )
}


# -- Generic SODA access -------------------------------------------------------

#' Query any FCC Open Data dataset via SODA
#'
#' Generic SODA query against any opendata.fcc.gov view. Supports SoQL
#' filtering, selection, grouping, ordering, full-text search, and pagination.
#'
#' @param view_id Socrata 4x4 view identifier (e.g. "3xyp-aqkj")
#' @param where SoQL WHERE clause (e.g. "state='CA'")
#' @param select SoQL SELECT clause (e.g. "state,count(*)")
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause (e.g. "ticket_created DESC")
#' @param q Full-text search term
#' @param limit Max rows (default 1000, max 50000)
#' @param offset Pagination offset
#' @return tibble
#' @export
fcc_view <- function(view_id, where = NULL, select = NULL, group = NULL,
                     order = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select, group = group,
               order = order, q = q, limit = limit, offset = offset)
}


#' Fetch all rows from an FCC dataset with auto-pagination
#'
#' Pages through SODA results automatically up to max_rows.
#'
#' @param view_id Socrata 4x4 view identifier
#' @param where SoQL WHERE clause
#' @param max_rows Maximum total rows (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble
#' @export
fcc_fetch_all <- function(view_id, where = NULL, max_rows = 10000,
                          page_size = 1000) {
  ._soda_fetch_all(view_id, where = where, max_rows = max_rows,
                   page_size = page_size)
}


# -- Consumer Complaints ------------------------------------------------------

#' FCC consumer complaints
#'
#' Queries informal consumer complaint data filed with the FCC Consumer Help
#' Center (since October 2014). Covers unwanted calls, billing, service
#' quality, and more.
#'
#' @param issue_type Filter by issue type: "Phone", "Internet", "TV", "Radio",
#'   "Emergency", "Accessibility" (case-sensitive, partial match)
#' @param state Two-letter state abbreviation (e.g. "CA", "NY")
#' @param issue Filter by specific issue (e.g. "Unwanted Calls", "Billing")
#' @param since Date string "YYYY-MM-DD" -- only complaints after this date
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: id, ticket_created, issue_type, method, issue,
#'   caller_id_number, city, state, zip
#' @export
fcc_complaints <- function(issue_type = NULL, state = NULL, issue = NULL,
                           since = NULL, limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(issue_type)) clauses <- c(clauses, sprintf("issue_type='%s'", issue_type))
  if (!is.null(state))      clauses <- c(clauses, sprintf("state='%s'", state))
  if (!is.null(issue))      clauses <- c(clauses, sprintf("issue='%s'", issue))
  if (!is.null(since))      clauses <- c(clauses, sprintf("date_created>='%s'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  result <- ._soda_query(.fcc_views$complaints, where = where,
                         order = "ticket_created DESC",
                         limit = limit, offset = offset)
  if (nrow(result) == 0) return(.schema_complaints)

  result |>
    mutate(
      id             = as.character(id %||% NA_character_),
      ticket_created = as.character(ticket_created %||% NA_character_),
      issue_type     = as.character(issue_type %||% NA_character_),
      method         = as.character(method %||% NA_character_),
      issue          = as.character(issue %||% NA_character_),
      city           = as.character(city %||% NA_character_),
      state          = as.character(state %||% NA_character_),
      zip            = as.character(zip %||% NA_character_)
    ) |>
    select(id, ticket_created, issue_type, method, issue,
           caller_id_number, city, state, zip)
}


#' Count FCC complaints by category
#'
#' Aggregates complaint counts by issue_type, issue, state, or method.
#'
#' @param group_by Column to group by: "issue_type", "issue", "state",
#'   "method" (default "issue_type")
#' @param since Date string to filter complaints after
#' @return tibble: group column + count
#' @export
fcc_complaint_counts <- function(group_by = "issue_type", since = NULL) {
  where <- if (!is.null(since)) sprintf("date_created>='%s'", since) else NULL
  ._soda_query(.fcc_views$complaints,
               select = sprintf("%s, count(*) as count", group_by),
               group = group_by,
               order = "count DESC",
               where = where,
               limit = 5000)
}


# -- Broadband Deployment -----------------------------------------------------

#' FCC broadband deployment data
#'
#' Fixed broadband deployment from Form 477 filings. Each row represents a
#' provider's service in a census block. Multiple vintage datasets available.
#'
#' @param state Two-letter state abbreviation (e.g. "CA")
#' @param provider Provider name (partial match with SoQL LIKE)
#' @param min_down Minimum advertised download speed (Mbps)
#' @param vintage Which filing: "dec2020" (default), "dec2019", "jun2021",
#'   "jun2020"
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: provider_id, providername, stateabbr, blockcode, techcode,
#'   maxaddown, maxadup, consumer
#' @export
fcc_broadband <- function(state = NULL, provider = NULL, min_down = NULL,
                          vintage = "dec2020", limit = 1000, offset = 0) {
  view_key <- paste0("broadband_", vintage)
  view_id <- .fcc_views[[view_key]]
  if (is.null(view_id)) {
    stop("Unknown vintage '", vintage, "'. Options: dec2020, dec2019, jun2021, jun2020")
  }

  clauses <- character()
  if (!is.null(state))    clauses <- c(clauses, sprintf("stateabbr='%s'", state))
  if (!is.null(provider)) clauses <- c(clauses, sprintf("upper(providername) like upper('%%%s%%')", provider))
  if (!is.null(min_down)) clauses <- c(clauses, sprintf("maxaddown>=%s", min_down))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  result <- ._soda_query(view_id, where = where, limit = limit, offset = offset)
  if (nrow(result) == 0) return(.schema_broadband)
  result
}


#' Count broadband providers by state
#'
#' @param vintage Filing vintage (default "dec2020")
#' @return tibble: stateabbr, provider_count
#' @export
fcc_broadband_by_state <- function(vintage = "dec2020") {
  view_key <- paste0("broadband_", vintage)
  view_id <- .fcc_views[[view_key]]
  if (is.null(view_id)) stop("Unknown vintage: ", vintage)

  ._soda_query(view_id,
               select = "stateabbr, count(distinct provider_id) as provider_count",
               group = "stateabbr",
               order = "provider_count DESC",
               limit = 100)
}


# -- Pirate Radio Enforcement --------------------------------------------------

#' FCC pirate radio enforcement actions
#'
#' Database of enforcement actions against pirate radio broadcasters under
#' the PIRATE Act.
#'
#' @param state Two-letter state abbreviation
#' @param action_type Type of enforcement (e.g. "NAL Issued", "Forfeiture Order Issued")
#' @param limit Max rows (default 500)
#' @return tibble: case_id_number, enforcement_target, state_or_territory,
#'   type_of_enforcement_action, issued_date, penalty_amount, frequency
#' @export
fcc_pirate_radio <- function(state = NULL, action_type = NULL, limit = 500) {
  clauses <- character()
  if (!is.null(state))       clauses <- c(clauses, sprintf("state_or_territory='%s'", state))
  if (!is.null(action_type)) clauses <- c(clauses, sprintf("type_of_enforcement_action='%s'", action_type))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  result <- ._soda_query(.fcc_views$pirate_radio, where = where,
                         order = "issued_date DESC", limit = limit)
  if (nrow(result) == 0) return(.schema_pirate_radio)

  result |>
    mutate(
      issued_date = as.Date(substr(issued_date, 1, 10))
    ) |>
    select(any_of(c("case_id_number", "enforcement_target", "state_or_territory",
                     "type_of_enforcement_action", "issued_date", "penalty_amount",
                     "frequency")))
}


# -- CSRIC Best Practices -----------------------------------------------------

#' CSRIC communications security best practices
#'
#' Searchable database of best practices from the Communications Security,
#' Reliability and Interoperability Council (CSRIC).
#'
#' @param keyword Search keyword in description
#' @param network_type Filter: "Cable", "Wireless", "Wireline", "Satellite",
#'   "Internet/Data"
#' @param limit Max rows (default 500)
#' @return tibble
#' @export
fcc_csric <- function(keyword = NULL, network_type = NULL, limit = 500) {
  clauses <- character()
  if (!is.null(network_type)) {
    col <- tolower(gsub("[/ ]", "_", network_type))
    clauses <- c(clauses, sprintf("%s='TRUE'", col))
  }
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  ._soda_query(.fcc_views$csric, where = where, q = keyword,
               limit = limit)
}


# -- 911 PSAP Registry --------------------------------------------------------

#' 911 Public Safety Answering Point (PSAP) registry
#'
#' Master list of PSAPs (911 call centers) in the US.
#'
#' @param state Two-letter state abbreviation
#' @param q Full-text search term
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
fcc_psap <- function(state = NULL, q = NULL, limit = 1000) {
  where <- if (!is.null(state)) sprintf("state='%s'", state) else NULL
  ._soda_query(.fcc_views$psap_registry, where = where, q = q, limit = limit)
}


# -- Equipment Authorization --------------------------------------------------

#' FCC equipment authorization grantee registrations
#'
#' RF device grantee registrations from the Equipment Authorization System (EAS).
#'
#' @param country Filter by country name
#' @param q Full-text search term
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
fcc_eas_grantees <- function(country = NULL, q = NULL, limit = 1000) {
  where <- if (!is.null(country)) sprintf("country='%s'", country) else NULL
  ._soda_query(.fcc_views$eas_grantees, where = where, q = q,
               order = "date_received DESC", limit = limit)
}


# -- Earth Stations ------------------------------------------------------------

#' Protected FSS earth station registrations
#'
#' Fixed-satellite service earth station antenna sites registered in the
#' FSS Antenna Registration System.
#'
#' @param q Full-text search term
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
fcc_earth_stations <- function(q = NULL, limit = 1000) {
  ._soda_query(.fcc_views$earth_stations, q = q, limit = limit)
}


# -- Paperwork Reduction Act ---------------------------------------------------

#' FCC information collections under the Paperwork Reduction Act
#'
#' Active and pending information collections for which FCC has OMB approval.
#'
#' @param q Full-text search term
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
fcc_pra <- function(q = NULL, limit = 1000) {
  ._soda_query(.fcc_views$pra_collections, q = q, limit = limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the FCC Open Data package
#'
#' Reads its own source and prints function signatures with roxygen docs.
#'
#' @return Character string (invisibly), also printed
#' @export
fcc_context <- function() {
  .build_context("fcc.gov", header_lines = c(
    "# fcc.gov - FCC Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Key datasets (view IDs for fcc_view()):",
    "#   3xyp-aqkj = Consumer Complaints",
    "#   xqgr-24et = Pirate Radio Enforcement",
    "#   qb45-rw2t = CSRIC Best Practices",
    "#   hicn-aujz = Broadband Deployment Dec 2020",
    "#   dpq5-ta9j = 911 PSAP Registry",
    "#   acbv-jbb4 = Protected Earth Stations",
    "#   3b3k-34jp = EAS Equipment Grantees",
    "#   mjr7-dxdj = PRA Information Collections",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering"
  ))
}
