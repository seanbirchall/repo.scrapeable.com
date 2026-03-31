#' Fetch FOIA agency components
#'
#' Returns all federal agency components registered with FOIA.gov.
#'
#' @param api_key API key for api.data.gov. DEMO_KEY works for testing.
#'   Register free at https://api.data.gov/signup/
#' @return tibble: id, agency_name, title, abbreviation, website
#' @export
foia_agencies <- function(api_key = NULL) {
  api_key <- api_key %||% "DEMO_KEY"
  url <- sprintf("%s/agency_components?api_key=%s", .foia_base, api_key)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_components)

  rows <- lapply(d, function(x) {
    tibble(
      id = as.integer(x$id %||% NA),
      agency_name = as.character(x$agency$name %||% NA),
      title = as.character(x$title %||% NA),
      abbreviation = as.character(x$abbreviation %||% NA),
      website = as.character(x$website %||% NA)
    )
  })
  bind_rows(rows)
}

# == FOIA annual report data ===================================================

#' Fetch FOIA annual report request data
#'
#' Retrieves FOIA request statistics from annual reports for a specific
#' agency component.
#'
#' @param agency_abbreviation Agency abbreviation (e.g. "DOJ", "DHS", "EPA")
#' @param year Fiscal year (e.g. 2023)
#' @param api_key API key for api.data.gov. DEMO_KEY works for testing.
#' @return tibble: agency, year, received, processed, pending_start, pending_end
#' @export
foia_requests <- function(agency_abbreviation, year = 2023, api_key = NULL) {
  api_key <- api_key %||% "DEMO_KEY"
  url <- sprintf(
    "%s/annual_foia_report/request/%s/%d?api_key=%s",
    .foia_base, agency_abbreviation, as.integer(year), api_key
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_requests)

  # The response structure varies; try to extract top-level stats

  tibble(
    agency = as.character(agency_abbreviation),
    year = as.integer(year),
    received = as.integer(raw$request_received %||% NA),
    processed = as.integer(raw$request_processed %||% NA),
    pending_start = as.integer(raw$pending_start_of_year %||% NA),
    pending_end = as.integer(raw$pending_end_of_year %||% NA)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the foia.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
foia_context <- function() {
  .build_context("foia.gov", header_lines = c(
    "# foia.gov - Freedom of Information Act API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: api_key param (DEMO_KEY works for testing, register at https://api.data.gov/signup/)",
    "# Rate limit: DEMO_KEY 30/hr, registered 1000/hr",
    "# All functions return tibbles with typed columns."
  ))
}
