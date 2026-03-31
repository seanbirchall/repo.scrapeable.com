
#' Search GLEIF LEI records by legal entity name
#'
#' @param name Legal name to search (e.g. "Google", "Apple Inc")
#' @param size Results per page (default 20, max 200)
#' @param page Page number (default 1)
#' @return tibble: lei, legal_name, status, jurisdiction, country, city,
#'   address, category, registration_date, last_update
#' @export
lei_search <- function(name, size = 20, page = 1) {
  url <- sprintf(
    "%s/lei-records?filter%%5Bentity.legalName%%5D=%s&page%%5Bsize%%5D=%d&page%%5Bnumber%%5D=%d",
    .lei_base, utils::URLencode(name), as.integer(size), as.integer(page)
  )
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_lei)
  bind_rows(lapply(d, .parse_lei_record))
}

#' Fetch a single LEI record by LEI code
#'
#' @param lei The 20-character LEI code
#' @return tibble: one row with lei, legal_name, status, jurisdiction, country,
#'   city, address, category, registration_date, last_update
#' @export
lei_record <- function(lei) {
  url <- paste0(.lei_base, "/lei-records/", lei)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d)) return(.schema_lei)
  .parse_lei_record(d)
}

#' GLEIF LEI context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
lei_context <- function() {
  .build_context("gleif.org", header_lines = c(
    "# gleif.org - GLEIF Legal Entity Identifier (LEI) Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite",
    "#",
    "# LEI codes are 20-character alphanumeric identifiers.",
    "# Example: 5493003SBER0QP75NP15 (Google LLC)"
  ))
}
