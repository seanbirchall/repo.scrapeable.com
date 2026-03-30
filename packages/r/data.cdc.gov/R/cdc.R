# == Dataset discovery =========================================================

#' Search CDC datasets by keyword
#'
#' Queries the Socrata catalog for datasets matching a search term.
#'
#' @param query Search keyword (e.g. "covid", "mortality", "vaccination")
#' @param max_results Max datasets to return (default 20)
#' @return tibble: id, name, description, type, updated_at, columns
#' @export
cdc_search <- function(query, max_results = 20) {
  url <- sprintf("%s/api/catalog/v1?q=%s&limit=%d&domains=data.cdc.gov&search_context=data.cdc.gov",
                 .cdc_base, utils::URLencode(query), max_results)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0)
    return(tibble(id = character(), name = character(), description = character()))

  bind_rows(lapply(results, function(r) {
    res <- r$resource
    tibble(
      id          = res$id %||% NA_character_,
      name        = res$name %||% NA_character_,
      description = substr(res$description %||% "", 1, 200),
      type        = res$type %||% NA_character_,
      updated_at  = res$data_updated_at %||% NA_character_,
      columns     = paste(res$columns_field_name %||% character(), collapse = ", ")
    )
  })) |> filter(type == "dataset")
}

#' Get column metadata for a CDC dataset
#'
#' @param dataset_id Socrata dataset ID (e.g. "vbim-akqf")
#' @return tibble: field_name, name, datatype, description
#' @export
cdc_columns <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .cdc_base, dataset_id)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  cols <- raw$columns
  if (is.null(cols) || length(cols) == 0)
    return(tibble(field_name = character(), name = character(), datatype = character()))

  bind_rows(lapply(cols, function(c) {
    tibble(
      field_name  = c$fieldName %||% NA_character_,
      name        = c$name %||% NA_character_,
      datatype    = c$dataTypeName %||% NA_character_,
      description = c$description %||% NA_character_
    )
  }))
}


# == Core data fetching ========================================================

#' Fetch data from any CDC Socrata dataset
#'
#' Uses SoQL (Socrata Query Language) for filtering and aggregation.
#'
#' @param dataset_id Socrata dataset ID (e.g. "vbim-akqf" for COVID cases)
#' @param where SoQL WHERE clause (e.g. "sex='Female' AND age_group='50-59 Years'")
#' @param select SoQL SELECT clause (e.g. "sex, count(*) as n")
#' @param group SoQL GROUP BY clause (e.g. "sex")
#' @param order SoQL ORDER BY clause (e.g. "count DESC")
#' @param token Optional Socrata app token
#' @param max_results Max rows (default 1000)
#' @return tibble with all columns as character (Socrata returns strings)
#' @export
cdc_get <- function(dataset_id, where = NULL, select = NULL,
                    group = NULL, order = NULL, token = NULL,
                    max_results = 1000) {
  .cdc_get(dataset_id, where, select, group, order,
           limit = max_results, token = token, max_results = max_results)
}


# == Convenience: COVID-19 =====================================================

#' COVID-19 case surveillance data
#'
#' Individual-level case records from CDC case surveillance.
#'
#' @param state State name filter (e.g. "California")
#' @param sex Sex filter: "Male", "Female"
#' @param age_group Age filter: "0 - 9 Years", "10 - 19 Years", etc.
#' @param where Additional SoQL WHERE clause
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of COVID-19 case records
#' @export
cdc_covid_cases <- function(state = NULL, sex = NULL, age_group = NULL,
                            where = NULL, token = NULL, max_results = 1000) {
  parts <- character()
  if (!is.null(state))     parts <- c(parts, sprintf("res_state='%s'", state))
  if (!is.null(sex))       parts <- c(parts, sprintf("sex='%s'", sex))
  if (!is.null(age_group)) parts <- c(parts, sprintf("age_group='%s'", age_group))
  if (!is.null(where))     parts <- c(parts, where)
  where_clause <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("vbim-akqf", where = where_clause, token = token, max_results = max_results)
}

#' COVID-19 vaccination data by jurisdiction
#'
#' @param location State/territory abbreviation (e.g. "CA", "NY")
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of vaccination counts by date and jurisdiction
#' @export
cdc_covid_vaccinations <- function(location = NULL, token = NULL,
                                   max_results = 1000) {
  where <- if (!is.null(location)) sprintf("location='%s'", location) else NULL
  cdc_get("unsk-b7fc", where = where, order = "date DESC", token = token,
          max_results = max_results)
}


# == Convenience: Notifiable diseases ==========================================

#' NNDSS (National Notifiable Diseases Surveillance System) weekly data
#'
#' @param disease Disease label filter (partial match)
#' @param year Year filter
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of weekly disease counts by state
#' @export
cdc_nndss <- function(disease = NULL, year = NULL, token = NULL,
                      max_results = 1000) {
  parts <- character()
  if (!is.null(disease)) parts <- c(parts, sprintf("label like '%%%s%%'", disease))
  if (!is.null(year))    parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("x9gk-5huc", where = where, token = token, max_results = max_results)
}


# == Convenience: Mortality ====================================================

#' Provisional mortality counts (weekly, by cause and jurisdiction)
#'
#' @param jurisdiction State name or "United States"
#' @param cause Cause of death filter (partial match)
#' @param year Year filter
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of weekly mortality counts
#' @export
cdc_mortality <- function(jurisdiction = NULL, cause = NULL, year = NULL,
                          token = NULL, max_results = 1000) {
  parts <- character()
  if (!is.null(jurisdiction)) parts <- c(parts, sprintf("jurisdiction_of_occurrence='%s'", jurisdiction))
  if (!is.null(cause)) parts <- c(parts, sprintf("all_cause IS NOT NULL"))
  if (!is.null(year))         parts <- c(parts, sprintf("mmwryear='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("muzy-jte6", where = where, order = "week_ending_date DESC",
          token = token, max_results = max_results)
}


# == Convenience: Flu surveillance =============================================

#' ILINet (Influenza-Like Illness Surveillance) data
#'
#' @param region Region filter
#' @param year Year filter
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of weekly ILI rates by region
#' @export
cdc_flu <- function(region = NULL, year = NULL, token = NULL,
                    max_results = 1000) {
  parts <- character()
  if (!is.null(region)) parts <- c(parts, sprintf("region like '%%%s%%'", region))
  if (!is.null(year))   parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("ite7-j2w7", where = where, token = token, max_results = max_results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the data.cdc.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
cdc_context <- function() {
  .build_context("data.cdc.gov", header_lines = c(
    "# data.cdc.gov - CDC Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: optional Socrata app token (reduces throttling)",
    "# All functions return tibbles. Uses SoQL query language.",
    "#",
    "# Key dataset IDs:",
    "#   vbim-akqf = COVID-19 Case Surveillance",
    "#   unsk-b7fc = COVID-19 Vaccinations by Jurisdiction",
    "#   x9gk-5huc = NNDSS Weekly Disease Tables",
    "#   muzy-jte6 = Provisional Mortality Counts",
    "#   ite7-j2w7 = ILINet Flu Surveillance",
    "#",
    "# SoQL syntax (use in `where` param):",
    "#   field='value'           exact match",
    "#   field like '%pattern%'  partial match",
    "#   field > '2024-01-01'    comparison",
    "#   field IS NOT NULL       null check",
    "#   Use cdc_search('keyword') to discover datasets.",
    "#   Use cdc_columns('dataset-id') to see available fields."
  ))
}
