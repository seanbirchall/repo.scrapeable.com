# == Dataset discovery =========================================================

#' List all CMS provider data datasets
#'
#' Returns metadata for all ~233 datasets in the CMS provider data catalog.
#'
#' @param max_results Max datasets to return (default: all)
#' @return tibble: identifier (dataset ID), title, description, modified
#' @export
cms_datasets <- function(max_results = NULL) {
  url <- paste0(.cms_base, "/metastore/schemas/dataset/items?show-reference-ids")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  if (length(raw) == 0) return(tibble(identifier = character(), title = character()))

  df <- bind_rows(lapply(raw, function(d) {
    tibble(
      identifier  = d$identifier %||% NA_character_,
      title       = d$title %||% NA_character_,
      description = d$description %||% NA_character_,
      modified    = d$modified %||% NA_character_,
      theme       = paste(d$theme %||% character(), collapse = "; "),
      keyword     = paste(head(d$keyword %||% character(), 5), collapse = "; ")
    )
  }))

  df$modified <- suppressWarnings(as.Date(df$modified))
  if (!is.null(max_results)) df <- head(df, max_results)
  df |> arrange(desc(modified))
}

#' Search CMS datasets by keyword
#'
#' @param query Search term (case-insensitive, matches title and description)
#' @return tibble: identifier, title, description, modified
#' @export
cms_search <- function(query) {
  ds <- cms_datasets()
  ds |> filter(
    grepl(query, title, ignore.case = TRUE) |
    grepl(query, description, ignore.case = TRUE) |
    grepl(query, keyword, ignore.case = TRUE)
  )
}


# == Core data fetching ========================================================

#' Fetch data from any CMS provider dataset
#'
#' @param dataset_id CMS dataset identifier (e.g. "xubh-q36u" for Hospital
#'   General Information). Use cms_datasets() or cms_search() to find IDs.
#' @param ... Named filter arguments. Name = column name, value = filter value.
#'   e.g. state = "CA", hospital_type = "Acute Care Hospitals"
#' @param max_results Max rows to return (default 1000)
#' @return tibble with all columns from the dataset
#' @export
cms_get <- function(dataset_id, ..., max_results = 1000) {
  args <- list(...)
  conditions <- lapply(names(args), function(nm) {
    list(property = nm, value = as.character(args[[nm]]))
  })
  .cms_query(dataset_id, conditions, max_results = max_results)
}


# == Convenience: Hospital data ================================================

#' Hospital general information
#'
#' CMS Hospital Compare general information including ratings.
#'
#' @param state 2-letter state code filter
#' @param max_results Max results (default 1000)
#' @return tibble: facility_id, facility_name, address, city, state,
#'   hospital_type, hospital_ownership, emergency_services,
#'   hospital_overall_rating, ...
#' @export
cms_hospitals <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("xubh-q36u", conditions, max_results = max_results)
}

#' Hospital quality measures - timely and effective care
#'
#' @param state 2-letter state code filter
#' @param measure_id Specific measure ID filter
#' @param max_results Max results (default 1000)
#' @return tibble with quality measure scores by hospital
#' @export
cms_hospital_quality <- function(state = NULL, measure_id = NULL,
                                 max_results = 1000) {
  conditions <- list()
  if (!is.null(state))      conditions <- c(conditions, list(list(property = "state", value = state)))
  if (!is.null(measure_id)) conditions <- c(conditions, list(list(property = "measure_id", value = measure_id)))
  .cms_query("yv7e-xc69", conditions, max_results = max_results)
}


# == Convenience: Nursing homes ================================================

#' Nursing home (skilled nursing facility) information
#'
#' @param state 2-letter state code
#' @param max_results Max results (default 1000)
#' @return tibble with facility info, ratings, staffing data
#' @export
cms_nursing_homes <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("4pq5-n9py", conditions, max_results = max_results)
}


# == Convenience: Dialysis facilities ==========================================

#' Dialysis facility information
#'
#' @param state 2-letter state code
#' @param max_results Max results (default 1000)
#' @return tibble with facility info and quality measures
#' @export
cms_dialysis <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("23ew-n7w9", conditions, max_results = max_results)
}


# == Convenience: Home health agencies =========================================

#' Home health agency information
#'
#' @param state 2-letter state code
#' @param max_results Max results (default 1000)
#' @return tibble with agency info and quality ratings
#' @export
cms_home_health <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("6jpm-sxkc", conditions, max_results = max_results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the data.cms.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
cms_context <- function() {
  .build_context("data.cms.gov", header_lines = c(
    "# data.cms.gov - CMS Provider Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API)",
    "# All functions return tibbles.",
    "#",
    "# Key dataset IDs:",
    "#   xubh-q36u = Hospital General Information (5,400+ hospitals)",
    "#   yv7e-xc69 = Hospital Quality Measures",
    "#   4pq5-n9py = Nursing Home Provider Information",
    "#   23ew-n7w9 = Dialysis Facility Listing",
    "#   6jpm-sxkc = Home Health Agency Information",
    "#",
    "# Use cms_datasets() or cms_search('keyword') to discover all 233 datasets.",
    "# Use cms_get('dataset-id', column = 'value') to query any dataset."
  ))
}
