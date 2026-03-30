# == Core: generic table query =================================================

#' Query any EPA Envirofacts table
#'
#' The universal engine. Builds the URL path from table name and filters.
#'
#' @param table Envirofacts table name (e.g. "TRI_FACILITY", "FRS_FACILITY_SITE")
#' @param ... Named filter arguments. Each should be a value to match.
#'   Name = column name (case-insensitive, will be uppercased).
#'   e.g. state_abbr = "CA", reporting_year = "2022"
#' @param max_rows Maximum rows to return (default 1000)
#' @return tibble with auto-typed columns (lowercase names)
#' @export
epa_get <- function(table, ..., max_rows = 1000) {
  args <- list(...)
  filters <- lapply(names(args), function(nm) {
    list(column = toupper(nm), op = "=", value = args[[nm]])
  })
  if (max_rows > 1000) {
    .epa_fetch_all(table, filters, max_rows)
  } else {
    .epa_query(table, filters, max_rows)
  }
}


# == TRI (Toxic Release Inventory) =============================================

#' TRI facility information
#'
#' Facilities reporting to the Toxic Release Inventory.
#'
#' @param state 2-letter state abbreviation (e.g. "CA", "TX")
#' @param city City name filter
#' @param zip ZIP code filter
#' @param max_rows Max rows (default 1000)
#' @return tibble: tri_facility_id, facility_name, street_address, city_name,
#'   county_name, state_abbr, zip_code, latitude, longitude, ...
#' @export
epa_tri_facility <- function(state = NULL, city = NULL, zip = NULL,
                             max_rows = 1000) {
  filters <- list()
  if (!is.null(state)) filters <- c(filters, list(list(column = "STATE_ABBR", value = state)))
  if (!is.null(city))  filters <- c(filters, list(list(column = "CITY_NAME", value = city)))
  if (!is.null(zip))   filters <- c(filters, list(list(column = "ZIP_CODE", value = zip)))
  .epa_query("TRI_FACILITY", filters, max_rows)
}

#' TRI chemical release quantities
#'
#' Chemical release data from TRI-reporting facilities.
#'
#' @param state 2-letter state abbreviation
#' @param year Reporting year (e.g. "2022")
#' @param chemical Chemical name filter
#' @param max_rows Max rows (default 5000)
#' @return tibble: tri_facility_id, chemical_name, unit_of_measure,
#'   total_releases, on_site_release_total, off_site_release_total, ...
#' @export
epa_tri_release <- function(state = NULL, year = NULL, chemical = NULL,
                            max_rows = 5000) {
  filters <- list()
  if (!is.null(state))    filters <- c(filters, list(list(column = "STATE_ABBR", value = state)))
  if (!is.null(year))     filters <- c(filters, list(list(column = "REPORTING_YEAR", value = year)))
  if (!is.null(chemical)) filters <- c(filters, list(list(column = "CHEMICAL_NAME", value = chemical)))
  .epa_fetch_all("TRI_RELEASE_QTY", filters, max_rows)
}


# == FRS (Facility Registry Service) ===========================================

#' EPA facility registry
#'
#' Master list of EPA-regulated facilities across all programs.
#'
#' @param state 2-letter state code
#' @param city City name filter
#' @param zip ZIP code filter
#' @param max_rows Max rows (default 1000)
#' @return tibble: registry_id, primary_name, city_name, state_code,
#'   postal_code, latitude, longitude, ...
#' @export
epa_facility <- function(state = NULL, city = NULL, zip = NULL,
                         max_rows = 1000) {
  filters <- list()
  if (!is.null(state)) filters <- c(filters, list(list(column = "STATE_CODE", value = state)))
  if (!is.null(city))  filters <- c(filters, list(list(column = "CITY_NAME", value = city)))
  if (!is.null(zip))   filters <- c(filters, list(list(column = "POSTAL_CODE", value = zip)))
  .epa_query("FRS_FACILITY_SITE", filters, max_rows)
}


# == Water (PCS/NPDES) =========================================================

#' Water discharge permits (PCS/NPDES)
#'
#' Facilities with water discharge permits under the Clean Water Act.
#'
#' @param state 2-letter state code
#' @param city City name filter
#' @param max_rows Max rows (default 1000)
#' @return tibble with permit and facility information
#' @export
epa_water_permit <- function(state = NULL, city = NULL, max_rows = 1000) {
  filters <- list()
  if (!is.null(state)) filters <- c(filters, list(list(column = "STATE_CODE", value = state)))
  if (!is.null(city))  filters <- c(filters, list(list(column = "CITY", value = city)))
  .epa_query("PCS_PERMIT_FACILITY", filters, max_rows)
}


# == GHG (Greenhouse Gas) =====================================================

#' Greenhouse gas reporting facilities (GHGRP)
#'
#' Facilities reporting greenhouse gas emissions under EPA's GHGRP.
#'
#' @param state 2-letter state abbreviation
#' @param max_rows Max rows (default 1000)
#' @return tibble: facility_id, latitude, longitude, city, state, zip,
#'   county_fips, county, ...
#' @export
epa_ghg_facility <- function(state = NULL, max_rows = 1000) {
  filters <- list()
  if (!is.null(state)) filters <- c(filters, list(list(column = "STATE", value = state)))
  .epa_query("PUB_DIM_FACILITY", filters, max_rows)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the epa.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
epa_context <- function() {
  .build_context("epa.gov", header_lines = c(
    "# epa.gov - EPA Envirofacts API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API)",
    "# All functions return tibbles with auto-typed columns.",
    "#",
    "# Key tables:",
    "#   TRI_FACILITY       = Toxic Release Inventory facilities",
    "#   TRI_RELEASE_QTY    = Chemical release quantities",
    "#   FRS_FACILITY_SITE  = Master facility registry",
    "#   PCS_PERMIT_FACILITY = Water discharge permits",
    "#   PUB_DIM_FACILITY   = Greenhouse gas reporting facilities",
    "#",
    "# Query syntax: epa_get('TABLE', column = 'value', max_rows = N)",
    "# Filters are case-insensitive column names matched with ="
  ))
}
