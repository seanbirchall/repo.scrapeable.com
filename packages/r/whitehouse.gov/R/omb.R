# == Public functions ==========================================================

#' List available OMB historical budget tables
#'
#' Returns a catalog of the 24 most commonly used historical budget tables.
#' These cover budget totals, receipts, outlays, budget authority, debt,
#' and economic indicators from 1789 to present.
#'
#' @return tibble: table_id (character), title (character), section (character)
#' @export
omb_tables <- function() {
  .omb_tables
}


#' Fetch an OMB historical budget table
#'
#' Downloads and parses a specific table from the OMB Historical Tables XLSX.
#' Tables in "tall" format return fiscal_year + named value columns.
#' Tables in "wide" format return category + fiscal_year + value (long form).
#'
#' @param table_id Table identifier (e.g. "1.1", "3.2", "7.1").
#'   Use omb_tables() to see available tables.
#' @param url Optional URL to the HIST.xlsx file (default: FY2026 budget)
#' @return tibble with fiscal_year and table data
#' @export
omb_table <- function(table_id, url = NULL) {
  if (is.null(url)) url <- .omb_hist_url

  sheet <- .table_to_sheet(table_id)
  xlsx_path <- .fetch(url)
  result <- .parse_hist_sheet(xlsx_path, sheet, table_id)

  if (is.null(result) || nrow(result) == 0) {
    warning("No data parsed from table ", table_id)
    return(.schema_wide)
  }
  result
}


#' Fetch budget summary (Table 1.1)
#'
#' Returns receipts, outlays, and surplus/deficit from 1789 to present.
#' Values in millions of dollars.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, receipts, outlays, surplus_deficit, table_id
#' @export
omb_summary <- function(url = NULL) {
  omb_table("1.1", url = url)
}


#' Fetch budget as percentage of GDP (Table 1.2)
#'
#' Returns receipts, outlays, and surplus/deficit as percentages of GDP.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, receipts_pct_gdp, outlays_pct_gdp,
#'   surplus_deficit_pct_gdp, table_id
#' @export
omb_gdp_pct <- function(url = NULL) {
  omb_table("1.2", url = url)
}


#' Fetch outlays by agency (Table 4.1)
#'
#' Returns federal outlays by department/agency in long format.
#' Values in millions of dollars.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: category (agency name), fiscal_year, value, table_id
#' @export
omb_outlays_by_agency <- function(url = NULL) {
  omb_table("4.1", url = url)
}


#' Fetch federal debt (Table 7.1)
#'
#' Returns federal debt at end of year: gross, held by public,
#' held by government accounts. Values in millions of dollars.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, debt columns, table_id
#' @export
omb_debt <- function(url = NULL) {
  omb_table("7.1", url = url)
}


#' Fetch GDP and economic indicators (Table 10.1)
#'
#' Returns GDP, deflators, population, and other economic series.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, economic indicator columns, table_id
#' @export
omb_gdp <- function(url = NULL) {
  omb_table("10.1", url = url)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the OMB package
#'
#' @return Character string (invisibly), also printed
#' @export
omb_context <- function() {
  .build_context("whitehouse.gov", header_lines = c(
    "# whitehouse.gov - OMB Historical Budget Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr, readxl",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Source: OMB Historical Tables XLSX (48 tables, 1789-present)",
    "# Values in millions of dollars unless noted as percentages.",
    "#",
    "# Key tables:",
    "#   1.1 = Budget summary (receipts, outlays, surplus/deficit)",
    "#   1.2 = Budget as % of GDP",
    "#   3.2 = Outlays by function/subfunction",
    "#   4.1 = Outlays by agency",
    "#   7.1 = Federal debt",
    "#   10.1 = GDP and economic indicators"
  ))
}
