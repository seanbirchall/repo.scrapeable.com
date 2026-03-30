# epa-gov.R
# Self-contained EPA Envirofacts API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www.epa.gov/enviro/envirofacts-data-service-api

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.epa_base <- "https://data.epa.gov/efservice"

# -- Context generator ---------------------------------------------------------

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# -- Envirofacts query builder -------------------------------------------------
# Pattern: {base}/{table}/{col}/{op}/{value}/rows/{start}:{end}/JSON
# Operators: = (equals), != (not equals), < > (comparison), BEGINNING (starts with)

.epa_query <- function(table, filters = list(), max_rows = 1000) {
  # Build filter path segments
  filter_path <- ""
  for (f in filters) {
    if (!is.null(f$value))
      filter_path <- paste0(filter_path, "/", f$column, "/", f$op %||% "=", "/",
                            utils::URLencode(as.character(f$value)))
  }

  url <- paste0(.epa_base, "/", table, filter_path, "/rows/0:", max_rows - 1, "/JSON")

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  names(df) <- tolower(names(df))

  # Auto-type: dates, numbers
  for (col in names(df)) {
    vals <- df[[col]]
    if (!is.character(vals)) next
    # Date columns (contain "date" in name)
    if (grepl("date", col)) {
      parsed <- suppressWarnings(as.Date(vals))
      if (sum(!is.na(parsed)) > sum(!is.na(vals)) * 0.5) { df[[col]] <- parsed; next }
    }
    # Numeric columns (lat, lon, amounts, quantities)
    if (grepl("latitude|longitude|amt|qty|quantity|amount|rate|num|count|year", col)) {
      parsed <- suppressWarnings(as.numeric(vals))
      if (sum(!is.na(parsed)) >= sum(vals != "" & !is.na(vals)) * 0.8) df[[col]] <- parsed
    }
  }
  df
}

# -- Pagination for large queries ----------------------------------------------

.epa_fetch_all <- function(table, filters = list(), max_rows = 10000,
                           chunk_size = 1000) {
  all_data <- list()
  offset <- 0

  repeat {
    end <- offset + chunk_size - 1
    filter_path <- ""
    for (f in filters) {
      if (!is.null(f$value))
        filter_path <- paste0(filter_path, "/", f$column, "/", f$op %||% "=", "/",
                              utils::URLencode(as.character(f$value)))
    }

    url <- paste0(.epa_base, "/", table, filter_path,
                  "/rows/", offset, ":", end, "/JSON")

    raw <- tryCatch({
      tmp <- tempfile(fileext = ".json")
      httr2::request(url) |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_perform(path = tmp)
      jsonlite::fromJSON(tmp)
    }, error = function(e) NULL)

    if (is.null(raw) || length(raw) == 0) break

    df <- as_tibble(raw)
    names(df) <- tolower(names(df))
    all_data[[length(all_data) + 1]] <- df

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (nrow(df) < chunk_size || n_so_far >= max_rows) break
    offset <- offset + chunk_size

    if (length(all_data) %% 5 == 0)
      message(sprintf("  ...fetched %d rows", n_so_far))
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (nrow(result) > max_rows) result <- head(result, max_rows)
  result
}


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
epa_ghg_facility <- function(state = NULL, max_rows = 1000) {
  filters <- list()
  if (!is.null(state)) filters <- c(filters, list(list(column = "STATE", value = state)))
  .epa_query("PUB_DIM_FACILITY", filters, max_rows)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the epa.gov package
#'
#' @return Character string (invisibly), also printed
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
