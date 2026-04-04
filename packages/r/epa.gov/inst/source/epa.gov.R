# epa.gov.R
# Self-contained EPA Envirofacts API client.
# Covers: TRI, FRS, NPDES/PCS, GHGRP, and SDWIS (Safe Drinking Water).
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www.epa.gov/enviro/envirofacts-data-service-api

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.epa_base <- "https://data.epa.gov/efservice"

.epa_query <- function(table, filters = list(), max_rows = 1000) {
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

  for (col in names(df)) {
    vals <- df[[col]]
    if (!is.character(vals)) next
    if (grepl("date", col)) {
      parsed <- suppressWarnings(as.Date(vals))
      if (sum(!is.na(parsed)) > sum(!is.na(vals)) * 0.5) { df[[col]] <- parsed; next }
    }
    if (grepl("latitude|longitude|amt|qty|quantity|amount|rate|num|count|year", col)) {
      parsed <- suppressWarnings(as.numeric(vals))
      if (sum(!is.na(parsed)) >= sum(vals != "" & !is.na(vals)) * 0.8) df[[col]] <- parsed
    }
  }
  df
}

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)


# == Schemas ===================================================================

.schema_sdwis_systems <- tibble(
  pwsid = character(), pws_name = character(), state_code = character(),
  pws_type_code = character(), primary_source_code = character(),
  population_served = integer(), service_connections = integer(),
  owner_type_code = character(), pws_activity_code = character()
)

.schema_sdwis_violations <- tibble(
  pwsid = character(), violation_id = character(), pws_name = character(),
  contaminant_code = character(), violation_type_code = character(),
  compliance_period_begin_date = character(),
  compliance_period_end_date = character(),
  state_code = character()
)


# == Core: generic table query =================================================

#' Query any EPA Envirofacts table
#'
#' Universal engine for the EPA Envirofacts Data Service API. Builds
#' the URL path from table name and filters, and auto-types columns
#' (dates parsed from date-named columns, numerics from quantity columns).
#'
#' @param table Character. Envirofacts table name (e.g. \code{"TRI_FACILITY"},
#'   \code{"FRS_FACILITY_SITE"}, \code{"PCS_PERMIT_FACILITY"},
#'   \code{"PUB_DIM_FACILITY"}). Table names are case-insensitive.
#' @param ... Named filter arguments. Name = column name (uppercased
#'   automatically), value = filter value. Examples:
#'   \code{state_abbr = "CA"}, \code{reporting_year = "2022"}.
#' @param max_rows Integer. Maximum rows to return (default 1000).
#'   Values above 1000 trigger paginated fetching.
#' @return A tibble with auto-typed, lowercase column names. Column set
#'   varies by table.
#' @examples
#' epa_get("TRI_FACILITY", state_abbr = "CA", max_rows = 5)
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
#' Returns facilities reporting to the EPA Toxic Release Inventory (TRI).
#' The TRI tracks toxic chemical releases and pollution prevention
#' activities reported by industrial and federal facilities.
#'
#' @param state Character. 2-letter state abbreviation (e.g. \code{"CA"},
#'   \code{"TX"}). Default \code{NULL} returns all states.
#' @param city Character. City name filter. Default \code{NULL}.
#' @param zip Character. ZIP code filter. Default \code{NULL}.
#' @param max_rows Integer. Max rows (default 1000).
#' @return A tibble with 48 columns (lowercase names), key columns include:
#'   \describe{
#'     \item{tri_facility_id}{character -- TRI facility identifier}
#'     \item{facility_name}{character -- facility name}
#'     \item{street_address}{character -- street address}
#'     \item{city_name}{character -- city}
#'     \item{county_name}{character -- county}
#'     \item{state_abbr}{character -- 2-letter state}
#'     \item{zip_code}{character -- ZIP code}
#'     \item{pref_latitude}{numeric -- latitude}
#'     \item{pref_longitude}{numeric -- longitude}
#'     \item{parent_co_name}{character -- parent company name}
#'   }
#' @examples
#' epa_tri_facility(state = "CA", max_rows = 5)
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
#' Chemical release data from TRI-reporting facilities. Each row represents
#' one chemical released by one facility in one year.
#'
#' @param state Character. 2-letter state abbreviation. Default \code{NULL}.
#' @param year Character. Reporting year (e.g. \code{"2022"}). Default \code{NULL}.
#' @param chemical Character. Chemical name filter (exact match). Default \code{NULL}.
#' @param max_rows Integer. Max rows (default 5000). Uses paginated fetching.
#' @return A tibble with lowercase column names. Key columns include
#'   \code{tri_facility_id}, \code{chemical_name}, \code{unit_of_measure},
#'   \code{total_releases} (numeric), \code{on_site_release_total} (numeric),
#'   \code{off_site_release_total} (numeric).
#' @examples
#' epa_tri_release(state = "CA", year = "2022", max_rows = 10)
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

#' EPA Facility Registry Service (FRS)
#'
#' Master list of EPA-regulated facilities across all programs, from the
#' Facility Registry Service. Includes facilities from TRI, NPDES, RCRA,
#' and other EPA programs.
#'
#' @param state Character. 2-letter state code (e.g. \code{"CA"}). Default \code{NULL}.
#' @param city Character. City name filter. Default \code{NULL}.
#' @param zip Character. ZIP code filter. Default \code{NULL}.
#' @param max_rows Integer. Max rows (default 1000).
#' @return A tibble with 52 columns (lowercase names), key columns include:
#'   \describe{
#'     \item{registry_id}{character -- EPA FRS registry ID}
#'     \item{primary_name}{character -- facility name}
#'     \item{city_name}{character -- city}
#'     \item{state_code}{character -- 2-letter state}
#'     \item{postal_code}{character -- ZIP code}
#'     \item{county_name}{character -- county name}
#'     \item{epa_region_code}{character -- EPA region (01--10)}
#'   }
#' @examples
#' epa_facility(state = "CA", max_rows = 5)
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
#' Facilities with water discharge permits under the Clean Water Act
#' (NPDES/PCS program). Each row represents one permitted facility.
#'
#' @param state Character. 2-letter state code (e.g. \code{"CA"}). Default \code{NULL}.
#' @param city Character. City name filter. Default \code{NULL}.
#' @param max_rows Integer. Max rows (default 1000).
#' @return A tibble with lowercase column names including permit and
#'   facility information. Column set varies by data availability.
#' @examples
#' epa_water_permit(state = "CA", max_rows = 5)
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
#' Facilities reporting greenhouse gas emissions under EPA's Greenhouse
#' Gas Reporting Program (GHGRP), which covers large emitters
#' (25,000+ metric tons CO2e/year).
#'
#' @param state Character. 2-letter state abbreviation (e.g. \code{"CA"}).
#'   Default \code{NULL} returns all states.
#' @param max_rows Integer. Max rows (default 1000).
#' @return A tibble with lowercase column names. Key columns include
#'   \code{facility_id}, \code{latitude} (numeric), \code{longitude} (numeric),
#'   \code{city}, \code{state}, \code{zip}, \code{county_fips}, \code{county}.
#' @examples
#' epa_ghg_facility(state = "CA", max_rows = 5)
#' @export
epa_ghg_facility <- function(state = NULL, max_rows = 1000) {
  filters <- list()
  if (!is.null(state)) filters <- c(filters, list(list(column = "STATE", value = state)))
  .epa_query("PUB_DIM_FACILITY", filters, max_rows)
}


# == SDWIS (Safe Drinking Water Information System) ============================

#' Fetch SDWIS public water systems by state
#'
#' Returns public water systems from the Safe Drinking Water Information
#' System (SDWIS). Each row represents one public water system.
#'
#' @param state Character. Two-letter state code (e.g. \code{"CA"}, \code{"NY"}).
#' @param rows Character. Row range in \code{"start:end"} format (default \code{"0:99"}).
#'   Increase the end value for more results (e.g. \code{"0:999"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pwsid}{character -- public water system ID}
#'     \item{pws_name}{character -- system name}
#'     \item{state_code}{character -- primacy agency / state code}
#'     \item{pws_type_code}{character -- system type}
#'     \item{primary_source_code}{character -- primary water source}
#'     \item{population_served}{integer -- population served}
#'     \item{service_connections}{integer -- number of service connections}
#'     \item{owner_type_code}{character -- owner type code}
#'     \item{pws_activity_code}{character -- activity status code}
#'   }
#' @examples
#' sdwis_systems("CA", rows = "0:9")
#' @export
sdwis_systems <- function(state, rows = "0:99") {
  url <- sprintf("%s/WATER_SYSTEM/STATE_CODE/%s/ROWS/%s/JSON",
                 .epa_base, toupper(state), rows)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_sdwis_systems)

  as_tibble(raw) |>
    transmute(
      pwsid               = as.character(pwsid),
      pws_name            = as.character(pws_name),
      state_code          = as.character(if ("primacy_agency_code" %in% names(raw)) primacy_agency_code else state_code),
      pws_type_code       = as.character(pws_type_code),
      primary_source_code = as.character(if ("primary_source_code" %in% names(raw)) primary_source_code else NA),
      population_served   = as.integer(if ("population_served_count" %in% names(raw)) population_served_count else NA),
      service_connections = as.integer(if ("service_connections_count" %in% names(raw)) service_connections_count else NA),
      owner_type_code     = as.character(if ("owner_type_code" %in% names(raw)) owner_type_code else NA),
      pws_activity_code   = as.character(if ("pws_activity_code" %in% names(raw)) pws_activity_code else NA)
    )
}

#' Fetch SDWIS drinking water violations by state
#'
#' Returns Safe Drinking Water Act violations from SDWIS, including
#' the water system, contaminant, and compliance period.
#'
#' @param state Character. Two-letter state code (e.g. \code{"CA"}, \code{"NY"}).
#' @param rows Character. Row range in \code{"start:end"} format (default \code{"0:99"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{pwsid}{character -- public water system ID}
#'     \item{violation_id}{character -- violation identifier}
#'     \item{pws_name}{character -- water system name}
#'     \item{contaminant_code}{character -- contaminant code}
#'     \item{violation_type_code}{character -- violation type}
#'     \item{compliance_period_begin_date}{character -- compliance period start}
#'     \item{compliance_period_end_date}{character -- compliance period end}
#'     \item{state_code}{character -- primacy agency / state code}
#'   }
#' @examples
#' sdwis_violations("CA", rows = "0:9")
#' @export
sdwis_violations <- function(state, rows = "0:99") {
  url <- sprintf("%s/VIOLATION/PRIMACY_AGENCY_CODE/%s/ROWS/%s/JSON",
                 .epa_base, toupper(state), rows)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_sdwis_violations)

  as_tibble(raw) |>
    transmute(
      pwsid                        = as.character(if ("pwsid" %in% names(raw)) pwsid else NA),
      violation_id                 = as.character(if ("violation_id" %in% names(raw)) violation_id else NA),
      pws_name                     = as.character(if ("pws_name" %in% names(raw)) pws_name else NA),
      contaminant_code             = as.character(if ("contaminant_code" %in% names(raw)) contaminant_code else NA),
      violation_type_code          = as.character(if ("violation_type_code" %in% names(raw)) violation_type_code else NA),
      compliance_period_begin_date = as.character(if ("compliance_period_begin_date" %in% names(raw)) compliance_period_begin_date else NA),
      compliance_period_end_date   = as.character(if ("compliance_period_end_date" %in% names(raw)) compliance_period_end_date else NA),
      state_code                   = as.character(if ("primacy_agency_code" %in% names(raw)) primacy_agency_code else NA)
    )
}


# == Context ===================================================================

#' Get epa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
epa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(epa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/epa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "epa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# epa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# epa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
