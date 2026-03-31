
#' Fetch SDWIS public water systems by state
#'
#' @param state Two-letter state code (e.g. "CA", "NY")
#' @param rows Row range "start:end" (default "0:99")
#' @return tibble: pwsid, pws_name, state_code, pws_type_code,
#'   primary_source_code, population_served, service_connections,
#'   owner_type_code, pws_activity_code
#' @export
sdwis_systems <- function(state, rows = "0:99") {
  url <- sprintf("%s/WATER_SYSTEM/STATE_CODE/%s/ROWS/%s/JSON",
                 .sdwis_base, toupper(state), rows)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_systems)

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
#' @param state Two-letter state code (e.g. "CA", "NY")
#' @param rows Row range "start:end" (default "0:99")
#' @return tibble: pwsid, violation_id, pws_name, contaminant_code,
#'   violation_type_code, compliance_period_begin_date,
#'   compliance_period_end_date, state_code
#' @export
sdwis_violations <- function(state, rows = "0:99") {
  url <- sprintf("%s/VIOLATION/PRIMACY_AGENCY_CODE/%s/ROWS/%s/JSON",
                 .sdwis_base, toupper(state), rows)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_violations)

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

#' EPA SDWIS context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
sdwis_context <- function() {
  .build_context("sdwis.epa.gov", header_lines = c(
    "# sdwis.epa.gov - EPA Safe Drinking Water Information System Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite",
    "#",
    "# State codes: CA, NY, TX, FL, etc. (standard 2-letter FIPS)",
    "# PWS types: CWS=Community, TNCWS=Transient Non-Community, NTNCWS=Non-Transient",
    "# Source codes: GW=Groundwater, SW=Surface Water, GU=Groundwater Under Influence"
  ))
}
