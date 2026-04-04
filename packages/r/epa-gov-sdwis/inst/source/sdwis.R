# epa-gov-sdwis.R
# Self-contained EPA SDWIS (Safe Drinking Water Information System) client.
# Uses the Envirofacts Data Service API (different from epa.gov eGRID).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sdwis_base <- "https://data.epa.gov/efservice"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
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

.schema_systems <- tibble(
  pwsid = character(), pws_name = character(), state_code = character(),
  pws_type_code = character(), primary_source_code = character(),
  population_served = integer(), service_connections = integer(),
  owner_type_code = character(), pws_activity_code = character()
)

.schema_violations <- tibble(
  pwsid = character(), violation_id = character(), pws_name = character(),
  contaminant_code = character(), violation_type_code = character(),
  compliance_period_begin_date = character(),
  compliance_period_end_date = character(),
  state_code = character()
)

# == Water systems =============================================================

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
