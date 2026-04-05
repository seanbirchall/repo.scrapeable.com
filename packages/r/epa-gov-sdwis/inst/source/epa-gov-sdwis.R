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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Queries the EPA Envirofacts SDWIS (Safe Drinking Water Information System)
#' database for public water systems in a given state. Returns system IDs,
#' names, type codes, source water type, and population served.
#'
#' @param state Two-letter state abbreviation (e.g. "CA", "NY", "TX").
#'   Case-insensitive -- converted to uppercase internally.
#' @param rows Row range as "start:end" string (default "0:99"). The API
#'   uses zero-based indexing. Use "0:999" for up to 1000 results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{pwsid}{Public water system ID (unique identifier)}
#'     \item{pws_name}{Water system name}
#'     \item{state_code}{State/primacy agency code}
#'     \item{pws_type_code}{System type: "CWS" (community), "TNCWS" (transient non-community), "NTNCWS" (non-transient non-community)}
#'     \item{primary_source_code}{Primary water source: "GW" (groundwater), "SW" (surface water), "GWP" (groundwater under influence of surface water)}
#'     \item{population_served}{Estimated population served (integer)}
#'     \item{service_connections}{Number of service connections (integer)}
#'     \item{owner_type_code}{Owner type code}
#'     \item{pws_activity_code}{Activity status code}
#'   }
#' @examples
#' sdwis_systems("CA")
#' sdwis_systems("NY", rows = "0:9")
#' @seealso [sdwis_violations()], [sdwis_context()]
#' @source <https://data.epa.gov/efservice>
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
#' Queries the EPA Envirofacts SDWIS database for drinking water rule
#' violations in a given state. Returns violation details including the
#' contaminant involved, violation type, and compliance period dates.
#'
#' @param state Two-letter state abbreviation (e.g. "CA", "NY", "TX").
#'   Case-insensitive.
#' @param rows Row range as "start:end" string (default "0:99"). Use
#'   "0:999" for up to 1000 results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{pwsid}{Public water system ID that received the violation}
#'     \item{violation_id}{Unique violation identifier}
#'     \item{pws_name}{Water system name}
#'     \item{contaminant_code}{EPA contaminant code}
#'     \item{violation_type_code}{Type of violation (e.g. MCL, TT, MR)}
#'     \item{compliance_period_begin_date}{Start of compliance period (character)}
#'     \item{compliance_period_end_date}{End of compliance period (character)}
#'     \item{state_code}{State/primacy agency code}
#'   }
#' @examples
#' sdwis_violations("CA")
#' sdwis_violations("NY", rows = "0:9")
#' @seealso [sdwis_systems()], [sdwis_context()]
#' @source <https://data.epa.gov/efservice>
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

#' Get epa-gov-sdwis client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the EPA SDWIS client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' sdwis_context()
#' @export
sdwis_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sdwis_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/epa-gov-sdwis.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "epa-gov-sdwis")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# epa-gov-sdwis context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# epa-gov-sdwis", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
