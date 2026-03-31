# == Data ======================================================================

#' Fetch OECD data as CSV
#'
#' Queries the OECD SDMX API and returns data in CSV format.
#' The key parameter specifies dimension filters separated by dots.
#'
#' @param agency Agency ID (e.g. "OECD.SDD.NAD")
#' @param dataflow Dataflow ID (e.g. "DSD_NAAG@DF_NAAG")
#' @param key Dimension key with dots separating dimensions.
#'   Use empty string for wildcard. Example for NAAG (5 dims):
#'   "A.USA...." = Annual, USA, all other dims wild.
#' @param start_period Start year (e.g. "2020")
#' @param end_period End year (e.g. "2023")
#' @param first_n Limit to first N observations per series
#' @return tibble with SDMX columns (varies by dataflow)
#' @export
oecd_data <- function(agency, dataflow, key = "",
                      start_period = NULL, end_period = NULL,
                      first_n = NULL) {
  url <- sprintf("%s/data/%s,%s,/%s",
                 .oecd_base, agency, dataflow, key)

  params <- list()
  if (!is.null(start_period)) params$startPeriod <- start_period
  if (!is.null(end_period))   params$endPeriod   <- end_period
  if (!is.null(first_n))      params$firstNObservations <- first_n

  if (length(params) > 0) {
    query <- paste(names(params), unlist(params), sep = "=", collapse = "&")
    url <- paste0(url, "?", query)
  }

  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("OECD API error: ", e$message)
    return(NULL)
  })
  if (is.null(df) || nrow(df) == 0) return(.schema_data)

  # Convert OBS_VALUE to numeric
  if ("OBS_VALUE" %in% names(df)) {
    df$OBS_VALUE <- as.numeric(df$OBS_VALUE)
  }

  as_tibble(df)
}


#' Fetch OECD data using a popular dataflow shortname
#'
#' Convenience wrapper using predefined popular dataflows.
#'
#' @param name Short name: "NAAG", "CLI", "CPI", "ULC", "STLABOUR",
#'   "SNA_TABLE1", "MEI", "QNA". Use oecd_dataflows_popular() to see all.
#' @param key Dimension key (default: all)
#' @param start_period Start year
#' @param end_period End year
#' @return tibble with SDMX columns
#' @export
oecd_fetch <- function(name, key = "", start_period = NULL, end_period = NULL) {
  match <- .oecd_popular |> filter(short_name == toupper(name))
  if (nrow(match) == 0) {
    stop("Unknown dataflow '", name, "'. Use oecd_dataflows_popular() to see options.")
  }
  oecd_data(match$agency[1], match$dataflow[1], key = key,
            start_period = start_period, end_period = end_period)
}


# == Dataflows =================================================================

#' List popular OECD dataflows
#'
#' Returns a curated list of commonly used OECD dataflows.
#'
#' @return tibble: short_name, agency, dataflow, description
#' @export
oecd_dataflows_popular <- function() {
  .oecd_popular
}


# == Context ===================================================================

#' Generate LLM-friendly context for the OECD package
#'
#' @return Character string (invisibly), also printed
#' @export
oecd_context <- function() {
  .build_context("data.oecd.org", header_lines = c(
    "# data.oecd.org - OECD SDMX Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# 1,481 dataflows across OECD departments",
    "# Data returned as CSV with SDMX columns",
    "#",
    "# Popular dataflows: NAAG (GDP), CLI (Leading Indicators),",
    "#   CPI (Consumer Prices), ULC (Unit Labour Costs),",
    "#   STLABOUR (Labour Force), MEI (Main Economic Indicators)",
    "#",
    "# Key dimension: dot-separated, empty = wildcard",
    "# Example: 'A.USA....' = Annual, USA, all others wild"
  ))
}
