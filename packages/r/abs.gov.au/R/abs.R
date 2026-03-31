# == Data ======================================================================

#' Fetch ABS SDMX data
#'
#' @param dataflow Dataflow identifier (e.g. "CPI", "LF", "ERP").
#'   Use abs_dataflows() to discover available dataflows.
#' @param key SDMX key path (e.g. "1.10001.10.Q" or ".." for all).
#'   Default ".." returns all dimensions.
#' @param start Start period (e.g. "2023-Q1", "2023-01", "2023")
#' @param end End period
#' @param agency Agency ID (default "ABS")
#' @param version Dataflow version (default "1.0.0")
#' @return tibble: SDMX-CSV columns including DATAFLOW, TIME_PERIOD, OBS_VALUE
#' @export
abs_data <- function(dataflow, key = "..", start = NULL, end = NULL,
                     agency = "ABS", version = "1.0.0") {
  url <- paste0(.abs_base, "/data/", agency, ",", dataflow, ",", version, "/", key)
  params <- character()
  if (!is.null(start)) params <- c(params, paste0("startPeriod=", start))
  if (!is.null(end)) params <- c(params, paste0("endPeriod=", end))
  params <- c(params, "dimensionAtObservation=AllDimensions")
  if (length(params) > 0) url <- paste0(url, "?", paste(params, collapse = "&"))

  df <- .fetch_csv(url)
  if (nrow(df) == 0) return(.schema_data)

  if ("OBS_VALUE" %in% names(df)) {
    df$OBS_VALUE <- as.numeric(df$OBS_VALUE)
  }
  df
}

# == Dataflows =================================================================

#' List available ABS SDMX dataflows
#'
#' @return tibble: id, name, version, agencyID
#' @export
abs_dataflows <- function() {
  url <- paste0(.abs_base, "/dataflow/ABS")
  raw <- .fetch_json(url)

  dfs <- tryCatch({
    structures <- raw$data$dataflows %||% raw$`Dataflow`
    if (is.null(structures)) structures <- raw$data$dataStructures
    structures
  }, error = function(e) NULL)

  if (is.null(dfs) || length(dfs) == 0) return(.schema_dataflows)

  # Handle nested SDMX JSON structure
  if (is.data.frame(dfs)) {
    nm <- if ("name" %in% names(dfs)) {
      vapply(dfs$name, function(x) {
        if (is.list(x) || is.data.frame(x)) as.character(x[[1]])
        else as.character(x)
      }, character(1))
    } else NA_character_

    return(tibble(
      id = as.character(dfs$id %||% NA_character_),
      name = nm,
      version = as.character(dfs$version %||% NA_character_),
      agencyID = as.character(dfs$agencyID %||% NA_character_)
    ))
  }

  .schema_dataflows
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the ABS package
#'
#' @return Character string (invisibly), also printed
#' @export
abs_context <- function() {
  .build_context("abs.gov.au", header_lines = c(
    "# abs.gov.au - Australian Bureau of Statistics SDMX API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Uses SDMX-CSV format. Common dataflows:",
    "#   CPI (Consumer Price Index), LF (Labour Force),",
    "#   ERP (Estimated Resident Population), RT (Retail Trade)",
    "#",
    "# Key format: dimension values separated by dots.",
    "#   Use '..' to get all values, or specify specific dimensions."
  ))
}
