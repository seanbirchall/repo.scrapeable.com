# abs.R
# Self-contained Australian Bureau of Statistics (ABS) SDMX API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful
# Format: SDMX-CSV

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.abs_base <- "https://data.api.abs.gov.au/rest"

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
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/vnd.sdmx.data+csv;file=true") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  f <- .fetch(url, ext = ".csv")
  tryCatch(
    utils::read.csv(f, stringsAsFactors = FALSE) |> as_tibble(),
    error = function(e) tibble()
  )
}

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

# == Schemas ===================================================================

.schema_data <- tibble(
  DATAFLOW = character(), FREQ = character(), TIME_PERIOD = character(),
  OBS_VALUE = numeric()
)

.schema_dataflows <- tibble(
  id = character(), name = character(), version = character(),
  agencyID = character()
)

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
