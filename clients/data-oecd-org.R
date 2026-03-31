# data-oecd-org.R
# Self-contained OECD SDMX data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: SDMX REST at sdmx.oecd.org (CSV format for data)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.oecd_base <- "https://sdmx.oecd.org/public/rest"

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = .ua,
      Accept = "application/vnd.sdmx.data+csv"
    ) |>
    httr2::req_perform(path = tmp)
  utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_data <- tibble(
  ref_area = character(), measure = character(),
  time_period = character(), obs_value = numeric()
)

.schema_dataflows <- tibble(
  agency = character(), id = character(), version = character(),
  name = character()
)

# == Popular dataflows ---------------------------------------------------------

.oecd_popular <- tibble(
  short_name = c("NAAG", "CLI", "CPI", "ULC", "STLABOUR",
                  "SNA_TABLE1", "MEI", "QNA"),
  agency     = c("OECD.SDD.NAD", "OECD.SDD.STES", "OECD.SDD.TPS",
                  "OECD.SDD.TPS", "OECD.SDD.TPS",
                  "OECD.SDD.NAD", "OECD.SDD.STES", "OECD.SDD.NAD"),
  dataflow   = c("DSD_NAAG@DF_NAAG", "DSD_STES@CL_STES_CLI", "DSD_PRICES@DF_PRICES_ALL",
                  "DSD_STES@CL_STES_ULC", "DSD_LFS@DF_IALFS_INDIC",
                  "DSD_NAMAIN10@DF_TABLE1_EXPENDITURE_HCPC",
                  "DSD_MEI@DF_MEI", "DSD_NAMAIN1@DF_QNA_EXPENDITURE_CAPITA"),
  description = c("National Accounts at a Glance", "Composite Leading Indicators",
                   "Consumer Prices", "Unit Labour Costs",
                   "Labour Force Statistics", "GDP Expenditure",
                   "Main Economic Indicators", "Quarterly National Accounts")
)

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
oecd_dataflows_popular <- function() {
  .oecd_popular
}


# == Context ===================================================================

#' Generate LLM-friendly context for the OECD package
#'
#' @return Character string (invisibly), also printed
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
