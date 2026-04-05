# data.oecd.org.R - Self-contained data.oecd.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)


# data-oecd-org.R
# Self-contained OECD SDMX data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: SDMX REST at sdmx.oecd.org (CSV format for data)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.oecd_base <- "https://sdmx.oecd.org/public/rest"

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
#' Queries the OECD SDMX REST API and returns data in CSV format. The OECD
#' provides thousands of statistical dataflows covering economics, education,
#' health, environment, and more across member countries.
#'
#' The returned columns vary by dataflow but typically include SDMX-standard
#' fields. For the NAAG dataflow, columns include: DATAFLOW, FREQ, REF_AREA,
#' MEASURE, UNIT_MEASURE, CHAPTER, TIME_PERIOD, OBS_VALUE, ADJUSTMENT,
#' COUNTERPART_AREA, SECTOR, PRICE_BASE, OBS_STATUS, UNIT_MULT, CURRENCY,
#' and others.
#'
#' @param agency Character. Agency ID (e.g. "OECD.SDD.NAD"). Use
#'   \code{oecd_dataflows_popular()} to see common agencies.
#' @param dataflow Character. Dataflow ID (e.g. "DSD_NAAG@@DF_NAAG").
#' @param key Character. Dimension key with dots separating dimensions.
#'   Use empty string for wildcard. Example for NAAG (5 dims):
#'   "A.USA...." = Annual, USA, all other dims wild.
#' @param start_period Character or NULL. Start year (e.g. "2020").
#' @param end_period Character or NULL. End year (e.g. "2023").
#' @param first_n Integer or NULL. Limit to first N observations per series.
#' @return A tibble with SDMX columns. Key columns typically include:
#' \describe{
#'   \item{REF_AREA}{Character. Country or area code (e.g. "USA", "GBR").}
#'   \item{TIME_PERIOD}{Integer. Year of observation.}
#'   \item{OBS_VALUE}{Numeric. The observed value.}
#'   \item{MEASURE}{Character. Measure code.}
#'   \item{UNIT_MEASURE}{Character. Unit of measurement (e.g. "USD_PPP", "IX").}
#'   \item{FREQ}{Character. Frequency code (e.g. "A" for annual, "Q" for quarterly).}
#' }
#' @export
#' @examples
#' \dontrun{
#' oecd_data("OECD.SDD.NAD", "DSD_NAAG@@DF_NAAG",
#'           key = "A.USA....", start_period = "2020", end_period = "2023")
#' }
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
#' Convenience wrapper around \code{oecd_data()} that maps short human-readable
#' names to their full agency and dataflow identifiers. Supports 8 popular
#' OECD dataflows including GDP, CPI, employment, and leading indicators.
#'
#' @param name Character. Short name: "NAAG" (National Accounts at a Glance),
#'   "CLI" (Composite Leading Indicators), "CPI" (Consumer Prices),
#'   "ULC" (Unit Labour Costs), "STLABOUR" (Labour Force Statistics),
#'   "SNA_TABLE1" (GDP Expenditure), "MEI" (Main Economic Indicators),
#'   "QNA" (Quarterly National Accounts). Use \code{oecd_dataflows_popular()}
#'   to see all options.
#' @param key Character. Dimension key (default: "" for all). Dots separate
#'   dimensions; empty segments are wildcards (e.g. "A.USA....").
#' @param start_period Character or NULL. Start year (e.g. "2020").
#' @param end_period Character or NULL. End year (e.g. "2023").
#' @return A tibble with SDMX columns (same as \code{oecd_data()}).
#' @export
#' @examples
#' \dontrun{
#' oecd_fetch("NAAG", key = "A.USA....", start_period = "2022", end_period = "2022")
#' oecd_fetch("CPI", key = "A.GBR....")
#' }
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
#' Returns a curated list of 8 commonly used OECD dataflows with their full
#' agency and dataflow identifiers. Use this to discover valid inputs for
#' \code{oecd_fetch()} and \code{oecd_data()}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{short_name}{Character. Short alias for use with \code{oecd_fetch()} (e.g. "NAAG", "CPI", "CLI").}
#'   \item{agency}{Character. Full OECD agency identifier (e.g. "OECD.SDD.NAD").}
#'   \item{dataflow}{Character. Full SDMX dataflow identifier (e.g. "DSD_NAAG@@DF_NAAG").}
#'   \item{description}{Character. Human-readable description (e.g. "National Accounts at a Glance").}
#' }
#' @export
#' @examples
#' \dontrun{
#' oecd_dataflows_popular()
#' }
oecd_dataflows_popular <- function() {
  .oecd_popular
}


# == Context ===================================================================

#' Get data.oecd.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
oecd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(oecd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.oecd.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.oecd.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.oecd.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.oecd.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
