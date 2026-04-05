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

#' Fetch OECD data as CSV via SDMX
#'
#' Queries the OECD SDMX REST API and returns data in CSV format.
#' The key parameter specifies dimension filters separated by dots.
#' This is the low-level function; see [oecd_fetch()] for a convenience wrapper.
#'
#' @param agency Agency ID (e.g. "OECD.SDD.NAD", "OECD.SDD.STES")
#' @param dataflow Dataflow ID (e.g. "DSD_NAAG@DF_NAAG")
#' @param key Dimension key with dots separating dimensions.
#'   Use empty string for wildcard. Example for NAAG (5 dims):
#'   "A.USA...." = Annual, USA, all other dims wild.
#' @param start_period Start year or period (e.g. "2020", "2020-Q1")
#' @param end_period End year or period (e.g. "2023")
#' @param first_n Limit to first N observations per series (integer or NULL)
#' @return A tibble with SDMX columns that vary by dataflow. Common columns
#'   include REF_AREA, MEASURE, TIME_PERIOD, OBS_VALUE.
#' @examples
#' oecd_data("OECD.SDD.NAD", "DSD_NAAG@DF_NAAG", "A.USA....",
#'           start_period = "2020", end_period = "2023")
#' @seealso [oecd_fetch()], [oecd_dataflows_popular()], [oecd_context()]
#' @source <https://sdmx.oecd.org/public/rest>
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
#' Convenience wrapper around [oecd_data()] that maps human-friendly
#' shortnames to the correct agency and dataflow IDs.
#'
#' @param name Short name: "NAAG" (National Accounts), "CLI" (Composite
#'   Leading Indicators), "CPI" (Consumer Prices), "ULC" (Unit Labour Costs),
#'   "STLABOUR" (Labour Force), "SNA_TABLE1" (GDP), "MEI" (Main Economic
#'   Indicators), "QNA" (Quarterly National Accounts). See
#'   [oecd_dataflows_popular()] for full list.
#' @param key Dimension key with dots separating dimensions (default: all)
#' @param start_period Start year or period
#' @param end_period End year or period
#' @return A tibble with SDMX columns (varies by dataflow).
#' @examples
#' oecd_fetch("NAAG", key = "A.USA....", start_period = "2020")
#' oecd_fetch("CPI", start_period = "2023")
#' @seealso [oecd_data()], [oecd_dataflows_popular()], [oecd_context()]
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
#' Returns a curated list of commonly used OECD dataflows with their
#' agency IDs, dataflow identifiers, and descriptions. Use these with
#' [oecd_fetch()] or [oecd_data()].
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{short_name}{Short name for use with oecd_fetch() (character)}
#'     \item{agency}{OECD agency ID (character)}
#'     \item{dataflow}{SDMX dataflow identifier (character)}
#'     \item{description}{Human-readable description (character)}
#'   }
#' @examples
#' oecd_dataflows_popular()
#' @seealso [oecd_fetch()], [oecd_data()], [oecd_context()]
#' @export
oecd_dataflows_popular <- function() {
  .oecd_popular
}


# == Context ===================================================================

#' Get oecd.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/oecd.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "oecd.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# oecd.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# oecd.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
