# metoffice.gov.uk.R - Self-contained metoffice.gov.uk client

library(httr2)
library(tibble)
library(dplyr)


# metoffice-gov-uk.R
# Self-contained Met Office HadCRUT5 temperature data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Data: CSV download from Met Office Hadley Centre


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hadcrut_base <- "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/diagnostics"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_annual <- tibble(
  year = integer(), anomaly = numeric(),
  lower_ci = numeric(), upper_ci = numeric()
)

.schema_monthly <- tibble(
  year = integer(), month = integer(), anomaly = numeric(),
  lower_ci = numeric(), upper_ci = numeric()
)


# == Annual global temperature =================================================

#' Fetch HadCRUT5 global annual temperature anomalies
#'
#' Downloads annual global mean surface temperature anomalies from the
#' Met Office Hadley Centre HadCRUT5 dataset (version 5.0.2.0). Anomalies
#' are measured in degrees Celsius relative to the 1961-1990 baseline
#' average. Data spans from 1850 to the most recent complete year.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (1850 onward).}
#'     \item{anomaly}{Numeric. Global mean temperature anomaly in
#'       degrees Celsius relative to 1961-1990.}
#'     \item{lower_ci}{Numeric. Lower bound of the 95\% confidence
#'       interval.}
#'     \item{upper_ci}{Numeric. Upper bound of the 95\% confidence
#'       interval.}
#'   }
#' @examples
#' hadcrut_global_annual()
hadcrut_global_annual <- function() {
  url <- paste0(.hadcrut_base,
                "/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")
  f <- .fetch(url, ext = ".csv")
  df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_annual)
  df <- as_tibble(df)
  names(df) <- tolower(names(df))
  # Columns typically: Time, Anomaly (deg C), Lower confidence limit, Upper confidence limit
  year_col <- intersect(c("time", "year"), names(df))
  if (length(year_col) == 0) year_col <- names(df)[1]
  tibble(
    year = as.integer(df[[year_col[1]]]),
    anomaly = if (ncol(df) >= 2) suppressWarnings(as.numeric(df[[2]])) else NA_real_,
    lower_ci = if (ncol(df) >= 3) suppressWarnings(as.numeric(df[[3]])) else NA_real_,
    upper_ci = if (ncol(df) >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_
  ) |> filter(!is.na(year))
}

# == Monthly global temperature ================================================

#' Fetch HadCRUT5 global monthly temperature anomalies
#'
#' Downloads monthly global mean surface temperature anomalies from the
#' Met Office Hadley Centre HadCRUT5 dataset (version 5.0.2.0). Anomalies
#' are in degrees Celsius relative to the 1961-1990 baseline average.
#' Data spans from January 1850 to the most recent available month.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (1850 onward).}
#'     \item{month}{Integer. Month number (1-12).}
#'     \item{anomaly}{Numeric. Global mean temperature anomaly in
#'       degrees Celsius relative to 1961-1990.}
#'     \item{lower_ci}{Numeric. Lower bound of the 95\% confidence
#'       interval.}
#'     \item{upper_ci}{Numeric. Upper bound of the 95\% confidence
#'       interval.}
#'   }
#' @examples
#' hadcrut_global_monthly()
hadcrut_global_monthly <- function() {
  url <- paste0(.hadcrut_base,
                "/HadCRUT.5.0.2.0.analysis.summary_series.global.monthly.csv")
  f <- .fetch(url, ext = ".csv")
  df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_monthly)
  df <- as_tibble(df)
  names(df) <- tolower(names(df))
  # Time column is YYYY-MM format
  time_col <- intersect(c("time", "year"), names(df))
  if (length(time_col) == 0) time_col <- names(df)[1]
  time_vals <- as.character(df[[time_col[1]]])
  tibble(
    year = as.integer(sub("-.*", "", time_vals)),
    month = as.integer(sub(".*-", "", time_vals)),
    anomaly = if (ncol(df) >= 2) suppressWarnings(as.numeric(df[[2]])) else NA_real_,
    lower_ci = if (ncol(df) >= 3) suppressWarnings(as.numeric(df[[3]])) else NA_real_,
    upper_ci = if (ncol(df) >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_
  ) |> filter(!is.na(year))
}

# == Context ===================================================================

#' Get metoffice.gov.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hadcrut_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hadcrut_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/metoffice.gov.uk.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "metoffice.gov.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# metoffice.gov.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# metoffice.gov.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
