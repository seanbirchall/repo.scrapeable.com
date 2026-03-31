
# == Annual global temperature =================================================

#' Fetch HadCRUT5 global annual temperature anomalies
#'
#' Downloads annual global mean temperature anomalies from the Met Office
#' HadCRUT5 dataset. Anomalies are relative to the 1961-1990 average.
#'
#' @return tibble: year (integer), anomaly (numeric), lower_ci (numeric),
#'   upper_ci (numeric)
#' @export
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
#' Downloads monthly global mean temperature anomalies from the Met Office
#' HadCRUT5 dataset. Anomalies are relative to the 1961-1990 average.
#'
#' @return tibble: year (integer), month (integer), anomaly (numeric),
#'   lower_ci (numeric), upper_ci (numeric)
#' @export
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

#' Show HadCRUT5 API context for LLMs
#'
#' Displays package overview and function signatures.
#' @return Invisibly returns the context string
#' @export
hadcrut_context <- function() {
  .build_context(
    "metoffice.gov.uk",
    header_lines = c(
      "# metoffice.gov.uk",
      "# Met Office HadCRUT5 global temperature anomaly client",
      "# Auth: none required",
      "# Data: Global mean temperature anomalies vs 1961-1990 average",
      "#",
      "# Source: Met Office Hadley Centre / University of East Anglia CRU",
      "# Coverage: 1850 to present, annual and monthly resolution"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
