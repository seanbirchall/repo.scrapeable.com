
# == Global temperature ========================================================

#' Fetch Berkeley Earth global land+ocean temperature data
#'
#' Downloads the complete Land_and_Ocean data from Berkeley Earth.
#' Contains monthly temperature anomalies relative to the 1951-1980 average.
#'
#' @return tibble: year (integer), month (integer), anomaly (numeric),
#'   anomaly_unc (numeric), annual_anomaly (numeric),
#'   annual_anomaly_unc (numeric)
#' @export
berk_global_temp <- function() {
  url <- paste0(.berk_s3, "/Land_and_Ocean_complete.txt")
  f <- .fetch(url, ext = ".txt")
  df <- .parse_berk_txt(f)
  if (is.null(df) || nrow(df) == 0) return(.schema_global_temp)
  # The file has monthly data with columns:
  # Year, Month, Monthly_Anomaly, Monthly_Unc, Annual_Anomaly, Annual_Unc, ...
  # Number of columns can vary; take first 6
  nc <- min(ncol(df), 6)
  result <- tibble(
    year = as.integer(df[[1]]),
    month = as.integer(df[[2]]),
    anomaly = suppressWarnings(as.numeric(df[[3]])),
    anomaly_unc = if (nc >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_,
    annual_anomaly = if (nc >= 5) suppressWarnings(as.numeric(df[[5]])) else NA_real_,
    annual_anomaly_unc = if (nc >= 6) suppressWarnings(as.numeric(df[[6]])) else NA_real_
  )
  result |> filter(!is.na(year), !is.na(month))
}

# == Context ===================================================================

#' Show Berkeley Earth context for LLMs
#'
#' Displays package overview and function signatures.
#' @return Invisibly returns the context string
#' @export
berk_context <- function() {
  .build_context(
    "berkeleyearth.org",
    header_lines = c(
      "# berkeleyearth.org",
      "# Berkeley Earth global temperature data client",
      "# Auth: none required",
      "# Data: Global land+ocean temperature anomalies (vs 1951-1980 avg)",
      "#",
      "# Source: Berkeley Earth Surface Temperature project",
      "# Resolution: monthly, from 1850 to present"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
