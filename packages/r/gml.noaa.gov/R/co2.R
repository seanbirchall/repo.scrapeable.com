
#' Fetch daily global CO2 trend data
#'
#' Smoothed and trend values from NOAA GML. Updated daily.
#'
#' @return tibble: date (Date), smoothed (numeric ppm), trend (numeric ppm)
#' @export
co2_global_trend <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_trend_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_trend)
  raw |>
    as_tibble() |>
    transmute(
      date     = as.Date(sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))),
      smoothed = as.numeric(smoothed),
      trend    = as.numeric(trend)
    )
}

#' Fetch annual global mean CO2
#'
#' @return tibble: year (integer), mean (numeric ppm), unc (numeric)
#' @export
co2_annual <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_annmean_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_annual)
  raw |>
    as_tibble() |>
    transmute(
      year = as.integer(year),
      mean = as.numeric(mean),
      unc  = as.numeric(unc)
    )
}

#' Fetch monthly global CO2 data
#'
#' @return tibble: year, month, decimal, average, average_unc, trend, trend_unc
#' @export
co2_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}

#' Fetch monthly global CH4 (methane) data
#'
#' @return tibble: year, month, decimal, average, average_unc, trend, trend_unc
#' @export
co2_ch4_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/ch4/ch4_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  # ch4 csv may have blank rows
  raw <- raw[!is.na(raw$year) & raw$year != "", ]
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}

#' NOAA GML context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
co2_context <- function() {
  .build_context("gml.noaa.gov", header_lines = c(
    "# gml.noaa.gov - NOAA Global Monitoring Lab CO2/CH4 Client",
    "# Deps: httr2, dplyr, tibble",
    "# Auth: none",
    "# Data: static CSV files updated daily/monthly",
    "#",
    "# CO2 measured in ppm, CH4 in ppb"
  ))
}
