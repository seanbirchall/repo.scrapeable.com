# == H.15 Selected Interest Rates =============================================

#' Treasury yield curve rates (daily)
#'
#' Constant maturity Treasury yields from 1-month to 30-year.
#' Primary source: Federal Reserve H.15 release.
#'
#' @param lastobs Number of most recent observations (default 250 = ~1yr)
#' @param from Start date (optional)
#' @param to End date (optional)
#' @return tibble: date, series_code, value, description
#' @export
fed_treasury_yields <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_treasury, lastobs, from, to)
}

#' Federal funds rate (daily)
#'
#' Effective federal funds rate and target range.
#' Primary source: Federal Reserve H.15 release.
#'
#' @param lastobs Number of recent observations (default 250)
#' @param from Start date
#' @param to End date
#' @return tibble: date, series_code, value, description
#' @export
fed_funds_rate <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_fedfunds, lastobs, from, to)
}

#' Commercial paper rates (daily)
#'
#' @param lastobs Number of recent observations (default 250)
#' @param from Start date
#' @param to End date
#' @return tibble: date, series_code, value, description
#' @export
fed_commercial_paper <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_cp, lastobs, from, to)
}

#' Bank prime loan rate
#'
#' @param lastobs Number of recent observations (default 250)
#' @param from Start date
#' @param to End date
#' @return tibble: date, series_code, value, description
#' @export
fed_prime_rate <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_prime, lastobs, from, to)
}

#' All H.15 interest rates (Treasury + Fed funds + CP + prime)
#'
#' Convenience function that fetches all major H.15 series in one call.
#' Makes 4 HTTP requests.
#'
#' @param lastobs Number of recent observations (default 60)
#' @return tibble: date, series_code, value, description
#' @export
fed_h15 <- function(lastobs = 60) {
  bind_rows(
    tryCatch(fed_treasury_yields(lastobs), error = function(e) NULL),
    tryCatch(fed_funds_rate(lastobs), error = function(e) NULL),
    tryCatch(fed_commercial_paper(lastobs), error = function(e) NULL),
    tryCatch(fed_prime_rate(lastobs), error = function(e) NULL)
  )
}


# == Yield curve snapshot ======================================================

#' Current yield curve (wide format)
#'
#' Returns the most recent Treasury yields in wide format — one column per
#' maturity (1mo, 3mo, 6mo, 1yr, 2yr, 3yr, 5yr, 7yr, 10yr, 20yr, 30yr).
#'
#' @param n Number of days to include (default 1 = latest only)
#' @return tibble: date + one column per maturity
#' @export
fed_yield_curve <- function(n = 1) {
  df <- fed_treasury_yields(lastobs = n * 2)  # extra buffer for missing days
  if (nrow(df) == 0) return(tibble())

  # Extract maturity from description
  df <- df |>
    mutate(
      maturity = case_when(
        grepl("1-month", description)  ~ "1mo",
        grepl("3-month", description)  ~ "3mo",
        grepl("6-month", description)  ~ "6mo",
        grepl("1-year", description)   ~ "1yr",
        grepl("2-year", description)   ~ "2yr",
        grepl("3-year", description)   ~ "3yr",
        grepl("5-year", description)   ~ "5yr",
        grepl("7-year", description)   ~ "7yr",
        grepl("10-year", description)  ~ "10yr",
        grepl("20-year", description)  ~ "20yr",
        grepl("30-year", description)  ~ "30yr",
        TRUE ~ series_code
      )
    ) |>
    select(date, maturity, value) |>
    pivot_wider(names_from = maturity, values_from = value)

  # Keep only the most recent n dates
  df |> arrange(desc(date)) |> head(n)
}


# == Available releases ========================================================

#' List Federal Reserve statistical releases
#'
#' Returns the major Fed Board statistical releases with descriptions.
#'
#' @return tibble: release_code, name, description, frequency
#' @export
fed_releases <- function() {
  tibble(
    release_code = c("H15", "H41", "H8", "H3", "H6", "G17", "G19", "G5", "Z1"),
    name = c(
      "H.15 Selected Interest Rates",
      "H.4.1 Factors Affecting Reserve Balances",
      "H.8 Assets and Liabilities of Commercial Banks",
      "H.3 Aggregate Reserves",
      "H.6 Money Stock Measures",
      "G.17 Industrial Production and Capacity Utilization",
      "G.19 Consumer Credit",
      "G.5 Foreign Exchange Rates",
      "Z.1 Financial Accounts of the United States"
    ),
    description = c(
      "Treasury yields, fed funds, commercial paper, prime rate",
      "Federal Reserve balance sheet, reserve factors",
      "Bank assets, liabilities, loans, deposits",
      "Reserve balances, monetary base",
      "M1, M2 money supply aggregates",
      "Industrial production indexes, capacity utilization",
      "Consumer credit outstanding (revolving, nonrevolving)",
      "Foreign exchange rates against US dollar",
      "Flow of funds, financial accounts, wealth"
    ),
    frequency = c("daily", "weekly", "weekly", "biweekly", "weekly",
                  "monthly", "monthly", "daily", "quarterly")
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the federalreserve.gov package
#'
#' @return Character string (invisibly), also printed
#' @export
fed_context <- function() {
  .build_context("federalreserve.gov", header_lines = c(
    "# federalreserve.gov - Federal Reserve Board Data Client for R",
    "# Dependencies: httr2, dplyr, tidyr, tibble",
    "# Auth: none (public CSV downloads)",
    "# Primary source for: Treasury yields, fed funds rate, bank rates",
    "# All functions return tibbles.",
    "#",
    "# Key releases:",
    "#   H.15 = Interest rates (Treasury yields, fed funds, CP, prime)",
    "#   H.4.1 = Fed balance sheet",
    "#   H.8 = Commercial bank assets/liabilities",
    "#   G.17 = Industrial production",
    "#   G.19 = Consumer credit",
    "#",
    "# For broader Fed data access, also see: fred.stlouisfed.org package"
  ))
}
