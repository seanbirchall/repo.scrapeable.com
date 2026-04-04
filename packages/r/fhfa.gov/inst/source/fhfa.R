# fhfa.gov.R
# Self-contained FHFA (Federal Housing Finance Agency) client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Docs: https://www.fhfa.gov/data/hpi

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

.ua <- "support@scrapeable.com"
.fhfa_base <- "https://www.fhfa.gov"

# -- Core fetch: download CSV to temp file, then read locally -----------------

.fhfa_fetch_csv <- function(url, header = TRUE, col_names = NULL) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  if (file.info(tmp)$size == 0) return(tibble())
  if (!is.null(col_names)) {
    read.csv(tmp, header = FALSE, stringsAsFactors = FALSE,
             col.names = col_names, na.strings = c("", "-", "NA", "."))
  } else {
    read.csv(tmp, header = header, stringsAsFactors = FALSE,
             na.strings = c("", "-", "NA", "."))
  }
}

# -- Schemas -------------------------------------------------------------------

.schema_hpi_master <- tibble(
  hpi_type = character(), hpi_flavor = character(), frequency = character(),
  level = character(), place_name = character(), place_id = character(),
  yr = integer(), period = integer(), index_nsa = numeric(), index_sa = numeric()
)

.schema_hpi_state <- tibble(
  state = character(), yr = integer(), qtr = integer(),
  index_nsa = numeric(), index_sa = numeric()
)

.schema_hpi_metro <- tibble(
  metro_name = character(), metro_code = character(), yr = integer(),
  qtr = integer(), index_nsa = numeric(), index_sa = numeric()
)

.schema_hpi_summary <- tibble(
  name = character(), rank = integer(),
  change_1qtr = numeric(), change_1yr = numeric(),
  change_5yr = numeric(), change_since_1991 = numeric()
)

# == Public functions ==========================================================

#' List available FHFA HPI datasets
#'
#' Returns a tibble describing each downloadable HPI dataset with its URL.
#' @return tibble
#' @export
fhfa_list <- function() {
  tibble(
    dataset = c(
      "hpi_master", "hpi_state", "hpi_metro",
      "hpi_us_census", "hpi_tract",
      "summary_state", "summary_city", "summary_regional"
    ),
    description = c(
      "Master HPI file (all types, flavors, geographies)",
      "All-Transactions quarterly index by state (no header)",
      "All-Transactions quarterly index by metro area (no header)",
      "All-Transactions quarterly index for US and Census divisions (no header)",
      "Annual All-Transactions index by census tract",
      "State HPI summary (1-qtr, 1-yr, 5-yr, since 1991 pct change)",
      "Metro/city HPI summary (1-qtr, 1-yr, 5-yr, since 1991 pct change)",
      "Regional HPI summary (1-qtr, 1-yr, 5-yr, since 1991 pct change)"
    ),
    frequency = c("monthly", "quarterly", "quarterly", "quarterly",
                   "annual", "quarterly", "quarterly", "quarterly"),
    url = c(
      "https://www.fhfa.gov/hpi/download/monthly/hpi_master.csv",
      "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_state.csv",
      "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_metro.csv",
      "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_us_and_census.csv",
      "https://www.fhfa.gov/hpi/download/annual/hpi_at_tract.csv",
      "https://www.fhfa.gov/hpi-state/csv",
      "https://www.fhfa.gov/hpi-city/csv",
      "https://www.fhfa.gov/hpi-regional/csv"
    )
  )
}

#' Search HPI master data
#'
#' Search the master HPI file by place name, type, flavor, frequency, or level.
#' @param query Character. Regex pattern to match against place_name.
#' @param hpi_type Optional filter: "traditional", "distortion-free", "expanded-data".
#' @param hpi_flavor Optional filter: "purchase-only", "all-transactions", "refinancing".
#' @param frequency Optional filter: "monthly", "quarterly", "annual".
#' @param level Optional filter: e.g. "State", "MSA", "USA or Census Division".
#' @param max_rows Maximum rows to return. Default 500.
#' @return tibble
#' @export
fhfa_search <- function(query = NULL, hpi_type = NULL, hpi_flavor = NULL,
                        frequency = NULL, level = NULL, max_rows = 500) {
  url <- paste0(.fhfa_base, "/hpi/download/monthly/hpi_master.csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_master)

  df <- df |>
    mutate(
      yr = as.integer(yr),
      period = as.integer(period),
      index_nsa = suppressWarnings(as.numeric(index_nsa)),
      index_sa = suppressWarnings(as.numeric(index_sa))
    )

  if (!is.null(query))      df <- df |> filter(grepl(query, place_name, ignore.case = TRUE))
  if (!is.null(hpi_type))   df <- df |> filter(.data$hpi_type == .env$hpi_type)
  if (!is.null(hpi_flavor)) df <- df |> filter(.data$hpi_flavor == .env$hpi_flavor)
  if (!is.null(frequency))  df <- df |> filter(.data$frequency == .env$frequency)
  if (!is.null(level))      df <- df |> filter(.data$level == .env$level)

  df |> head(max_rows)
}

#' Get HPI data by state (All-Transactions quarterly index)
#'
#' @param state Optional two-letter state abbreviation to filter.
#' @param start_year Optional. Filter to years >= this.
#' @param end_year Optional. Filter to years <= this.
#' @return tibble with columns: state, yr, qtr, index_nsa, index_sa
#' @export
fhfa_hpi_state <- function(state = NULL, start_year = NULL, end_year = NULL) {
  url <- paste0(.fhfa_base, "/hpi/download/quarterly_datasets/hpi_at_state.csv")
  df <- .fhfa_fetch_csv(url, header = FALSE,
                        col_names = c("state", "yr", "qtr", "index_nsa", "index_sa")) |>
    as_tibble() |>
    mutate(
      yr = as.integer(yr),
      qtr = as.integer(qtr),
      index_nsa = suppressWarnings(as.numeric(index_nsa)),
      index_sa = suppressWarnings(as.numeric(index_sa))
    )

  if (!is.null(state))      df <- df |> filter(state == !!state)
  if (!is.null(start_year)) df <- df |> filter(yr >= start_year)
  if (!is.null(end_year))   df <- df |> filter(yr <= end_year)
  df
}

#' Get HPI data by metro area (All-Transactions quarterly index)
#'
#' @param query Regex pattern to match against metro_name.
#' @param start_year Optional. Filter to years >= this.
#' @param end_year Optional. Filter to years <= this.
#' @return tibble with columns: metro_name, metro_code, yr, qtr, index_nsa, index_sa
#' @export
fhfa_hpi_metro <- function(query = NULL, start_year = NULL, end_year = NULL) {
  url <- paste0(.fhfa_base, "/hpi/download/quarterly_datasets/hpi_at_metro.csv")
  df <- .fhfa_fetch_csv(url, header = FALSE,
                        col_names = c("metro_name", "metro_code", "yr", "qtr",
                                      "index_nsa", "index_sa")) |>
    as_tibble() |>
    mutate(
      yr = as.integer(yr),
      qtr = as.integer(qtr),
      index_nsa = suppressWarnings(as.numeric(index_nsa)),
      index_sa = suppressWarnings(as.numeric(index_sa))
    )

  if (!is.null(query))      df <- df |> filter(grepl(query, metro_name, ignore.case = TRUE))
  if (!is.null(start_year)) df <- df |> filter(yr >= start_year)
  if (!is.null(end_year))   df <- df |> filter(yr <= end_year)
  df
}

#' Get HPI summary table by state
#'
#' Returns ranked percentage changes over 1 quarter, 1 year, 5 years, and since 1991.
#' @return tibble
#' @export
fhfa_summary_state <- function() {
  url <- paste0(.fhfa_base, "/hpi-state/csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_summary)
  names(df) <- c("name", "rank", "change_1qtr", "change_1yr",
                  "change_5yr", "change_since_1991")
  df |>
    mutate(
      rank = as.integer(rank),
      change_1qtr = as.numeric(change_1qtr),
      change_1yr = as.numeric(change_1yr),
      change_5yr = as.numeric(change_5yr),
      change_since_1991 = as.numeric(change_since_1991)
    )
}

#' Get HPI summary table by city/metro
#'
#' Returns ranked percentage changes over 1 quarter, 1 year, 5 years, and since 1991.
#' @return tibble
#' @export
fhfa_summary_city <- function() {
  url <- paste0(.fhfa_base, "/hpi-city/csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_summary)
  names(df) <- c("name", "rank", "change_1qtr", "change_1yr",
                  "change_5yr", "change_since_1991")
  df |>
    mutate(
      rank = as.integer(rank),
      change_1qtr = as.numeric(change_1qtr),
      change_1yr = as.numeric(change_1yr),
      change_5yr = as.numeric(change_5yr),
      change_since_1991 = as.numeric(change_since_1991)
    )
}

#' Get HPI summary table by region
#'
#' Returns ranked percentage changes over 1 quarter, 1 year, 5 years, and since 1991.
#' @return tibble
#' @export
fhfa_summary_regional <- function() {
  url <- paste0(.fhfa_base, "/hpi-regional/csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_summary)
  names(df) <- c("name", "rank", "change_1qtr", "change_1yr",
                  "change_5yr", "change_since_1991")
  df |>
    mutate(
      rank = as.integer(rank),
      change_1qtr = as.numeric(change_1qtr),
      change_1yr = as.numeric(change_1yr),
      change_5yr = as.numeric(change_5yr),
      change_since_1991 = as.numeric(change_since_1991)
    )
}

#' Self-reading context function
#'
#' Returns the full source code of this client file.
#' @return character vector (one element per line)
#' @export
fhfa_context <- function() {
  src <- tryCatch(
    readLines(sys.frame(1)$ofile %||%
      file.path(system.file(package = "fhfa.gov"), "source", "fhfa.R") %||%
      "clients/fhfa.gov.R"),
    error = function(e) {
      f <- system.file("source", "fhfa.R", package = "fhfa.gov")
      if (nzchar(f)) readLines(f) else character()
    }
  )
  src
}
