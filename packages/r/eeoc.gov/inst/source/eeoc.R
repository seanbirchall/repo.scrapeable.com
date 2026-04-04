# eeoc.gov.R
# Self-contained EEOC workforce data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data files)
# Docs: https://www.eeoc.gov/data
#
# Data sources:
#   EEO-4 (state & local government): CSV 2005-2021
#   EEO-1 (private industry): XLSX by year (2014-2021)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.eeoc_base <- "https://www.eeoc.gov"

# EEO-4 CSV URL (state & local government, 2005-2021)
.eeo4_url <- "https://www.eeoc.gov/sites/default/files/2022-09/puf_eeo4_2005_2021.csv"

# EEO-1 XLSX URLs by year (private industry)
.eeo1_years <- c(
  "2021" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2020" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2019" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2018" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202018%20PUF.xlsx",
  "2017" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202017%20PUF.xlsx",
  "2016" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202016%20PUF.xlsx",
  "2015" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202015%20PUF.xlsx",
  "2014" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202014%20PUF.xlsx"
)

# -- Fetch helper (always via httr2) -------------------------------------------

.eeoc_download <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

# -- Context generator ---------------------------------------------------------

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

# == Public functions ==========================================================

#' List available EEOC datasets
#'
#' Returns a tibble describing the available EEOC public use datasets.
#'
#' @return A tibble with columns: dataset, description, format, years, url
#' @export
eeoc_list <- function() {
  tibble(
    dataset = c("eeo4", rep("eeo1", length(.eeo1_years))),
    description = c(
      "State & Local Government Workforce (EEO-4)",
      paste0("Private Industry Workforce (EEO-1) ", names(.eeo1_years))
    ),
    format = c("CSV", rep("XLSX", length(.eeo1_years))),
    years = c("2005-2021", names(.eeo1_years)),
    url = c(.eeo4_url, unname(.eeo1_years))
  )
}

#' Search EEOC EEO-4 state/local government workforce data
#'
#' Downloads and filters the EEO-4 public use CSV (state & local government
#' workforce data from 2005-2021). The full file is ~13 MB.
#'
#' @param state Character. Two-letter state abbreviation (optional).
#' @param year Integer. Year to filter (2005-2021). If NULL, returns all years.
#' @param sex Character. "Male" or "Female" (optional).
#' @param data_type Character. Race/ethnicity category (e.g., "White", "Black",
#'   "Hispanic", "Asian", "AIAN", "NHOPI", "Two_or_more"). Optional.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns: state, emp_type, type, govt_type, year, sex,
#'   bracket, job_cat, functcd, data_type, value
#' @export
eeoc_search <- function(state = NULL, year = NULL, sex = NULL,
                        data_type = NULL, max_results = 1000) {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  df <- as_tibble(df)

  if (!is.null(state))     df <- df |> filter(.data$state == !!state)
  if (!is.null(year))      df <- df |> filter(.data$year == !!as.integer(year))
  if (!is.null(sex))       df <- df |> filter(.data$sex == !!sex)
  if (!is.null(data_type)) df <- df |> filter(.data$data_type == !!data_type)

  df |> head(max_results)
}

#' Get EEOC EEO-4 summary by state and year
#'
#' Aggregates EEO-4 data to show total workforce counts by state, year, and
#' demographic group.
#'
#' @param year Integer. Year (2005-2021). Default 2021.
#' @return A tibble summarized by state and data_type.
#' @export
eeoc_state_summary <- function(year = 2021) {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  df <- as_tibble(df)

  df |>
    filter(.data$year == !!as.integer(year)) |>
    filter(.data$state != "") |>
    group_by(.data$state, .data$data_type) |>
    summarise(total = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    arrange(.data$state, .data$data_type)
}

#' Get EEOC EEO-4 data by government function
#'
#' @param year Integer. Year (2005-2021). Default 2021.
#' @param state Character. Optional state filter.
#' @return A tibble summarized by function code and demographic.
#' @export
eeoc_by_function <- function(year = 2021, state = NULL) {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  df <- as_tibble(df)

  df <- df |> filter(.data$year == !!as.integer(year))
  if (!is.null(state)) df <- df |> filter(.data$state == !!state)

  df |>
    filter(.data$functcd != "") |>
    group_by(.data$functcd, .data$data_type) |>
    summarise(total = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    arrange(.data$functcd, .data$data_type)
}

#' Get available years in EEO-4 data
#'
#' @return Integer vector of available years.
#' @export
eeoc_years <- function() {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE,
                        nrows = 100000)
  sort(unique(df$year))
}

#' Print eeoc.gov client context
#'
#' @return Character string of function signatures (invisibly).
#' @export
eeoc_context <- function() {
  src <- system.file("source", "eeoc.R", package = "eeoc.gov")
  if (src == "") {
    f <- sys.frame(sys.nframe())$ofile %||%
      attr(body(eeoc_context), "srcfile")$filename %||% ""
    if (nzchar(f)) src <- f
  }
  .build_context("eeoc.gov", src_file = if (nzchar(src)) src else NULL,
                 header_lines = c(
                   "# eeoc.gov -- EEOC Workforce Data R client",
                   "# Data: EEO-4 (state/local govt) CSV, EEO-1 (private) XLSX",
                   "# Auth: none"
                 ))
}
