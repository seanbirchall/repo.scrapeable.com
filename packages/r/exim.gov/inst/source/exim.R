# exim.gov.R
# Self-contained Export-Import Bank of the US client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public CSV)
# Source: https://img.exim.gov/s3fs-public/dataset/vbhv-d8am/Data.Gov+-+FY25+Q4.csv

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.exim_csv_url <- "https://img.exim.gov/s3fs-public/dataset/vbhv-d8am/Data.Gov+-+FY25+Q4.csv"

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.exim_cache <- new.env(parent = emptyenv())

.exim_load <- function(force = FALSE) {
  if (!force && exists("data", envir = .exim_cache)) {
    return(get("data", envir = .exim_cache))
  }
  tmp <- .fetch_csv(.exim_csv_url)
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  df <- tibble::as_tibble(raw)
  names(df) <- trimws(names(df))

  df <- df |>
    mutate(
      `Fiscal Year` = as.integer(`Fiscal Year`),
      `Decision Date` = suppressWarnings(as.Date(`Decision Date`, format = "%m/%d/%Y")),
      `Effective Date` = suppressWarnings(as.Date(`Effective Date`, format = "%m/%d/%Y")),
      `Expiration Date` = suppressWarnings(as.Date(`Expiration Date`, format = "%m/%d/%Y")),
      `Approved/Declined Amount` = suppressWarnings(as.numeric(gsub(",", "", `Approved/Declined Amount`))),
      `Disbursed/Shipped Amount` = suppressWarnings(as.numeric(gsub(",", "", `Disbursed/Shipped Amount`))),
      `Undisbursed Exposure Amount` = suppressWarnings(as.numeric(gsub(",", "", `Undisbursed Exposure Amount`))),
      `Outstanding Exposure Amount` = suppressWarnings(as.numeric(gsub(",", "", `Outstanding Exposure Amount`))),
      `Small Business Authorized Amount` = suppressWarnings(as.numeric(gsub(",", "", `Small Business Authorized Amount`))),
      `Woman Owned Authorized Amount` = suppressWarnings(as.numeric(gsub(",", "", `Woman Owned Authorized Amount`))),
      `Minority Owned Authorized Amount` = suppressWarnings(as.numeric(gsub(",", "", `Minority Owned Authorized Amount`)))
    )

  assign("data", df, envir = .exim_cache)
  df
}

# == Schemas ===================================================================

.schema_exim <- tibble(
  fiscal_year = integer(), unique_id = character(), deal_number = character(),
  decision = character(), decision_date = as.Date(character()),
  country = character(), program = character(), policy_type = character(),
  product_description = character(), term = character(),
  primary_exporter = character(), exporter_state = character(),
  approved_amount = numeric(), disbursed_amount = numeric(),
  small_business_amount = numeric()
)

# == Public functions ==========================================================

#' List EXIM Bank authorizations
#'
#' Returns all EXIM Bank authorizations. Data covers FY2007-present.
#'
#' @param fiscal_year Optional integer; filter to a specific fiscal year.
#' @param country Optional character; filter to a specific country (case-insensitive partial match).
#' @param program Optional character; filter by program type (e.g. "Guarantee", "Insurance", "Loan", "Working Capital").
#' @param limit Maximum rows to return (default 500).
#' @return tibble of EXIM authorizations
#' @export
exim_list <- function(fiscal_year = NULL, country = NULL, program = NULL, limit = 500) {
  df <- .exim_load()

  if (!is.null(fiscal_year)) {
    df <- df |> filter(`Fiscal Year` == as.integer(fiscal_year))
  }
  if (!is.null(country)) {
    df <- df |> filter(grepl(country, Country, ignore.case = TRUE))
  }
  if (!is.null(program)) {
    df <- df |> filter(grepl(program, Program, ignore.case = TRUE))
  }

  df |> head(limit)
}

#' Search EXIM Bank authorizations
#'
#' Free-text search across exporter name, product description, borrower, and country.
#'
#' @param query Character string to search for (case-insensitive).
#' @param fiscal_year Optional integer; filter to a specific fiscal year.
#' @param limit Maximum rows to return (default 100).
#' @return tibble of matching EXIM authorizations
#' @export
exim_search <- function(query, fiscal_year = NULL, limit = 100) {
  df <- .exim_load()

  if (!is.null(fiscal_year)) {
    df <- df |> filter(`Fiscal Year` == as.integer(fiscal_year))
  }

  pattern <- query
  df <- df |>
    filter(
      grepl(pattern, `Primary Exporter` %||% "", ignore.case = TRUE) |
      grepl(pattern, `Product Description` %||% "", ignore.case = TRUE) |
      grepl(pattern, `Primary Borrower` %||% "", ignore.case = TRUE) |
      grepl(pattern, Country %||% "", ignore.case = TRUE) |
      grepl(pattern, `Primary Applicant` %||% "", ignore.case = TRUE)
    )

  df |> head(limit)
}

#' Summarize EXIM authorizations by country
#'
#' @param fiscal_year Optional integer; filter to a specific fiscal year.
#' @param top_n Number of top countries to show (default 20).
#' @return tibble: country, n_deals, total_approved, total_disbursed
#' @export
exim_by_country <- function(fiscal_year = NULL, top_n = 20) {
  df <- .exim_load()

  if (!is.null(fiscal_year)) {
    df <- df |> filter(`Fiscal Year` == as.integer(fiscal_year))
  }

  df |>
    group_by(country = Country) |>
    summarise(
      n_deals = n(),
      total_approved = sum(`Approved/Declined Amount`, na.rm = TRUE),
      total_disbursed = sum(`Disbursed/Shipped Amount`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(total_approved)) |>
    head(top_n)
}

#' Summarize EXIM authorizations by fiscal year
#'
#' @return tibble: fiscal_year, n_deals, total_approved, total_disbursed
#' @export
exim_by_year <- function() {
  df <- .exim_load()

  df |>
    group_by(fiscal_year = `Fiscal Year`) |>
    summarise(
      n_deals = n(),
      total_approved = sum(`Approved/Declined Amount`, na.rm = TRUE),
      total_disbursed = sum(`Disbursed/Shipped Amount`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(fiscal_year)
}

#' Summarize EXIM authorizations by program
#'
#' @param fiscal_year Optional integer; filter to a specific fiscal year.
#' @return tibble: program, n_deals, total_approved
#' @export
exim_by_program <- function(fiscal_year = NULL) {
  df <- .exim_load()

  if (!is.null(fiscal_year)) {
    df <- df |> filter(`Fiscal Year` == as.integer(fiscal_year))
  }

  df |>
    group_by(program = Program) |>
    summarise(
      n_deals = n(),
      total_approved = sum(`Approved/Declined Amount`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(total_approved))
}

# == Context ===================================================================

#' Show EXIM client context for LLM use
#'
#' @return Invisibly returns context string
#' @export
exim_context <- function() {
  .build_context("exim.gov")
}

.build_context <- function(pkg_name) {
  src_dir <- system.file("source", package = pkg_name)
  if (src_dir != "" && length(list.files(src_dir, pattern = "[.]R$")) > 0) {
    src_file <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)[1]
  } else {
    src_file <- NULL
    tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
    if (is.null(src_file)) src_file <- paste0("clients/", pkg_name, ".R")
  }
  if (is.null(src_file) || !file.exists(src_file)) {
    cat("# ", pkg_name, " context - source not found\n")
    return(invisible(paste0("# ", pkg_name, " context - source not found")))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
