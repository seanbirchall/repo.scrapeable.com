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
#' Returns Export-Import Bank of the United States authorization records.
#' Data covers FY2007--present and includes guarantees, insurance, loans,
#' and working capital transactions. The full dataset has ~50K rows with
#' 34 columns covering deal details, amounts, exporters, and borrowers.
#'
#' @param fiscal_year Integer or NULL. Filter to a specific fiscal year
#'   (e.g. \code{2024}). Valid range: 2007--2025.
#' @param country Character or NULL. Filter by country name (case-insensitive
#'   partial match). Examples: \code{"Mexico"}, \code{"Korea"}, \code{"India"}.
#' @param program Character or NULL. Filter by program type (case-insensitive
#'   partial match): \code{"Guarantee"}, \code{"Insurance"}, \code{"Loan"},
#'   \code{"Working Capital"}.
#' @param limit Integer. Maximum rows to return (default 500).
#' @return A tibble with 34 columns including: Fiscal Year, Unique Identifier,
#'   Deal Number, Decision, Decision Date (Date), Country, Program,
#'   Policy Type, Product Description, Primary Exporter, Primary Borrower,
#'   Approved/Declined Amount (numeric), Disbursed/Shipped Amount (numeric),
#'   and more.
#' @export
#' @examples
#' \dontrun{
#' exim_list(fiscal_year = 2024, country = "Mexico", limit = 50)
#' exim_list(program = "Guarantee")
#' }
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
#' Free-text search across exporter name, product description, borrower,
#' applicant, and country fields. Useful for finding deals involving specific
#' companies or products.
#'
#' @param query Character. Search string (case-insensitive regex). Examples:
#'   \code{"Boeing"}, \code{"solar"}, \code{"aircraft"}, \code{"Caterpillar"}.
#' @param fiscal_year Integer or NULL. Filter to a specific fiscal year.
#' @param limit Integer. Maximum rows to return (default 100).
#' @return A tibble with 34 columns (same schema as \code{exim_list()}).
#' @export
#' @examples
#' \dontrun{
#' exim_search("Boeing")
#' exim_search("solar", fiscal_year = 2023)
#' }
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
#' Aggregates authorization data by country, showing deal counts and total
#' dollar amounts. Sorted by total approved amount descending.
#'
#' @param fiscal_year Integer or NULL. Filter to a specific fiscal year.
#' @param top_n Integer. Number of top countries to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{country}{Character. Country name.}
#'     \item{n_deals}{Integer. Number of authorization records.}
#'     \item{total_approved}{Numeric. Sum of approved/declined amounts in USD.}
#'     \item{total_disbursed}{Numeric. Sum of disbursed/shipped amounts in USD.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exim_by_country()
#' exim_by_country(fiscal_year = 2024, top_n = 10)
#' }
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
#' Aggregates all authorization data by fiscal year, showing deal counts
#' and total dollar amounts. Covers FY2007--FY2025.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fiscal_year}{Integer. Fiscal year (2007--2025).}
#'     \item{n_deals}{Integer. Number of authorization records.}
#'     \item{total_approved}{Numeric. Sum of approved/declined amounts in USD.}
#'     \item{total_disbursed}{Numeric. Sum of disbursed/shipped amounts in USD.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exim_by_year()
#' }
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
#' Aggregates authorization data by program type (Guarantee, Insurance,
#' Loan, Working Capital). Sorted by total approved amount descending.
#'
#' @param fiscal_year Integer or NULL. Filter to a specific fiscal year.
#' @return A tibble with columns:
#'   \describe{
#'     \item{program}{Character. Program type (e.g. "Guarantee", "Insurance", "Loan", "Working Capital").}
#'     \item{n_deals}{Integer. Number of authorization records.}
#'     \item{total_approved}{Numeric. Sum of approved/declined amounts in USD.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' exim_by_program()
#' exim_by_program(fiscal_year = 2024)
#' }
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

#' Get exim.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
exim_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(exim_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/exim.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "exim.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# exim.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# exim.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
