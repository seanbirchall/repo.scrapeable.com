# justice.gov.R
# Self-contained U.S. Department of Justice open-data client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public endpoints)
# Docs: https://www.justice.gov/developer

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.doj_base <- "https://www.justice.gov"

.doj_fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.doj_fetch_csv <- function(url) {

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  if (!file.exists(tmp) || file.size(tmp) == 0) return(tibble())
  tryCatch(
    as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)),
    error = function(e) tibble()
  )
}

.doj_fetch_html_links <- function(url, pattern = NULL) {
  tmp <- tempfile(fileext = ".html")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  html <- readLines(tmp, warn = FALSE)
  full <- paste(html, collapse = "\n")
  # extract href values
  hrefs <- regmatches(full, gregexpr('href="([^"]*)"', full))[[1]]
  hrefs <- sub('^href="', '', sub('"$', '', hrefs))
  if (!is.null(pattern)) hrefs <- hrefs[grepl(pattern, hrefs, ignore.case = TRUE)]
  hrefs
}

# == Datasets catalogue ========================================================

.doj_datasets <- tibble(
  name = c(
    "IT Leadership (Bureau Directory)",
    "CIO Governance Boards",
    "IT Cost Savings and Avoidance",
    "Language Assistance Program (Latest)",
    "Antitrust Appropriation Figures",
    "Antitrust Sherman Act Violations ($10M+ Fines)",
    "Chapter 7 Trustee Final Reports (Latest CSV)",
    "Formal Enforcement Actions (Latest)",
    "Informal Enforcement Actions (Latest)",
    "Litigation Outcomes (Latest)",
    "Potential Financial Impact (Latest)"
  ),
  category = c(
    "IT", "IT", "IT",
    "Bankruptcy", "Antitrust", "Antitrust",
    "Bankruptcy", "Enforcement", "Enforcement",
    "Enforcement", "Enforcement"
  ),
  format = c(
    "JSON", "JSON", "JSON",
    "CSV", "XLS", "XLS",
    "CSV", "XLSX", "XLSX",
    "XLSX", "XLSX"
  ),
  url = c(
    "https://www.justice.gov/digitalstrategy/bureaudirectory.json",
    "https://www.justice.gov/digitalstrategy/governanceboards.json",
    "https://www.justice.gov/digitalstrategy/costsavings.json",
    "https://www.justice.gov/media/1380551/dl",
    "https://www.justice.gov/atr/file/788491/dl?inline",
    "https://www.justice.gov/atr/file/790681/dl?inline",
    "https://www.justice.gov/ust/file/ch7data2016.csv/download",
    "https://www.justice.gov/ust/file/Formal_Enforcement_Actions_FY13_19_1.xlsx/download",
    "https://www.justice.gov/ust/file/Informal_Enforcement_Actions_FY13_19_1.xlsx/download",
    "https://www.justice.gov/ust/file/Litigation_Outcomes_FY13_FY19_1.xlsx/download",
    "https://www.justice.gov/ust/file/Potential_Financial_Impact_FY13_19_1.xlsx/download"
  )
)

# == Public functions ==========================================================

#' List available DOJ open datasets
#'
#' Returns a tibble of known machine-readable datasets from justice.gov,
#' including JSON, CSV, and XLS/XLSX resources.
#'
#' @param category Optional filter: "IT", "Bankruptcy", "Antitrust", "Enforcement"
#' @return tibble with name, category, format, url
doj_list <- function(category = NULL) {
  out <- .doj_datasets
  if (!is.null(category)) {
    out <- out |> filter(tolower(.data$category) == tolower(category))
  }
  out
}

#' Search DOJ datasets by keyword
#'
#' Simple keyword search across dataset names and categories.
#'
#' @param query character search term
#' @return tibble of matching datasets
doj_search <- function(query) {
  q <- tolower(query)
  .doj_datasets |>
    filter(grepl(q, tolower(name)) | grepl(q, tolower(category)))
}

#' Get DOJ IT Leadership (Bureau Directory)
#'
#' Returns the Department of Justice bureau CIO directory as a tibble.
#'
#' @return tibble with bureau_code, first_name, last_name, employment_type,
#'   appointment_type, email, component, title, key_bureau_cio
doj_it_leadership <- function() {
  raw <- .doj_fetch_json("https://www.justice.gov/digitalstrategy/bureaudirectory.json")
  if (is.null(raw) || length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  result <- tibble(
    bureau_code      = as.character(df[["Bureau Code"]]         %||% NA),
    first_name       = as.character(df[["First Name"]]          %||% NA),
    last_name        = as.character(df[["Last Name"]]           %||% NA),
    employment_type  = as.character(df[["Employment Type"]]     %||% NA),
    appointment_type = as.character(df[["Type Of Appointment"]] %||% NA),
    email            = as.character(df[["Email Address"]]       %||% NA),
    component        = as.character(df[["Component"]]           %||% NA),
    title            = as.character(df[["Official Title"]]      %||% NA),
    key_bureau_cio   = as.character(df[["Key Bureau CIO"]]      %||% NA)
  )
  result
}

#' Get DOJ CIO Governance Boards
#'
#' Returns a tibble of governance boards and CIO involvement.
#'
#' @return tibble with board_name, bureau_code, cio_involvement
doj_governance_boards <- function() {
  raw <- .doj_fetch_json("https://www.justice.gov/digitalstrategy/governanceboards.json")
  if (is.null(raw) || length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  tibble(
    board_name      = as.character(df[["Governance Board Name"]]       %||% NA),
    bureau_code     = as.character(df[["Bureau Code"]]                 %||% NA),
    cio_involvement = as.character(df[["CIO Involvement Description"]] %||% NA)
  )
}

#' Get DOJ IT Cost Savings and Avoidance data
#'
#' Returns IT cost savings strategies and fiscal year amounts.
#'
#' @return tibble with strategy_id, strategy_title, decision_date,
#'   omb_initiative, amount_type, use_of_savings
doj_cost_savings <- function() {
  raw <- .doj_fetch_json("https://www.justice.gov/digitalstrategy/costsavings.json")
  if (is.null(raw) || length(raw) == 0) return(tibble())
  items <- raw[["realizedSavingsOrAvoidances"]]
  if (is.null(items) || length(items) == 0) return(tibble())
  # items is a data.frame with nested list columns
  tibble(
    strategy_id    = as.integer(items[["strategyId"]]            %||% NA),
    strategy_title = as.character(items[["strategyTitle"]]        %||% NA),
    decision_date  = as.character(items[["decisionDate"]]         %||% NA),
    omb_initiative = as.character(items[["ombInitiative"]]        %||% NA),
    amount_type    = as.character(items[["amountType"]]           %||% NA),
    use_of_savings = as.character(items[["useOfSavingsAvoidance"]] %||% NA)
  )
}

#' Get DOJ Language Assistance Program data
#'
#' Returns bankruptcy interpreter services data. Fetches the latest annual
#' CSV file from the DOJ website.
#'
#' @param url Optional: override the CSV URL. Defaults to latest (FY2024).
#' @return tibble with location, language, count, month, year
doj_language_assistance <- function(url = NULL) {
  url <- url %||% "https://www.justice.gov/media/1380551/dl"
  raw <- .doj_fetch_csv(url)
  if (nrow(raw) == 0) return(tibble())
  # The CSV has generic column names (Var1-Var5)
  if (ncol(raw) >= 5) {
    names(raw) <- c("location", "language", "count", "month", "year")
    raw <- raw |>
      mutate(
        count = suppressWarnings(as.integer(count)),
        month = suppressWarnings(as.integer(month)),
        year  = suppressWarnings(as.integer(year))
      )
  }
  raw
}

#' List DOJ Language Assistance Program CSV file URLs
#'
#' Scrapes the LAP data page to extract all available monthly/yearly CSV links.
#'
#' @return tibble with period, url
doj_lap_files <- function() {
  page_url <- "https://www.justice.gov/ust/bankruptcy-data-statistics/language-assistance-program-data"
  tmp <- tempfile(fileext = ".html")
  httr2::request(page_url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Extract CSV links with their titles
  pattern <- 'title="([^"]*)"[^>]*>\\[CSV'
  matches <- gregexpr(pattern, html, perl = TRUE)
  m <- regmatches(html, matches)[[1]]
  titles <- sub('^title="([^"]*)".*', '\\1', m)

  # Extract corresponding href values
  pattern2 <- 'href="([^"]*)"[^>]*title="[^"]*"[^>]*>\\[CSV'
  matches2 <- gregexpr(pattern2, html, perl = TRUE)
  m2 <- regmatches(html, matches2)[[1]]
  urls <- sub('^href="([^"]*)".*', '\\1', m2)

  if (length(titles) == 0 || length(urls) == 0) return(tibble())

  # Make URLs absolute
  urls <- ifelse(startsWith(urls, "http"), urls, paste0(.doj_base, urls))

  tibble(
    period = titles,
    url = urls
  )
}

#' List Chapter 7 Trustee Final Report data files
#'
#' Scrapes the Chapter 7 Trustee Final Reports page for downloadable data
#' files (XLS, XLSX, CSV).
#'
#' @return tibble with filename, format, url
doj_ch7_files <- function() {
  page_url <- "https://www.justice.gov/ust/bankruptcy-data-statistics/chapter-7-trustee-final-reports"
  links <- .doj_fetch_html_links(page_url, pattern = "\\.(xls|xlsx|csv)")
  if (length(links) == 0) return(tibble())
  links <- unique(links)
  urls <- ifelse(startsWith(links, "http"), links, paste0(.doj_base, links))
  fmt <- ifelse(grepl("\\.csv", links, ignore.case = TRUE), "CSV",
         ifelse(grepl("\\.xlsx", links, ignore.case = TRUE), "XLSX", "XLS"))
  tibble(
    filename = basename(sub("\\?.*$", "", sub("/download$", "", links))),
    format = fmt,
    url = urls
  )
}

#' Get Chapter 7 Trustee Final Report CSV data
#'
#' Downloads and parses a specific Chapter 7 CSV data file.
#'
#' @param year Calendar year (e.g. 2016). Defaults to 2016 (latest CSV).
#' @return tibble of Chapter 7 trustee final report data
doj_ch7_data <- function(year = 2016) {
  url <- paste0(.doj_base, "/ust/file/ch7data", year, ".csv/download")
  .doj_fetch_csv(url)
}

#' List USTP enforcement activity data files
#'
#' Scrapes the USTP enforcement activity page for downloadable XLSX files.
#'
#' @return tibble with filename, format, url
doj_enforcement_files <- function() {
  page_url <- "https://www.justice.gov/ust/bankruptcy-data-statistics/ustp-enforcement-activity"
  links <- .doj_fetch_html_links(page_url, pattern = "\\.(xls|xlsx)")
  if (length(links) == 0) return(tibble())
  links <- unique(links)
  urls <- ifelse(startsWith(links, "http"), links, paste0(.doj_base, links))
  fmt <- ifelse(grepl("\\.xlsx", links, ignore.case = TRUE), "XLSX", "XLS")
  tibble(
    filename = basename(sub("\\?.*$", "", sub("/download$", "", links))),
    format = fmt,
    url = urls
  )
}

# == Context ===================================================================

#' Return full source of all public functions in this file
#'
#' Reads its own source file and extracts all public function bodies
#' (with roxygen comments). Useful for LLM context injection.
#'
#' @return character string of all public function source code
doj_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/justice.gov.R"
  if (!file.exists(src_file)) {
    cat("# justice.gov context - source not found\n")
    return(invisible("# justice.gov context - source not found"))
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
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) -
        nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- paste(c(rox, body), collapse = "\n")
  }
  out <- paste(blocks, collapse = "\n\n")
  cat(out, "\n")
  invisible(out)
}
