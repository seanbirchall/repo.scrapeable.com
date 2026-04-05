# justice.gov.R
# Self-contained U.S. Department of Justice open-data client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public endpoints)
# Docs: https://www.justice.gov/developer


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
#' Returns a catalogue of known machine-readable datasets published by
#' the U.S. Department of Justice, spanning IT governance, antitrust
#' enforcement, bankruptcy statistics, and USTP enforcement actions.
#'
#' @param category Character or NULL. Filter by category: `"IT"`,
#'   `"Bankruptcy"`, `"Antitrust"`, or `"Enforcement"`. Default `NULL`
#'   returns all datasets.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Dataset title.}
#'     \item{category}{Character. Thematic category.}
#'     \item{format}{Character. File format (`"JSON"`, `"CSV"`, `"XLS"`, `"XLSX"`).}
#'     \item{url}{Character. Direct download URL.}
#'   }
#'
#' @family doj discovery
#' @seealso [doj_search()] for keyword search, [doj_it_leadership()] and
#'   other named accessors for parsed data
#'
#' @examples
#' \dontrun{
#' doj_list()
#' doj_list(category = "Antitrust")
#' }
#' @export
doj_list <- function(category = NULL) {
  out <- .doj_datasets
  if (!is.null(category)) {
    out <- out |> filter(tolower(.data$category) == tolower(category))
  }
  out
}

#' Search DOJ datasets by keyword
#'
#' Performs a case-insensitive keyword search across dataset names and
#' categories in the DOJ catalogue. Returns all matching entries.
#'
#' @param query Character. Search term (e.g. `"bankruptcy"`, `"IT"`,
#'   `"enforcement"`).
#'
#' @return A tibble with columns: name, category, format, url.
#'   Same structure as [doj_list()], filtered to matching rows.
#'
#' @family doj discovery
#' @seealso [doj_list()] for the full catalogue
#'
#' @examples
#' \dontrun{
#' doj_search("enforcement")
#' doj_search("IT")
#' }
#' @export
doj_search <- function(query) {
  q <- tolower(query)
  .doj_datasets |>
    filter(grepl(q, tolower(name)) | grepl(q, tolower(category)))
}

#' Get DOJ IT Leadership (Bureau Directory)
#'
#' Downloads and parses the DOJ Digital Strategy bureau directory JSON
#' feed, returning the current CIO leadership roster with contact details
#' and appointment information.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{bureau_code}{Character. Bureau numeric code.}
#'     \item{first_name}{Character. First name.}
#'     \item{last_name}{Character. Last name.}
#'     \item{employment_type}{Character. e.g. `"GS"`, `"SES"`.}
#'     \item{appointment_type}{Character. e.g. `"Career"`.}
#'     \item{email}{Character. Official email address.}
#'     \item{component}{Character. DOJ component name.}
#'     \item{title}{Character. Official job title.}
#'     \item{key_bureau_cio}{Character. Whether this is a key bureau CIO.}
#'   }
#'
#' @family doj data
#' @seealso [doj_governance_boards()], [doj_cost_savings()]
#'
#' @examples
#' \dontrun{
#' doj_it_leadership()
#' }
#' @export
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
#' Downloads and parses the DOJ governance boards JSON feed, showing
#' which IT governance boards exist and how the CIO is involved in each.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{board_name}{Character. Name of the governance board.}
#'     \item{bureau_code}{Character. Bureau numeric code.}
#'     \item{cio_involvement}{Character. Description of CIO's role.}
#'   }
#'
#' @family doj data
#' @seealso [doj_it_leadership()], [doj_cost_savings()]
#'
#' @examples
#' \dontrun{
#' doj_governance_boards()
#' }
#' @export
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
#' Downloads and parses the DOJ Digital Strategy cost savings JSON feed,
#' returning realized IT cost savings and avoidance strategies with
#' OMB initiative details.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{strategy_id}{Integer. Unique strategy identifier.}
#'     \item{strategy_title}{Character. Title of the cost savings strategy.}
#'     \item{decision_date}{Character. Date the decision was made.}
#'     \item{omb_initiative}{Character. Related OMB initiative.}
#'     \item{amount_type}{Character. Whether savings or avoidance.}
#'     \item{use_of_savings}{Character. How savings were redeployed.}
#'   }
#'
#' @family doj data
#' @seealso [doj_it_leadership()], [doj_governance_boards()]
#'
#' @examples
#' \dontrun{
#' doj_cost_savings()
#' }
#' @export
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
#' Downloads and parses the bankruptcy interpreter services CSV data
#' from the DOJ website. Shows language assistance requests by location,
#' language, and time period.
#'
#' @param url Character or NULL. Override the CSV URL. Defaults to the
#'   latest available file (FY2024).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{location}{Character. Location where service was provided.}
#'     \item{language}{Character. Language of the interpreter.}
#'     \item{count}{Integer. Number of requests.}
#'     \item{month}{Integer. Month number.}
#'     \item{year}{Integer. Year.}
#'   }
#'
#' @family doj data
#' @seealso [doj_lap_files()] to discover available LAP CSV URLs
#'
#' @examples
#' \dontrun{
#' doj_language_assistance()
#' }
#' @export
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
#' Scrapes the USTP Language Assistance Program data page to extract
#' all available CSV download links, organized by reporting period.
#' Use the returned URLs with [doj_language_assistance()] to fetch
#' individual datasets.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{period}{Character. Reporting period label.}
#'     \item{url}{Character. Absolute download URL for the CSV file.}
#'   }
#'
#' @family doj discovery
#' @seealso [doj_language_assistance()] to download and parse a specific file
#'
#' @examples
#' \dontrun{
#' doj_lap_files()
#' }
#' @export
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
#' Scrapes the USTP Chapter 7 Trustee Final Reports page for all
#' downloadable data files in XLS, XLSX, and CSV format. Returns
#' filenames and absolute download URLs.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{filename}{Character. Base filename of the download.}
#'     \item{format}{Character. File format (`"CSV"`, `"XLSX"`, `"XLS"`).}
#'     \item{url}{Character. Absolute download URL.}
#'   }
#'
#' @family doj discovery
#' @seealso [doj_ch7_data()] to download and parse a specific CSV file
#'
#' @examples
#' \dontrun{
#' doj_ch7_files()
#' }
#' @export
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
#' Downloads and parses a Chapter 7 trustee final report CSV file from
#' the USTP website. Contains case-level data on trustee activities,
#' distributions, and administrative details.
#'
#' @param year Integer. Calendar year of the data file (e.g. `2016`).
#'   Default 2016 (latest known CSV).
#'
#' @return A tibble with columns from the CSV file (varies by year).
#'   Returns an empty tibble if the download fails.
#'
#' @family doj data
#' @seealso [doj_ch7_files()] to discover available years and formats
#'
#' @examples
#' \dontrun{
#' doj_ch7_data(2016)
#' }
#' @export
doj_ch7_data <- function(year = 2016) {
  url <- paste0(.doj_base, "/ust/file/ch7data", year, ".csv/download")
  .doj_fetch_csv(url)
}

#' List USTP enforcement activity data files
#'
#' Scrapes the USTP enforcement activity statistics page for downloadable
#' XLSX and XLS files covering formal enforcement actions, informal
#' enforcement actions, litigation outcomes, and potential financial impact.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{filename}{Character. Base filename of the download.}
#'     \item{format}{Character. File format (`"XLSX"` or `"XLS"`).}
#'     \item{url}{Character. Absolute download URL.}
#'   }
#'
#' @family doj discovery
#'
#' @examples
#' \dontrun{
#' doj_enforcement_files()
#' }
#' @export
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

#' Get justice.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
doj_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(doj_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/justice.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "justice.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# justice.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# justice.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
