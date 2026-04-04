# nrc.gov.R - Self-contained NRC (Nuclear Regulatory Commission) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required (public data)
# Data sources: TSV/pipe-delimited files from nrc.gov

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nrc_base <- "https://www.nrc.gov"
.nrc_cdn  <- "https://www.nrc.gov/cdn/data/rango"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".txt") {
  tmp <- tempfile(fileext = ext)
  for (attempt in 1:4) {
    ok <- tryCatch({
      httr2::request(url) |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_timeout(seconds = 60) |>
        httr2::req_perform(path = tmp)
      TRUE
    }, error = function(e) {
      if (attempt < 4) { Sys.sleep(attempt); FALSE }
      else stop(e)
    })
    if (ok) break
  }
  tmp
}

.fetch_tsv <- function(url) {
  f <- .fetch(url, ext = ".tsv")
  utils::read.delim(f, sep = "\t", stringsAsFactors = FALSE,
                    check.names = FALSE, na.strings = c("", "NA"))
}

.fetch_pipe <- function(url) {
  f <- .fetch(url, ext = ".txt")
  utils::read.delim(f, sep = "|", stringsAsFactors = FALSE,
                    check.names = FALSE, na.strings = c("", "NA"))
}

.safe_date <- function(x, fmt = "%m/%d/%Y") {
  suppressWarnings(as.Date(x, format = fmt))
}

# == Schemas ===================================================================

.schema_reactor_status <- tibble(
  report_date = as.Date(character()), unit = character(), power_pct = integer()
)

.schema_findings <- tibble(
  procedure = character(), issue_date = as.Date(character()),
  report_number = character(), type = character(),
  cornerstone_code = character(), cornerstone = character(),
  title = character(), docket_number = character(),
  site_name = character(), site_code = character(),
  cross_cutting_aspect = character(), identified_by = character(),
  significance = character(), region = integer(),
  accession_number = character(), link = character(),
  is_traditional_enforcement = logical(),
  severity_type_code = character(),
  cornerstone_attribute_type = character(),
  year = integer(), quarter = integer(),
  cross_cutting_area = character()
)

.schema_pi <- tibble(
  date = character(), docket = character()
)

# == Public functions ==========================================================

#' List available NRC datasets
#'
#' Returns a tibble describing the major publicly available NRC datasets
#' with their names, descriptions, and URLs.
#'
#' @return tibble: name, description, url, format
nrc_list <- function() {
  tibble(
    name = c(
      "reactor_status",
      "inspection_findings",
      "performance_indicators",
      "action_matrix",
      "operating_reactors",
      "decommissioning_reactors",
      "cancelled_reactors",
      "research_test_reactors",
      "enforcement_actions",
      "generic_issues",
      "fire_events",
      "fuel_cycle_facilities",
      "material_licenses",
      "spent_fuel_storage"
    ),
    description = c(
      "Daily power reactor status for last 365 days",
      "Inspection findings for operating reactors",
      "Performance indicators for operating reactors",
      "Reactor oversight action matrix history",
      "Operating commercial nuclear power reactors",
      "Reactors undergoing decommissioning",
      "Cancelled U.S. commercial nuclear power reactors",
      "Operating research and test reactors",
      "Significant NRC enforcement actions",
      "Generic safety issues since 1978",
      "Fire event data from licensee event reports",
      "Major U.S. fuel cycle facility sites",
      "Material licenses by state",
      "Dry cask spent fuel storage licensees"
    ),
    url = c(
      paste0(.nrc_base, "/reading-rm/doc-collections/event-status/reactor-status/PowerReactorStatusForLast365Days.txt"),
      paste0(.nrc_cdn, "/findings_data.tsv"),
      paste0(.nrc_cdn, "/all_pi_values.tsv"),
      paste0(.nrc_cdn, "/action_matrix_all.tsv"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/reactors-operating.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/reactors-decommissioning.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/reactors-canceled.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/reactors-research-test.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/enforcement-actions.xls"),
      paste0(.nrc_base, "/about-nrc/regulatory/gen-issues/generic-issues-dataset.xls"),
      paste0(.nrc_base, "/reactors/operating/oversight/fire-event-data.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/materials-fuel-cycle-facilities.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/materials-licenses-state.xls"),
      paste0(.nrc_base, "/reading-rm/doc-collections/datasets/waste-spent-fuel-storage-dry-cask.xls")
    ),
    format = c("pipe-delimited", "TSV", "TSV", "TSV",
               "XLS", "XLS", "XLS", "XLS", "XLS", "XLS",
               "XLS", "XLS", "XLS", "XLS")
  )
}


#' Search NRC datasets by keyword
#'
#' Searches the NRC dataset catalog by name or description.
#'
#' @param query Character string to search for (case-insensitive).
#' @return tibble: matching datasets with name, description, url, format
nrc_search <- function(query) {
  datasets <- nrc_list()
  pattern <- tolower(query)
  matches <- grepl(pattern, tolower(datasets$name)) |
    grepl(pattern, tolower(datasets$description))
  datasets[matches, ]
}


#' Get daily reactor power status for the last 365 days
#'
#' Returns a tibble with daily power output (as percent of capacity)
#' for every operating U.S. nuclear power reactor over the past year.
#'
#' @param unit Optional reactor unit name to filter (partial match, case-insensitive).
#' @return tibble: report_date (Date), unit (character), power_pct (integer)
nrc_reactor_status <- function(unit = NULL) {
  url <- paste0(.nrc_base,
    "/reading-rm/doc-collections/event-status/reactor-status/PowerReactorStatusForLast365Days.txt")
  raw <- .fetch_pipe(url)

  if (nrow(raw) == 0) return(.schema_reactor_status)

  result <- tibble(
    report_date = .safe_date(
      sub(" \\d+:\\d+:\\d+ [AP]M$", "", raw$ReportDt),
      fmt = "%m/%d/%Y"
    ),
    unit       = as.character(raw$Unit),
    power_pct  = as.integer(raw$Power)
  )

  if (!is.null(unit)) {
    pat <- tolower(unit)
    result <- result |>
      filter(grepl(pat, tolower(.data$unit), fixed = FALSE))
  }
  result
}


#' Get current reactor status (latest date only)
#'
#' Returns today's power status for all operating reactors.
#'
#' @return tibble: unit (character), power_pct (integer), report_date (Date)
nrc_reactor_status_current <- function() {
  status <- nrc_reactor_status()
  if (nrow(status) == 0) return(.schema_reactor_status)
  latest <- max(status$report_date, na.rm = TRUE)
  status |> filter(.data$report_date == latest)
}


#' Get NRC inspection findings for operating reactors
#'
#' Returns inspection findings from the Reactor Oversight Process.
#' Data includes significance color, cornerstone, site, and cross-cutting aspects.
#'
#' @param site_name Optional site name to filter (partial match, case-insensitive).
#' @param significance Optional significance level filter (e.g., "Green", "White",
#'   "Yellow", "Red", "Greater Than Green").
#' @param year Optional year(s) to filter (integer vector).
#' @return tibble with 22 columns of finding details
nrc_findings <- function(site_name = NULL, significance = NULL, year = NULL) {
  raw <- .fetch_tsv(paste0(.nrc_cdn, "/findings_data.tsv"))

  if (nrow(raw) == 0) return(.schema_findings)

  result <- tibble(
    procedure              = as.character(raw$Procedure),
    issue_date             = .safe_date(raw$`Issue Date`),
    report_number          = as.character(raw$`Report Number`),
    type                   = as.character(raw$Type),
    cornerstone_code       = as.character(raw$CornerstoneCode),
    cornerstone            = as.character(raw$Cornerstone),
    title                  = as.character(raw$Title),
    docket_number          = as.character(raw$`Docket Number`),
    site_name              = as.character(raw$`Site Name`),
    site_code              = as.character(raw$`Site Code`),
    cross_cutting_aspect   = as.character(raw$CrossCuttingAspect),
    identified_by          = as.character(raw$Idby),
    significance           = as.character(raw$Significance),
    region                 = as.integer(raw$Region),
    accession_number       = as.character(raw$`Accession Number`),
    link                   = as.character(raw$Link),
    is_traditional_enforcement = as.logical(toupper(raw$IsTraditionalEnforcement)),
    severity_type_code     = as.character(raw$`Item Severity Type Code`),
    cornerstone_attribute_type = as.character(raw$`Cornerstone Attribute Type`),
    year                   = as.integer(raw$Year),
    quarter                = as.integer(raw$Quarter),
    cross_cutting_area     = as.character(raw$`CrossCutting Area`)
  )

  if (!is.null(site_name)) {
    pat <- tolower(site_name)
    result <- result |>
      filter(grepl(pat, tolower(.data$site_name), fixed = FALSE))
  }
  if (!is.null(significance)) {
    result <- result |>
      filter(tolower(.data$significance) %in% tolower(significance))
  }
  if (!is.null(year)) {
    result <- result |>
      filter(.data$year %in% as.integer(year))
  }
  result
}


#' Get performance indicator values for operating reactors
#'
#' Returns performance indicator values from the Reactor Oversight Process.
#' Indicators cover safety cornerstones: initiating events, mitigating systems,
#' barrier integrity, emergency preparedness, occupational radiation, public radiation.
#'
#' @param docket Optional docket number to filter (e.g., "05000311").
#' @return tibble with PI indicator columns plus date and docket
nrc_performance_indicators <- function(docket = NULL) {
  raw <- .fetch_tsv(paste0(.nrc_cdn, "/all_pi_values.tsv"))

  if (nrow(raw) == 0) return(.schema_pi)

  # Clean column names
  cols <- names(raw)
  result <- tibble::as_tibble(raw)
  names(result) <- tolower(names(result))

  # Convert numeric columns
  for (col in setdiff(names(result), c("date", "docket"))) {
    result[[col]] <- suppressWarnings(as.numeric(result[[col]]))
  }
  result$docket <- as.character(result$docket)
  result$date   <- as.character(result$date)

  if (!is.null(docket)) {
    result <- result |>
      filter(.data$docket == as.character(docket))
  }
  result
}


#' Get sites and their current NRC action matrix status
#'
#' Returns a summary of which action matrix column each reactor site
#' is in for the most recent quarter. The action matrix describes
#' the NRC's regulatory response based on plant performance.
#'
#' Columns 1-5 represent increasing NRC oversight:
#' 1 = Licensee Response, 2 = Regulatory Response,
#' 3 = Degraded Cornerstone, 4 = Multiple/Repetitive Degraded,
#' 5 = Unacceptable Performance.
#'
#' @return tibble: docket, column (action matrix column 1-5), quarter
nrc_action_matrix <- function() {
  raw <- .fetch_tsv(paste0(.nrc_cdn, "/action_matrix_all.tsv"))

  if (nrow(raw) == 0 || ncol(raw) == 0) {
    return(tibble(docket = character(), column = integer(), quarter = character()))
  }

  # The TSV has docket numbers as column headers and the last column is the quarter
  # Each row is a quarter, values are the action matrix column (1-5)
  n <- ncol(raw)
  dockets <- names(raw)[1:(n - 1)]
  quarters <- raw[[n]]

  rows <- lapply(seq_len(nrow(raw)), function(i) {
    tibble(
      docket  = dockets,
      column  = as.integer(unlist(raw[i, 1:(n - 1)])),
      quarter = as.character(quarters[i])
    )
  })
  bind_rows(rows)
}


#' Summarize findings by site
#'
#' Returns a summary tibble showing count of findings per site,
#' broken down by significance level.
#'
#' @param year Optional year(s) to filter.
#' @return tibble: site_name, total, green, white, yellow, red, greater_than_green
nrc_findings_summary <- function(year = NULL) {
  findings <- nrc_findings(year = year)
  if (nrow(findings) == 0) {
    return(tibble(
      site_name = character(), total = integer(),
      green = integer(), white = integer(),
      yellow = integer(), red = integer(),
      greater_than_green = integer()
    ))
  }

  findings |>
    group_by(.data$site_name) |>
    summarise(
      total = n(),
      green = sum(tolower(.data$significance) == "green", na.rm = TRUE),
      white = sum(tolower(.data$significance) == "white", na.rm = TRUE),
      yellow = sum(tolower(.data$significance) == "yellow", na.rm = TRUE),
      red = sum(tolower(.data$significance) == "red", na.rm = TRUE),
      greater_than_green = sum(tolower(.data$significance) == "greater than green", na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(.data$total))
}


#' Return full function source for LLM context
#'
#' Reads and returns the source code of this client file,
#' suitable for providing as context to an LLM.
#'
#' @return Character string with the complete source code
nrc_context <- function() {
  src <- tryCatch(
    {
      f <- sys.frame(0)$ofile
      if (is.null(f)) {
        f <- this.path::this.path()
      }
      paste(readLines(f), collapse = "\n")
    },
    error = function(e) {
      f <- system.file("source", "nrc.R", package = "nrc.gov")
      if (nzchar(f)) paste(readLines(f), collapse = "\n")
      else "Source not available"
    }
  )
  src
}
