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
#' Returns a tibble describing the 14 major publicly available NRC
#' datasets covering reactor status, inspections, performance,
#' enforcement, and facilities.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- dataset name (e.g. \code{"reactor_status"},
#'       \code{"inspection_findings"})}
#'     \item{description}{character -- human-readable description}
#'     \item{url}{character -- download URL for the dataset}
#'     \item{format}{character -- file format: \code{"pipe-delimited"},
#'       \code{"TSV"}, or \code{"XLS"}}
#'   }
#' @examples
#' nrc_list()
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
#' Searches the NRC dataset catalog by name or description
#' (case-insensitive substring match).
#'
#' @param query Character. Search term (e.g. \code{"reactor"},
#'   \code{"enforcement"}, \code{"fuel"}, \code{"fire"}).
#' @return A tibble with columns: name, description, url, format
#'   (same schema as \code{nrc_list()}).
#' @examples
#' nrc_search("reactor")
#' nrc_search("enforcement")
nrc_search <- function(query) {
  datasets <- nrc_list()
  pattern <- tolower(query)
  matches <- grepl(pattern, tolower(datasets$name)) |
    grepl(pattern, tolower(datasets$description))
  datasets[matches, ]
}


#' Get daily reactor power status for the last 365 days
#'
#' Returns daily power output (as percent of capacity) for every
#' operating U.S. nuclear power reactor (~95 units) over the past
#' year. Approximately 34,000+ rows.
#'
#' @param unit Character or NULL. Reactor unit name filter (partial
#'   match, case-insensitive). Examples: \code{"Diablo Canyon"},
#'   \code{"Vogtle"}, \code{"Beaver Valley"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{report_date}{Date -- date of the status report}
#'     \item{unit}{character -- reactor unit name (e.g.
#'       \code{"Arkansas Nuclear 1"})}
#'     \item{power_pct}{integer -- power output as percent of
#'       rated capacity (0--100+)}
#'   }
#' @examples
#' nrc_reactor_status()
#' nrc_reactor_status(unit = "Diablo Canyon")
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
#' Returns the most recent day's power status for all ~95 operating
#' reactors. A convenience wrapper around \code{nrc_reactor_status()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{report_date}{Date -- the latest available report date}
#'     \item{unit}{character -- reactor unit name}
#'     \item{power_pct}{integer -- current power output as percent
#'       of rated capacity}
#'   }
#' @examples
#' nrc_reactor_status_current()
nrc_reactor_status_current <- function() {
  status <- nrc_reactor_status()
  if (nrow(status) == 0) return(.schema_reactor_status)
  latest <- max(status$report_date, na.rm = TRUE)
  status |> filter(.data$report_date == latest)
}


#' Get NRC inspection findings for operating reactors
#'
#' Returns inspection findings from the Reactor Oversight Process (ROP).
#' Each finding is color-coded by significance (Green < White < Yellow < Red)
#' and associated with safety cornerstones.
#'
#' @param site_name Character or NULL. Site name filter (partial match,
#'   case-insensitive). Examples: \code{"Palo Verde"}, \code{"Sequoyah"},
#'   \code{"Farley"}.
#' @param significance Character vector or NULL. Significance level filter.
#'   Valid values: \code{"Green"}, \code{"White"}, \code{"Yellow"},
#'   \code{"Red"}, \code{"Greater Than Green"}.
#' @param year Integer vector or NULL. Year(s) to filter (e.g. \code{2024}
#'   or \code{c(2023, 2024)}).
#' @return A tibble with 22 columns including:
#'   \describe{
#'     \item{site_name}{character -- nuclear plant site name}
#'     \item{significance}{character -- color-coded significance}
#'     \item{issue_date}{Date -- finding issue date}
#'     \item{cornerstone}{character -- safety cornerstone}
#'     \item{title}{character -- finding title}
#'     \item{report_number}{character -- inspection report number}
#'     \item{docket_number}{character -- NRC docket}
#'     \item{region}{integer -- NRC region (1--4)}
#'     \item{cross_cutting_aspect}{character -- cross-cutting aspect}
#'     \item{link}{character -- URL to the report}
#'   }
#' @examples
#' nrc_findings(year = 2024)
#' nrc_findings(site_name = "Vogtle", significance = "Green")
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
#' Returns performance indicator (PI) values from the Reactor
#' Oversight Process. Indicators cover safety cornerstones: initiating
#' events, mitigating systems, barrier integrity, emergency
#' preparedness, occupational radiation, and public radiation safety.
#'
#' @param docket Character or NULL. Docket number to filter
#'   (e.g. \code{"05000311"}). If NULL, returns all dockets (~10,000+ rows).
#' @return A tibble with 19 columns including:
#'   \describe{
#'     \item{date}{character -- reporting period}
#'     \item{docket}{character -- NRC docket number}
#'     \item{bi01--or01}{numeric -- performance indicator values for
#'       various safety cornerstones}
#'   }
#' @examples
#' nrc_performance_indicators(docket = "05000311")
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
#' Returns the action matrix column assignment for all reactor
#' dockets across all quarters. The action matrix describes the
#' NRC's regulatory response based on plant performance.
#'
#' Columns 1--5 represent increasing NRC oversight:
#' 1 = Licensee Response, 2 = Regulatory Response,
#' 3 = Degraded Cornerstone, 4 = Multiple/Repetitive Degraded,
#' 5 = Unacceptable Performance.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{docket}{character -- NRC docket number (e.g. \code{"05000277"})}
#'     \item{column}{integer -- action matrix column (1--5)}
#'     \item{quarter}{character -- reporting quarter (e.g. \code{"2024Q3"})}
#'   }
#' @examples
#' nrc_action_matrix()
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


#' Summarize inspection findings by site
#'
#' Returns an aggregate count of inspection findings per site,
#' broken down by significance color. Sorted by total findings
#' descending.
#'
#' @param year Integer vector or NULL. Year(s) to filter
#'   (e.g. \code{2024} or \code{c(2022, 2023, 2024)}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{site_name}{character -- nuclear plant site name}
#'     \item{total}{integer -- total findings}
#'     \item{green}{integer -- count of Green (very low safety significance) findings}
#'     \item{white}{integer -- count of White (low to moderate) findings}
#'     \item{yellow}{integer -- count of Yellow (substantial) findings}
#'     \item{red}{integer -- count of Red (high) findings}
#'     \item{greater_than_green}{integer -- count of "Greater Than Green" findings}
#'   }
#' @examples
#' nrc_findings_summary(year = 2024)
#' nrc_findings_summary()
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

#' Get nrc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nrc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nrc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nrc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nrc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nrc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nrc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
