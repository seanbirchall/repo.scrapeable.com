# neh.gov.R - National Endowment for the Humanities open data client
#
# Data source: https://apps.neh.gov/open/data
# Datasets:
#   - NEH Grant data by decade (1960s-2020s) as CSV
#   - NEH Peer-Review Evaluators (1970-present) as CSV
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.neh_base <- "https://apps.neh.gov/open/data"

`%||%` <- function(x, y) if (is.null(x)) y else x

.neh_decades <- c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")

.neh_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  tryCatch({
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_timeout(120) |>
      httr2::req_perform(path = tmp)
    # NEH CSVs use standard comma-delimited format
    utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    warning("Failed to fetch ", url, ": ", conditionMessage(e))
    NULL
  })
}

# == Schemas ===================================================================

.schema_grants <- tibble::tibble(
  app_number           = character(),
  applicant_type       = character(),
  institution          = character(),
  organization_type    = character(),
  city                 = character(),
  state                = character(),
  postal_code          = character(),
  country              = character(),
  congressional_district = character(),
  latitude             = double(),
  longitude            = double(),
  council_date         = as.Date(character()),
  year_awarded         = integer(),
  project_title        = character(),
  program              = character(),
  division             = character(),
  approved_outright    = double(),
  approved_matching    = double(),
  award_outright       = double(),
  award_matching       = double(),
  original_amount      = double(),
  supplement_amount    = double(),
  begin_grant          = as.Date(character()),
  end_grant            = as.Date(character()),
  project_desc         = character(),
  to_support           = character(),
  primary_discipline   = character(),
  supplement_count     = integer(),
  participant_count    = integer(),
  discipline_count     = integer()
)

.schema_evaluators <- tibble::tibble(
  id              = integer(),
  name_prefix     = character(),
  firstname       = character(),
  middlename      = character(),
  lastname        = character(),
  name_suffix     = character(),
  title           = character(),
  department      = character(),
  institution     = character(),
  city            = character(),
  state           = character(),
  postal_code     = character(),
  country         = character(),
  start_date      = as.Date(character()),
  end_date        = as.Date(character()),
  fiscal_year     = integer()
)

# == Internal parsing ==========================================================

.neh_parse_grants <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(.schema_grants)

  # Standardize column names
  cn <- names(raw)
  cn <- tolower(cn)
  cn <- gsub("\\s+", "_", cn)
  names(raw) <- cn

  .safe_date <- function(x) {
    if (is.null(x)) return(as.Date(NA))
    tryCatch(as.Date(x, format = "%m/%d/%Y"), error = function(e) as.Date(NA))
  }

  tibble::tibble(
    app_number           = as.character(raw$appnumber %||% raw$app_number %||% NA),
    applicant_type       = as.character(raw$applicanttype %||% raw$applicant_type %||% NA),
    institution          = as.character(raw$institution %||% NA),
    organization_type    = as.character(raw$organizationtype %||% raw$organization_type %||% NA),
    city                 = as.character(raw$instcity %||% raw$inst_city %||% NA),
    state                = as.character(raw$inststate %||% raw$inst_state %||% NA),
    postal_code          = as.character(raw$instpostalcode %||% raw$inst_postal_code %||% NA),
    country              = as.character(raw$instcountry %||% raw$inst_country %||% NA),
    congressional_district = as.character(raw$congressionaldistrict %||% raw$congressional_district %||% NA),
    latitude             = suppressWarnings(as.double(raw$latitude %||% NA)),
    longitude            = suppressWarnings(as.double(raw$longitude %||% NA)),
    council_date         = .safe_date(raw$councildate %||% raw$council_date %||% NA),
    year_awarded         = suppressWarnings(as.integer(raw$yearawarded %||% raw$year_awarded %||% NA)),
    project_title        = as.character(raw$projecttitle %||% raw$project_title %||% NA),
    program              = as.character(raw$program %||% NA),
    division             = as.character(raw$division %||% NA),
    approved_outright    = suppressWarnings(as.double(raw$approvedoutright %||% raw$approved_outright %||% NA)),
    approved_matching    = suppressWarnings(as.double(raw$approvedmatching %||% raw$approved_matching %||% NA)),
    award_outright       = suppressWarnings(as.double(raw$awardoutright %||% raw$award_outright %||% NA)),
    award_matching       = suppressWarnings(as.double(raw$awardmatching %||% raw$award_matching %||% NA)),
    original_amount      = suppressWarnings(as.double(raw$originalamount %||% raw$original_amount %||% NA)),
    supplement_amount    = suppressWarnings(as.double(raw$supplementamount %||% raw$supplement_amount %||% NA)),
    begin_grant          = .safe_date(raw$begingrant %||% raw$begin_grant %||% NA),
    end_grant            = .safe_date(raw$endgrant %||% raw$end_grant %||% NA),
    project_desc         = as.character(raw$projectdesc %||% raw$project_desc %||% NA),
    to_support           = as.character(raw$tosupport %||% raw$to_support %||% NA),
    primary_discipline   = as.character(raw$primarydiscipline %||% raw$primary_discipline %||% NA),
    supplement_count     = suppressWarnings(as.integer(raw$supplementcount %||% raw$supplement_count %||% NA)),
    participant_count    = suppressWarnings(as.integer(raw$participantcount %||% raw$participant_count %||% NA)),
    discipline_count     = suppressWarnings(as.integer(raw$disciplinecount %||% raw$discipline_count %||% NA))
  )
}

.neh_parse_evaluators <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(.schema_evaluators)

  cn <- names(raw)
  cn <- tolower(cn)
  cn <- gsub("\\s+", "_", cn)
  names(raw) <- cn

  .safe_date <- function(x) {
    if (is.null(x)) return(as.Date(NA))
    tryCatch(as.Date(x, format = "%m/%d/%Y"), error = function(e) as.Date(NA))
  }

  tibble::tibble(
    id              = suppressWarnings(as.integer(raw$id %||% NA)),
    name_prefix     = as.character(raw$nameprefix %||% raw$name_prefix %||% NA),
    firstname       = as.character(raw$firstname %||% NA),
    middlename      = as.character(raw$middlename %||% NA),
    lastname        = as.character(raw$lastname %||% NA),
    name_suffix     = as.character(raw$namesuffix %||% raw$name_suffix %||% NA),
    title           = as.character(raw$title %||% NA),
    department      = as.character(raw$department %||% NA),
    institution     = as.character(raw$institution %||% NA),
    city            = as.character(raw$city %||% NA),
    state           = as.character(raw$state %||% NA),
    postal_code     = as.character(raw$postalcode %||% raw$postal_code %||% NA),
    country         = as.character(raw$country %||% NA),
    start_date      = .safe_date(raw$startdate %||% raw$start_date %||% NA),
    end_date        = .safe_date(raw$enddate %||% raw$end_date %||% NA),
    fiscal_year     = suppressWarnings(as.integer(raw$fiscalyear %||% raw$fiscal_year %||% NA))
  )
}

# == Public functions ==========================================================

#' List available NEH open datasets
#'
#' Returns a tibble describing each downloadable dataset from the NEH open data
#' portal, including grant data by decade and the evaluators dataset.
#'
#' @return tibble with columns: dataset, decade, format, url, description
#' @export
neh_list <- function() {
  rows <- list()
  for (dec in .neh_decades) {
    rows <- c(rows, list(tibble::tibble(
      dataset     = "grants",
      decade      = dec,
      format      = "csv",
      url         = paste0(.neh_base, "/NEH_Grants", dec, ".csv"),
      description = paste0("NEH grants, fiscal years ", dec)
    )))
  }
  rows <- c(rows, list(tibble::tibble(
    dataset     = "evaluators",
    decade      = NA_character_,
    format      = "csv",
    url         = paste0(.neh_base, "/NEH_Evaluators.csv"),
    description = "NEH peer-review evaluators, 1970-present"
  )))
  dplyr::bind_rows(rows)
}

#' Search NEH grants by keyword, state, year, program, or discipline
#'
#' Downloads grant data for the specified decade(s) and filters by the given
#' criteria. All filter arguments are optional; when omitted the full dataset
#' for that decade is returned.
#'
#' @param query Character. Case-insensitive text search across project title,
#'   description, and institution name. Default NULL (no text filter).
#' @param state Character. Two-letter state abbreviation (e.g. "CA"). Default NULL.
#' @param year Integer. Fiscal year awarded. Default NULL.
#' @param program Character. Case-insensitive partial match on program name.
#'   Default NULL.
#' @param discipline Character. Case-insensitive partial match on primary
#'   discipline. Default NULL.
#' @param decade Character vector. Which decade file(s) to search. Default
#'   "2020s". Use "all" for all decades (slow -- ~55 MB total).
#' @param limit Integer. Max rows to return. Default 500.
#' @return tibble of matching grants
#' @export
neh_search <- function(query = NULL, state = NULL, year = NULL,
                       program = NULL, discipline = NULL,
                       decade = "2020s", limit = 500L) {
  if (identical(decade, "all")) decade <- .neh_decades
  decade <- match.arg(decade, .neh_decades, several.ok = TRUE)

  all_grants <- list()
  for (dec in decade) {
    url <- paste0(.neh_base, "/NEH_Grants", dec, ".csv")
    raw <- .neh_fetch_csv(url)
    if (!is.null(raw) && nrow(raw) > 0) {
      parsed <- .neh_parse_grants(raw)
      all_grants <- c(all_grants, list(parsed))
    }
  }
  if (length(all_grants) == 0) return(.schema_grants)
  result <- dplyr::bind_rows(all_grants)

  # Apply filters
  if (!is.null(query) && nzchar(query)) {
    q <- tolower(query)
    result <- result |>
      dplyr::filter(
        grepl(q, tolower(project_title), fixed = TRUE) |
        grepl(q, tolower(project_desc), fixed = TRUE) |
        grepl(q, tolower(institution), fixed = TRUE)
      )
  }
  if (!is.null(state) && nzchar(state)) {
    result <- result |> dplyr::filter(toupper(.data$state) == toupper(!!state))
  }
  if (!is.null(year)) {
    result <- result |> dplyr::filter(.data$year_awarded == !!as.integer(year))
  }
  if (!is.null(program) && nzchar(program)) {
    p <- tolower(program)
    result <- result |> dplyr::filter(grepl(p, tolower(.data$program), fixed = TRUE))
  }
  if (!is.null(discipline) && nzchar(discipline)) {
    d <- tolower(discipline)
    result <- result |> dplyr::filter(grepl(d, tolower(.data$primary_discipline), fixed = TRUE))
  }

  result |> dplyr::slice_head(n = as.integer(limit))
}

#' Get NEH grants for a specific decade
#'
#' Downloads and parses the full grant dataset for a given decade.
#'
#' @param decade Character. One of "1960s", "1970s", "1980s", "1990s",
#'   "2000s", "2010s", "2020s".
#' @return tibble of all grants in that decade
#' @export
neh_grants <- function(decade = "2020s") {
  decade <- match.arg(decade, .neh_decades)
  url <- paste0(.neh_base, "/NEH_Grants", decade, ".csv")
  raw <- .neh_fetch_csv(url)
  if (is.null(raw)) return(.schema_grants)
  .neh_parse_grants(raw)
}

#' Get a single NEH grant by application number
#'
#' Looks up a specific grant across all decade files (or a specified decade).
#'
#' @param app_number Character. The NEH application number (e.g. "CHA-261870-20").
#' @param decade Character. Decade to search in. Default NULL searches the most
#'   likely decade based on the suffix, or "2020s" if ambiguous.
#' @return tibble with one row (or zero if not found)
#' @export
neh_grant <- function(app_number, decade = NULL) {
  if (is.null(decade)) {
    # Try to infer decade from app number suffix (last 2 digits = year)
    suffix <- sub(".*-(\\d{2})$", "\\1", app_number)
    yr <- suppressWarnings(as.integer(suffix))
    if (!is.na(yr)) {
      if (yr >= 60 && yr <= 69) decade <- "1960s"
      else if (yr >= 70 && yr <= 79) decade <- "1970s"
      else if (yr >= 80 && yr <= 89) decade <- "1980s"
      else if (yr >= 90 && yr <= 99) decade <- "1990s"
      else if (yr >= 0 && yr <= 9) decade <- "2000s"
      else if (yr >= 10 && yr <= 19) decade <- "2010s"
      else if (yr >= 20 && yr <= 29) decade <- "2020s"
    }
    if (is.null(decade)) decade <- "2020s"
  }
  grants <- neh_grants(decade)
  grants |> dplyr::filter(.data$app_number == !!app_number)
}

#' Get NEH peer-review evaluators
#'
#' Downloads and parses the full evaluators dataset (1970-present). Optionally
#' filter by name, institution, state, or fiscal year.
#'
#' @param name Character. Case-insensitive partial match on first or last name.
#'   Default NULL.
#' @param institution Character. Case-insensitive partial match. Default NULL.
#' @param state Character. Two-letter state abbreviation. Default NULL.
#' @param fiscal_year Integer. Filter to a specific fiscal year. Default NULL.
#' @param limit Integer. Max rows to return. Default 500.
#' @return tibble of evaluators
#' @export
neh_evaluators <- function(name = NULL, institution = NULL, state = NULL,
                           fiscal_year = NULL, limit = 500L) {
  url <- paste0(.neh_base, "/NEH_Evaluators.csv")
  raw <- .neh_fetch_csv(url)
  if (is.null(raw)) return(.schema_evaluators)
  result <- .neh_parse_evaluators(raw)

  if (!is.null(name) && nzchar(name)) {
    n <- tolower(name)
    result <- result |>
      dplyr::filter(
        grepl(n, tolower(firstname), fixed = TRUE) |
        grepl(n, tolower(lastname), fixed = TRUE)
      )
  }
  if (!is.null(institution) && nzchar(institution)) {
    inst <- tolower(institution)
    result <- result |> dplyr::filter(grepl(inst, tolower(.data$institution), fixed = TRUE))
  }
  if (!is.null(state) && nzchar(state)) {
    result <- result |> dplyr::filter(toupper(.data$state) == toupper(!!state))
  }
  if (!is.null(fiscal_year)) {
    result <- result |> dplyr::filter(.data$fiscal_year == !!as.integer(fiscal_year))
  }

  result |> dplyr::slice_head(n = as.integer(limit))
}

#' Summarize NEH grants by program, division, state, or discipline
#'
#' Provides aggregate statistics for a decade of grants.
#'
#' @param decade Character. Decade to summarize. Default "2020s".
#' @param by Character. Group by "program", "division", "state", or
#'   "discipline". Default "program".
#' @return tibble with group, count, total_award, and mean_award columns
#' @export
neh_summary <- function(decade = "2020s", by = c("program", "division", "state", "discipline")) {
  by <- match.arg(by)
  grants <- neh_grants(decade)
  if (nrow(grants) == 0) {
    return(tibble::tibble(group = character(), n_grants = integer(),
                          total_award = double(), mean_award = double()))
  }

  group_col <- switch(by,
    program    = "program",
    division   = "division",
    state      = "state",
    discipline = "primary_discipline"
  )

  grants |>
    dplyr::mutate(.total = .data$award_outright + .data$award_matching) |>
    dplyr::group_by(group = .data[[group_col]]) |>
    dplyr::summarise(
      n_grants    = dplyr::n(),
      total_award = sum(.total, na.rm = TRUE),
      mean_award  = mean(.total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$total_award))
}

#' Return the full source of this client file
#'
#' Reads the neh.gov.R source file and returns it as a single string.
#'
#' @return character string containing the full source code
#' @export
neh_context <- function() {
  src <- readLines(sys.frame(environment(neh_context))$ofile %||% {
    f <- getSrcFilename(neh_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else "clients/neh.gov.R"
  })
  paste(src, collapse = "\n")
}
