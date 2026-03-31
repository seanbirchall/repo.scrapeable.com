# == Public functions ==========================================================

#' Search cases in the Caselaw Access Project
#'
#' @param query Search query (e.g. "first amendment", "miranda rights")
#' @param jurisdiction Filter by jurisdiction slug (e.g. "us", "cal", "ny")
#' @param decision_date_min Earliest decision date (YYYY-MM-DD)
#' @param decision_date_max Latest decision date (YYYY-MM-DD)
#' @param page_size Results per page (default 10, max 100)
#' @return tibble: id, name, name_abbreviation, decision_date, court,
#'   jurisdiction, citations, url
#' @export
caselaw_search <- function(query, jurisdiction = NULL,
                           decision_date_min = NULL, decision_date_max = NULL,
                           page_size = 10) {
  params <- list(
    search = utils::URLencode(query, reserved = TRUE),
    page_size = as.integer(page_size)
  )
  if (!is.null(jurisdiction)) params$jurisdiction <- jurisdiction
  if (!is.null(decision_date_min)) params$decision_date_min <- decision_date_min
  if (!is.null(decision_date_max)) params$decision_date_max <- decision_date_max
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.caselaw_base, "/cases/?", qstr)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0 ||
      (is.data.frame(results) && nrow(results) == 0)) return(.schema_cases)

  tibble(
    id = as.integer(results$id),
    name = as.character(results$name),
    name_abbreviation = as.character(results$name_abbreviation %||% NA_character_),
    decision_date = tryCatch(as.Date(results$decision_date), error = function(e) as.Date(NA)),
    court = as.character(
      if (!is.null(results$court) && !is.null(results$court$name)) results$court$name
      else NA_character_
    ),
    jurisdiction = as.character(
      if (!is.null(results$jurisdiction) && !is.null(results$jurisdiction$name)) results$jurisdiction$name
      else NA_character_
    ),
    citations = vapply(results$citations, function(x) {
      if (is.data.frame(x) && "cite" %in% names(x)) paste(x$cite, collapse = "; ")
      else NA_character_
    }, character(1)),
    url = as.character(results$url %||% NA_character_)
  )
}

#' Get a single case by ID
#'
#' @param id Case ID (integer)
#' @return tibble: one row with id, name, name_abbreviation, decision_date,
#'   court, jurisdiction, citations, docket_number, url
#' @export
caselaw_case <- function(id) {
  url <- sprintf("%s/cases/%s/", .caselaw_base, as.character(id))
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_case)

  tibble(
    id = as.integer(raw$id),
    name = as.character(raw$name),
    name_abbreviation = as.character(raw$name_abbreviation %||% NA_character_),
    decision_date = tryCatch(as.Date(raw$decision_date), error = function(e) as.Date(NA)),
    court = as.character(
      if (!is.null(raw$court) && !is.null(raw$court$name)) raw$court$name
      else NA_character_
    ),
    jurisdiction = as.character(
      if (!is.null(raw$jurisdiction) && !is.null(raw$jurisdiction$name)) raw$jurisdiction$name
      else NA_character_
    ),
    citations = paste(
      if (!is.null(raw$citations) && is.data.frame(raw$citations)) raw$citations$cite
      else NA_character_,
      collapse = "; "
    ),
    docket_number = as.character(raw$docket_number %||% NA_character_),
    url = as.character(raw$url %||% NA_character_)
  )
}

#' Show Caselaw client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
caselaw_context <- function() {
  .build_context(
    pkg_name = "case.law",
    header_lines = c(
      "# case.law -- Caselaw Access Project Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for basic metadata",
      "# Jurisdictions: us, cal, ny, tex, ill, mass, pa",
      "# Example queries: first amendment, miranda, due process, commerce clause"
    )
  )
}

