#' Search NIH-funded research projects
#'
#' @param text Full-text search across project title and abstract
#' @param pi_names PI last names (character vector)
#' @param org_names Organization names
#' @param fiscal_years Fiscal years (integer vector)
#' @param agencies NIH institute codes (e.g. "NCI", "NIAID")
#' @param limit Max results (default 50, max 500)
#' @param offset Pagination offset
#' @return tibble: project_num, title, pi_name, org_name, fiscal_year,
#'   award_amount, agency
#' @export
nih_projects <- function(text = NULL, pi_names = NULL, org_names = NULL,
                         fiscal_years = NULL, agencies = NULL,
                         limit = 50, offset = 0) {
  criteria <- list()
  if (!is.null(text))         criteria$advanced_text_search <- list(operator = "and", search_field = "projecttitle,terms", search_text = text)
  if (!is.null(pi_names))     criteria$pi_names <- lapply(pi_names, function(n) list(last_name = n, any_name = n))
  if (!is.null(org_names))    criteria$org_names <- as.list(org_names)
  if (!is.null(fiscal_years)) criteria$fiscal_years <- as.list(as.integer(fiscal_years))
  if (!is.null(agencies))     criteria$agencies <- as.list(agencies)

  body <- list(criteria = criteria, limit = limit, offset = offset)

  resp <- httr2::request(paste0(.nih_base, "/projects/search")) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_projects)

  tibble(
    project_num  = vapply(results, function(r) r$project_num %||% NA_character_, character(1)),
    title        = vapply(results, function(r) r$project_title %||% NA_character_, character(1)),
    pi_name      = vapply(results, function(r) {
      pis <- r$principal_investigators
      if (is.null(pis) || length(pis) == 0) NA_character_
      else paste(vapply(pis, function(p) p$full_name %||% "", character(1)), collapse = "; ")
    }, character(1)),
    org_name     = vapply(results, function(r) r$organization$org_name %||% NA_character_, character(1)),
    fiscal_year  = vapply(results, function(r) as.integer(r$fiscal_year %||% NA_integer_), integer(1)),
    award_amount = vapply(results, function(r) as.numeric(r$award_amount %||% NA_real_), numeric(1)),
    agency       = vapply(results, function(r) r$agency_ic_fundings[[1]]$abbreviation %||% r$agency_code %||% NA_character_, character(1))
  )
}

#' Search publications linked to NIH grants
#'
#' @param text Search text
#' @param project_nums Project numbers to filter by
#' @param limit Max results (default 50)
#' @param offset Pagination offset
#' @return tibble: pmid, title, journal, pub_year, project_num
#' @export
nih_publications <- function(text = NULL, project_nums = NULL,
                             limit = 50, offset = 0) {
  criteria <- list()
  if (!is.null(text))         criteria$advanced_text_search <- list(operator = "and", search_field = "title,abstract", search_text = text)
  if (!is.null(project_nums)) criteria$project_nums <- as.list(project_nums)

  body <- list(criteria = criteria, limit = limit, offset = offset)

  resp <- httr2::request(paste0(.nih_base, "/publications/search")) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pubs)

  tibble(
    pmid        = vapply(results, function(r) as.character(r$pmid %||% NA_character_), character(1)),
    title       = vapply(results, function(r) r$title %||% NA_character_, character(1)),
    journal     = vapply(results, function(r) r$journal %||% NA_character_, character(1)),
    pub_year    = vapply(results, function(r) as.integer(r$pub_year %||% NA_integer_), integer(1)),
    project_num = vapply(results, function(r) {
      cnums <- r$coreproject
      if (is.null(cnums)) NA_character_ else as.character(cnums)
    }, character(1))
  )
}

#' Generate context
#' @return Character string (invisibly)
#' @export
nih_context <- function() {
  .build_context("api.reporter.nih.gov", header_lines = c(
    "# api.reporter.nih.gov - NIH RePORTER Grants Search Client for R",
    "# Auth: none required. POST-based search API.",
    "# Search NIH-funded projects and linked publications.",
    "# Agencies: NCI, NIAID, NHLBI, NIGMS, NIMH, etc."
  ))
}
