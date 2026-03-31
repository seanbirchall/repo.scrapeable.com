# == Schemas ===================================================================

.schema_records <- tibble(
  osti_id = character(), title = character(), authors = character(),
  publication_date = as.Date(character()), doi = character(),
  product_type = character(), journal_name = character(),
  sponsor_orgs = character(), description = character()
)

# == Public functions ==========================================================

#' Search OSTI scientific/technical records
#'
#' @param query Search query string
#' @param rows Maximum records to return (default 20)
#' @param page Page number (1-indexed, default 1)
#' @return tibble: osti_id, title, authors, publication_date, doi,
#'   product_type, journal_name, sponsor_orgs, description
#' @export
osti_search <- function(query, rows = 20, page = 1) {
  url <- sprintf("%s/records?q=%s&rows=%d&page=%d",
                 .osti_base, utils::URLencode(query), rows, page)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  if (!is.data.frame(raw)) return(.schema_records)

  as_tibble(raw) |>
    transmute(
      osti_id = as.character(osti_id),
      title = as.character(title),
      authors = vapply(authors, function(a) {
        if (is.null(a) || length(a) == 0) return(NA_character_)
        paste(a, collapse = "; ")
      }, character(1)),
      publication_date = tryCatch(as.Date(substr(publication_date, 1, 10)),
                                  error = function(e) as.Date(NA)),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA),
      product_type = as.character(if ("product_type" %in% names(raw)) product_type else NA),
      journal_name = as.character(if ("journal_name" %in% names(raw)) journal_name else NA),
      sponsor_orgs = vapply(sponsor_orgs, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_character_)
        paste(s, collapse = "; ")
      }, character(1)),
      description = as.character(if ("description" %in% names(raw)) description else NA)
    )
}

#' Fetch a single OSTI record by ID
#'
#' @param id OSTI record ID
#' @return tibble: single row with record details
#' @export
osti_record <- function(id) {
  url <- sprintf("%s/records/%s", .osti_base, as.character(id))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  if (!is.data.frame(raw)) return(.schema_records)

  as_tibble(raw) |>
    transmute(
      osti_id = as.character(osti_id),
      title = as.character(title),
      authors = vapply(authors, function(a) {
        if (is.null(a) || length(a) == 0) return(NA_character_)
        paste(a, collapse = "; ")
      }, character(1)),
      publication_date = tryCatch(as.Date(substr(publication_date, 1, 10)),
                                  error = function(e) as.Date(NA)),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA),
      product_type = as.character(if ("product_type" %in% names(raw)) product_type else NA),
      journal_name = as.character(if ("journal_name" %in% names(raw)) journal_name else NA),
      sponsor_orgs = vapply(sponsor_orgs, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_character_)
        paste(s, collapse = "; ")
      }, character(1)),
      description = as.character(if ("description" %in% names(raw)) description else NA)
    )
}

#' OSTI package context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
osti_context <- function() {
  .build_context("osti.gov", header_lines = c(
    "# osti.gov - OSTI DOE Scientific Records Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Product types: Journal Article, Technical Report, Conference, Dataset",
    "# Sponsors: DOE Office of Science, NNSA, ARPA-E, etc."
  ))
}
