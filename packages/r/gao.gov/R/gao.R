# == Recent reports ============================================================

#' Fetch recent GAO reports
#'
#' Returns GAO reports modified since a given date, with pagination.
#' Uses the GovInfo collections API. Total archive: ~16,500 reports.
#'
#' @param since Start date for modifications (default: 30 days ago).
#'   ISO 8601 format or Date object.
#' @param api_key API key from api.data.gov. Register free at
#'   https://api.data.gov/signup/
#' @param page_size Results per page (default 100, max 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble: package_id, title, date_issued (Date),
#'   doc_class, last_modified (POSIXct), summary_url
#' @export
gao_recent <- function(since = NULL, api_key = NULL, page_size = 100, offset = 0) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )
  if (is.null(since)) since <- format(Sys.Date() - 30, "%Y-%m-%dT00:00:00Z")
  if (inherits(since, "Date")) since <- format(since, "%Y-%m-%dT00:00:00Z")

  url <- sprintf(
    "%s/collections/GAOREPORTS/%s?pageSize=%d&offset=%d&api_key=%s",
    .govinfo_base, since, page_size, offset, api_key
  )

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovInfo API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(raw)) return(.schema_reports)

  pkgs <- raw$packages
  if (is.null(pkgs) || length(pkgs) == 0) return(.schema_reports)

  tibble(
    package_id    = vapply(pkgs, function(p) p$packageId %||% NA_character_, character(1)),
    title         = vapply(pkgs, function(p) p$title %||% NA_character_, character(1)),
    date_issued   = as.Date(vapply(pkgs, function(p) p$dateIssued %||% NA_character_, character(1))),
    doc_class     = vapply(pkgs, function(p) p$docClass %||% NA_character_, character(1)),
    last_modified = as.POSIXct(vapply(pkgs, function(p) p$lastModified %||% NA_character_, character(1)),
                               format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    summary_url   = vapply(pkgs, function(p) p$packageLink %||% NA_character_, character(1))
  )
}


#' Fetch all GAO reports with pagination
#'
#' Loops through all pages of GAO reports since a given date.
#'
#' @param since Start date (default: 90 days ago)
#' @param api_key API key from api.data.gov
#' @param page_size Results per page (default 1000)
#' @param max_pages Maximum pages to fetch (default 10)
#' @param sleep Seconds between requests (default 0.5)
#' @return tibble: same columns as gao_recent
#' @export
gao_recent_all <- function(since = NULL, api_key = NULL, page_size = 1000,
                           max_pages = 10, sleep = 0.5) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )
  if (is.null(since)) since <- format(Sys.Date() - 90, "%Y-%m-%dT00:00:00Z")

  results <- list()
  offset <- 0
  for (page in seq_len(max_pages)) {
    if (page > 1) Sys.sleep(sleep)
    message(sprintf("[page %d] offset=%d", page, offset))
    batch <- gao_recent(since = since, api_key = api_key,
                        page_size = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[page]] <- batch
    offset <- offset + page_size
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Report details ============================================================

#' Fetch detailed metadata for a GAO report
#'
#' Returns full metadata including download links for a single report.
#'
#' @param package_id GovInfo package ID (e.g. "GAOREPORTS-GAO-24-106130")
#' @param api_key API key from api.data.gov
#' @return tibble: one row with package_id, title, date_issued,
#'   doc_class, government_author, pages, su_doc_class, pdf_url, details_url
#' @export
gao_report <- function(package_id, api_key = NULL) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )

  url <- sprintf("%s/packages/%s/summary?api_key=%s",
                 .govinfo_base, package_id, api_key)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovInfo API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(raw)) return(.schema_report_detail)

  tibble(
    package_id        = raw$packageId %||% NA_character_,
    title             = raw$title %||% NA_character_,
    date_issued       = as.Date(raw$dateIssued %||% NA_character_),
    doc_class         = raw$category %||% NA_character_,
    government_author = raw$governmentAuthor1 %||% NA_character_,
    pages             = as.integer(raw$pages %||% NA_integer_),
    su_doc_class      = raw$suDocClassNumber %||% NA_character_,
    pdf_url           = raw$download$pdfLink %||% NA_character_,
    details_url       = raw$detailsLink %||% NA_character_
  )
}


# == Search ====================================================================

#' Search GAO reports by keyword
#'
#' Full-text search across all GAO reports in GovInfo.
#' Supports Solr-like query syntax.
#'
#' @param query Search term (e.g. "cybersecurity", "defense spending")
#' @param api_key API key from api.data.gov
#' @param page_size Results per page (default 25, max 100)
#' @param offset_mark Pagination cursor (default "*" for first page).
#'   Use the returned cursor from a previous call for next pages.
#' @param sort_field Sort by: "score" (relevance), "publishdate",
#'   "lastModified", "title". Default "score".
#' @param sort_order "DESC" or "ASC". Default "DESC".
#' @return list with `results` (tibble) and `next_cursor` (character)
#' @export
gao_search <- function(query, api_key = NULL, page_size = 25,
                       offset_mark = "*", sort_field = "score",
                       sort_order = "DESC") {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )

  url <- sprintf("%s/search?api_key=%s", .govinfo_base, api_key)

  body <- list(
    query = paste0(query, " collection:GAOREPORTS"),
    pageSize = as.character(page_size),
    offsetMark = offset_mark,
    sorts = list(list(field = sort_field, sortOrder = sort_order))
  )

  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results_list <- raw$results
  if (is.null(results_list) || length(results_list) == 0) {
    return(list(results = .schema_search, next_cursor = NULL))
  }

  results <- tibble(
    package_id        = vapply(results_list, function(r) r$packageId %||% NA_character_, character(1)),
    title             = vapply(results_list, function(r) r$title %||% NA_character_, character(1)),
    date_issued       = as.Date(vapply(results_list, function(r) r$dateIssued %||% NA_character_, character(1))),
    collection        = vapply(results_list, function(r) r$collectionCode %||% NA_character_, character(1)),
    government_author = vapply(results_list, function(r) {
      authors <- r$governmentAuthor
      if (is.null(authors) || length(authors) == 0) NA_character_
      else paste(authors, collapse = "; ")
    }, character(1)),
    pdf_url           = vapply(results_list, function(r) r$download$pdfLink %||% NA_character_, character(1))
  )

  list(results = results, next_cursor = raw$offsetMark)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the GAO package
#'
#' @return Character string (invisibly), also printed
#' @export
gao_context <- function() {
  .build_context("gao.gov", header_lines = c(
    "# gao.gov - GAO Reports Client for R (via GovInfo API)",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: required api_key from api.data.gov (free signup)",
    "#   Register at https://api.data.gov/signup/",
    "# Rate limit: 1,200 req/min",
    "# All functions return tibbles with typed columns.",
    "#",
    "# ~16,500 GAO reports and Comptroller General decisions (1993-present)",
    "#",
    "# Workflow: gao_search() or gao_recent() -> gao_report() -> pdf_url"
  ))
}
