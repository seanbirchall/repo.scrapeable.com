# uspto.gov.R - Self-contained USPTO (US Patent and Trademark Office) client
#
# Data source: USPTO DS-API (developer.uspto.gov/ds-api)
# Provides access to patent office actions, citations, rejections, and
# enriched citation metadata via the Solr-based Dataset API.
#
# Datasets available:
#   - oa_actions (v1): 19M+ patent office action documents
#   - oa_citations (v1): 58M+ prior art citations from office actions
#   - oa_rejections (v2): 86M+ claim rejections from office actions
#   - enriched_cited_reference_metadata (v2): 60M+ enriched citation records
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.uspto_base <- "https://developer.uspto.gov/ds-api"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.fetch_compressed <- function(url, body = NULL) {
  tmp <- tempfile(fileext = ".json")
  req <- httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = .ua,
      `Accept` = "application/json",
      `Accept-Encoding` = "gzip, deflate"
    ) |>
    httr2::req_timeout(60)

  if (!is.null(body)) {
    req <- req |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/x-www-form-urlencoded") |>
      httr2::req_body_raw(body, type = "application/x-www-form-urlencoded")
  }

  req |> httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# -- Solr query builder --------------------------------------------------------

.solr_query <- function(dataset, version, criteria = "*:*",
                        start = 0, rows = 25) {
  url <- paste0(.uspto_base, "/", dataset, "/", version, "/records")
  body <- paste0(
    "criteria=", utils::URLencode(criteria, reserved = TRUE),
    "&start=", start,
    "&rows=", rows
  )
  .fetch_compressed(url, body = body)
}

.solr_result <- function(dataset, version, criteria = "*:*",
                         start = 0, rows = 25) {
  raw <- .solr_query(dataset, version, criteria, start, rows)
  resp <- raw$response
  if (is.null(resp)) {
    warning("USPTO DS-API error: no response object")
    return(list(total = 0L, docs = list()))
  }
  list(
    total = as.integer(resp$numFound %||% 0L),
    docs  = resp$docs %||% list()
  )
}

# -- Field helpers -------------------------------------------------------------

.safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- x[[1]]
  if (is.null(v)) NA_character_ else as.character(v)
}

.safe_int <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  v <- x[[1]]
  if (is.null(v)) NA_integer_ else as.integer(v)
}

.safe_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(as.Date(NA_character_))
  v <- as.character(x[[1]])
  if (is.na(v) || v == "" || startsWith(v, "0001")) return(as.Date(NA_character_))
  as.Date(substr(v, 1, 10))
}

.collapse_list <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  paste(as.character(unlist(x)), collapse = "; ")
}

# == Schemas ===================================================================

.schema_actions <- tibble(
  id = character(), patent_app_number = character(),
  patent_number = character(), invention_title = character(),
  filing_date = as.Date(character()), grant_date = as.Date(character()),
  submission_date = as.Date(character()),
  tech_center = character(), work_group = character(),
  art_unit = character(), status_number = integer(),
  doc_code = character()
)

.schema_citations <- tibble(
  id = character(), application_id = character(),
  cited_patent = character(),
  form892 = logical(), form1449 = logical(),
  cited_in_oa = logical()
)

.schema_rejections <- tibble(
  id = character(), patent_app_number = character(),
  invention_title = character(),
  office_action_date = as.Date(character()),
  action_category = character(),
  has_rej_101 = logical(), has_rej_102 = logical(),
  has_rej_103 = logical(), has_rej_dp = logical(),
  tech_center = character(), art_unit = character(),
  cited_document = character()
)

.schema_enriched <- tibble(
  id = character(), patent_app_number = character(),
  inventor = character(), cited_document = character(),
  citation_category = character(),
  office_action_date = as.Date(character()),
  action_category = character(), tech_center = character(),
  art_unit = character(), examiner_cited = logical(),
  applicant_cited = logical()
)

# == Office Actions ============================================================

#' Search USPTO patent office actions
#'
#' Searches 19M+ office action documents from the USPTO patent examination process.
#' Supports Solr/Lucene query syntax for field-level searches.
#'
#' @param query Solr query string. Examples:
#'   - "*:*" (all records)
#'   - "inventionTitle:quantum" (title contains quantum)
#'   - "patentNumber:10000000" (specific patent)
#'   - "techCenter:2800" (specific tech center)
#'   - "patentApplicationNumber:15061308" (specific application)
#' @param rows Number of results (default 25, max 100)
#' @param start Offset for pagination (default 0)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Document identifier}
#'     \item{patent_app_number}{Patent application number}
#'     \item{patent_number}{Granted patent number (if available)}
#'     \item{invention_title}{Title of the invention}
#'     \item{filing_date}{Date application was filed}
#'     \item{grant_date}{Date patent was granted (if applicable)}
#'     \item{submission_date}{Date of office action submission}
#'     \item{tech_center}{USPTO technology center}
#'     \item{work_group}{Work group identifier}
#'     \item{art_unit}{Art unit number}
#'     \item{status_number}{Application status number}
#'     \item{doc_code}{Document code identifier}
#'   }
#' @export
#' @family USPTO functions
#' @seealso \code{\link{uspto_search}} for title-based search,
#'   \code{\link{uspto_app_actions}} for application-specific lookup
#' @examples
#' \dontrun{
#' uspto_actions("inventionTitle:quantum", rows = 5)
#' }
uspto_actions <- function(query = "*:*", rows = 25, start = 0) {
  result <- .solr_result("oa_actions", "v1", criteria = query,
                         start = start, rows = min(rows, 100))

  if (result$total == 0 || length(result$docs) == 0) return(.schema_actions)

  docs <- result$docs
  tibble(
    id                = vapply(docs, function(d) d$id %||% NA_character_, character(1)),
    patent_app_number = vapply(docs, function(d) .safe_chr(d$patentApplicationNumber), character(1)),
    patent_number     = vapply(docs, function(d) .safe_chr(d$patentNumber), character(1)),
    invention_title   = vapply(docs, function(d) .safe_chr(d$inventionTitle), character(1)),
    filing_date       = do.call(c, lapply(docs, function(d) .safe_date(d$filingDate))),
    grant_date        = do.call(c, lapply(docs, function(d) .safe_date(d$grantDate))),
    submission_date   = do.call(c, lapply(docs, function(d) .safe_date(d$submissionDate))),
    tech_center       = vapply(docs, function(d) .safe_chr(d$techCenter), character(1)),
    work_group        = vapply(docs, function(d) .collapse_list(d$workGroup), character(1)),
    art_unit          = vapply(docs, function(d) .safe_chr(d$groupArtUnitNumber), character(1)),
    status_number     = vapply(docs, function(d) .safe_int(d$applicationStatusNumber), integer(1)),
    doc_code          = vapply(docs, function(d) .collapse_list(d$legacyDocumentCodeIdentifier), character(1))
  )
}

#' Get office action text for a patent application
#'
#' Returns the full body text of office actions for a given application number.
#'
#' @param app_number Character patent application number (e.g., \code{"15061308"}).
#' @param rows Integer number of actions to return (default 10).
#' @return A tibble with id, patent_app_number, submission_date,
#'   doc_code, and body_text (truncated to 5000 chars) columns.
#' @export
#' @family USPTO functions
#' @seealso \code{\link{uspto_app_actions}} for action metadata
uspto_action_text <- function(app_number, rows = 10) {
  query <- paste0("patentApplicationNumber:", app_number)
  result <- .solr_result("oa_actions", "v1", criteria = query,
                         start = 0, rows = min(rows, 100))

  schema <- tibble(
    id = character(), patent_app_number = character(),
    submission_date = as.Date(character()),
    doc_code = character(), body_text = character()
  )

  if (result$total == 0 || length(result$docs) == 0) return(schema)

  docs <- result$docs
  tibble(
    id                = vapply(docs, function(d) d$id %||% NA_character_, character(1)),
    patent_app_number = vapply(docs, function(d) .safe_chr(d$patentApplicationNumber), character(1)),
    submission_date   = do.call(c, lapply(docs, function(d) .safe_date(d$submissionDate))),
    doc_code          = vapply(docs, function(d) .collapse_list(d$legacyDocumentCodeIdentifier), character(1)),
    body_text         = vapply(docs, function(d) {
      bt <- d$bodyText
      if (is.null(bt) || length(bt) == 0) return(NA_character_)
      txt <- paste(as.character(unlist(bt)), collapse = "\n")
      if (nchar(txt) > 5000) paste0(substr(txt, 1, 5000), "...") else txt
    }, character(1))
  )
}

# == Citations =================================================================

#' Search USPTO patent office action citations
#'
#' Searches 58M+ prior art citations found in patent office actions.
#' Each record links a patent application to a cited document.
#'
#' @param query Solr query string. Examples:
#'   - "applicationId:15061308" (citations for an application)
#'   - "Patent_PGPub:10000000" (where a patent was cited)
#'   - "*:*" (all citations)
#' @param rows Number of results (default 25, max 100)
#' @param start Offset for pagination (default 0)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Citation record identifier}
#'     \item{application_id}{Patent application number}
#'     \item{cited_patent}{Cited patent/publication number}
#'     \item{form892}{Logical: cited on Form 892}
#'     \item{form1449}{Logical: cited on Form 1449}
#'     \item{cited_in_oa}{Logical: cited in office action text}
#'   }
#' @export
#' @family USPTO functions
#' @seealso \code{\link{uspto_app_citations}} for application-specific lookup
uspto_citations <- function(query = "*:*", rows = 25, start = 0) {
  result <- .solr_result("oa_citations", "v1", criteria = query,
                         start = start, rows = min(rows, 100))

  if (result$total == 0 || length(result$docs) == 0) return(.schema_citations)

  docs <- result$docs
  tibble(
    id             = vapply(docs, function(d) d$id %||% NA_character_, character(1)),
    application_id = vapply(docs, function(d) .collapse_list(d$applicationId), character(1)),
    cited_patent   = vapply(docs, function(d) .collapse_list(d$Patent_PGPub), character(1)),
    form892        = vapply(docs, function(d) {
      v <- d$form892
      if (is.null(v) || length(v) == 0) NA else as.logical(as.integer(v[[1]]))
    }, logical(1)),
    form1449       = vapply(docs, function(d) {
      v <- d$form1449
      if (is.null(v) || length(v) == 0) NA else as.logical(as.integer(v[[1]]))
    }, logical(1)),
    cited_in_oa    = vapply(docs, function(d) {
      v <- d$citationInOA
      if (is.null(v) || length(v) == 0) NA else as.logical(as.integer(v[[1]]))
    }, logical(1))
  )
}

# == Rejections ================================================================

#' Search USPTO patent claim rejections
#'
#' Searches 86M+ claim rejections from patent office actions. Each record
#' represents a specific rejection of claims under 35 USC 101, 102, 103,
#' or double patenting.
#'
#' @param query Solr query string. Examples:
#'   - "patentApplicationNumber:15061308" (rejections for an app)
#'   - "hasRej101:1" (all 101 rejections)
#'   - "techCenter:2800" (rejections in tech center 2800)
#'   - "inventorNameText:Smith" (by inventor name)
#' @param rows Number of results (default 25, max 100)
#' @param start Offset for pagination (default 0)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Rejection record identifier}
#'     \item{patent_app_number}{Application number}
#'     \item{invention_title}{Invention title}
#'     \item{office_action_date}{Date of the office action}
#'     \item{action_category}{Action category code}
#'     \item{has_rej_101}{Logical: 35 USC 101 rejection}
#'     \item{has_rej_102}{Logical: 35 USC 102 rejection}
#'     \item{has_rej_103}{Logical: 35 USC 103 rejection}
#'     \item{has_rej_dp}{Logical: double patenting rejection}
#'     \item{tech_center}{Technology center}
#'     \item{art_unit}{Art unit number}
#'     \item{cited_document}{Cited document identifier}
#'   }
#' @export
#' @family USPTO functions
#' @seealso \code{\link{uspto_app_rejections}} for application-specific lookup
uspto_rejections <- function(query = "*:*", rows = 25, start = 0) {
  result <- .solr_result("oa_rejections", "v2", criteria = query,
                         start = start, rows = min(rows, 100))

  if (result$total == 0 || length(result$docs) == 0) return(.schema_rejections)

  docs <- result$docs
  tibble(
    id                 = vapply(docs, function(d) d$id %||% NA_character_, character(1)),
    patent_app_number  = vapply(docs, function(d) .safe_chr(d$patentApplicationNumber), character(1)),
    invention_title    = vapply(docs, function(d) .safe_chr(d$inventionTitle), character(1)),
    office_action_date = do.call(c, lapply(docs, function(d) .safe_date(d$officeActionDate))),
    action_category    = vapply(docs, function(d) .safe_chr(d$legacyDocumentCodeIdentifier), character(1)),
    has_rej_101        = vapply(docs, function(d) as.logical(as.integer(d$hasRej101 %||% 0)), logical(1)),
    has_rej_102        = vapply(docs, function(d) as.logical(as.integer(d$hasRej102 %||% 0)), logical(1)),
    has_rej_103        = vapply(docs, function(d) as.logical(as.integer(d$hasRej103 %||% 0)), logical(1)),
    has_rej_dp         = vapply(docs, function(d) as.logical(as.integer(d$hasRejDP %||% 0)), logical(1)),
    tech_center        = vapply(docs, function(d) .safe_chr(d$techCenter), character(1)),
    art_unit           = vapply(docs, function(d) .safe_chr(d$groupArtUnitNumber), character(1)),
    cited_document     = vapply(docs, function(d) .safe_chr(d$citedDocumentIdentifier), character(1))
  )
}

# == Enriched Citation Metadata ================================================

#' Search USPTO enriched citation metadata
#'
#' Searches 60M+ enriched prior art citation records with details about the
#' cited documents, examiners, and the context of the citation.
#'
#' @param query Solr query string. Examples:
#'   - "patentApplicationNumber:17533310" (citations for an application)
#'   - "citedDocumentIdentifier:US 10000000" (where a patent was cited)
#'   - "inventorNameText:Smith" (citations by inventor)
#'   - "techCenter:3600" (citations in tech center 3600)
#' @param rows Number of results (default 25, max 100)
#' @param start Offset for pagination (default 0)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Record identifier}
#'     \item{patent_app_number}{Application number}
#'     \item{inventor}{Inventor name}
#'     \item{cited_document}{Cited document identifier}
#'     \item{citation_category}{Citation category code}
#'     \item{office_action_date}{Date of the office action}
#'     \item{action_category}{Office action category}
#'     \item{tech_center}{Technology center}
#'     \item{art_unit}{Art unit number}
#'     \item{examiner_cited}{Logical: examiner cited this reference}
#'     \item{applicant_cited}{Logical: applicant cited this reference}
#'   }
#' @export
#' @family USPTO functions
#' @seealso \code{\link{uspto_citations}} for basic citation data
uspto_enriched_citations <- function(query = "*:*", rows = 25, start = 0) {
  result <- .solr_result("enriched_cited_reference_metadata", "v2",
                         criteria = query, start = start, rows = min(rows, 100))

  if (result$total == 0 || length(result$docs) == 0) return(.schema_enriched)

  docs <- result$docs
  tibble(
    id                  = vapply(docs, function(d) d$id %||% NA_character_, character(1)),
    patent_app_number   = vapply(docs, function(d) .safe_chr(d$patentApplicationNumber), character(1)),
    inventor            = vapply(docs, function(d) .safe_chr(d$inventorNameText), character(1)),
    cited_document      = vapply(docs, function(d) .safe_chr(d$citedDocumentIdentifier), character(1)),
    citation_category   = vapply(docs, function(d) .safe_chr(d$citationCategoryCode), character(1)),
    office_action_date  = do.call(c, lapply(docs, function(d) .safe_date(d$officeActionDate))),
    action_category     = vapply(docs, function(d) .safe_chr(d$officeActionCategory), character(1)),
    tech_center         = vapply(docs, function(d) .safe_chr(d$techCenter), character(1)),
    art_unit            = vapply(docs, function(d) .safe_chr(d$groupArtUnitNumber), character(1)),
    examiner_cited      = vapply(docs, function(d) {
      v <- d$examinerCitedReferenceIndicator
      if (is.null(v)) NA else as.logical(v)
    }, logical(1)),
    applicant_cited     = vapply(docs, function(d) {
      v <- d$applicantCitedExaminerReferenceIndicator
      if (is.null(v)) NA else as.logical(v)
    }, logical(1))
  )
}

# == Dataset fields ============================================================

#' List searchable fields for a USPTO dataset
#'
#' Returns the API metadata for a dataset, including available fields
#' and data freshness information.
#'
#' @param dataset One of: "oa_actions", "oa_citations", "oa_rejections",
#'   "enriched_cited_reference_metadata"
#' @param version API version (default: auto-selects latest working version)
#' @return A list with apiKey, apiVersionNumber, apiUrl, and
#'   lastDataUpdatedDate.
#' @export
#' @family USPTO functions
uspto_fields <- function(dataset = "oa_actions", version = NULL) {
  if (is.null(version)) {
    version <- switch(dataset,
      oa_actions = "v1",
      oa_citations = "v1",
      oa_rejections = "v2",
      enriched_cited_reference_metadata = "v2",
      "v1"
    )
  }
  url <- paste0(.uspto_base, "/", dataset, "/", version, "/fields")
  .fetch_compressed(url)
}

# == Convenience: search by patent number ======================================

#' Get all office actions for a patent application
#'
#' Convenience wrapper around uspto_actions() to find all office actions
#' for a specific patent application number.
#'
#' @param app_number Character patent application number (e.g., \code{"15061308"}).
#' @param rows Integer maximum results (default 50).
#' @return A tibble of office actions (same format as \code{\link{uspto_actions}}).
#' @export
#' @family USPTO functions
#' @examples
#' \dontrun{
#' uspto_app_actions("15061308")
#' }
uspto_app_actions <- function(app_number, rows = 50) {
  query <- paste0("patentApplicationNumber:", app_number)
  uspto_actions(query = query, rows = rows)
}

#' Get all citations for a patent application
#'
#' Convenience wrapper to find all prior art citations for a specific
#' patent application.
#'
#' @param app_number Character patent application number (e.g., \code{"15061308"}).
#' @param rows Integer maximum results (default 100).
#' @return A tibble of citations (same format as \code{\link{uspto_citations}}).
#' @export
#' @family USPTO functions
uspto_app_citations <- function(app_number, rows = 100) {
  query <- paste0("applicationId:", app_number)
  uspto_citations(query = query, rows = rows)
}

#' Get all rejections for a patent application
#'
#' Convenience wrapper to find all claim rejections for a specific
#' patent application.
#'
#' @param app_number Character patent application number (e.g., \code{"15061308"}).
#' @param rows Integer maximum results (default 100).
#' @return A tibble of rejections (same format as \code{\link{uspto_rejections}}).
#' @export
#' @family USPTO functions
uspto_app_rejections <- function(app_number, rows = 100) {
  query <- paste0("patentApplicationNumber:", app_number)
  uspto_rejections(query = query, rows = rows)
}

# == Search by invention title =================================================

#' Search patents by invention title
#'
#' Convenience function to search office actions by invention title keywords.
#'
#' @param title_query Character keywords to search in invention titles
#'   (e.g., \code{"quantum computing"}, \code{"machine learning"}).
#' @param rows Integer number of results (default 25).
#' @param start Integer offset for pagination (default 0).
#' @return A tibble of office actions matching the title query
#'   (same format as \code{\link{uspto_actions}}).
#' @export
#' @family USPTO functions
#' @seealso \code{\link{uspto_actions}} for Solr query syntax
#' @examples
#' \dontrun{
#' uspto_search("quantum computing", rows = 10)
#' }
uspto_search <- function(title_query, rows = 25, start = 0) {
  query <- paste0("inventionTitle:", title_query)
  uspto_actions(query = query, rows = rows, start = start)
}

# == Context ===================================================================

#' Get uspto.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
uspto_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(uspto_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/uspto.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "uspto.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# uspto.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# uspto.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
