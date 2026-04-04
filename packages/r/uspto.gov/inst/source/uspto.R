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
#' @return tibble with columns: id, patent_app_number, patent_number,
#'   invention_title, filing_date, grant_date, submission_date,
#'   tech_center, work_group, art_unit, status_number, doc_code
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
#' @param app_number Patent application number (e.g. "15061308")
#' @param rows Number of actions to return (default 10)
#' @return tibble with id, patent_app_number, submission_date, doc_code, body_text
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
#' @return tibble: id, application_id, cited_patent, form892, form1449, cited_in_oa
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
#' @return tibble: id, patent_app_number, invention_title, office_action_date,
#'   action_category, has_rej_101, has_rej_102, has_rej_103, has_rej_dp,
#'   tech_center, art_unit, cited_document
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
#' @return tibble: id, patent_app_number, inventor, cited_document,
#'   citation_category, office_action_date, action_category,
#'   tech_center, art_unit, examiner_cited, applicant_cited
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
#' @return list with apiKey, apiVersionNumber, apiUrl, lastDataUpdatedDate
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
#' @param app_number Patent application number (e.g. "15061308")
#' @param rows Maximum results (default 50)
#' @return tibble of office actions (same format as uspto_actions())
uspto_app_actions <- function(app_number, rows = 50) {
  query <- paste0("patentApplicationNumber:", app_number)
  uspto_actions(query = query, rows = rows)
}

#' Get all citations for a patent application
#'
#' Convenience wrapper to find all prior art citations for a specific
#' patent application.
#'
#' @param app_number Patent application number (e.g. "15061308")
#' @param rows Maximum results (default 100)
#' @return tibble of citations (same format as uspto_citations())
uspto_app_citations <- function(app_number, rows = 100) {
  query <- paste0("applicationId:", app_number)
  uspto_citations(query = query, rows = rows)
}

#' Get all rejections for a patent application
#'
#' Convenience wrapper to find all claim rejections for a specific
#' patent application.
#'
#' @param app_number Patent application number (e.g. "15061308")
#' @param rows Maximum results (default 100)
#' @return tibble of rejections (same format as uspto_rejections())
uspto_app_rejections <- function(app_number, rows = 100) {
  query <- paste0("patentApplicationNumber:", app_number)
  uspto_rejections(query = query, rows = rows)
}

# == Search by invention title =================================================

#' Search patents by invention title
#'
#' Convenience function to search office actions by invention title keywords.
#'
#' @param title_query Keywords to search in invention titles (e.g. "quantum computing")
#' @param rows Number of results (default 25)
#' @param start Offset for pagination (default 0)
#' @return tibble of office actions matching the title query
uspto_search <- function(title_query, rows = 25, start = 0) {
  query <- paste0("inventionTitle:", title_query)
  uspto_actions(query = query, rows = rows, start = start)
}

# == Context ===================================================================

#' Return full function source for LLM context
#'
#' Reads this source file and returns all public function bodies with their
#' roxygen documentation. Useful for providing complete API context to LLMs.
#'
#' @return character string of all public function definitions (invisibly)
uspto_context <- function() {
  src_file <- NULL
  tryCatch({
    for (i in rev(seq_len(sys.nframe()))) {
      env <- sys.frame(i)
      if (exists("ofile", envir = env, inherits = FALSE)) {
        src_file <- get("ofile", envir = env, inherits = FALSE)
        break
      }
    }
    if (is.null(src_file)) {
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <- f
    }
  }, error = function(e) NULL)
  if (is.null(src_file)) src_file <- "clients/uspto.gov.R"
  if (!file.exists(src_file)) {
    cat("# uspto.gov context - source not found\n")
    return(invisible("# uspto.gov context - source not found"))
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
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
