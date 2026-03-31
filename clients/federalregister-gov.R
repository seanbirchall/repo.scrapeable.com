# federalregister-gov.R
# Self-contained Federal Register API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://www.federalregister.gov/api/v1

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fr_base <- "https://www.federalregister.gov/api/v1"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_documents <- tibble(
  document_number = character(), title = character(), type = character(),
  publication_date = as.Date(character()), agency = character(),
  abstract = character(), html_url = character(), pdf_url = character()
)

.schema_agencies <- tibble(
  id = integer(), name = character(), short_name = character(),
  slug = character(), url = character(), parent_id = integer()
)

# == Documents =================================================================

#' Search Federal Register documents
#'
#' Searches rules, proposed rules, notices, and presidential documents.
#' Returns up to 10,000 results with pagination.
#'
#' @param term Search term (e.g. "climate change", "artificial intelligence")
#' @param type Document type filter: "RULE", "PRORULE", "NOTICE", "PRESDOCU".
#'   Can be a character vector for multiple types.
#' @param agency Agency slug filter (e.g. "environmental-protection-agency").
#'   Use fr_agencies() to find slugs.
#' @param date_gte Start date filter (Date or "YYYY-MM-DD")
#' @param date_lte End date filter (Date or "YYYY-MM-DD")
#' @param per_page Results per page (default 50, max 1000)
#' @param page Page number (default 1)
#' @param order Sort: "relevance" or "newest" (default "newest")
#' @return tibble: document_number, title, type, publication_date (Date),
#'   agency, abstract, html_url, pdf_url
fr_search <- function(term = NULL, type = NULL, agency = NULL,
                      date_gte = NULL, date_lte = NULL,
                      per_page = 50, page = 1, order = "newest") {
  params <- list(per_page = per_page, page = page, order = order)
  if (!is.null(term)) params[["conditions[term]"]] <- utils::URLencode(term, reserved = TRUE)
  if (!is.null(date_gte)) params[["conditions[publication_date][gte]"]] <- as.character(date_gte)
  if (!is.null(date_lte)) params[["conditions[publication_date][lte]"]] <- as.character(date_lte)

  query_parts <- paste(names(params), vapply(params, as.character, character(1)),
                       sep = "=", collapse = "&")

  # Handle array params
  type_parts <- ""
  if (!is.null(type)) {
    type_parts <- paste0("&", paste(sprintf("conditions[type][]=%s", type), collapse = "&"))
  }
  agency_parts <- ""
  if (!is.null(agency)) {
    agency_parts <- paste0("&", paste(sprintf("conditions[agencies][]=%s", agency), collapse = "&"))
  }

  url <- paste0(.fr_base, "/documents.json?", query_parts, type_parts, agency_parts)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Federal Register API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_documents)

  results <- raw$results
  if (length(results) == 0) return(.schema_documents)

  tibble(
    document_number  = vapply(results, function(d) d$document_number %||% NA_character_, character(1)),
    title            = vapply(results, function(d) d$title %||% NA_character_, character(1)),
    type             = vapply(results, function(d) d$type %||% NA_character_, character(1)),
    publication_date = as.Date(vapply(results, function(d) d$publication_date %||% NA_character_, character(1))),
    agency           = vapply(results, function(d) {
      agencies <- d$agencies
      if (is.null(agencies) || length(agencies) == 0) NA_character_
      else paste(vapply(agencies, function(a) a$name %||% "", character(1)), collapse = "; ")
    }, character(1)),
    abstract         = vapply(results, function(d) {
      ab <- d$abstract %||% ""
      if (nchar(ab) > 300) paste0(substr(ab, 1, 300), "...") else ab
    }, character(1)),
    html_url         = vapply(results, function(d) d$html_url %||% NA_character_, character(1)),
    pdf_url          = vapply(results, function(d) d$pdf_url %||% NA_character_, character(1))
  )
}


#' Fetch a single Federal Register document
#'
#' @param document_number Document number (e.g. "2026-06029")
#' @return tibble: one row with document details
fr_document <- function(document_number) {
  url <- sprintf("%s/documents/%s.json", .fr_base, document_number)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Federal Register API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_documents)

  tibble(
    document_number  = raw$document_number %||% NA_character_,
    title            = raw$title %||% NA_character_,
    type             = raw$type %||% NA_character_,
    publication_date = as.Date(raw$publication_date %||% NA_character_),
    agency           = paste(vapply(raw$agencies %||% list(), function(a) a$name %||% "", character(1)), collapse = "; "),
    abstract         = raw$abstract %||% NA_character_,
    html_url         = raw$html_url %||% NA_character_,
    pdf_url          = raw$pdf_url %||% NA_character_
  )
}


# == Agencies ==================================================================

#' List Federal Register agencies
#'
#' Returns all federal agencies that publish in the Federal Register.
#'
#' @return tibble: id, name, short_name, slug, url, parent_id
fr_agencies <- function() {
  url <- paste0(.fr_base, "/agencies.json")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Federal Register API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_agencies)

  tibble(
    id         = vapply(raw, function(a) as.integer(a$id %||% NA_integer_), integer(1)),
    name       = vapply(raw, function(a) a$name %||% NA_character_, character(1)),
    short_name = vapply(raw, function(a) a$short_name %||% NA_character_, character(1)),
    slug       = vapply(raw, function(a) a$slug %||% NA_character_, character(1)),
    url        = vapply(raw, function(a) a$agency_url %||% NA_character_, character(1)),
    parent_id  = vapply(raw, function(a) as.integer(a$parent_id %||% NA_integer_), integer(1))
  )
}


# == Public Inspection =========================================================

#' Fetch documents currently on public inspection
#'
#' Returns documents filed for public inspection before official publication.
#'
#' @param per_page Results per page (default 50)
#' @param page Page number (default 1)
#' @return tibble: document_number, title, type, publication_date,
#'   agency, abstract, html_url, pdf_url
fr_public_inspection <- function(per_page = 50, page = 1) {
  url <- sprintf("%s/public-inspection-documents/current.json?per_page=%d&page=%d",
                 .fr_base, per_page, page)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Federal Register API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_documents)

  results <- raw$results
  if (length(results) == 0) return(.schema_documents)

  tibble(
    document_number  = vapply(results, function(d) d$document_number %||% NA_character_, character(1)),
    title            = vapply(results, function(d) d$title %||% NA_character_, character(1)),
    type             = vapply(results, function(d) d$type %||% NA_character_, character(1)),
    publication_date = as.Date(vapply(results, function(d) d$publication_date %||% NA_character_, character(1))),
    agency           = vapply(results, function(d) {
      names <- d$agency_names
      if (is.null(names) || length(names) == 0) NA_character_
      else paste(names, collapse = "; ")
    }, character(1)),
    abstract         = vapply(results, function(d) d$abstract %||% NA_character_, character(1)),
    html_url         = vapply(results, function(d) d$html_url %||% NA_character_, character(1)),
    pdf_url          = vapply(results, function(d) d$pdf_url %||% NA_character_, character(1))
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the Federal Register package
#'
#' @return Character string (invisibly), also printed
fr_context <- function() {
  .build_context("federalregister.gov", header_lines = c(
    "# federalregister.gov - Federal Register API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Document types: RULE, PRORULE (proposed), NOTICE, PRESDOCU (presidential)",
    "# Filters: term, type, agency slug, date range",
    "# Pagination: page-based, max 10,000 results"
  ))
}
