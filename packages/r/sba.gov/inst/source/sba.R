# sba.gov.R - Self-contained SBA (Small Business Administration) data client
#
# Data source: data.sba.gov CKAN v3 Action API
# Provides access to SBA datasets: loan programs (7a, 504, PPP, EIDL),
# disaster loans, size standards, HUBZone designations, and more.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sba_base <- "https://data.sba.gov/api/3/action"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- CKAN URL builder ----------------------------------------------------------

.sba_url <- function(action, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params) == 0) return(paste0(.sba_base, "/", action))
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(.sba_base, "/", action, "?", query)
}

.sba_result <- function(url) {
  raw <- .fetch_json(url)
  if (!isTRUE(raw$success)) {
    warning("SBA CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(),
  metadata_created = as.POSIXct(character()), notes = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character(),
  created = as.POSIXct(character()), size = numeric()
)

.schema_organizations <- tibble(
  id = character(), name = character(), title = character(),
  package_count = integer(), description = character()
)

.schema_tags <- tibble(
  id = character(), name = character()
)

# == Dataset search ============================================================

#' Search SBA datasets
#'
#' Searches the SBA open data catalog (data.sba.gov). Covers loan programs
#' (7a, 504, PPP, EIDL), disaster loans, size standards, HUBZone data, and more.
#'
#' @param query Search term (e.g. "loan", "disaster", "ppp", "size standards")
#' @param organization Filter by SBA office slug (e.g. "office-of-capital-access",
#'   "advocacy", "disaster"). Use sba_organizations() to see all.
#' @param tag Filter by tag (e.g. "sba", "disaster-loan", "covid-19")
#' @param rows Number of results (default 50, max 1000)
#' @param start Offset for pagination (default 0)
#' @return tibble: id, name, title, organization, num_resources,
#'   metadata_created (POSIXct), notes
sba_search <- function(query = NULL, organization = NULL, tag = NULL,
                       rows = 50, start = 0) {
  fq_parts <- character()
  if (!is.null(organization)) fq_parts <- c(fq_parts, paste0("organization:", organization))
  if (!is.null(tag)) fq_parts <- c(fq_parts, paste0("tags:", tag))
  fq <- if (length(fq_parts) > 0) paste(fq_parts, collapse = " ") else NULL

  url <- .sba_url("package_search",
    q     = if (!is.null(query)) utils::URLencode(query, reserved = TRUE) else NULL,
    fq    = if (!is.null(fq)) utils::URLencode(fq, reserved = TRUE) else NULL,
    rows  = rows,
    start = start
  )

  result <- .sba_result(url)
  if (is.null(result)) return(.schema_datasets)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_datasets)

  tibble(
    id               = vapply(datasets, function(d) d$id %||% NA_character_, character(1)),
    name             = vapply(datasets, function(d) d$name %||% NA_character_, character(1)),
    title            = vapply(datasets, function(d) d$title %||% NA_character_, character(1)),
    organization     = vapply(datasets, function(d) {
      d$organization$name %||% NA_character_
    }, character(1)),
    num_resources    = vapply(datasets, function(d) as.integer(d$num_resources %||% 0L), integer(1)),
    metadata_created = as.POSIXct(vapply(datasets, function(d) {
      d$metadata_created %||% NA_character_
    }, character(1))),
    notes            = vapply(datasets, function(d) {
      n <- d$notes %||% ""
      if (nchar(n) > 300) paste0(substr(n, 1, 300), "...") else n
    }, character(1))
  )
}

# == List all datasets =========================================================

#' List all SBA dataset names
#'
#' Returns a character vector of all dataset identifiers available on data.sba.gov.
#'
#' @return character vector of dataset names/slugs
sba_list <- function() {
  result <- .sba_result(.sba_url("package_list"))
  if (is.null(result)) return(character())
  as.character(unlist(result))
}

# == Dataset details ===========================================================

#' Get full details for an SBA dataset
#'
#' Returns metadata and resource list for a single dataset.
#'
#' @param id Dataset name or UUID (e.g. "7-a-504-foia", "ppp-foia", "disaster-loan-data")
#' @return tibble: one row with id, name, title, organization,
#'   num_resources, metadata_created, notes
sba_dataset <- function(id) {
  result <- .sba_result(.sba_url("package_show", id = id))
  if (is.null(result)) return(.schema_datasets)

  tibble(
    id               = result$id %||% NA_character_,
    name             = result$name %||% NA_character_,
    title            = result$title %||% NA_character_,
    organization     = result$organization$name %||% NA_character_,
    num_resources    = as.integer(result$num_resources %||% 0L),
    metadata_created = as.POSIXct(result$metadata_created %||% NA_character_),
    notes            = result$notes %||% NA_character_
  )
}

# == Dataset resources =========================================================

#' Get resources (downloadable files) for an SBA dataset
#'
#' Lists all downloadable files (CSV, XLSX, JSON, etc.) within a dataset.
#'
#' @param id Dataset name or UUID (e.g. "7-a-504-foia", "ppp-foia")
#' @return tibble: id, name, format, url, description, created, size
sba_resources <- function(id) {
  result <- .sba_result(.sba_url("package_show", id = id))
  if (is.null(result)) return(.schema_resources)

  res <- result$resources
  if (length(res) == 0) return(.schema_resources)

  tibble(
    id          = vapply(res, function(r) r$id %||% NA_character_, character(1)),
    name        = vapply(res, function(r) r$name %||% NA_character_, character(1)),
    format      = vapply(res, function(r) toupper(r$format %||% ""), character(1)),
    url         = vapply(res, function(r) r$url %||% NA_character_, character(1)),
    description = vapply(res, function(r) {
      d <- r$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    created     = as.POSIXct(vapply(res, function(r) r$created %||% NA_character_, character(1))),
    size        = vapply(res, function(r) as.numeric(r$size %||% NA_real_), numeric(1))
  )
}

# == Organizations =============================================================

#' List SBA organizations (offices)
#'
#' Returns all SBA organizational units that publish data on data.sba.gov.
#'
#' @param all_fields If TRUE, returns full details; if FALSE just names
#' @return tibble: id, name, title, package_count, description (if all_fields)
#'   or character vector of names (if not)
sba_organizations <- function(all_fields = TRUE) {
  if (!all_fields) {
    result <- .sba_result(.sba_url("organization_list"))
    if (is.null(result)) return(character())
    return(as.character(unlist(result)))
  }

  result <- .sba_result(.sba_url("organization_list", all_fields = "true"))
  if (is.null(result)) return(.schema_organizations)

  orgs <- result
  if (length(orgs) == 0) return(.schema_organizations)

  tibble(
    id            = vapply(orgs, function(o) o$id %||% NA_character_, character(1)),
    name          = vapply(orgs, function(o) o$name %||% NA_character_, character(1)),
    title         = vapply(orgs, function(o) o$title %||% NA_character_, character(1)),
    package_count = vapply(orgs, function(o) as.integer(o$package_count %||% 0L), integer(1)),
    description   = vapply(orgs, function(o) {
      d <- o$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1))
  )
}

# == Tags ======================================================================

#' List SBA dataset tags
#'
#' Returns all tags used to categorize datasets on data.sba.gov.
#'
#' @return character vector of tag names
sba_tags <- function() {
  result <- .sba_result(.sba_url("tag_list"))
  if (is.null(result)) return(character())
  as.character(unlist(result))
}

# == Download resource as tibble ===============================================

#' Download and parse an SBA CSV resource
#'
#' Downloads a CSV resource by URL and returns it as a tibble.
#' For XLSX resources, downloads the file and returns the file path.
#'
#' @param url Full URL to the resource (from sba_resources()$url)
#' @param ... Additional arguments passed to read.csv()
#' @return tibble of parsed CSV data, or file path for non-CSV formats
sba_download <- function(url, ...) {
  ext <- if (grepl("\\.xlsx?$", url, ignore.case = TRUE)) ".xlsx"
         else if (grepl("\\.csv$", url, ignore.case = TRUE)) ".csv"
         else ".dat"

  tmp <- .fetch(url, ext = ext)

  if (ext == ".csv") {
    df <- utils::read.csv(tmp, stringsAsFactors = FALSE, ...)
    return(tibble::as_tibble(df))
  }

  message("File downloaded to: ", tmp)
  invisible(tmp)
}

# == Convenience: Loan programs ================================================

#' List SBA loan program datasets
#'
#' Searches for datasets related to SBA loan programs (7a, 504, PPP, EIDL,
#' disaster loans, microloans).
#'
#' @return tibble of loan-related datasets with id, name, title, organization,
#'   num_resources, metadata_created, notes
sba_loan_programs <- function() {
  sba_search(query = "loan", rows = 100)
}

# == Convenience: Size standards ===============================================

#' Get SBA small business size standards dataset
#'
#' Returns metadata and resources for the current small business size standards,
#' which define thresholds for qualifying as a "small business" by NAICS code.
#'
#' @return tibble of resources for the size standards dataset
sba_size_standards <- function() {
  sba_resources("small-business-size-standards")
}

# == Convenience: PPP data =====================================================

#' Get SBA Paycheck Protection Program (PPP) FOIA resources
#'
#' Returns downloadable resources for PPP loan data released under FOIA.
#' Includes loans over $150k (with business names) and under $150k.
#'
#' @return tibble of PPP FOIA data resources
sba_ppp <- function() {
  sba_resources("ppp-foia")
}

# == Convenience: Disaster loans ===============================================

#' Get SBA disaster loan data resources
#'
#' Returns downloadable resources for historical SBA disaster loan data,
#' segmented by fiscal year (FY2000-FY2022).
#'
#' @return tibble of disaster loan data resources
sba_disaster_loans <- function() {
  sba_resources("disaster-loan-data")
}

# == Context ===================================================================

#' Return full function source for LLM context
#'
#' Reads this source file and returns all public function bodies with their
#' roxygen documentation. Useful for providing complete API context to LLMs.
#'
#' @return character string of all public function definitions (invisibly)
sba_context <- function() {
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
  if (is.null(src_file)) src_file <- "clients/sba.gov.R"
  if (!file.exists(src_file)) {
    cat("# sba.gov context - source not found\n")
    return(invisible("# sba.gov context - source not found"))
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
