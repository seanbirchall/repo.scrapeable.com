# regulations-gov.R
# Self-contained Regulations.gov API v4 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. DEMO_KEY works for testing (limited rate).
#   Get a key at https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 30/hr. Real key = 1000/hr.
# Docs: https://open.gsa.gov/api/regulationsgov/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.regs_base <- "https://api.regulations.gov/v4"

# -- Context generator ---------------------------------------------------------

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

# -- Fetch helper (JSON:API format) --------------------------------------------

.regs_get <- function(endpoint, params = list(), api_key = "DEMO_KEY",
                      max_results = NULL) {
  params$api_key <- api_key
  if (is.null(params[["page[size]"]])) params[["page[size]"]] <- min(max_results %||% 250, 250)
  if (is.null(params[["page[number]"]])) params[["page[number]"]] <- 1

  all_data <- list()
  repeat {
    query <- paste(names(params),
                   vapply(params, function(v) utils::URLencode(as.character(v), reserved = FALSE),
                          character(1)),
                   sep = "=", collapse = "&")
    url <- paste0(.regs_base, "/", endpoint, "?", query)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_retry(max_tries = 2, backoff = function(...) 10) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    data <- raw$data
    if (is.null(data) || length(data) == 0 || nrow(data) == 0) break

    # Flatten JSON:API structure: merge id + attributes
    df <- as_tibble(data$attributes)
    df$id <- data$id
    df$type <- data$type
    all_data[[length(all_data) + 1]] <- df

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break

    has_next <- raw$meta$hasNextPage %||% FALSE
    if (!has_next) break
    params[["page[number]"]] <- params[["page[number]"]] + 1
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Type date columns
  for (col in names(result)) {
    if (grepl("date|Date", col) && is.character(result[[col]])) {
      vals <- result[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike) > length(vals) * 0.5)
        result[[col]] <- as.Date(substr(vals, 1, 10))
    }
  }
  result
}


# == Documents =================================================================

#' Search federal regulatory documents
#'
#' Searches proposed rules, final rules, notices, and other documents
#' published in the Federal Register.
#'
#' @param query Search term
#' @param agency Agency acronym filter (e.g. "EPA", "FDA", "DOL", "SEC")
#' @param document_type "Rule", "Proposed Rule", "Notice", "Other"
#' @param posted_date_from Start date (YYYY-MM-DD)
#' @param posted_date_to End date
#' @param docket_id Docket ID filter
#' @param api_key API key (default: DEMO_KEY)
#' @param max_results Max results (default 100)
#' @return tibble: id, title, agencyId, documentType, postedDate,
#'   commentEndDate, frDocNum, ...
regs_documents <- function(query = NULL, agency = NULL, document_type = NULL,
                           posted_date_from = NULL, posted_date_to = NULL,
                           docket_id = NULL, api_key = "DEMO_KEY",
                           max_results = 100) {
  params <- list()
  if (!is.null(query))            params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))           params[["filter[agencyId]"]] <- agency
  if (!is.null(document_type))    params[["filter[documentType]"]] <- document_type
  if (!is.null(posted_date_from)) params[["filter[postedDate][ge]"]] <- posted_date_from
  if (!is.null(posted_date_to))   params[["filter[postedDate][le]"]] <- posted_date_to
  if (!is.null(docket_id))        params[["filter[docketId]"]] <- docket_id
  .regs_get("documents", params, api_key, max_results)
}

#' Fetch a single document by ID
#'
#' @param document_id Document ID (from regs_documents results)
#' @param api_key API key
#' @return tibble: one row with full document details
regs_document <- function(document_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/documents/%s?api_key=%s", .regs_base, document_id, api_key)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw$data)) return(tibble())
  df <- as_tibble(as.list(raw$data$attributes))
  df$id <- raw$data$id
  df
}


# == Comments ==================================================================

#' Search public comments on regulations
#'
#' @param query Search term
#' @param agency Agency acronym filter
#' @param document_id Filter to comments on a specific document
#' @param posted_date_from Start date
#' @param posted_date_to End date
#' @param api_key API key
#' @param max_results Max results (default 100)
#' @return tibble: id, title, agencyId, postedDate, comment text, ...
regs_comments <- function(query = NULL, agency = NULL, document_id = NULL,
                          posted_date_from = NULL, posted_date_to = NULL,
                          api_key = "DEMO_KEY", max_results = 100) {
  params <- list()
  if (!is.null(query))            params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))           params[["filter[agencyId]"]] <- agency
  if (!is.null(document_id))      params[["filter[commentOnId]"]] <- document_id
  if (!is.null(posted_date_from)) params[["filter[postedDate][ge]"]] <- posted_date_from
  if (!is.null(posted_date_to))   params[["filter[postedDate][le]"]] <- posted_date_to
  .regs_get("comments", params, api_key, max_results)
}

#' Fetch a single comment by ID
#'
#' @param comment_id Comment ID
#' @param api_key API key
#' @return tibble: one row with full comment details including text
regs_comment <- function(comment_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/comments/%s?api_key=%s", .regs_base, comment_id, api_key)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw$data)) return(tibble())
  df <- as_tibble(as.list(raw$data$attributes))
  df$id <- raw$data$id
  df
}


# == Dockets ===================================================================

#' Search regulatory dockets
#'
#' A docket is a collection of related documents and comments on a rulemaking.
#'
#' @param query Search term
#' @param agency Agency acronym filter
#' @param docket_type "Rulemaking" or "Nonrulemaking"
#' @param api_key API key
#' @param max_results Max results (default 50)
#' @return tibble: id, title, agencyId, docketType, ...
regs_dockets <- function(query = NULL, agency = NULL, docket_type = NULL,
                         api_key = "DEMO_KEY", max_results = 50) {
  params <- list()
  if (!is.null(query))       params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))      params[["filter[agencyId]"]] <- agency
  if (!is.null(docket_type)) params[["filter[docketType]"]] <- docket_type
  .regs_get("dockets", params, api_key, max_results)
}

#' Fetch a single docket by ID
#'
#' @param docket_id Docket ID (e.g. "EPA-HQ-OAR-2021-0208")
#' @param api_key API key
#' @return tibble: one row with docket details
regs_docket <- function(docket_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/dockets/%s?api_key=%s", .regs_base, docket_id, api_key)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw$data)) return(tibble())
  df <- as_tibble(as.list(raw$data$attributes))
  df$id <- raw$data$id
  df
}


# == Context ===================================================================

#' Generate LLM-friendly context for the api.regulations.gov package
#'
#' @return Character string (invisibly), also printed
regs_context <- function() {
  .build_context("api.regulations.gov", header_lines = c(
    "# api.regulations.gov - Federal Rulemaking Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: API key required (api_key param). DEMO_KEY for testing.",
    "#   Get a key at https://api.data.gov/signup/",
    "# All functions return tibbles.",
    "#",
    "# Three main resources:",
    "#   Documents = proposed rules, final rules, notices",
    "#   Comments  = public comments on documents",
    "#   Dockets   = collections of related documents/comments",
    "#",
    "# Agency acronyms: EPA, FDA, DOL, SEC, DOT, FCC, USDA, HHS, DOD, etc."
  ))
}
