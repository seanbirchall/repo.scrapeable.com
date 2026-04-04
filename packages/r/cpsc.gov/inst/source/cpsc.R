# cpsc.gov.R
# Self-contained CPSC (Consumer Product Safety Commission) recall client.
# Uses the SaferProducts.gov REST API for recall data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www.saferproducts.gov/RestWebServices

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cpsc_base <- "https://www.saferproducts.gov/RestWebServices"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.cpsc_recall_url <- function(start_date = NULL, end_date = NULL, keyword = NULL) {
  params <- list(format = "json")
  if (!is.null(start_date)) params$RecallDateStart <- format(as.Date(start_date), "%Y-%m-%d")
  if (!is.null(end_date)) params$RecallDateEnd <- format(as.Date(end_date), "%Y-%m-%d")
  if (!is.null(keyword)) params$RecallTitle <- keyword
  query <- paste(names(params), vapply(params, as.character, character(1)), sep = "=", collapse = "&")
  paste0(.cpsc_base, "/Recall?", query)
}

.parse_recalls <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(.schema_recalls)
  if (!is.data.frame(raw)) return(.schema_recalls)

  products <- vapply(seq_len(nrow(raw)), function(i) {
    p <- raw$Products[[i]]
    if (is.null(p) || (is.data.frame(p) && nrow(p) == 0)) return(NA_character_)
    if (is.data.frame(p)) paste(p$Name, collapse = "; ") else NA_character_
  }, character(1))

  injuries <- vapply(seq_len(nrow(raw)), function(i) {
    inj <- raw$Injuries[[i]]
    if (is.null(inj) || (is.data.frame(inj) && nrow(inj) == 0)) return(NA_character_)
    if (is.data.frame(inj)) paste(inj$Name, collapse = "; ") else NA_character_
  }, character(1))

  mfr_countries <- vapply(seq_len(nrow(raw)), function(i) {
    mc <- raw$ManufacturerCountries[[i]]
    if (is.null(mc) || (is.data.frame(mc) && nrow(mc) == 0)) return(NA_character_)
    if (is.data.frame(mc)) paste(mc$Country, collapse = "; ") else NA_character_
  }, character(1))

  tibble(
    recall_id = as.integer(raw$RecallID),
    recall_number = as.character(raw$RecallNumber),
    recall_date = as.Date(raw$RecallDate),
    title = as.character(raw$Title),
    description = as.character(raw$Description),
    products = products,
    injuries = injuries,
    manufacturer_countries = mfr_countries,
    consumer_contact = as.character(raw$ConsumerContact),
    url = as.character(raw$URL),
    last_published = as.Date(raw$LastPublishDate)
  )
}

# == Schemas ===================================================================

.schema_recalls <- tibble(
  recall_id = integer(), recall_number = character(), recall_date = as.Date(character()),
  title = character(), description = character(), products = character(),
  injuries = character(), manufacturer_countries = character(),
  consumer_contact = character(), url = character(),
  last_published = as.Date(character())
)

# == Public functions ==========================================================

#' List recent CPSC recalls
#'
#' @param start_date Start date (YYYY-MM-DD). Defaults to 90 days ago.
#' @param end_date End date (YYYY-MM-DD). Defaults to today.
#' @param limit Maximum rows to return (default 100).
#' @return tibble of recalls
#' @export
cpsc_list <- function(start_date = NULL, end_date = NULL, limit = 100) {
  if (is.null(start_date)) start_date <- Sys.Date() - 90
  if (is.null(end_date)) end_date <- Sys.Date()
  url <- .cpsc_recall_url(start_date = start_date, end_date = end_date)
  raw <- .fetch_json(url)
  result <- .parse_recalls(raw)
  head(result, limit)
}

#' Search CPSC recalls by keyword
#'
#' @param query Search term matched against recall titles.
#' @param start_date Optional start date (YYYY-MM-DD).
#' @param end_date Optional end date (YYYY-MM-DD).
#' @param limit Maximum rows to return (default 100).
#' @return tibble of matching recalls
#' @export
cpsc_search <- function(query, start_date = NULL, end_date = NULL, limit = 100) {
  if (is.null(start_date)) start_date <- "2000-01-01"
  if (is.null(end_date)) end_date <- Sys.Date()

  url <- .cpsc_recall_url(start_date = start_date, end_date = end_date, keyword = query)
  raw <- .fetch_json(url)
  result <- .parse_recalls(raw)
  head(result, limit)
}

#' Get a single CPSC recall by recall number
#'
#' @param recall_number Recall number (e.g. "24090").
#' @return tibble with one row of recall details
#' @export
cpsc_recall <- function(recall_number) {
  url <- paste0(.cpsc_base, "/Recall?format=json&RecallNumber=", recall_number)
  raw <- .fetch_json(url)
  .parse_recalls(raw)
}

#' Get CPSC recalls by year
#'
#' @param year Integer year (e.g. 2024).
#' @param limit Maximum rows to return (default 500).
#' @return tibble of recalls for the given year
#' @export
cpsc_by_year <- function(year, limit = 500) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  url <- .cpsc_recall_url(start_date = start_date, end_date = end_date)
  raw <- .fetch_json(url)
  result <- .parse_recalls(raw)
  head(result, limit)
}

#' Summarize CPSC recalls by year
#'
#' Fetches recalls from 2015 to present and counts per year.
#'
#' @return tibble: year, n_recalls
#' @export
cpsc_summary <- function() {
  years <- seq(2015, as.integer(format(Sys.Date(), "%Y")))
  results <- lapply(years, function(y) {
    url <- .cpsc_recall_url(start_date = paste0(y, "-01-01"), end_date = paste0(y, "-12-31"))
    raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
    n <- if (is.null(raw)) 0L else if (is.data.frame(raw)) nrow(raw) else length(raw)
    tibble(year = y, n_recalls = as.integer(n))
  })
  bind_rows(results)
}

# == Context ===================================================================

#' Show CPSC client context for LLM use
#'
#' @return Invisibly returns context string
#' @export
cpsc_context <- function() {
  .build_context("cpsc.gov")
}

.build_context <- function(pkg_name) {
  src_dir <- system.file("source", package = pkg_name)
  if (src_dir != "" && length(list.files(src_dir, pattern = "[.]R$")) > 0) {
    src_file <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)[1]
  } else {
    src_file <- NULL
    tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
    if (is.null(src_file)) src_file <- paste0("clients/", pkg_name, ".R")
  }
  if (is.null(src_file) || !file.exists(src_file)) {
    cat("# ", pkg_name, " context - source not found\n")
    return(invisible(paste0("# ", pkg_name, " context - source not found")))
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
