# saferproducts.gov.R
# Self-contained SaferProducts.gov client (CPSC).
# Covers Recalls and Civil/Criminal Penalties APIs.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www.saferproducts.gov/

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.safer_base <- "https://www.saferproducts.gov/RestWebServices"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.parse_recalls <- function(raw) {
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_recalls)

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

  n_units <- vapply(seq_len(nrow(raw)), function(i) {
    p <- raw$Products[[i]]
    if (is.null(p) || !is.data.frame(p) || nrow(p) == 0) return(NA_character_)
    paste(p$NumberOfUnits, collapse = "; ")
  }, character(1))

  tibble(
    recall_id = as.integer(raw$RecallID),
    recall_number = as.character(raw$RecallNumber),
    recall_date = as.Date(raw$RecallDate),
    title = as.character(raw$Title),
    description = as.character(raw$Description),
    products = products,
    number_of_units = n_units,
    injuries = injuries,
    manufacturer_countries = mfr_countries,
    consumer_contact = as.character(raw$ConsumerContact),
    url = as.character(raw$URL),
    last_published = as.Date(raw$LastPublishDate)
  )
}

.parse_penalties <- function(raw) {
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_penalties)

  product_types <- vapply(seq_len(nrow(raw)), function(i) {
    pt <- raw$ProductTypes[[i]]
    if (is.null(pt) || (is.data.frame(pt) && nrow(pt) == 0)) return(NA_character_)
    if (is.data.frame(pt)) paste(pt$Type, collapse = "; ") else NA_character_
  }, character(1))

  tibble(
    penalty_id = as.integer(raw$PenaltyID),
    recall_number = as.character(raw$RecallNo),
    firm = as.character(raw$Firm),
    penalty_type = as.character(raw$PenaltyType),
    penalty_date = as.Date(raw$PenaltyDate),
    act = as.character(raw$Act),
    fine = as.character(raw$Fine),
    fine_numeric = suppressWarnings(as.numeric(gsub("[^0-9.]", "", raw$Fine))),
    fiscal_year = as.integer(raw$FiscalYear),
    product_types = product_types,
    release_title = as.character(raw$ReleaseTitle),
    release_url = as.character(raw$ReleaseURL)
  )
}

# == Schemas ===================================================================

.schema_recalls <- tibble(
  recall_id = integer(), recall_number = character(), recall_date = as.Date(character()),
  title = character(), description = character(), products = character(),
  number_of_units = character(), injuries = character(),
  manufacturer_countries = character(), consumer_contact = character(),
  url = character(), last_published = as.Date(character())
)

.schema_penalties <- tibble(
  penalty_id = integer(), recall_number = character(), firm = character(),
  penalty_type = character(), penalty_date = as.Date(character()),
  act = character(), fine = character(), fine_numeric = numeric(),
  fiscal_year = integer(), product_types = character(),
  release_title = character(), release_url = character()
)

# == Public functions ==========================================================

#' List recent product recalls
#'
#' @param start_date Start date (YYYY-MM-DD). Defaults to 90 days ago.
#' @param end_date End date (YYYY-MM-DD). Defaults to today.
#' @param limit Maximum rows to return (default 100).
#' @return tibble of recalls
#' @export
safer_list <- function(start_date = NULL, end_date = NULL, limit = 100) {
  if (is.null(start_date)) start_date <- Sys.Date() - 90
  if (is.null(end_date)) end_date <- Sys.Date()
  url <- sprintf("%s/Recall?format=json&RecallDateStart=%s&RecallDateEnd=%s",
                 .safer_base, format(as.Date(start_date), "%Y-%m-%d"),
                 format(as.Date(end_date), "%Y-%m-%d"))
  raw <- .fetch_json(url)
  result <- .parse_recalls(raw)
  head(result, limit)
}

#' Search product recalls by keyword
#'
#' @param query Search term (matched against recall titles).
#' @param start_date Optional start date (YYYY-MM-DD). Defaults to 2000-01-01.
#' @param end_date Optional end date (YYYY-MM-DD). Defaults to today.
#' @param limit Maximum rows (default 100).
#' @return tibble of matching recalls
#' @export
safer_search <- function(query, start_date = NULL, end_date = NULL, limit = 100) {
  if (is.null(start_date)) start_date <- "2000-01-01"
  if (is.null(end_date)) end_date <- Sys.Date()
  url <- sprintf("%s/Recall?format=json&RecallDateStart=%s&RecallDateEnd=%s&RecallTitle=%s",
                 .safer_base, format(as.Date(start_date), "%Y-%m-%d"),
                 format(as.Date(end_date), "%Y-%m-%d"),
                 utils::URLencode(query, reserved = TRUE))
  raw <- .fetch_json(url)
  result <- .parse_recalls(raw)
  head(result, limit)
}

#' Get all CPSC civil and criminal penalties
#'
#' Returns the full penalty database (all years).
#'
#' @param penalty_type Optional: "Civil" or "Criminal".
#' @param limit Maximum rows (default 500).
#' @return tibble of penalties
#' @export
safer_penalties <- function(penalty_type = NULL, limit = 500) {
  url <- paste0(.safer_base, "/Penalty?format=json")
  raw <- .fetch_json(url)
  result <- .parse_penalties(raw)
  if (!is.null(penalty_type)) {
    result <- result |> filter(grepl(penalty_type, .data$penalty_type, ignore.case = TRUE))
  }
  head(result, limit)
}

#' Search penalties by firm name
#'
#' @param firm Firm name (case-insensitive partial match).
#' @param limit Maximum rows (default 100).
#' @return tibble of matching penalties
#' @export
safer_penalty_search <- function(firm, limit = 100) {
  url <- paste0(.safer_base, "/Penalty?format=json")
  raw <- .fetch_json(url)
  result <- .parse_penalties(raw)
  pattern <- firm
  result <- result |> filter(grepl(pattern, firm, ignore.case = TRUE))
  head(result, limit)
}

#' Summarize penalties by fiscal year
#'
#' @return tibble: fiscal_year, n_penalties, total_fines
#' @export
safer_penalties_by_year <- function() {
  url <- paste0(.safer_base, "/Penalty?format=json")
  raw <- .fetch_json(url)
  result <- .parse_penalties(raw)
  result |>
    group_by(fiscal_year) |>
    summarise(
      n_penalties = n(),
      total_fines = sum(fine_numeric, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(fiscal_year)
}

#' Get a single recall by recall number
#'
#' @param recall_number Recall number (e.g. "24090").
#' @return tibble with one row
#' @export
safer_recall <- function(recall_number) {
  url <- sprintf("%s/Recall?format=json&RecallNumber=%s", .safer_base, recall_number)
  raw <- .fetch_json(url)
  .parse_recalls(raw)
}

# == Context ===================================================================

#' Show SaferProducts client context for LLM use
#'
#' @return Invisibly returns context string
#' @export
safer_context <- function() {
  .build_context("saferproducts.gov")
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
