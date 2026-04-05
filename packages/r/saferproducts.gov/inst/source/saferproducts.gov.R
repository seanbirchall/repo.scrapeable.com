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

#' List recent CPSC product recalls
#'
#' Fetches consumer product recalls from the U.S. Consumer Product Safety
#' Commission (CPSC) within a date range. Defaults to the last 90 days.
#'
#' @param start_date Character or Date. Start of date range in
#'   \code{"YYYY-MM-DD"} format. Defaults to 90 days before today.
#' @param end_date Character or Date. End of date range in
#'   \code{"YYYY-MM-DD"} format. Defaults to today.
#' @param limit Integer. Maximum number of recalls to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{recall_id}{Integer. Unique CPSC recall identifier.}
#'     \item{recall_number}{Character. CPSC recall number (e.g. \code{"26365"}).}
#'     \item{recall_date}{Date. Date the recall was issued.}
#'     \item{title}{Character. Recall headline.}
#'     \item{description}{Character. Full recall description.}
#'     \item{products}{Character. Semicolon-separated recalled product names.}
#'     \item{number_of_units}{Character. Number of units affected (e.g. \code{"About 200"}).}
#'     \item{injuries}{Character. Semicolon-separated reported injury types.}
#'     \item{manufacturer_countries}{Character. Semicolon-separated countries of manufacture.}
#'     \item{consumer_contact}{Character. Consumer contact information.}
#'     \item{url}{Character. URL to full recall notice.}
#'     \item{last_published}{Date. Date of last publication update.}
#'   }
#' @examples
#' safer_list()
#' safer_list(start_date = "2024-01-01", end_date = "2024-06-30")
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

#' Search CPSC product recalls by keyword
#'
#' Searches consumer product recall titles for a keyword or phrase.
#' Covers recalls from 2000 onward by default.
#'
#' @param query Character. Search term matched against recall titles
#'   (e.g. \code{"battery"}, \code{"lead paint"}, \code{"stroller"}).
#' @param start_date Character or Date. Start of date range
#'   (\code{"YYYY-MM-DD"}). Defaults to \code{"2000-01-01"}.
#' @param end_date Character or Date. End of date range. Defaults to today.
#' @param limit Integer. Maximum rows to return (default 100).
#' @return A tibble with the same columns as \code{safer_list()}: recall_id,
#'   recall_number, recall_date, title, description, products,
#'   number_of_units, injuries, manufacturer_countries, consumer_contact,
#'   url, last_published.
#' @examples
#' safer_search("battery")
#' safer_search("stroller", start_date = "2020-01-01")
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
#' Returns the complete CPSC penalty database covering fines levied against
#' manufacturers and distributors for safety violations since the 1970s.
#'
#' @param penalty_type Character or NULL. Filter by type: \code{"Civil"} or
#'   \code{"Criminal"}. NULL (default) returns both.
#' @param limit Integer. Maximum rows to return (default 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{penalty_id}{Integer. Unique penalty identifier.}
#'     \item{recall_number}{Character. Associated recall number.}
#'     \item{firm}{Character. Penalized firm name (e.g. \code{"Mattel / Fisher-Price"}).}
#'     \item{penalty_type}{Character. \code{"Civil"} or \code{"Criminal"}.}
#'     \item{penalty_date}{Date. Date the penalty was assessed.}
#'     \item{act}{Character. Governing statute (e.g. \code{"CPSA"}, \code{"FHSA"}).}
#'     \item{fine}{Character. Fine amount as formatted string (e.g. \code{"$1,100,000"}).}
#'     \item{fine_numeric}{Numeric. Fine parsed to a number for analysis.}
#'     \item{fiscal_year}{Integer. Fiscal year of the penalty.}
#'     \item{product_types}{Character. Semicolon-separated product categories.}
#'     \item{release_title}{Character. Title of the press release.}
#'     \item{release_url}{Character. URL to the CPSC press release.}
#'   }
#' @examples
#' safer_penalties()
#' safer_penalties(penalty_type = "Civil", limit = 10)
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

#' Search CPSC penalties by firm name
#'
#' Filters the full CPSC penalty database for firms matching a keyword.
#' Uses case-insensitive partial matching.
#'
#' @param firm Character. Firm name to search for (e.g. \code{"Fisher"},
#'   \code{"Mattel"}, \code{"IKEA"}).
#' @param limit Integer. Maximum rows to return (default 100).
#' @return A tibble with the same columns as \code{safer_penalties()}:
#'   penalty_id, recall_number, firm, penalty_type, penalty_date, act,
#'   fine, fine_numeric, fiscal_year, product_types, release_title,
#'   release_url.
#' @examples
#' safer_penalty_search("Fisher")
#' safer_penalty_search("IKEA")
#' @export
safer_penalty_search <- function(firm, limit = 100) {
  url <- paste0(.safer_base, "/Penalty?format=json")
  raw <- .fetch_json(url)
  result <- .parse_penalties(raw)
  pattern <- firm
  result <- result |> filter(grepl(pattern, firm, ignore.case = TRUE))
  head(result, limit)
}

#' Summarize CPSC penalties by fiscal year
#'
#' Aggregates the full penalty database by fiscal year, showing the number
#' of penalties and total fines per year.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fiscal_year}{Integer. Fiscal year (e.g. 2020, 2021).}
#'     \item{n_penalties}{Integer. Number of penalties assessed that year.}
#'     \item{total_fines}{Numeric. Sum of all fines in USD for that year.}
#'   }
#' @examples
#' safer_penalties_by_year()
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

#' Get a single CPSC recall by recall number
#'
#' Fetches the full details for a specific recall by its CPSC recall number.
#'
#' @param recall_number Character. CPSC recall number (e.g. \code{"26365"},
#'   \code{"24090"}). Obtain from \code{safer_list()} or \code{safer_search()}.
#' @return A tibble with one row and the same columns as \code{safer_list()}:
#'   recall_id, recall_number, recall_date, title, description, products,
#'   number_of_units, injuries, manufacturer_countries, consumer_contact,
#'   url, last_published.
#' @examples
#' safer_recall("26365")
#' @export
safer_recall <- function(recall_number) {
  url <- sprintf("%s/Recall?format=json&RecallNumber=%s", .safer_base, recall_number)
  raw <- .fetch_json(url)
  .parse_recalls(raw)
}

# == Context ===================================================================

#' Get saferproducts.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' safer_context()
#' @export
safer_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(safer_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/saferproducts.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "saferproducts.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# saferproducts.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# saferproducts.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
