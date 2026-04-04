# cpsc.gov.R
# Self-contained CPSC (Consumer Product Safety Commission) recall client.
# Uses the SaferProducts.gov REST API for recall data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www.saferproducts.gov/RestWebServices


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
#' Fetches consumer product recalls from the SaferProducts.gov API for a
#' date range. Defaults to the last 90 days. Each row is one recall with
#' product names, injury info, and manufacturer countries parsed from
#' nested JSON.
#'
#' @param start_date Character or Date or NULL. Start date (YYYY-MM-DD).
#'   Defaults to 90 days ago. Example: \code{"2025-01-01"}
#' @param end_date Character or Date or NULL. End date (YYYY-MM-DD).
#'   Defaults to today. Example: \code{"2025-06-30"}
#' @param limit Integer. Maximum rows to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{recall_id}{Integer. Internal recall ID.}
#'     \item{recall_number}{Character. Recall number (e.g. "26365").}
#'     \item{recall_date}{Date. Date the recall was issued.}
#'     \item{title}{Character. Recall title (e.g. "Children's Backpack Clips Recalled...").}
#'     \item{description}{Character. Full recall description.}
#'     \item{products}{Character. Semicolon-delimited product names.}
#'     \item{injuries}{Character. Injury descriptions (e.g. "None reported").}
#'     \item{manufacturer_countries}{Character. Semicolon-delimited country names.}
#'     \item{consumer_contact}{Character. Contact information.}
#'     \item{url}{Character. Recall detail URL.}
#'     \item{last_published}{Date. Last publication date.}
#'   }
#' @examples
#' cpsc_list(limit = 10)
#' cpsc_list(start_date = "2025-01-01", end_date = "2025-03-31")
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
#' Searches the SaferProducts.gov recall database by keyword, matched
#' against recall titles. Defaults to searching from 2000 to present.
#'
#' @param query Character. Search term matched against recall titles.
#'   Example: \code{"bicycle"}, \code{"crib"}, \code{"heater"}
#' @param start_date Character or Date or NULL. Start date (YYYY-MM-DD).
#'   Defaults to "2000-01-01". Example: \code{"2020-01-01"}
#' @param end_date Character or Date or NULL. End date (YYYY-MM-DD).
#'   Defaults to today.
#' @param limit Integer. Maximum rows to return (default 100).
#' @return A tibble with the same 11 columns as \code{\link{cpsc_list}}:
#'   recall_id, recall_number, recall_date, title, description, products,
#'   injuries, manufacturer_countries, consumer_contact, url, last_published.
#' @examples
#' cpsc_search("bicycle", limit = 5)
#' cpsc_search("crib", start_date = "2020-01-01")
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
#' Fetches full details for a specific recall by its recall number.
#' Returns a one-row tibble with the same schema as \code{\link{cpsc_list}}.
#'
#' @param recall_number Character. Recall number (e.g. \code{"24090"},
#'   \code{"26365"}). Find these via \code{\link{cpsc_search}} or
#'   \code{\link{cpsc_list}}.
#' @return A tibble with one row and 11 columns: recall_id, recall_number,
#'   recall_date, title, description, products, injuries,
#'   manufacturer_countries, consumer_contact, url, last_published.
#' @examples
#' cpsc_recall("26365")
#' @export
cpsc_recall <- function(recall_number) {
  url <- paste0(.cpsc_base, "/Recall?format=json&RecallNumber=", recall_number)
  raw <- .fetch_json(url)
  .parse_recalls(raw)
}

#' Get CPSC recalls by year
#'
#' Fetches all recalls issued during a specific calendar year.
#' Returns the same schema as \code{\link{cpsc_list}}.
#'
#' @param year Integer. Calendar year (e.g. \code{2024}, \code{2023}).
#' @param limit Integer. Maximum rows to return (default 500).
#' @return A tibble with 11 columns (same as \code{\link{cpsc_list}}).
#' @examples
#' cpsc_by_year(2024, limit = 10)
#' cpsc_by_year(2023)
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
#' Fetches recall counts from 2015 to the current year. Makes one API
#' request per year, so this function may take several seconds. Useful
#' for trend analysis.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (2015 through current year).}
#'     \item{n_recalls}{Integer. Number of recalls issued that year.}
#'   }
#' @examples
#' cpsc_summary()
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

#' Get cpsc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cpsc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cpsc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cpsc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cpsc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cpsc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cpsc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
