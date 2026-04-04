# ffiec.cfpb.gov.R - Self-contained ffiec.cfpb.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# ffiec-cfpb-gov.R
# Self-contained HMDA (Home Mortgage Disclosure Act) data browser client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hmda_base <- "https://ffiec.cfpb.gov/v2/data-browser-api"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_aggregations <- tibble(
  count = integer(), sum = numeric(), actions_taken = character(),
  loan_types = character(), purposes = character(),
  races = character(), ethnicities = character()
)

.schema_filers <- tibble(
  lei = character(), name = character(), period = integer()
)


# == Public functions ==========================================================

#' Fetch HMDA nationwide aggregations
#'
#' Returns aggregated Home Mortgage Disclosure Act data from the CFPB
#' Data Browser API. Each row represents an aggregation bucket defined by
#' the active filter dimensions. Use multiple filters to cross-tabulate
#' (e.g. loan originations by race).
#'
#' @param years Integer. Filing year (required). Available years: 2018--2023.
#'   Example: \code{2022}.
#' @param actions_taken Character or NULL. Action taken code:
#'   \code{"1"} (loan originated), \code{"2"} (approved not accepted),
#'   \code{"3"} (denied), \code{"4"} (withdrawn), \code{"5"} (file
#'   closed for incompleteness), \code{"6"} (purchased by institution),
#'   \code{"7"} (preapproval denied), \code{"8"} (preapproval approved).
#' @param loan_types Character or NULL. Loan type code:
#'   \code{"1"} (conventional), \code{"2"} (FHA-insured),
#'   \code{"3"} (VA-guaranteed), \code{"4"} (USDA/RHS).
#' @param purposes Character or NULL. Loan purpose code:
#'   \code{"1"} (home purchase), \code{"2"} (home improvement),
#'   \code{"31"} (refinancing), \code{"32"} (cash-out refinancing),
#'   \code{"4"} (other), \code{"5"} (not applicable).
#' @param races Character or NULL. Applicant race code:
#'   \code{"1"} (American Indian/Alaska Native), \code{"2"} (Asian),
#'   \code{"3"} (Black/African American), \code{"4"} (Native Hawaiian/
#'   Pacific Islander), \code{"5"} (White), \code{"6"} (information not
#'   provided), \code{"7"} (not applicable).
#' @param ethnicities Character or NULL. Applicant ethnicity code:
#'   \code{"1"} (Hispanic/Latino), \code{"2"} (Not Hispanic/Latino),
#'   \code{"3"} (information not provided), \code{"4"} (not applicable).
#' @return A tibble with columns:
#'   \describe{
#'     \item{count}{integer -- Number of loan records in this bucket}
#'     \item{sum}{numeric -- Total loan amount in dollars}
#'     \item{actions_taken}{character -- Action taken code (if filtered)}
#'     \item{loan_types}{character -- Loan type code (if filtered)}
#'     \item{purposes}{character -- Loan purpose code (if filtered)}
#'     \item{races}{character -- Race code (if filtered)}
#'     \item{ethnicities}{character -- Ethnicity code (if filtered)}
#'   }
#' @examples
#' hmda_aggregations(2022, actions_taken = "1")
#' hmda_aggregations(2022, actions_taken = "3", races = "3")
hmda_aggregations <- function(years, actions_taken = NULL, loan_types = NULL,
                              purposes = NULL, races = NULL,
                              ethnicities = NULL) {
  params <- list(years = years, actions_taken = actions_taken,
                 loan_types = loan_types, purposes = purposes,
                 races = races, ethnicities = ethnicities)
  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/view/nationwide/aggregations?%s", .hmda_base, query)

  raw <- .fetch_json(url)
  aggs <- raw$aggregations
  if (is.null(aggs) || length(aggs) == 0) return(.schema_aggregations[0, names(aggs)])

  result <- as_tibble(aggs)
  if ("count" %in% names(result)) result$count <- as.integer(result$count)
  if ("sum" %in% names(result)) result$sum <- as.numeric(result$sum)
  result
}

#' Fetch HMDA filer institutions for a given year
#'
#' Returns all financial institutions that filed HMDA data for the
#' specified year. Each institution is identified by its LEI (Legal
#' Entity Identifier). Typically returns thousands of filers.
#'
#' @param years Integer. Filing year (e.g. \code{2022}). Available: 2018--2023.
#' @return A tibble with columns:
#'   \describe{
#'     \item{lei}{character -- Legal Entity Identifier (20-character code,
#'       e.g. "549300AQ3T62GXDU7D76")}
#'     \item{name}{character -- Institution name (e.g. "GUILD MORTGAGE
#'       COMPANY")}
#'     \item{period}{integer -- Filing year}
#'   }
#' @examples
#' hmda_filers(2022)
hmda_filers <- function(years) {
  url <- sprintf("%s/view/filers?years=%s", .hmda_base, years)
  raw <- .fetch_json(url)
  filers <- raw$institutions
  if (is.null(filers) || length(filers) == 0) return(.schema_filers)

  as_tibble(filers) |>
    transmute(
      lei = as.character(lei),
      name = as.character(name),
      period = as.integer(period)
    )
}

# == State-level aggregations ==================================================

#' Fetch HMDA aggregations by state
#'
#' Returns aggregated HMDA data filtered to a specific state using
#' the state's FIPS code. Same structure as \code{hmda_aggregations()}
#' but scoped to one state.
#'
#' @param years Integer. Filing year (e.g. \code{2022}).
#' @param state_fips Character. Two-digit state FIPS code. Examples:
#'   \code{"06"} (California), \code{"36"} (New York), \code{"48"} (Texas),
#'   \code{"17"} (Illinois), \code{"12"} (Florida).
#' @param actions_taken Character or NULL. Action taken code (see
#'   \code{hmda_aggregations()} for valid values).
#' @return A tibble with columns: count (integer), sum (numeric), plus
#'   any active filter columns. Returns empty tibble if no data found.
#' @examples
#' hmda_state(2022, "06")
#' hmda_state(2022, "36", actions_taken = "1")
hmda_state <- function(years, state_fips, actions_taken = NULL) {
  params <- list(years = years)
  if (!is.null(actions_taken)) params$actions_taken <- actions_taken
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/view/nationwide/aggregations?%s&state_code=%s",
                 .hmda_base, query, state_fips)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$aggregations)) return(.schema_aggregations)

  result <- as_tibble(raw$aggregations)
  if ("count" %in% names(result)) result$count <- as.integer(result$count)
  if ("sum" %in% names(result)) result$sum <- as.numeric(result$sum)
  result
}

# == Context ===================================================================

#' Get cfpb.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hmda_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hmda_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cfpb.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cfpb.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cfpb.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cfpb.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
