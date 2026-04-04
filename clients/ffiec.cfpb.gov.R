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

#' Fetch HMDA nationwide mortgage data aggregations
#'
#' Queries the CFPB Data Browser API for aggregated Home Mortgage Disclosure
#' Act data at the national level. Multiple filter parameters can be combined
#' to slice data by action taken, loan type, purpose, race, and ethnicity.
#'
#' @param years Integer. Filing year (e.g. \code{2022}). Required.
#' @param actions_taken Character or \code{NULL}. Action taken code:
#'   \code{"1"} (originated), \code{"2"} (approved not accepted),
#'   \code{"3"} (denied), \code{"4"} (withdrawn), \code{"5"} (incomplete),
#'   \code{"6"} (purchased), \code{"7"} (preapproval denied),
#'   \code{"8"} (preapproval approved).
#' @param loan_types Character or \code{NULL}. Loan type code:
#'   \code{"1"} (conventional), \code{"2"} (FHA), \code{"3"} (VA),
#'   \code{"4"} (USDA/RHS).
#' @param purposes Character or \code{NULL}. Loan purpose code:
#'   \code{"1"} (purchase), \code{"2"} (improvement),
#'   \code{"31"} (refinance), \code{"32"} (cash-out refinance),
#'   \code{"4"} (other), \code{"5"} (not applicable).
#' @param races Character or \code{NULL}. Applicant race code:
#'   \code{"1"} (American Indian/Alaska Native), \code{"2"} (Asian),
#'   \code{"3"} (Black), \code{"4"} (Native Hawaiian/Pacific Islander),
#'   \code{"5"} (White), \code{"6"} (info not provided),
#'   \code{"7"} (not applicable).
#' @param ethnicities Character or \code{NULL}. Applicant ethnicity:
#'   \code{"1"} (Hispanic/Latino), \code{"2"} (Not Hispanic/Latino),
#'   \code{"3"} (info not provided), \code{"4"} (not applicable).
#' @return A tibble with columns:
#'   \describe{
#'     \item{count}{Integer. Number of loan records.}
#'     \item{sum}{Numeric. Total loan amount (dollars).}
#'     \item{actions_taken}{Character. Action taken code (when filtered).}
#'     \item{loan_types}{Character. Loan type code (when filtered).}
#'     \item{purposes}{Character. Purpose code (when filtered).}
#'     \item{races}{Character. Race code (when filtered).}
#'     \item{ethnicities}{Character. Ethnicity code (when filtered).}
#'   }
#'   Columns only appear for filters that are applied.
#' @examples
#' hmda_aggregations(2022, actions_taken = "1")
#' hmda_aggregations(2022, loan_types = "2", races = "5")
#' @export
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

#' List institutions that filed HMDA data in a given year
#'
#' Returns all financial institutions that submitted Home Mortgage
#' Disclosure Act data for the specified year. Identified by their
#' Legal Entity Identifier (LEI).
#'
#' @param years Integer. Filing year (e.g. \code{2022}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{lei}{Character. Legal Entity Identifier (20-character code).}
#'     \item{name}{Character. Institution name.}
#'     \item{period}{Integer. Filing year.}
#'   }
#' @examples
#' hmda_filers(2022)
#' @export
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

#' Fetch HMDA aggregations filtered by state
#'
#' Returns aggregated HMDA mortgage data for a specific U.S. state,
#' identified by its two-digit FIPS code.
#'
#' @param years Integer. Filing year (e.g. \code{2022}).
#' @param state_fips Character. Two-digit state FIPS code (e.g. \code{"06"}
#'   for California, \code{"36"} for New York, \code{"48"} for Texas).
#' @param actions_taken Character or \code{NULL}. Optional action taken code
#'   to further filter (see \code{hmda_aggregations()} for valid codes).
#' @return A tibble with columns:
#'   \describe{
#'     \item{count}{Integer. Number of loan records.}
#'     \item{sum}{Numeric. Total loan amount (dollars).}
#'   }
#'   Additional filter columns appear when filters are applied.
#' @examples
#' hmda_state(2022, "06")
#' hmda_state(2022, "36", actions_taken = "3")
#' @export
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

#' Get ffiec.cfpb.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ffiec.cfpb.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ffiec.cfpb.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ffiec.cfpb.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ffiec.cfpb.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
