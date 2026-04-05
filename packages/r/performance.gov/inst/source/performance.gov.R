# performance.gov.R - Self-contained performance.gov CX/HISP client
#
# Customer Experience (CX) data from Performance.gov
# High Impact Service Providers (HISP) data from GSA
#
# Data sources: GitHub-hosted CSV files from GSA repos
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required (public data)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

# GitHub raw URLs for the data sources
.perf_urls <- list(
  providers = "https://raw.githubusercontent.com/GSA/Trump-archives-performance-gov/master/_data/hisps.csv",
  quarterly = "https://raw.githubusercontent.com/GSA/Trump-archives-performance-gov/master/_data/hisp-quarterly-data.csv",
  cx_survey = "https://raw.githubusercontent.com/GSA/HISP-CX-Data/main/trust_survey_counts_quick_flattened_data_transformed%20-%20trust_survey_counts_quick.csv",
  cx_merged = "https://raw.githubusercontent.com/GSA/HISP-CX-Data/main/mergedcxdata-2023-05-21.csv"
)

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  path <- .fetch(url, ext = ".csv")
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) |>
    tibble::as_tibble()
}

# == Schemas ===================================================================

.schema_providers <- tibble(
  department = character(), department_abbreviation = character(),
  bureau = character(), bureau_abbreviation = character(),
  service = character(), service_abbreviation = character(),
  service_id = character(), url = character(),
  headline = character(), blurb = character()
)

.schema_cx_survey <- tibble(
  fiscal_year = integer(), quarter = integer(),
  organization_name = character(), service_provider_name = character(),
  service_name = character(), service_provided = character(),
  channel = character(), question_text = character(),
  strongly_disagree_num = integer(), disagree_num = integer(),
  neutral_num = integer(), agree_num = integer(),
  strongly_agree_num = integer(), point_scale = numeric(),
  metric_category = character()
)

.schema_cx_merged <- tibble(
  id = character(), type = character(),
  attributes_organization_name = character(),
  attributes_service_provider_name = character(),
  attributes_collection_name = character(),
  attributes_collection_year = integer(),
  attributes_collection_quarter = integer(),
  attributes_service_provided = character(),
  attributes_channel = character(),
  attributes_volume_of_customers = numeric(),
  attributes_volume_of_respondents = numeric()
)

.schema_quarterly <- tibble(
  hisp = character(), service = character(),
  year = integer(), quarter = integer(),
  universe = character(), surveyed = character(),
  respondents = character(), response_rate = character()
)

# == Public functions ==========================================================

#' List HISP service providers
#'
#' Returns the catalog of federal High Impact Service Providers (HISPs)
#' designated under OMB Circular A-11 Section 280. HISPs are federal
#' entities that provide the most impactful customer-facing services.
#' Data is sourced from the GSA GitHub repository.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{department}{Character. Federal department name.}
#'   \item{department_abbreviation}{Character. Department abbreviation.}
#'   \item{bureau}{Character. Bureau or agency name.}
#'   \item{bureau_abbreviation}{Character. Bureau abbreviation.}
#'   \item{service}{Character. Specific service provided.}
#'   \item{service_abbreviation}{Character. Service abbreviation.}
#'   \item{service_id}{Character. Unique service identifier.}
#'   \item{url}{Character. Service page URL.}
#'   \item{headline}{Character. Short headline description.}
#'   \item{blurb}{Character. Longer description of the service.}
#' }
#'
#' @examples
#' \dontrun{
#' perf_providers()
#' perf_providers() |> dplyr::count(department, sort = TRUE)
#' }
#'
#' @export
perf_providers <- function() {
  tryCatch({
    raw <- .fetch_csv(.perf_urls$providers)
    if (nrow(raw) == 0) return(.schema_providers)
    raw |>
      dplyr::select(
        department, department_abbreviation,
        bureau, bureau_abbreviation,
        service, service_abbreviation,
        service_id, url, headline, blurb
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }, error = function(e) {
    warning("perf_providers: ", conditionMessage(e))
    .schema_providers
  })
}

#' Get CX survey data
#'
#' Returns federal Customer Experience (CX) trust survey results at the
#' question level. Includes Likert-scale response counts (strongly disagree
#' through strongly agree), point scales, and metric categories. Data covers
#' HISPs across multiple fiscal years and quarters.
#'
#' @param agency Character. Optional agency or organization name filter
#'   (case-insensitive partial match against \code{organization_name} and
#'   \code{service_provider_name}).
#' @param year Integer. Optional fiscal year filter.
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{fiscal_year}{Integer. Federal fiscal year.}
#'   \item{quarter}{Integer. Fiscal quarter (1-4).}
#'   \item{organization_name}{Character. Federal department/agency name.}
#'   \item{service_provider_name}{Character. Specific service provider.}
#'   \item{service_name}{Character. Service being measured.}
#'   \item{question_text}{Character. Survey question text.}
#'   \item{strongly_disagree_num}{Integer. Count of "strongly disagree" responses.}
#'   \item{disagree_num}{Integer. Count of "disagree" responses.}
#'   \item{neutral_num}{Integer. Count of "neutral" responses.}
#'   \item{agree_num}{Integer. Count of "agree" responses.}
#'   \item{strongly_agree_num}{Integer. Count of "strongly agree" responses.}
#'   \item{point_scale}{Numeric. Calculated satisfaction score.}
#'   \item{metric_category}{Character. Category of metric being measured.}
#' }
#'
#' @examples
#' \dontrun{
#' perf_cx_survey()
#' perf_cx_survey(agency = "Veterans", year = 2023)
#' }
#'
#' @seealso \code{\link{perf_collection_details}} for merged CX data.
#' @export
perf_cx_survey <- function(agency = NULL, year = NULL) {
  tryCatch({
    raw <- .fetch_csv(.perf_urls$cx_survey)
    if (nrow(raw) == 0) return(.schema_cx_survey)
    out <- raw |>
      dplyr::mutate(
        fiscal_year = as.integer(fiscal_year),
        quarter = as.integer(quarter),
        strongly_disagree_num = suppressWarnings(as.integer(strongly_disagree_num)),
        disagree_num = suppressWarnings(as.integer(disagree_num)),
        neutral_num = suppressWarnings(as.integer(neutral_num)),
        agree_num = suppressWarnings(as.integer(agree_num)),
        strongly_agree_num = suppressWarnings(as.integer(strongly_agree_num)),
        strongly_agree_or_agree_num = suppressWarnings(as.integer(strongly_agree_or_agree_num)),
        point_scale = suppressWarnings(as.numeric(point_scale)),
        population_size = suppressWarnings(as.integer(population_size)),
        volume_of_customers_provided_survey_opportunity = suppressWarnings(as.integer(volume_of_customers_provided_survey_opportunity)),
        volume_of_respondents = suppressWarnings(as.integer(volume_of_respondents)),
        sample_size = suppressWarnings(as.integer(sample_size))
      )
    if (!is.null(agency)) {
      out <- out |> dplyr::filter(grepl(agency, organization_name, ignore.case = TRUE) |
                                    grepl(agency, service_provider_name, ignore.case = TRUE))
    }
    if (!is.null(year)) {
      out <- out |> dplyr::filter(fiscal_year == as.integer(year))
    }
    out
  }, error = function(e) {
    warning("perf_cx_survey: ", conditionMessage(e))
    .schema_cx_survey
  })
}

#' Get detailed CX reporting collections (merged dataset)
#'
#' Returns the full merged CX dataset combining survey results with
#' service descriptions, operational context, and detailed question-level
#' data. This is the most comprehensive CX dataset, linking collection
#' metadata with response data.
#'
#' @param agency Character. Optional agency or organization name filter
#'   (case-insensitive partial match against organization and service
#'   provider names).
#' @param year Integer. Optional collection year filter.
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{attributes_organization_name}{Character. Federal department/agency.}
#'   \item{attributes_service_provider_name}{Character. Service provider.}
#'   \item{attributes_collection_name}{Character. Data collection name.}
#'   \item{attributes_collection_year}{Integer. Collection year.}
#'   \item{attributes_collection_quarter}{Integer. Collection quarter.}
#'   \item{attributes_service_provided}{Character. Service description.}
#'   \item{attributes_channel}{Character. Service delivery channel.}
#'   \item{attributes_volume_of_customers}{Numeric. Total customer volume.}
#'   \item{attributes_volume_of_respondents}{Numeric. Survey respondent count.}
#' }
#'
#' @examples
#' \dontrun{
#' perf_collection_details()
#' perf_collection_details(agency = "SSA", year = 2023)
#' }
#'
#' @seealso \code{\link{perf_cx_survey}} for question-level survey data.
#' @export
perf_collection_details <- function(agency = NULL, year = NULL) {
  tryCatch({
    raw <- .fetch_csv(.perf_urls$cx_merged)
    if (nrow(raw) == 0) return(.schema_cx_merged)
    out <- raw |>
      dplyr::mutate(
        attributes_collection_year = suppressWarnings(as.integer(attributes_collection_year)),
        attributes_collection_quarter = suppressWarnings(as.integer(attributes_collection_quarter)),
        attributes_volume_of_customers = suppressWarnings(as.numeric(attributes_volume_of_customers)),
        attributes_volume_of_customers_provided_survey_opportunity =
          suppressWarnings(as.numeric(attributes_volume_of_customers_provided_survey_opportunity)),
        attributes_volume_of_respondents = suppressWarnings(as.numeric(attributes_volume_of_respondents))
      )
    if (!is.null(agency)) {
      out <- out |> dplyr::filter(
        grepl(agency, attributes_organization_name, ignore.case = TRUE) |
          grepl(agency, attributes_service_provider_name, ignore.case = TRUE)
      )
    }
    if (!is.null(year)) {
      out <- out |> dplyr::filter(attributes_collection_year == as.integer(year))
    }
    out
  }, error = function(e) {
    warning("perf_collection_details: ", conditionMessage(e))
    .schema_cx_merged
  })
}

#' Get quarterly CX data collections summary
#'
#' Returns quarterly summaries of HISP customer experience data collection
#' efforts. Each row represents a service provider's quarterly survey
#' collection with universe size, number surveyed, respondent count,
#' and response rate.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{hisp}{Character. HISP abbreviation (e.g. "va", "ssa").}
#'   \item{service}{Character. Service name.}
#'   \item{year}{Integer. Calendar year.}
#'   \item{quarter}{Integer. Quarter (1-4).}
#'   \item{universe}{Character. Total eligible customer population.}
#'   \item{surveyed}{Character. Number of customers surveyed.}
#'   \item{respondents}{Character. Number of survey respondents.}
#'   \item{response_rate}{Character. Response rate percentage.}
#' }
#'
#' @examples
#' \dontrun{
#' perf_collections()
#' }
#'
#' @export
perf_collections <- function() {
  tryCatch({
    raw <- .fetch_csv(.perf_urls$quarterly)
    if (nrow(raw) == 0) return(.schema_quarterly)
    raw |>
      dplyr::mutate(
        year = as.integer(year),
        quarter = as.integer(quarter)
      )
  }, error = function(e) {
    warning("perf_collections: ", conditionMessage(e))
    .schema_quarterly
  })
}

#' List all available performance.gov CX datasets
#'
#' Returns a catalog of all available Customer Experience datasets
#' accessible through this client. Each row includes the dataset name,
#' the R function used to access it, the GitHub source repository, a
#' description, and the raw data URL.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{dataset}{Character. Short dataset identifier.}
#'   \item{function_name}{Character. R function to call for this dataset.}
#'   \item{source}{Character. GitHub repository path.}
#'   \item{description}{Character. Brief description.}
#'   \item{url}{Character. Raw data URL.}
#' }
#'
#' @examples
#' \dontrun{
#' perf_list()
#' }
#'
#' @export
perf_list <- function() {
  tibble(
    dataset = c("providers", "cx_survey", "collection_details", "collections"),
    function_name = c("perf_providers", "perf_cx_survey",
                      "perf_collection_details", "perf_collections"),
    source = c(
      "GSA/Trump-archives-performance-gov",
      "GSA/HISP-CX-Data",
      "GSA/HISP-CX-Data",
      "GSA/Trump-archives-performance-gov"
    ),
    description = c(
      "High Impact Service Providers (HISPs) - departments, bureaus, services",
      "CX trust survey question-level response data by agency/quarter",
      "Merged CX reporting data with detailed question texts and scores",
      "Quarterly HISP survey collection summaries with response rates"
    ),
    url = unname(unlist(.perf_urls))
  )
}

#' Search across all performance.gov CX datasets
#'
#' Performs a keyword search across all three main CX datasets: providers,
#' CX survey data, and collection details. Returns matching rows from each
#' dataset, unified into a single tibble with a \code{dataset} column
#' indicating the source.
#'
#' @param query Character. Search term to match (case-insensitive) against
#'   text fields across all datasets. Required.
#'
#' @return A tibble with matched rows from all datasets. Columns vary by
#'   dataset but always include a \code{dataset} column identifying the source
#'   (\code{"providers"}, \code{"cx_survey"}, or \code{"collection_details"}).
#'   Returns an empty tibble with columns \code{dataset} and \code{info} if
#'   no matches are found.
#'
#' @examples
#' \dontrun{
#' perf_search("Veterans")
#' perf_search("passport")
#' }
#'
#' @seealso \code{\link{perf_list}} for available datasets.
#' @export
perf_search <- function(query) {
  if (missing(query) || is.null(query) || !nzchar(query)) {
    stop("query is required")
  }
  results <- list()

  # Search providers
  tryCatch({
    provs <- perf_providers()
    if (nrow(provs) > 0) {
      hits <- provs |>
        dplyr::filter(
          grepl(query, department, ignore.case = TRUE) |
            grepl(query, bureau, ignore.case = TRUE) |
            grepl(query, service, ignore.case = TRUE) |
            grepl(query, headline, ignore.case = TRUE) |
            grepl(query, blurb, ignore.case = TRUE)
        )
      if (nrow(hits) > 0) {
        results[["providers"]] <- hits |>
          dplyr::mutate(dataset = "providers") |>
          dplyr::select(dataset, department, bureau, service, headline)
      }
    }
  }, error = function(e) NULL)

  # Search CX survey
  tryCatch({
    survey <- .fetch_csv(.perf_urls$cx_survey)
    if (nrow(survey) > 0) {
      text_cols <- intersect(names(survey),
        c("organization_name", "service_provider_name", "service_name",
          "service_provided", "question_text", "channel", "metric_category"))
      matches <- rep(FALSE, nrow(survey))
      for (col in text_cols) {
        matches <- matches | grepl(query, survey[[col]], ignore.case = TRUE)
      }
      hits <- survey[matches, ]
      if (nrow(hits) > 0) {
        results[["cx_survey"]] <- hits |>
          dplyr::mutate(dataset = "cx_survey") |>
          dplyr::select(dataset,
            dplyr::any_of(c("organization_name", "service_provider_name",
                            "service_name", "fiscal_year", "quarter",
                            "metric_category")))
      }
    }
  }, error = function(e) NULL)

  # Search providers details from merged
  tryCatch({
    merged <- .fetch_csv(.perf_urls$cx_merged)
    if (nrow(merged) > 0) {
      text_cols <- intersect(names(merged),
        c("attributes_organization_name", "attributes_service_provider_name",
          "attributes_collection_name", "attributes_service_provided",
          "attributes_channel", "service_desc", "service_provider_desc"))
      matches <- rep(FALSE, nrow(merged))
      for (col in text_cols) {
        matches <- matches | grepl(query, merged[[col]], ignore.case = TRUE)
      }
      hits <- merged[matches, ]
      if (nrow(hits) > 0) {
        results[["collection_details"]] <- hits |>
          dplyr::mutate(dataset = "collection_details") |>
          dplyr::select(dataset,
            dplyr::any_of(c("attributes_organization_name",
                            "attributes_service_provider_name",
                            "attributes_collection_name",
                            "attributes_collection_year",
                            "attributes_collection_quarter")))
      }
    }
  }, error = function(e) NULL)

  if (length(results) == 0) {
    return(tibble(dataset = character(), info = character()))
  }

  dplyr::bind_rows(results)
}

#' Get performance.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
perf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(perf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/performance.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "performance.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# performance.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# performance.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
