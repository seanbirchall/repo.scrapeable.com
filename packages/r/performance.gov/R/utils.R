# utils.R - Private utilities for performance.gov package

#' @importFrom dplyr .data
#' @importFrom jsonlite fromJSON
NULL

# Column name bindings for R CMD check
utils::globalVariables(c(
  "department", "department_abbreviation", "bureau", "bureau_abbreviation",
  "service", "service_abbreviation", "service_id", "url", "headline", "blurb"
))

.ua <- "support@scrapeable.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

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

# Schemas
.schema_providers <- tibble::tibble(
  department = character(), department_abbreviation = character(),
  bureau = character(), bureau_abbreviation = character(),
  service = character(), service_abbreviation = character(),
  service_id = character(), url = character(),
  headline = character(), blurb = character()
)

.schema_cx_survey <- tibble::tibble(
  fiscal_year = integer(), quarter = integer(),
  organization_name = character(), service_provider_name = character(),
  service_name = character(), service_provided = character(),
  channel = character(), question_text = character(),
  strongly_disagree_num = integer(), disagree_num = integer(),
  neutral_num = integer(), agree_num = integer(),
  strongly_agree_num = integer(), point_scale = numeric(),
  metric_category = character()
)

.schema_cx_merged <- tibble::tibble(
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

.schema_quarterly <- tibble::tibble(
  hisp = character(), service = character(),
  year = integer(), quarter = integer(),
  universe = character(), surveyed = character(),
  respondents = character(), response_rate = character()
)
