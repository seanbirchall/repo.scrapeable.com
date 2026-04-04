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
#' Returns a tibble of High Impact Service Providers with department,
#' bureau, service details, and descriptions.
#'
#' @return tibble of HISP service providers
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
#' Returns a tibble of Customer Experience survey results with question-level
#' response counts, point scales, and metric categories across federal services.
#'
#' @param agency Optional agency/organization name filter (partial match, case-insensitive)
#' @param year Optional fiscal year filter (integer)
#' @return tibble of CX survey data
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
#' Returns the full merged CX dataset with detailed question-level data,
#' service descriptions, and operational metrics.
#'
#' @param agency Optional agency/organization name filter (partial match, case-insensitive)
#' @param year Optional collection year filter (integer)
#' @return tibble of CX reporting collection details
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
#' Returns a tibble of quarterly HISP data collection summaries
#' with universe size, survey volume, and response rates.
#'
#' @return tibble of quarterly collection summaries
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
#' Returns a catalog tibble describing each available dataset,
#' its source URL, record count, and description.
#'
#' @return tibble with dataset catalog
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
#' Performs a keyword search across providers, CX survey data, and
#' collection details. Returns matching rows from all datasets.
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble with columns: dataset, match_field, match_value, row_data
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

#' Get performance.gov client source and function signatures
#'
#' Reads and returns the source code of all public functions
#' defined in this client file.
#'
#' @return character string of function source (invisibly), also printed
perf_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/performance.gov.R"
  if (!file.exists(src_file)) {
    cat("# performance.gov context - source not found\n")
    return(invisible("# performance.gov context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    end <- if (fi == max(fn_indices)) n else fn_indices[which(fn_indices == fi) + 1] - 1
    while (end > fi && trimws(lines[end]) == "") end <- end - 1
    blocks[[fn_name]] <- paste(lines[fi:end], collapse = "\n")
  }
  out <- paste(blocks, collapse = "\n\n")
  cat(out, "\n")
  invisible(out)
}
