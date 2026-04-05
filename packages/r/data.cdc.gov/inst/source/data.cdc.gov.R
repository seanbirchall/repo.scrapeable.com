# data.cdc.gov.R
# Self-contained data.cdc.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# cdc-gov.R
# Self-contained CDC Open Data (Socrata SODA) client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional app token (reduces throttling). Register at data.cdc.gov.
# Rate limits: Without token: throttled. With token: higher limits.
# Docs: https://dev.socrata.com/docs/queries/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cdc_base <- "https://data.cdc.gov"
# -- SODA query engine ---------------------------------------------------------

.cdc_get <- function(dataset_id, where = NULL, select = NULL, group = NULL,
                     order = NULL, limit = 1000, offset = 0, token = NULL,
                     max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 50000)

  repeat {
    params <- list(`$limit` = page_size, `$offset` = current_offset)
    if (!is.null(where))  params[["$where"]] <- where
    if (!is.null(select)) params[["$select"]] <- select
    if (!is.null(group))  params[["$group"]] <- group
    if (!is.null(order))  params[["$order"]] <- order

    query <- paste(names(params),
                   vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE),
                          character(1)),
                   sep = "=", collapse = "&")
    url <- paste0(.cdc_base, "/resource/", dataset_id, ".json?", query)
    if (!is.null(token)) url <- paste0(url, "&$$app_token=", token)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    if (is.null(raw) || length(raw) == 0 || nrow(raw) == 0) break
    all_data[[length(all_data) + 1]] <- as_tibble(raw)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (nrow(raw) < page_size) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)
  result
}



# == Dataset discovery =========================================================

#' Search CDC datasets by keyword
#'
#' Queries the Socrata data catalog at data.cdc.gov for datasets matching
#' a search term. Filters to dataset type only (excludes maps, charts, etc.).
#'
#' @param query Character. Search keyword (e.g., \code{"covid"},
#'   \code{"mortality"}, \code{"vaccination"}, \code{"influenza"}).
#' @param max_results Integer. Maximum datasets to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{character} -- Socrata dataset ID (e.g., "vbim-akqf").
#'       Use with \code{cdc_get()} and \code{cdc_columns()}.}
#'     \item{name}{\code{character} -- Dataset name}
#'     \item{description}{\code{character} -- Dataset description (truncated to 200 chars)}
#'     \item{type}{\code{character} -- Resource type (always "dataset" after filtering)}
#'     \item{updated_at}{\code{character} -- Last data update timestamp (ISO 8601)}
#'     \item{columns}{\code{character} -- Comma-separated list of column field names}
#'   }
#' @examples
#' cdc_search("covid")
#' cdc_search("influenza", max_results = 5)
#' @export
cdc_search <- function(query, max_results = 20) {
  url <- sprintf("%s/api/catalog/v1?q=%s&limit=%d&domains=data.cdc.gov&search_context=data.cdc.gov",
                 .cdc_base, utils::URLencode(query), max_results)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0)
    return(tibble(id = character(), name = character(), description = character()))

  bind_rows(lapply(results, function(r) {
    res <- r$resource
    tibble(
      id          = res$id %||% NA_character_,
      name        = res$name %||% NA_character_,
      description = substr(res$description %||% "", 1, 200),
      type        = res$type %||% NA_character_,
      updated_at  = res$data_updated_at %||% NA_character_,
      columns     = paste(res$columns_field_name %||% character(), collapse = ", ")
    )
  })) |> filter(type == "dataset")
}

#' Get column metadata for a CDC dataset
#'
#' Returns column names, types, and descriptions for a Socrata dataset.
#' Useful for building SoQL queries with \code{cdc_get()}.
#'
#' @param dataset_id Character. Socrata dataset ID (e.g., \code{"vbim-akqf"},
#'   \code{"9bhg-hcku"}). Obtain IDs from \code{cdc_search()}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{\code{character} -- API field name (use in SoQL queries)}
#'     \item{name}{\code{character} -- Human-readable column name}
#'     \item{datatype}{\code{character} -- Socrata data type (e.g., "text",
#'       "number", "calendar_date")}
#'     \item{description}{\code{character} -- Column description}
#'   }
#' @examples
#' cdc_columns("vbim-akqf")
#' cdc_columns("9bhg-hcku")
#' @export
cdc_columns <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .cdc_base, dataset_id)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  cols <- raw$columns
  if (is.null(cols) || length(cols) == 0)
    return(tibble(field_name = character(), name = character(), datatype = character()))

  bind_rows(lapply(cols, function(c) {
    tibble(
      field_name  = c$fieldName %||% NA_character_,
      name        = c$name %||% NA_character_,
      datatype    = c$dataTypeName %||% NA_character_,
      description = c$description %||% NA_character_
    )
  }))
}


# == Core data fetching ========================================================

#' Fetch data from any CDC Socrata dataset
#'
#' General-purpose query function supporting full SoQL (Socrata Query Language)
#' for filtering, aggregation, and ordering. Handles automatic pagination
#' up to the requested \code{max_results}.
#'
#' @param dataset_id Character. Socrata four-by-four dataset ID (e.g.,
#'   \code{"vbim-akqf"} for COVID-19 cases, \code{"9bhg-hcku"} for
#'   provisional mortality).
#' @param where Character or \code{NULL}. SoQL WHERE clause for filtering.
#'   Examples: \code{"sex='Female'"}, \code{"year > '2020'"}.
#' @param select Character or \code{NULL}. SoQL SELECT clause for column
#'   selection or aggregation. Example: \code{"sex, count(*) as n"}.
#' @param group Character or \code{NULL}. SoQL GROUP BY clause.
#'   Example: \code{"sex"}.
#' @param order Character or \code{NULL}. SoQL ORDER BY clause.
#'   Example: \code{"date DESC"}.
#' @param token Character or \code{NULL}. Socrata app token to avoid throttling.
#' @param max_results Integer. Maximum rows to return (default 1000).
#'   Internally paginates in chunks of up to 50,000.
#' @return A tibble with columns varying by dataset. All columns are returned
#'   as character (Socrata JSON strings). Convert types as needed.
#' @examples
#' cdc_get("vbim-akqf", where = "sex='Female'", max_results = 10)
#' cdc_get("9bhg-hcku", select = "group, year, count(*) as n",
#'         group = "group, year", max_results = 50)
#' @export
cdc_get <- function(dataset_id, where = NULL, select = NULL,
                    group = NULL, order = NULL, token = NULL,
                    max_results = 1000) {
  .cdc_get(dataset_id, where, select, group, order,
           limit = max_results, token = token, max_results = max_results)
}


# == Convenience: COVID-19 =====================================================

#' COVID-19 case surveillance data
#'
#' Queries the CDC COVID-19 Case Surveillance Public Use Data (dataset
#' \code{vbim-akqf}). Returns de-identified individual-level case records.
#'
#' @param state Character or \code{NULL}. State name filter
#'   (e.g., \code{"California"}, \code{"New York"}).
#' @param sex Character or \code{NULL}. Sex filter: \code{"Male"} or \code{"Female"}.
#' @param age_group Character or \code{NULL}. Age group filter. Valid values:
#'   \code{"0 - 9 Years"}, \code{"10 - 19 Years"}, \code{"20 - 29 Years"},
#'   \code{"30 - 39 Years"}, \code{"40 - 49 Years"}, \code{"50 - 59 Years"},
#'   \code{"60 - 69 Years"}, \code{"70 - 79 Years"}, \code{"80+ Years"}.
#' @param where Character or \code{NULL}. Additional SoQL WHERE clause.
#' @param token Character or \code{NULL}. Socrata app token.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns: cdc_case_earliest_dt, cdc_report_dt,
#'   pos_spec_dt, onset_dt, current_status, sex, age_group,
#'   race_ethnicity_combined, hosp_yn, icu_yn, death_yn, medcond_yn.
#'   All columns are \code{character}.
#' @examples
#' cdc_covid_cases(sex = "Female", max_results = 10)
#' @export
cdc_covid_cases <- function(state = NULL, sex = NULL, age_group = NULL,
                            where = NULL, token = NULL, max_results = 1000) {
  parts <- character()
  if (!is.null(state))     parts <- c(parts, sprintf("res_state='%s'", state))
  if (!is.null(sex))       parts <- c(parts, sprintf("sex='%s'", sex))
  if (!is.null(age_group)) parts <- c(parts, sprintf("age_group='%s'", age_group))
  if (!is.null(where))     parts <- c(parts, where)
  where_clause <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("vbim-akqf", where = where_clause, token = token, max_results = max_results)
}

#' COVID-19 vaccination data by jurisdiction
#'
#' Queries the CDC COVID-19 Vaccination Trends dataset (\code{unsk-b7fc}).
#' Returns daily vaccination counts by state/territory.
#'
#' @param location Character or \code{NULL}. Two-letter state/territory
#'   abbreviation (e.g., \code{"CA"}, \code{"NY"}, \code{"US"}).
#' @param token Character or \code{NULL}. Socrata app token.
#' @param max_results Integer. Maximum rows (default 1000). Ordered by date DESC.
#' @return A tibble with columns varying by dataset version. Typical columns
#'   include date, location, administered, administered_dose1,
#'   series_complete, and booster counts. All columns are \code{character}.
#' @examples
#' cdc_covid_vaccinations(location = "CA", max_results = 10)
#' @export
cdc_covid_vaccinations <- function(location = NULL, token = NULL,
                                   max_results = 1000) {
  where <- if (!is.null(location)) sprintf("location='%s'", location) else NULL
  cdc_get("unsk-b7fc", where = where, order = "date DESC", token = token,
          max_results = max_results)
}


# == Convenience: Notifiable diseases ==========================================

#' NNDSS (National Notifiable Diseases Surveillance System) weekly data
#'
#' Queries the NNDSS weekly tables (dataset \code{x9gk-5huc}) for reportable
#' disease counts by state and MMWR week.
#'
#' @param disease Character or \code{NULL}. Disease label for partial matching
#'   (e.g., \code{"Measles"}, \code{"Pertussis"}, \code{"Hepatitis"}).
#' @param year Character or \code{NULL}. Year filter (e.g., \code{"2022"}).
#' @param token Character or \code{NULL}. Socrata app token.
#' @param max_results Integer. Maximum rows (default 1000).
#' @return A tibble with columns: states, year, week, label, m1_flag, m2,
#'   m2_flag, m3_flag, m4_flag, location2, sort_order, location1, geocode.
#'   All columns are \code{character} except geocode which is a nested data frame.
#' @examples
#' cdc_nndss(disease = "Measles", year = "2022", max_results = 10)
#' @export
cdc_nndss <- function(disease = NULL, year = NULL, token = NULL,
                      max_results = 1000) {
  parts <- character()
  if (!is.null(disease)) parts <- c(parts, sprintf("label like '%%%s%%'", disease))
  if (!is.null(year))    parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("x9gk-5huc", where = where, token = token, max_results = max_results)
}


# == Convenience: Mortality ====================================================

#' Provisional mortality counts (weekly, by cause and jurisdiction)
#'
#' Queries the NCHS provisional mortality data (dataset \code{muzy-jte6})
#' for weekly death counts by cause and jurisdiction. Ordered by
#' week_ending_date DESC.
#'
#' @param jurisdiction Character or \code{NULL}. Jurisdiction name
#'   (e.g., \code{"United States"}, \code{"California"}, \code{"New York"}).
#' @param cause Character or \code{NULL}. Currently triggers an
#'   \code{all_cause IS NOT NULL} filter (all causes returned by default).
#' @param year Character or \code{NULL}. MMWR year filter (e.g., \code{"2023"}).
#' @param token Character or \code{NULL}. Socrata app token.
#' @param max_results Integer. Maximum rows (default 1000).
#' @return A tibble with columns: data_as_of, jurisdiction_of_occurrence,
#'   mmwryear, mmwrweek, week_ending_date, all_cause, natural_cause,
#'   septicemia_a40_a41, malignant_neoplasms_c00_c97,
#'   diabetes_mellitus_e10_e14, alzheimer_disease_g30,
#'   influenza_and_pneumonia_j09_j18, chronic_lower_respiratory,
#'   other_diseases_of_respiratory, nephritis_nephrotic_syndrome,
#'   symptoms_signs_and_abnormal, diseases_of_heart_i00_i09,
#'   cerebrovascular_diseases, covid_19_u071_multiple_cause_of_death,
#'   covid_19_u071_underlying_cause_of_death. All \code{character}.
#' @examples
#' cdc_mortality(jurisdiction = "United States", year = "2023", max_results = 5)
#' @export
cdc_mortality <- function(jurisdiction = NULL, cause = NULL, year = NULL,
                          token = NULL, max_results = 1000) {
  parts <- character()
  if (!is.null(jurisdiction)) parts <- c(parts, sprintf("jurisdiction_of_occurrence='%s'", jurisdiction))
  if (!is.null(cause)) parts <- c(parts, sprintf("all_cause IS NOT NULL"))
  if (!is.null(year))         parts <- c(parts, sprintf("mmwryear='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("muzy-jte6", where = where, order = "week_ending_date DESC",
          token = token, max_results = max_results)
}


# == Convenience: Flu surveillance =============================================

#' ILINet (Influenza-Like Illness Surveillance) data
#'
#' Queries the ILINet outpatient surveillance data (dataset \code{ite7-j2w7})
#' for weekly influenza-like illness rates by HHS region.
#'
#' @param region Character or \code{NULL}. Region filter with partial matching
#'   (e.g., \code{"Region 1"}, \code{"National"}).
#' @param year Character or \code{NULL}. Year filter (e.g., \code{"2023"}).
#' @param token Character or \code{NULL}. Socrata app token.
#' @param max_results Integer. Maximum rows (default 1000).
#' @return A tibble with columns varying by dataset version. Typical columns
#'   include region, year, week, ilitotal, num_of_providers,
#'   total_patients, and unweighted_ili. All \code{character}.
#' @examples
#' cdc_flu(region = "National", year = "2023", max_results = 10)
#' @export
cdc_flu <- function(region = NULL, year = NULL, token = NULL,
                    max_results = 1000) {
  parts <- character()
  if (!is.null(region)) parts <- c(parts, sprintf("region like '%%%s%%'", region))
  if (!is.null(year))   parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("ite7-j2w7", where = where, token = token, max_results = max_results)
}


# == Context ===================================================================

#' Get data.cdc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cdc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cdc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.cdc.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.cdc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.cdc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.cdc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
