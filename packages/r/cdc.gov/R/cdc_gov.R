# data.cdc.gov.R
# Self-contained data.cdc.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


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
#' Queries the Socrata catalog API for datasets hosted on data.cdc.gov
#' matching a keyword. Returns metadata including column lists so you
#' can identify the right dataset ID to pass to \code{cdc_get()} or
#' the convenience functions. Only datasets (not charts/maps) are returned.
#'
#' @param query Character. Search keyword or phrase. Examples: \code{"covid"},
#'   \code{"mortality"}, \code{"vaccination"}, \code{"influenza"},
#'   \code{"birth"}, \code{"STD"}.
#' @param max_results Integer. Maximum number of datasets to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- Socrata dataset ID (e.g. "vbim-akqf"). Pass this
#'       to \code{cdc_get()}, \code{cdc_columns()}, etc.}
#'     \item{name}{character -- Dataset title}
#'     \item{description}{character -- First 200 characters of description}
#'     \item{type}{character -- Resource type (filtered to "dataset")}
#'     \item{updated_at}{character -- ISO 8601 datetime of last data update}
#'     \item{columns}{character -- Comma-separated list of column field names}
#'   }
#' @examples
#' cdc_search("covid")
#' cdc_search("mortality", max_results = 5)
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
#' Fetches the full column schema for a Socrata dataset, including field
#' names, human-readable labels, data types, and descriptions. Use this
#' to discover available fields before building SoQL queries with
#' \code{cdc_get()}.
#'
#' @param dataset_id Character. Socrata dataset ID (e.g. \code{"vbim-akqf"}
#'   for COVID-19 cases). Find IDs via \code{cdc_search()}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{field_name}{character -- API field name for use in SoQL queries
#'       (e.g. "sex", "age_group", "cdc_case_earliest_dt")}
#'     \item{name}{character -- Human-readable column label}
#'     \item{datatype}{character -- Socrata type: "text", "calendar_date",
#'       "number", "checkbox", etc.}
#'     \item{description}{character -- Column description (may be NA)}
#'   }
#' @examples
#' cdc_columns("vbim-akqf")  # COVID-19 case columns
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
#' The universal data-fetching engine for data.cdc.gov. Pass any dataset ID
#' and optional SoQL (Socrata Query Language) clauses to filter, aggregate,
#' and order results. Handles pagination internally up to \code{max_results}.
#'
#' @param dataset_id Character. Socrata dataset ID (e.g. \code{"vbim-akqf"}
#'   for COVID-19 cases). Find IDs via \code{cdc_search()}.
#' @param where Character or NULL. SoQL WHERE clause for filtering. Examples:
#'   \code{"sex='Female' AND age_group='50-59 Years'"},
#'   \code{"date > '2023-01-01'"}.
#' @param select Character or NULL. SoQL SELECT clause for column selection
#'   or aggregation. Examples: \code{"sex, count(*) as n"},
#'   \code{"date_trunc_y(date) as year, sum(count)"}.
#' @param group Character or NULL. SoQL GROUP BY clause for aggregation.
#'   Example: \code{"sex"}.
#' @param order Character or NULL. SoQL ORDER BY clause. Example:
#'   \code{"count DESC"}, \code{"date DESC"}.
#' @param token Character or NULL. Optional Socrata app token to avoid
#'   throttling. Register at \url{https://data.cdc.gov}.
#' @param max_results Integer. Maximum total rows to return (default 1000).
#'   Pagination is handled internally in 50,000-row pages.
#' @return A tibble. Columns depend on the dataset; all values are returned
#'   as character (Socrata default). Use \code{cdc_columns()} to discover
#'   available fields and types. For the COVID-19 case dataset
#'   \code{"vbim-akqf"}, typical columns include: cdc_case_earliest_dt,
#'   cdc_report_dt, pos_spec_dt, onset_dt, current_status, sex, age_group,
#'   race_ethnicity_combined, hosp_yn, icu_yn, death_yn, medcond_yn.
#' @examples
#' cdc_get("vbim-akqf", max_results = 10)
#' cdc_get("vbim-akqf", where = "sex='Female'", max_results = 5)
cdc_get <- function(dataset_id, where = NULL, select = NULL,
                    group = NULL, order = NULL, token = NULL,
                    max_results = 1000) {
  .cdc_get(dataset_id, where, select, group, order,
           limit = max_results, token = token, max_results = max_results)
}


# == Convenience: COVID-19 =====================================================

#' COVID-19 case surveillance data
#'
#' Individual-level case records from CDC case surveillance
#' (dataset \code{"vbim-akqf"}). Each row is one reported case with
#' demographics, dates, and outcome indicators.
#'
#' @param state Character or NULL. State name filter (e.g. \code{"California"},
#'   \code{"New York"}, \code{"Texas"}).
#' @param sex Character or NULL. Sex filter: \code{"Male"}, \code{"Female"},
#'   \code{"Other"}, \code{"Unknown"}, \code{"Missing"}.
#' @param age_group Character or NULL. Age group filter. Valid values:
#'   \code{"0 - 9 Years"}, \code{"10 - 19 Years"}, \code{"20 - 29 Years"},
#'   \code{"30 - 39 Years"}, \code{"40 - 49 Years"}, \code{"50 - 59 Years"},
#'   \code{"60 - 69 Years"}, \code{"70 - 79 Years"}, \code{"80+ Years"}.
#' @param where Character or NULL. Additional SoQL WHERE clause combined
#'   with AND to the other filters.
#' @param token Character or NULL. Socrata app token.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns (all character): cdc_case_earliest_dt,
#'   cdc_report_dt, pos_spec_dt, onset_dt, current_status, sex, age_group,
#'   race_ethnicity_combined, hosp_yn, icu_yn, death_yn, medcond_yn.
#' @examples
#' cdc_covid_cases(sex = "Female", max_results = 10)
#' cdc_covid_cases(state = "California", age_group = "50 - 59 Years")
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
#' Returns daily vaccination administration counts from dataset
#' \code{"unsk-b7fc"}, ordered by date descending. Each row is one
#' jurisdiction on one date.
#'
#' @param location Character or NULL. Two-letter state/territory abbreviation
#'   (e.g. \code{"CA"}, \code{"NY"}, \code{"TX"}, \code{"US"}).
#' @param token Character or NULL. Socrata app token.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns (all character) including date, location,
#'   and various dose count fields. Use \code{cdc_columns("unsk-b7fc")}
#'   for the full column list.
#' @examples
#' cdc_covid_vaccinations(location = "CA", max_results = 10)
cdc_covid_vaccinations <- function(location = NULL, token = NULL,
                                   max_results = 1000) {
  where <- if (!is.null(location)) sprintf("location='%s'", location) else NULL
  cdc_get("unsk-b7fc", where = where, order = "date DESC", token = token,
          max_results = max_results)
}


# == Convenience: Notifiable diseases ==========================================

#' NNDSS (National Notifiable Diseases Surveillance System) weekly data
#'
#' Returns weekly counts of nationally notifiable diseases reported
#' by state health departments (dataset \code{"x9gk-5huc"}). Each row
#' is one disease in one reporting area for one MMWR week.
#'
#' @param disease Character or NULL. Disease label filter using SQL LIKE
#'   partial matching. Examples: \code{"Salmonella"}, \code{"Hepatitis"},
#'   \code{"Pertussis"}, \code{"Lyme"}.
#' @param year Character or NULL. MMWR year filter (e.g. \code{"2023"}).
#' @param token Character or NULL. Socrata app token.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns (all character) including label (disease),
#'   year, week, reporting_area, and count fields. Use
#'   \code{cdc_columns("x9gk-5huc")} for the full column list.
#' @examples
#' cdc_nndss(disease = "Salmonella", year = "2023", max_results = 20)
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
#' Returns weekly provisional death counts from dataset \code{"muzy-jte6"},
#' ordered by week_ending_date descending. Includes all-cause and
#' cause-specific mortality.
#'
#' @param jurisdiction Character or NULL. Jurisdiction name. Examples:
#'   \code{"United States"}, \code{"California"}, \code{"New York"}.
#' @param cause Character or NULL. Currently used as a flag to filter for
#'   rows where all_cause is not null (any non-NULL value activates this).
#' @param year Character or NULL. MMWR year (e.g. \code{"2023"}).
#' @param token Character or NULL. Socrata app token.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns (all character) including
#'   jurisdiction_of_occurrence, mmwryear, mmwrweek, week_ending_date,
#'   all_cause, and cause-specific count fields. Use
#'   \code{cdc_columns("muzy-jte6")} for the full column list.
#' @examples
#' cdc_mortality(jurisdiction = "United States", year = "2023",
#'               max_results = 10)
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
#' Returns weekly influenza-like illness surveillance data from dataset
#' \code{"ite7-j2w7"}. Each row is one region for one surveillance week
#' with patient visit counts and ILI percentages.
#'
#' @param region Character or NULL. Region filter using SQL LIKE partial
#'   matching. Examples: \code{"Region 1"}, \code{"National"}.
#' @param year Character or NULL. Surveillance year (e.g. \code{"2023"}).
#' @param token Character or NULL. Socrata app token.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns (all character) including region, year,
#'   week, total_patients, and ILI-related count fields. Use
#'   \code{cdc_columns("ite7-j2w7")} for the full column list.
#' @examples
#' cdc_flu(year = "2023", max_results = 20)
cdc_flu <- function(region = NULL, year = NULL, token = NULL,
                    max_results = 1000) {
  parts <- character()
  if (!is.null(region)) parts <- c(parts, sprintf("region like '%%%s%%'", region))
  if (!is.null(year))   parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("ite7-j2w7", where = where, token = token, max_results = max_results)
}


# == Context ===================================================================

#' Get cdc.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cdc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cdc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cdc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cdc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
