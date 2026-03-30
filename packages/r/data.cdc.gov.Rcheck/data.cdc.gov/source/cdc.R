# cdc-gov.R
# Self-contained CDC Open Data (Socrata SODA) client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional app token (reduces throttling). Register at data.cdc.gov.
# Rate limits: Without token: throttled. With token: higher limits.
# Docs: https://dev.socrata.com/docs/queries/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cdc_base <- "https://data.cdc.gov"

# -- Context generator ---------------------------------------------------------

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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
#' Queries the Socrata catalog for datasets matching a search term.
#'
#' @param query Search keyword (e.g. "covid", "mortality", "vaccination")
#' @param max_results Max datasets to return (default 20)
#' @return tibble: id, name, description, type, updated_at, columns
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
#' @param dataset_id Socrata dataset ID (e.g. "vbim-akqf")
#' @return tibble: field_name, name, datatype, description
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
#' Uses SoQL (Socrata Query Language) for filtering and aggregation.
#'
#' @param dataset_id Socrata dataset ID (e.g. "vbim-akqf" for COVID cases)
#' @param where SoQL WHERE clause (e.g. "sex='Female' AND age_group='50-59 Years'")
#' @param select SoQL SELECT clause (e.g. "sex, count(*) as n")
#' @param group SoQL GROUP BY clause (e.g. "sex")
#' @param order SoQL ORDER BY clause (e.g. "count DESC")
#' @param token Optional Socrata app token
#' @param max_results Max rows (default 1000)
#' @return tibble with all columns as character (Socrata returns strings)
cdc_get <- function(dataset_id, where = NULL, select = NULL,
                    group = NULL, order = NULL, token = NULL,
                    max_results = 1000) {
  .cdc_get(dataset_id, where, select, group, order,
           limit = max_results, token = token, max_results = max_results)
}


# == Convenience: COVID-19 =====================================================

#' COVID-19 case surveillance data
#'
#' Individual-level case records from CDC case surveillance.
#'
#' @param state State name filter (e.g. "California")
#' @param sex Sex filter: "Male", "Female"
#' @param age_group Age filter: "0 - 9 Years", "10 - 19 Years", etc.
#' @param where Additional SoQL WHERE clause
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of COVID-19 case records
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
#' @param location State/territory abbreviation (e.g. "CA", "NY")
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of vaccination counts by date and jurisdiction
cdc_covid_vaccinations <- function(location = NULL, token = NULL,
                                   max_results = 1000) {
  where <- if (!is.null(location)) sprintf("location='%s'", location) else NULL
  cdc_get("unsk-b7fc", where = where, order = "date DESC", token = token,
          max_results = max_results)
}


# == Convenience: Notifiable diseases ==========================================

#' NNDSS (National Notifiable Diseases Surveillance System) weekly data
#'
#' @param disease Disease label filter (partial match)
#' @param year Year filter
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of weekly disease counts by state
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
#' @param jurisdiction State name or "United States"
#' @param cause Cause of death filter (partial match)
#' @param year Year filter
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of weekly mortality counts
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
#' @param region Region filter
#' @param year Year filter
#' @param token Socrata app token
#' @param max_results Max results (default 1000)
#' @return tibble of weekly ILI rates by region
cdc_flu <- function(region = NULL, year = NULL, token = NULL,
                    max_results = 1000) {
  parts <- character()
  if (!is.null(region)) parts <- c(parts, sprintf("region like '%%%s%%'", region))
  if (!is.null(year))   parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL

  cdc_get("ite7-j2w7", where = where, token = token, max_results = max_results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the data.cdc.gov package
#'
#' @return Character string (invisibly), also printed
cdc_context <- function() {
  .build_context("data.cdc.gov", header_lines = c(
    "# data.cdc.gov - CDC Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: optional Socrata app token (reduces throttling)",
    "# All functions return tibbles. Uses SoQL query language.",
    "#",
    "# Key dataset IDs:",
    "#   vbim-akqf = COVID-19 Case Surveillance",
    "#   unsk-b7fc = COVID-19 Vaccinations by Jurisdiction",
    "#   x9gk-5huc = NNDSS Weekly Disease Tables",
    "#   muzy-jte6 = Provisional Mortality Counts",
    "#   ite7-j2w7 = ILINet Flu Surveillance",
    "#",
    "# SoQL syntax (use in `where` param):",
    "#   field='value'           exact match",
    "#   field like '%pattern%'  partial match",
    "#   field > '2024-01-01'    comparison",
    "#   field IS NOT NULL       null check",
    "#   Use cdc_search('keyword') to discover datasets.",
    "#   Use cdc_columns('dataset-id') to see available fields."
  ))
}
