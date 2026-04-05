# data.cms.gov.R
# Self-contained data.cms.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


# cms-gov.R
# Self-contained CMS (Centers for Medicare & Medicaid Services) data client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://data.cms.gov/provider-data/api


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cms_base <- "https://data.cms.gov/provider-data/api/1"
# -- CMS datastore query engine ------------------------------------------------

.cms_query <- function(dataset_id, conditions = list(), limit = 1000,
                       offset = 0, max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 1000)
  total <- NULL

  repeat {
    # Build URL with conditions
    params <- sprintf("limit=%d&offset=%d", page_size, current_offset)
    for (i in seq_along(conditions)) {
      c <- conditions[[i]]
      params <- paste0(params,
        sprintf("&conditions[%d][property]=%s&conditions[%d][value]=%s&conditions[%d][operator]=%%3D",
                i - 1, utils::URLencode(c$property),
                i - 1, utils::URLencode(c$value),
                i - 1))
    }

    url <- sprintf("%s/datastore/query/%s/0?%s", .cms_base, dataset_id, params)
    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

    if (is.null(total)) {
      total <- raw$count
      if (total > page_size)
        message(sprintf("Found %d records, fetching...", min(total, max_results %||% total)))
    }

    results <- raw$results
    if (is.null(results) || length(results) == 0) break

    # Get column names from schema
    schema_id <- names(raw$schema)[1]
    col_names <- names(raw$schema[[schema_id]]$fields)

    # Parse array-of-arrays into tibble
    df <- bind_rows(lapply(results, function(r) {
      vals <- vapply(r, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
      if (length(vals) == length(col_names)) names(vals) <- col_names
      as.list(vals)
    }))
    all_data[[length(all_data) + 1]] <- as_tibble(df)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (n_so_far >= total) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Auto-type numeric-looking columns (exclude IDs, zip codes, phone numbers)
  skip_cols <- grep("id$|_id|zip|phone|fips|code$|number$|footnote", names(result), value = TRUE)
  type_cols <- setdiff(names(result), c(skip_cols, "state", "address", "citytown",
                                         "facility_name", "hospital_name"))
  for (col in type_cols) {
    vals <- result[[col]]
    if (!is.character(vals)) next
    nums <- suppressWarnings(as.numeric(vals))
    if (sum(!is.na(nums)) >= sum(vals != "" & !is.na(vals)) * 0.8)
      result[[col]] <- nums
  }
  result
}



# == Dataset discovery =========================================================

#' List all CMS provider data datasets
#'
#' Returns metadata for all datasets in the CMS (Centers for Medicare &
#' Medicaid Services) provider data catalog. The catalog includes hospital
#' quality measures, nursing home ratings, dialysis facility data, home health
#' agency information, and more. Use the returned \code{identifier} with
#' \code{cms_get()} to fetch actual data.
#'
#' @param max_results Optional integer. Maximum number of datasets to return.
#'   Default \code{NULL} returns all (~233 datasets).
#' @return A tibble sorted by \code{modified} (descending) with columns:
#'   \describe{
#'     \item{identifier}{Character. Dataset ID for use with \code{cms_get()} (e.g. \code{"xubh-q36u"}).}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Full description of the dataset contents.}
#'     \item{modified}{Date. Date the dataset was last updated.}
#'     \item{theme}{Character. Semicolon-separated thematic categories.}
#'     \item{keyword}{Character. Semicolon-separated keywords (up to 5).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # List all datasets
#' cms_datasets()
#'
#' # Get just the 10 most recently updated
#' cms_datasets(max_results = 10)
#' }
cms_datasets <- function(max_results = NULL) {
  url <- paste0(.cms_base, "/metastore/schemas/dataset/items?show-reference-ids")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  if (length(raw) == 0) return(tibble(identifier = character(), title = character()))

  df <- bind_rows(lapply(raw, function(d) {
    tibble(
      identifier  = d$identifier %||% NA_character_,
      title       = d$title %||% NA_character_,
      description = d$description %||% NA_character_,
      modified    = d$modified %||% NA_character_,
      theme       = paste(d$theme %||% character(), collapse = "; "),
      keyword     = paste(head(d$keyword %||% character(), 5), collapse = "; ")
    )
  }))

  df$modified <- suppressWarnings(as.Date(df$modified))
  if (!is.null(max_results)) df <- head(df, max_results)
  df |> arrange(desc(modified))
}

#' Search CMS datasets by keyword
#'
#' Filters the full CMS dataset catalog by a search term. Matches
#' case-insensitively against title, description, and keywords.
#'
#' @param query Character. Search term (case-insensitive). Examples:
#'   \code{"hospital"}, \code{"nursing"}, \code{"dialysis"},
#'   \code{"home health"}, \code{"physician"}, \code{"outpatient"}.
#' @return A tibble with the same columns as \code{cms_datasets()}: identifier,
#'   title, description, modified, theme, keyword.
#' @export
#' @examples
#' \dontrun{
#' cms_search("hospital")
#' cms_search("nursing home")
#' }
cms_search <- function(query) {
  ds <- cms_datasets()
  ds |> filter(
    grepl(query, title, ignore.case = TRUE) |
    grepl(query, description, ignore.case = TRUE) |
    grepl(query, keyword, ignore.case = TRUE)
  )
}


# == Core data fetching ========================================================

#' Fetch data from any CMS provider dataset
#'
#' Generic data fetcher that works with any CMS provider data dataset. Supports
#' filtering by column values and automatic pagination. Numeric-looking columns
#' are auto-typed. Use \code{cms_datasets()} or \code{cms_search()} to discover
#' dataset IDs.
#'
#' @param dataset_id Character. CMS dataset identifier. Well-known IDs:
#'   \code{"xubh-q36u"} (Hospital General Information),
#'   \code{"yv7e-xc69"} (Hospital Timely and Effective Care),
#'   \code{"4pq5-n9py"} (Nursing Home Provider Info),
#'   \code{"23ew-n7w9"} (Dialysis Facility Listing),
#'   \code{"6jpm-sxkc"} (Home Health Care Agencies).
#' @param ... Named filter arguments. Name must match a column name in the
#'   dataset, value is the exact string to match. Examples:
#'   \code{state = "CA"}, \code{hospital_type = "Acute Care Hospitals"}.
#' @param max_results Integer. Maximum rows to return (default 1000).
#'   Pagination is handled automatically.
#' @return A tibble with all columns from the dataset. Column names and count
#'   vary by dataset. Numeric-looking columns are auto-converted (excluding
#'   IDs, zip codes, phone numbers, and FIPS codes).
#' @export
#' @examples
#' \dontrun{
#' # Hospitals in Vermont
#' cms_get("xubh-q36u", state = "VT", max_results = 10)
#'
#' # All nursing homes in California
#' cms_get("4pq5-n9py", state = "CA", max_results = 50)
#' }
cms_get <- function(dataset_id, ..., max_results = 1000) {
  args <- list(...)
  conditions <- lapply(names(args), function(nm) {
    list(property = nm, value = as.character(args[[nm]]))
  })
  .cms_query(dataset_id, conditions, max_results = max_results)
}


# == Convenience: Hospital data ================================================

#' Hospital general information
#'
#' Returns CMS Hospital Compare general information including overall star
#' ratings, ownership, and measure group counts. Wraps dataset
#' \code{"xubh-q36u"}.
#'
#' @param state Optional character. Two-letter US state code to filter by
#'   (e.g. \code{"VT"}, \code{"CA"}, \code{"NY"}).
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with 38 columns including: facility_id, facility_name,
#'   address, citytown, state, zip_code, countyparish, telephone_number,
#'   hospital_type (character), hospital_ownership (character),
#'   emergency_services (character), hospital_overall_rating (numeric, 1-5),
#'   and measure group counts for mortality, safety, readmission, patient
#'   experience, and timely/effective care.
#' @export
#' @examples
#' \dontrun{
#' # All hospitals in Vermont
#' cms_hospitals(state = "VT")
#'
#' # First 50 hospitals nationwide
#' cms_hospitals(max_results = 50)
#' }
cms_hospitals <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("xubh-q36u", conditions, max_results = max_results)
}

#' Hospital quality measures -- timely and effective care
#'
#' Returns per-hospital scores on CMS quality measures for timely and effective
#' care. Wraps dataset \code{"yv7e-xc69"}.
#'
#' @param state Optional character. Two-letter US state code (e.g. \code{"CA"}).
#' @param measure_id Optional character. Specific CMS measure ID to filter
#'   (e.g. \code{"EDV"}, \code{"OP_18b"}, \code{"SEP_1"}).
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with 16 columns: facility_id, facility_name, address,
#'   citytown, state, zip_code, countyparish, telephone_number, _condition,
#'   measure_id, measure_name, score (numeric), sample (numeric), footnote,
#'   start_date, end_date.
#' @export
#' @examples
#' \dontrun{
#' cms_hospital_quality(state = "VT", max_results = 20)
#' cms_hospital_quality(measure_id = "EDV", max_results = 50)
#' }
cms_hospital_quality <- function(state = NULL, measure_id = NULL,
                                 max_results = 1000) {
  conditions <- list()
  if (!is.null(state))      conditions <- c(conditions, list(list(property = "state", value = state)))
  if (!is.null(measure_id)) conditions <- c(conditions, list(list(property = "measure_id", value = measure_id)))
  .cms_query("yv7e-xc69", conditions, max_results = max_results)
}


# == Convenience: Nursing homes ================================================

#' Nursing home (skilled nursing facility) information
#'
#' Returns CMS Nursing Home Compare data including overall star ratings,
#' staffing levels, quality measure scores, and inspection results. Wraps
#' dataset \code{"4pq5-n9py"}. Each row is one facility with ~95 columns.
#'
#' @param state Optional character. Two-letter US state code (e.g. \code{"VT"}).
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with ~95 columns including: provider_name, state,
#'   overall_rating (numeric, 1-5), health_inspection_rating,
#'   staffing_rating, qm_rating, number_of_certified_beds,
#'   reported_rn_staffing_hours_per_resident_per_day, total_nursing_staff_turnover,
#'   and many more quality and staffing metrics.
#' @export
#' @examples
#' \dontrun{
#' cms_nursing_homes(state = "VT")
#' cms_nursing_homes(state = "CA", max_results = 50)
#' }
cms_nursing_homes <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("4pq5-n9py", conditions, max_results = max_results)
}


# == Convenience: Dialysis facilities ==========================================

#' Dialysis facility information
#'
#' Returns CMS Dialysis Facility Compare data including five-star ratings,
#' mortality, hospitalization, infection, and adequacy measures. Wraps
#' dataset \code{"23ew-n7w9"}. Each row is one facility with ~130 columns.
#'
#' @param state Optional character. Two-letter US state code (e.g. \code{"VT"}).
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with ~130 columns including: facility_name, state,
#'   five_star (numeric, 1-5), of_dialysis_stations, mortality_rate_facility,
#'   hospitalization_rate_facility, readmission_rate_facility,
#'   transfusion_rate_facility, standard_infection_ratio, fistula_rate_facility,
#'   and many more clinical quality metrics.
#' @export
#' @examples
#' \dontrun{
#' cms_dialysis(state = "VT")
#' cms_dialysis(state = "NY", max_results = 50)
#' }
cms_dialysis <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("23ew-n7w9", conditions, max_results = max_results)
}


# == Convenience: Home health agencies =========================================

#' Home health agency information
#'
#' Returns CMS Home Health Compare data including star ratings, patient
#' outcome measures, and spending metrics. Wraps dataset \code{"6jpm-sxkc"}.
#' Each row is one agency with ~95 columns.
#'
#' @param state Optional character. Two-letter US state code (e.g. \code{"VT"}).
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with ~95 columns including: provider_name, state,
#'   quality_of_patient_care_star_rating, offers_nursing_care_services,
#'   offers_physical_therapy_services, and many patient outcome measures such
#'   as improvement in walking, bathing, breathing, and medication management.
#' @export
#' @examples
#' \dontrun{
#' cms_home_health(state = "VT")
#' cms_home_health(state = "TX", max_results = 50)
#' }
cms_home_health <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("6jpm-sxkc", conditions, max_results = max_results)
}


# == Context ===================================================================

#' Get data.cms.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cms_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cms_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.cms.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.cms.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.cms.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.cms.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
