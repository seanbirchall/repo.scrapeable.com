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
#' Returns metadata for all datasets in the CMS provider data catalog
#' (approximately 233 datasets). Sorted by most recently modified.
#'
#' @param max_results Integer or NULL. Maximum datasets to return
#'   (default: \code{NULL} returns all).
#' @return A tibble with one row per dataset and 6 columns:
#'   \describe{
#'     \item{identifier}{Character. Dataset ID for use with \code{\link{cms_get}}
#'       (e.g. "xubh-q36u").}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Full text description.}
#'     \item{modified}{Date. Last modification date.}
#'     \item{theme}{Character. Semicolon-delimited themes.}
#'     \item{keyword}{Character. Top 5 keywords, semicolon-delimited.}
#'   }
#' @examples
#' cms_datasets(max_results = 10)
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
#' Case-insensitive search across dataset titles, descriptions, and keywords.
#' Fetches the full catalog first, then filters locally.
#'
#' @param query Character. Search term.
#'   Example: \code{"hospital"}, \code{"nursing"}, \code{"dialysis"}
#' @return A tibble with same columns as \code{\link{cms_datasets}}: identifier,
#'   title, description, modified, theme, keyword. Only matching rows returned.
#' @examples
#' cms_search("hospital")
#' cms_search("nursing home")
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
#' Generic data accessor for any CMS Provider Data dataset. Columns vary
#' by dataset. Numeric-looking columns are auto-typed. Handles pagination
#' for large datasets.
#'
#' @param dataset_id Character. CMS dataset identifier (the short alphanumeric
#'   ID). Use \code{\link{cms_datasets}} or \code{\link{cms_search}} to find IDs.
#'   Example: \code{"xubh-q36u"} (Hospital General Information),
#'   \code{"4pq5-n9py"} (Nursing Homes).
#' @param ... Named filter arguments. Name is the column name, value is the
#'   exact filter value. Example: \code{state = "CA"},
#'   \code{hospital_type = "Acute Care Hospitals"}.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with all columns from the dataset. Columns vary by dataset;
#'   numeric-looking columns are auto-converted to numeric.
#' @examples
#' cms_get("xubh-q36u", max_results = 5)
#' cms_get("xubh-q36u", state = "CA", max_results = 10)
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
#' CMS Hospital Compare general information including overall quality ratings.
#' Dataset ID: xubh-q36u. Covers all Medicare-certified hospitals.
#'
#' @param state Character or NULL. Two-letter US state code filter.
#'   Example: \code{"CA"}, \code{"NY"}, \code{"VT"}
#' @param max_results Integer. Max rows to return (default 1000).
#' @return A tibble with 38 columns including:
#'   \describe{
#'     \item{facility_id}{Character. CMS Certification Number (CCN).}
#'     \item{facility_name}{Character. Hospital name.}
#'     \item{address}{Character. Street address.}
#'     \item{citytown}{Character. City.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{zip_code}{Character. ZIP code.}
#'     \item{countyparish}{Character. County name.}
#'     \item{telephone_number}{Character. Phone number.}
#'     \item{hospital_type}{Character. E.g. "Acute Care Hospitals".}
#'     \item{hospital_ownership}{Character. E.g. "Government - State".}
#'     \item{emergency_services}{Character. "Yes" or "No".}
#'     \item{hospital_overall_rating}{Numeric. 1-5 star rating (or NA).}
#'   }
#'   Plus 26 additional columns for mortality, safety, readmission, patient
#'   experience, and timely/effective care measure group counts and footnotes.
#' @examples
#' cms_hospitals(state = "VT")
#' cms_hospitals(state = "CA", max_results = 50)
cms_hospitals <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("xubh-q36u", conditions, max_results = max_results)
}

#' Hospital quality measures -- timely and effective care
#'
#' CMS Hospital Compare timely and effective care measures. Dataset ID:
#' yv7e-xc69. Each row is one hospital-measure combination.
#'
#' @param state Character or NULL. Two-letter US state code.
#'   Example: \code{"CA"}, \code{"NY"}
#' @param measure_id Character or NULL. Specific CMS measure ID.
#'   Example: \code{"OP_18b"}, \code{"ED_2b"}
#' @param max_results Integer. Max rows to return (default 1000).
#' @return A tibble with columns varying by dataset but typically including
#'   facility_id, facility_name, state, measure_id, measure_name, score,
#'   sample, footnote, and date fields.
#' @examples
#' cms_hospital_quality(state = "VT", max_results = 20)
#' cms_hospital_quality(measure_id = "OP_18b", max_results = 10)
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
#' CMS Nursing Home Compare facility information including star ratings
#' and staffing data. Dataset ID: 4pq5-n9py.
#'
#' @param state Character or NULL. Two-letter US state code.
#'   Example: \code{"CA"}, \code{"NY"}
#' @param max_results Integer. Max rows to return (default 1000).
#' @return A tibble with facility info including provider_name, state,
#'   overall_rating, health_inspection_rating, staffing_rating,
#'   quality_rating, number_of_certified_beds, and more.
#' @examples
#' cms_nursing_homes(state = "VT", max_results = 10)
cms_nursing_homes <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("4pq5-n9py", conditions, max_results = max_results)
}


# == Convenience: Dialysis facilities ==========================================

#' Dialysis facility information
#'
#' CMS Dialysis Facility Compare listing with quality measures.
#' Dataset ID: 23ew-n7w9.
#'
#' @param state Character or NULL. Two-letter US state code.
#'   Example: \code{"CA"}, \code{"TX"}
#' @param max_results Integer. Max rows to return (default 1000).
#' @return A tibble with facility info, quality metrics, and patient
#'   outcome measures. Columns vary but include facility name, address,
#'   state, phone, star ratings, and clinical measures.
#' @examples
#' cms_dialysis(state = "VT", max_results = 10)
cms_dialysis <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("23ew-n7w9", conditions, max_results = max_results)
}


# == Convenience: Home health agencies =========================================

#' Home health agency information
#'
#' CMS Home Health Compare agency information and quality ratings.
#' Dataset ID: 6jpm-sxkc.
#'
#' @param state Character or NULL. Two-letter US state code.
#'   Example: \code{"CA"}, \code{"FL"}
#' @param max_results Integer. Max rows to return (default 1000).
#' @return A tibble with agency info including provider name, address,
#'   state, phone, quality of patient care star rating, and various
#'   quality measure scores.
#' @examples
#' cms_home_health(state = "VT", max_results = 10)
cms_home_health <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("6jpm-sxkc", conditions, max_results = max_results)
}


# == Context ===================================================================

#' Get cms.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cms.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cms.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cms.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cms.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
