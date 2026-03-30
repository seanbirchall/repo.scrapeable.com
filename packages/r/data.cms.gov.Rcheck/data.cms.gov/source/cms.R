# cms-gov.R
# Self-contained CMS (Centers for Medicare & Medicaid Services) data client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://data.cms.gov/provider-data/api

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cms_base <- "https://data.cms.gov/provider-data/api/1"

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
#' Returns metadata for all ~233 datasets in the CMS provider data catalog.
#'
#' @param max_results Max datasets to return (default: all)
#' @return tibble: identifier (dataset ID), title, description, modified
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
#' @param query Search term (case-insensitive, matches title and description)
#' @return tibble: identifier, title, description, modified
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
#' @param dataset_id CMS dataset identifier (e.g. "xubh-q36u" for Hospital
#'   General Information). Use cms_datasets() or cms_search() to find IDs.
#' @param ... Named filter arguments. Name = column name, value = filter value.
#'   e.g. state = "CA", hospital_type = "Acute Care Hospitals"
#' @param max_results Max rows to return (default 1000)
#' @return tibble with all columns from the dataset
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
#' CMS Hospital Compare general information including ratings.
#'
#' @param state 2-letter state code filter
#' @param max_results Max results (default 1000)
#' @return tibble: facility_id, facility_name, address, city, state,
#'   hospital_type, hospital_ownership, emergency_services,
#'   hospital_overall_rating, ...
cms_hospitals <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("xubh-q36u", conditions, max_results = max_results)
}

#' Hospital quality measures - timely and effective care
#'
#' @param state 2-letter state code filter
#' @param measure_id Specific measure ID filter
#' @param max_results Max results (default 1000)
#' @return tibble with quality measure scores by hospital
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
#' @param state 2-letter state code
#' @param max_results Max results (default 1000)
#' @return tibble with facility info, ratings, staffing data
cms_nursing_homes <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("4pq5-n9py", conditions, max_results = max_results)
}


# == Convenience: Dialysis facilities ==========================================

#' Dialysis facility information
#'
#' @param state 2-letter state code
#' @param max_results Max results (default 1000)
#' @return tibble with facility info and quality measures
cms_dialysis <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("23ew-n7w9", conditions, max_results = max_results)
}


# == Convenience: Home health agencies =========================================

#' Home health agency information
#'
#' @param state 2-letter state code
#' @param max_results Max results (default 1000)
#' @return tibble with agency info and quality ratings
cms_home_health <- function(state = NULL, max_results = 1000) {
  conditions <- list()
  if (!is.null(state)) conditions <- list(list(property = "state", value = state))
  .cms_query("6jpm-sxkc", conditions, max_results = max_results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the data.cms.gov package
#'
#' @return Character string (invisibly), also printed
cms_context <- function() {
  .build_context("data.cms.gov", header_lines = c(
    "# data.cms.gov - CMS Provider Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API)",
    "# All functions return tibbles.",
    "#",
    "# Key dataset IDs:",
    "#   xubh-q36u = Hospital General Information (5,400+ hospitals)",
    "#   yv7e-xc69 = Hospital Quality Measures",
    "#   4pq5-n9py = Nursing Home Provider Information",
    "#   23ew-n7w9 = Dialysis Facility Listing",
    "#   6jpm-sxkc = Home Health Agency Information",
    "#",
    "# Use cms_datasets() or cms_search('keyword') to discover all 233 datasets.",
    "# Use cms_get('dataset-id', column = 'value') to query any dataset."
  ))
}
