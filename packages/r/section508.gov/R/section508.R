# section508.gov.R
# Self-contained Section 508 Assessment API client.
# All public functions return tibbles. All columns properly typed.
#
# Data source: assets.section508.gov (GSA Governmentwide Section 508 Assessment)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

.s508_fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.s508_fetch_json <- function(url) {
  jsonlite::fromJSON(.s508_fetch(url), simplifyVector = FALSE)
}

# == Data URLs =================================================================

.s508_urls <- list(
  "2024" = list(
    json = "https://assets.section508.gov/assets/files/assessment/2024/Reporting%20Entity%20Response%20Data%20for%20FY24%20Governmentwide%20Section%20508%20Assessment.json",
    csv  = "https://assets.section508.gov/assets/files/assessment/2024/Reporting%20Entity%20Response%20Data%20for%20FY24%20Governmentwide%20Section%20508%20Assessment.csv",
    dict = "https://assets.section508.gov/assets/files/assessment/2024/Data%20Dictionary%20for%20FY24%20Governmentwide%20Section%20508%20Assessment%20Final.xlsx"
  ),
  "2023" = list(
    json = "https://assets.section508.gov/assets/files/assessment/2023/Reporting+Entity+Response+Data+JSON+for+FY23+Government-wide+Section+508+Assessment.json",
    csv  = "https://assets.section508.gov/assets/files/assessment/2023/Reporting%20Entity%20Response%20Data%20for%20FY23%20Government-wide%20Section%20508%20Assessment.csv",
    dict = "https://assets.section508.gov/assets/files/assessment/2023/FY23%20Governmentwide%20Section%20508%20Assessment%20Data%20Dictionary%20Excel.xlsx"
  )
)

# == Summary column names =====================================================
# These are the key maturity/conformance columns present in both FY23 and FY24.

.s508_summary_cols <- c(
  "Reporting Entity Name",
  "Business Function Maturity Bracket",
  "Conformance Bracket",
  "Overall Performance Category",
  "IT Accessibility Program Office Calculation",
  "IT Accessibility Program Office - Bracket",
  "Policies, Procedures, and Practices Calculation",
  "Policies, Procedures, and Practices - Bracket",
  "Communications Calculation",
  "Communications - Bracket",
  "Content Creation Calculation",
  "Content Creation - Bracket",
  "Human Capital, Culture, and Leadership Calculation",
  "Human Capital, Culture, and Leadership - Bracket",
  "Technology Lifecycle Activities Calculation",
  "Technology Lifecycle Activities - Bracket",
  "Testing and Validation Calculation",
  "Testing and Validation - Bracket",
  "Acquisitions and Procurement Calculation",
  "Acquisitions and Procurement - Bracket",
  "Training Calculation",
  "Training - Bracket"
)

# FY24 also has "Parent Agency (If Applicable)"
.s508_fy24_extra <- c("Parent Agency (If Applicable)")

# == Private helpers ===========================================================

.s508_clean_names <- function(nms) {
  nms <- gsub("[[:space:]]+", "_", trimws(nms))
  nms <- gsub("[^A-Za-z0-9_.]", "", nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("_$|^_", "", nms)
  tolower(nms)
}

.s508_parse_records <- function(records, year) {
  # First record is the data dictionary / header row with N/A values; skip it
  if (length(records) == 0) return(tibble())
  first <- records[[1]]
  if (!is.null(first[["Reporting Entity Name"]]) &&
      first[["Reporting Entity Name"]] == "N/A") {
    records <- records[-1]
  }
  if (length(records) == 0) return(tibble())

  # Determine columns available for summary
  avail <- names(records[[1]])
  summary_cols <- intersect(.s508_summary_cols, avail)
  if (year == 2024) {
    summary_cols <- union(c("Reporting Entity Name", "Parent Agency (If Applicable)"), summary_cols)
    summary_cols <- intersect(summary_cols, avail)
  }

  rows <- lapply(records, function(r) {
    vals <- lapply(summary_cols, function(col) {
      v <- r[[col]] %||% NA_character_
      if (is.null(v) || identical(v, "")) NA_character_ else as.character(v)
    })
    names(vals) <- summary_cols
    vals
  })

  df <- bind_rows(lapply(rows, as_tibble))

  # Clean column names
  orig_names <- names(df)
  clean <- .s508_clean_names(orig_names)
  names(df) <- clean

  # Convert calculation columns to numeric
  calc_cols <- grep("_calculation$", names(df), value = TRUE)
  for (col in calc_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  # Add year column
  df <- df |> mutate(fiscal_year = as.integer(year), .before = 1)

  df
}

.s508_parse_full <- function(records, year) {
  # Parse all fields, not just summary
  if (length(records) == 0) return(tibble())
  first <- records[[1]]
  if (!is.null(first[["Reporting Entity Name"]]) &&
      first[["Reporting Entity Name"]] == "N/A") {
    records <- records[-1]
  }
  if (length(records) == 0) return(tibble())

  all_keys <- unique(unlist(lapply(records, names)))

  rows <- lapply(records, function(r) {
    vals <- lapply(all_keys, function(k) {
      v <- r[[k]] %||% NA_character_
      if (is.null(v) || identical(v, "")) NA_character_ else as.character(v)
    })
    names(vals) <- all_keys
    vals
  })

  df <- bind_rows(lapply(rows, as_tibble))
  orig_names <- names(df)
  names(df) <- .s508_clean_names(orig_names)

  # Convert calculation columns to numeric
  calc_cols <- grep("_calculation$", names(df), value = TRUE)
  for (col in calc_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  df <- df |> mutate(fiscal_year = as.integer(year), .before = 1)
  df
}

# == Public functions ==========================================================

#' List available Section 508 accessibility assessments
#'
#' Returns a catalog of available Governmentwide Section 508 Assessment
#' datasets published by GSA. Section 508 requires federal agencies to
#' make their IT accessible to people with disabilities.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fiscal_year}{Integer. Assessment fiscal year (e.g. 2023, 2024).}
#'     \item{format}{Character. File format: \code{"json"}, \code{"csv"}, or \code{"xlsx"} (data dictionary).}
#'     \item{description}{Character. Dataset description.}
#'     \item{url}{Character. Direct download URL.}
#'   }
#' @examples
#' s508_list()
#' @export
s508_list <- function() {
  rows <- list()
  for (yr in names(.s508_urls)) {
    urls <- .s508_urls[[yr]]
    rows[[length(rows) + 1]] <- tibble(
      fiscal_year = as.integer(yr),
      format = "json",
      description = paste0("FY", yr, " Reporting Entity Response Data"),
      url = urls$json
    )
    rows[[length(rows) + 1]] <- tibble(
      fiscal_year = as.integer(yr),
      format = "csv",
      description = paste0("FY", yr, " Reporting Entity Response Data"),
      url = urls$csv
    )
    rows[[length(rows) + 1]] <- tibble(
      fiscal_year = as.integer(yr),
      format = "xlsx",
      description = paste0("FY", yr, " Data Dictionary"),
      url = urls$dict
    )
  }
  bind_rows(rows) |> arrange(desc(fiscal_year), format)
}

#' Fetch Section 508 Assessment data
#'
#' Downloads and parses the Governmentwide Section 508 Assessment response
#' data for a given fiscal year. Each row represents one federal reporting
#' entity (agency or sub-agency) with maturity and conformance scores
#' across multiple dimensions.
#'
#' @param year Integer. Fiscal year: \code{2023} or \code{2024}.
#'   Default \code{2024}.
#' @param full Logical. If \code{TRUE}, return all question-level response
#'   columns (100+ columns). If \code{FALSE} (default), return summary
#'   columns only (~24 columns).
#' @return A tibble with columns (when \code{full = FALSE}):
#'   \describe{
#'     \item{fiscal_year}{Integer. Assessment fiscal year.}
#'     \item{reporting_entity_name}{Character. Federal agency or sub-agency name.}
#'     \item{parent_agency_if_applicable}{Character. Parent agency name (FY2024 only).}
#'     \item{business_function_maturity_bracket}{Character. Overall maturity bracket (e.g. \code{"Low"}, \code{"Moderate"}, \code{"High"}).}
#'     \item{conformance_bracket}{Character. Overall conformance bracket.}
#'     \item{overall_performance_category}{Character. Combined performance category.}
#'     \item{it_accessibility_program_office_calculation}{Numeric. Score (0-5 scale).}
#'     \item{it_accessibility_program_office_bracket}{Character. Bracket level.}
#'     \item{policies_procedures_and_practices_calculation}{Numeric. Score.}
#'     \item{policies_procedures_and_practices_bracket}{Character. Bracket level.}
#'     \item{communications_calculation}{Numeric. Score.}
#'     \item{communications_bracket}{Character. Bracket level.}
#'     \item{content_creation_calculation}{Numeric. Score.}
#'     \item{content_creation_bracket}{Character. Bracket level.}
#'     \item{human_capital_culture_and_leadership_calculation}{Numeric. Score.}
#'     \item{human_capital_culture_and_leadership_bracket}{Character. Bracket level.}
#'     \item{technology_lifecycle_activities_calculation}{Numeric. Score.}
#'     \item{technology_lifecycle_activities_bracket}{Character. Bracket level.}
#'     \item{testing_and_validation_calculation}{Numeric. Score.}
#'     \item{testing_and_validation_bracket}{Character. Bracket level.}
#'     \item{acquisitions_and_procurement_calculation}{Numeric. Score.}
#'     \item{acquisitions_and_procurement_bracket}{Character. Bracket level.}
#'     \item{training_calculation}{Numeric. Score.}
#'     \item{training_bracket}{Character. Bracket level.}
#'   }
#' @examples
#' s508_assessment(year = 2024)
#' s508_assessment(year = 2023, full = TRUE)
#' @export
s508_assessment <- function(year = 2024, full = FALSE) {
  year <- as.character(as.integer(year))
  if (!year %in% names(.s508_urls)) {
    stop("Year must be one of: ", paste(names(.s508_urls), collapse = ", "),
         call. = FALSE)
  }
  url <- .s508_urls[[year]]$json
  records <- .s508_fetch_json(url)

  if (full) {
    .s508_parse_full(records, as.integer(year))
  } else {
    .s508_parse_records(records, as.integer(year))
  }
}

#' Search Section 508 Assessment data by keyword
#'
#' Searches assessment response data for reporting entities matching a
#' keyword query. Matches case-insensitively against all character columns
#' including entity name, parent agency, and bracket fields.
#'
#' @param query Character. Search term (e.g. \code{"Department of"},
#'   \code{"Veterans"}, \code{"High"} to find high-maturity entities).
#' @param year Integer. Fiscal year to search: \code{2023} or \code{2024}.
#'   Default \code{2024}.
#' @return A tibble of matching reporting entities with the same columns
#'   as \code{s508_assessment()}.
#' @examples
#' s508_search("Department of", year = 2024)
#' s508_search("High")
#' @export
s508_search <- function(query, year = 2024) {
  if (missing(query) || !nzchar(trimws(query))) {
    stop("Please provide a search query.", call. = FALSE)
  }
  df <- s508_assessment(year = year, full = FALSE)
  pattern <- tolower(trimws(query))

  # Search across all character columns
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  matches <- rep(FALSE, nrow(df))
  for (col in char_cols) {
    matches <- matches | grepl(pattern, tolower(df[[col]]), fixed = TRUE)
  }

  df[matches, , drop = FALSE]
}

#' Get section508.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' s508_context()
#' @export
s508_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(s508_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/section508.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "section508.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# section508.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# section508.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
