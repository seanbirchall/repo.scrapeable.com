# section508.gov.R
# Self-contained Section 508 Assessment API client.
# All public functions return tibbles. All columns properly typed.
#
# Data source: assets.section508.gov (GSA Governmentwide Section 508 Assessment)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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

#' List available Section 508 assessments
#'
#' Returns a tibble cataloging the available Governmentwide Section 508
#' Assessment datasets, including fiscal year, format, and download URL.
#'
#' @return A tibble with columns: fiscal_year, format, description, url
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
#' data for a given fiscal year. By default returns a summary tibble with
#' entity-level maturity and conformance scores. Set \code{full = TRUE} to
#' get all question-level response columns.
#'
#' @param year Integer. Fiscal year: 2023 or 2024. Default 2024.
#' @param full Logical. If TRUE, return all question-level columns.
#'   Default FALSE (summary columns only).
#' @return A tibble of reporting entity assessment data.
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
#' keyword query. Matches against entity name, parent agency, and bracket
#' fields (case-insensitive).
#'
#' @param query Character. Search term (e.g., agency name, bracket level).
#' @param year Integer. Fiscal year to search. Default 2024.
#' @return A tibble of matching reporting entities with summary scores.
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

#' Show Section 508 client context
#'
#' Reads and displays the source code of this client, listing all public
#' functions with their signatures and roxygen documentation.
#'
#' @return Invisibly returns the context string.
#' @export
s508_context <- function() {
  src <- tryCatch({
    # Try to find source file via srcref
    fn_body <- body(s508_context)
    srcref <- attr(fn_body, "srcfile")
    if (!is.null(srcref)) srcref$filename
    else NULL
  }, error = function(e) NULL)

  if (is.null(src)) {
    # Fallback: search common locations
    candidates <- c(
      "clients/section508.gov.R",
      "section508.gov.R",
      system.file("source", "section508.gov.R", package = "section508.gov")
    )
    for (f in candidates) {
      if (nzchar(f) && file.exists(f)) { src <- f; break }
    }
  }

  .build_context(
    pkg_name = "section508.gov",
    src_file = src,
    header_lines = c(
      "# section508.gov -- Governmentwide Section 508 Assessment Data",
      "# Source: General Services Administration (GSA)",
      "# Datasets: FY2023, FY2024 assessment response data",
      "# Prefix: s508_"
    )
  )
}
