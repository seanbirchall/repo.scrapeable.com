# wakegov.com.R - Wake County Development data client
#
# Data source: Wake County Planning ArcGIS FeatureServer
# Provides: subdivision development and planning data for Wake County, NC
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

.base_url <- "https://maps.wakegov.com/arcgis/rest/services/Planning/Wake_County_Development/FeatureServer/0"

.fetch_arcgis <- function(url, where = "1=1", out_fields = "*",
                          result_offset = 0L, result_count = 1000L) {
  resp <- httr2::request(url) |>
    httr2::req_url_path_append("query") |>
    httr2::req_url_query(
      where            = where,
      outFields        = out_fields,
      f                = "json",
      resultOffset     = result_offset,
      resultRecordCount = result_count,
      returnGeometry   = "false"
    ) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform()
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

.fetch_all_arcgis <- function(url, where = "1=1", out_fields = "*",
                              page_size = 1000L) {
  all_rows <- list()
  offset <- 0L
  repeat {
    data <- .fetch_arcgis(url, where, out_fields, offset, page_size)
    feats <- data$features
    if (is.null(feats) || length(feats) == 0) break
    attrs <- feats$attributes
    if (is.null(attrs) || nrow(attrs) == 0) break
    all_rows[[length(all_rows) + 1]] <- tibble::as_tibble(attrs)
    if (isTRUE(data$exceededTransferLimit)) {
      offset <- offset + page_size
    } else {
      break
    }
  }
  if (length(all_rows) == 0) return(tibble::tibble())
  dplyr::bind_rows(all_rows)
}

.epoch_to_date <- function(ms) {
  if (is.null(ms)) return(as.Date(NA))
  as.Date(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"))
}

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

# == Schemas ===================================================================

.schema_development <- tibble(
  object_id        = integer(),
  case_id          = character(),
  case_number      = character(),
  work_class       = character(),
  application_date = as.Date(character()),
  pin_number       = character(),
  description      = character(),
  plan_status      = character(),
  subdivision_name = character(),
  proposed_lots    = double(),
  acres            = double()
)

# == Public functions ==========================================================

#' Get Wake County development planning records
#'
#' Fetches subdivision and development planning records from the
#' Wake County, NC ArcGIS FeatureServer. Auto-paginates to retrieve
#' all matching records. Covers preliminary and final plat
#' applications for residential and commercial developments.
#'
#' @param status Optional character plan status filter (e.g.,
#'   \code{"Approved"}, \code{"Approval Expired"}, \code{"Preliminary"}).
#'   Uses case-insensitive partial matching (SQL LIKE).
#' @return A tibble with columns:
#'   \describe{
#'     \item{object_id}{Integer ArcGIS feature ID}
#'     \item{case_id}{UUID case identifier}
#'     \item{case_number}{Human-readable case number}
#'     \item{work_class}{Classification (e.g., "Preliminary", "Final")}
#'     \item{application_date}{Date the application was submitted}
#'     \item{pin_number}{Parcel Identification Number (PIN)}
#'     \item{description}{Project description text}
#'     \item{plan_status}{Current plan status}
#'     \item{subdivision_name}{Name of the subdivision}
#'     \item{proposed_lots}{Numeric number of proposed lots}
#'     \item{acres}{Numeric project acreage}
#'   }
#' @export
#' @family Wake County functions
#' @seealso \code{\link{wake_status_summary}} for status counts,
#'   \code{\link{wake_search}} for keyword search
#' @examples
#' \dontrun{
#' # All developments
#' wake_developments()
#'
#' # Only approved plans
#' wake_developments(status = "Approved")
#' }
wake_developments <- function(status = NULL) {
  where <- "1=1"
  if (!is.null(status)) {
    where <- sprintf("PLAN_STATUS LIKE '%%%s%%'", status)
  }
  raw <- .fetch_all_arcgis(.base_url, where = where)
  if (nrow(raw) == 0) return(.schema_development)
  tibble(
    object_id        = as.integer(raw$OBJECTID),
    case_id          = as.character(raw$CASEID),
    case_number      = as.character(raw$CASENUMBER),
    work_class       = as.character(raw$WORK_CLASS),
    application_date = .epoch_to_date(raw$APPLICATIONDATE),
    pin_number       = as.character(raw$PIN_NUM),
    description      = as.character(raw$DESCRIPTION),
    plan_status      = as.character(raw$PLAN_STATUS),
    subdivision_name = as.character(raw$SUBDIVISION_NAME),
    proposed_lots    = as.double(raw$PROPOSED_NO_LOTS),
    acres            = as.double(raw$NUMBER_OF_ACRES)
  )
}

#' Summarize Wake County developments by plan status
#'
#' Fetches all development records and aggregates them by plan status,
#' showing the count of records, total proposed lots, and total acreage
#' for each status category.
#'
#' @return A tibble sorted by record count (descending) with columns:
#'   \describe{
#'     \item{plan_status}{Plan status category}
#'     \item{n_records}{Integer count of development records}
#'     \item{total_lots}{Numeric total proposed lots}
#'     \item{total_acres}{Numeric total acreage}
#'   }
#' @export
#' @family Wake County functions
#' @seealso \code{\link{wake_developments}} for the underlying records
#' @examples
#' \dontrun{
#' wake_status_summary()
#' }
wake_status_summary <- function() {
  all <- wake_developments()
  if (nrow(all) == 0) {
    return(tibble(plan_status = character(), n_records = integer(),
                  total_lots = double(), total_acres = double()))
  }
  all |>
    group_by(plan_status) |>
    summarise(
      n_records   = n(),
      total_lots  = sum(proposed_lots, na.rm = TRUE),
      total_acres = sum(acres, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(n_records))
}

#' Search Wake County development records by keyword
#'
#' Performs case-insensitive text search across subdivision name and
#' description fields. Fetches all records first, then filters locally.
#'
#' @param query Character string to search for (e.g., \code{"Cary"},
#'   \code{"townhome"}).
#' @return A tibble with the same columns as \code{\link{wake_developments}}.
#'   Returns only matching rows.
#' @export
#' @family Wake County functions
#' @seealso \code{\link{wake_developments}} for unfiltered results
#' @examples
#' \dontrun{
#' wake_search("Holly Springs")
#' }
wake_search <- function(query) {
  stopifnot(is.character(query), length(query) == 1, nzchar(query))
  all <- wake_developments()
  if (nrow(all) == 0) return(.schema_development)
  matches <- grepl(query, all$subdivision_name, ignore.case = TRUE) |
    grepl(query, all$description, ignore.case = TRUE)
  all[matches, , drop = FALSE]
}

#' Get wakegov.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wake_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wake_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/wakegov.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "wakegov.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# wakegov.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# wakegov.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
