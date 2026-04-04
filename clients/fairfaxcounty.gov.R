# fairfaxcounty.gov.R - Fairfax County Open Data Inventory client
#
# Data source: Fairfax County GIS ArcGIS FeatureServer
# Provides: catalog of all open datasets published by Fairfax County, VA
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

.base_url <- "https://www.fairfaxcounty.gov/mercator/rest/services/OpenData/OpenData_DataLists/FeatureServer/1"

.fetch_arcgis <- function(url, where = "1=1", out_fields = "*",
                          result_offset = 0L, result_count = 1000L) {
  resp <- httr2::request(url) |>
    httr2::req_url_path_append("query") |>
    httr2::req_url_query(
      where            = where,
      outFields        = out_fields,
      f                = "json",
      resultOffset     = result_offset,
      resultRecordCount = result_count
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

.schema_inventory <- tibble(
  object_id         = integer(),
  dataset_name      = character(),
  category          = character(),
  update_frequency  = character(),
  data_type         = character(),
  description       = character(),
  url               = character()
)

# == Public functions ==========================================================

#' List all Fairfax County open datasets
#'
#' Fetches the complete data inventory (~125 datasets) from Fairfax County's
#' ArcGIS FeatureServer. Pagination is handled automatically. Covers 15
#' categories including Environment, Property & Land, Transportation,
#' Education, and Recreation.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{object_id}{Integer. ArcGIS object identifier.}
#'     \item{dataset_name}{Character. Dataset name (e.g. "County Parks", "Fire Stations").}
#'     \item{category}{Character. Dataset category (e.g. "Recreation", "Transportation").}
#'     \item{update_frequency}{Character. How often updated: "Weekly", "Monthly", "Annually".}
#'     \item{data_type}{Character. GIS data type: "Polygon", "Line", "Points", "Table".}
#'     \item{description}{Character. Brief description of the dataset.}
#'     \item{url}{Character. URL to access the dataset.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ffx_datasets()
#' }
ffx_datasets <- function() {
  raw <- .fetch_all_arcgis(.base_url)
  if (nrow(raw) == 0) return(.schema_inventory)
  tibble(
    object_id        = as.integer(raw$OBJECTID %||% NA),
    dataset_name     = as.character(raw$LAYERNAME %||% NA),
    category         = as.character(raw$NEWCATEGORY %||% NA),
    update_frequency = as.character(raw$UPDATE_FREQUENCY %||% NA),
    data_type        = as.character(raw$DATATYPE %||% NA),
    description      = as.character(raw$DESCRIPTION %||% NA),
    url              = as.character(raw$URL %||% NA)
  )
}

#' Search Fairfax County open datasets by keyword
#'
#' Performs a case-insensitive search across dataset names, categories,
#' and descriptions. Fetches all datasets first, then filters locally.
#'
#' @param query Character. Search string (case-insensitive regex). Examples:
#'   \code{"school"}, \code{"park"}, \code{"water"}, \code{"transit"},
#'   \code{"zoning"}.
#' @return A tibble with the same schema as \code{ffx_datasets()}: object_id,
#'   dataset_name, category, update_frequency, data_type, description, url.
#' @export
#' @examples
#' \dontrun{
#' ffx_search("school")
#' ffx_search("water")
#' }
ffx_search <- function(query) {
  stopifnot(is.character(query), length(query) == 1, nzchar(query))
  all <- ffx_datasets()
  if (nrow(all) == 0) return(.schema_inventory)
  matches <- grepl(query, all$dataset_name, ignore.case = TRUE) |
    grepl(query, all$category, ignore.case = TRUE) |
    grepl(query, all$description, ignore.case = TRUE)
  all[matches, , drop = FALSE]
}

#' List Fairfax County datasets by category
#'
#' With no argument, returns a summary of all dataset categories and their
#' counts. With a category filter, returns all datasets in matching categories.
#'
#' @param category Character or NULL. Category name to filter by
#'   (case-insensitive partial match). If NULL (default), returns a summary
#'   of all categories. Examples: \code{"Environment"}, \code{"Transportation"},
#'   \code{"Education"}, \code{"Recreation"}.
#' @return If \code{category} is NULL: a tibble with columns:
#'   \describe{
#'     \item{category}{Character. Category name.}
#'     \item{n_datasets}{Integer. Number of datasets in this category.}
#'   }
#'   If \code{category} is provided: a tibble with the same schema as
#'   \code{ffx_datasets()}.
#' @export
#' @examples
#' \dontrun{
#' ffx_categories()
#' ffx_categories("Transportation")
#' }
ffx_categories <- function(category = NULL) {
  all <- ffx_datasets()
  if (nrow(all) == 0) {
    if (is.null(category)) {
      return(tibble(category = character(), n_datasets = integer()))
    }
    return(.schema_inventory)
  }
  if (is.null(category)) {
    all |>
      count(category, name = "n_datasets") |>
      arrange(desc(n_datasets))
  } else {
    all |> filter(grepl(category, .data$category, ignore.case = TRUE))
  }
}

#' Get fairfaxcounty.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ffx_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ffx_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fairfaxcounty.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fairfaxcounty.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fairfaxcounty.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fairfaxcounty.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
