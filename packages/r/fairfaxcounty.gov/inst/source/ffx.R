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
#' Fetches the complete data inventory from Fairfax County's ArcGIS
#' FeatureServer. Returns a tibble with one row per dataset, including
#' name, category, update frequency, data type, description, and URL.
#' Pagination is handled automatically.
#'
#' @return tibble with columns: object_id, dataset_name, category,
#'   update_frequency, data_type, description, url
#' @export
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
#' and descriptions.
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble (same schema as ffx_datasets)
#' @export
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
#' Returns all unique dataset categories and optionally filters the
#' inventory by category.
#'
#' @param category Optional character string to filter by category
#'   (case-insensitive partial match). If NULL, returns category summary.
#' @return If category is NULL: tibble with columns category, n_datasets.
#'   Otherwise: tibble (same schema as ffx_datasets).
#' @export
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

#' Return context about the Fairfax County client functions
#'
#' @return Character string of function signatures and documentation
#' @export
ffx_context <- function() {
  .build_context(
    pkg_name = "fairfaxcounty.gov",
    header_lines = c(
      "# Fairfax County Open Data Inventory Client",
      "# Source: ArcGIS FeatureServer",
      "# Base URL: https://www.fairfaxcounty.gov/mercator/rest/services/OpenData/OpenData_DataLists/FeatureServer/1"
    )
  )
}
