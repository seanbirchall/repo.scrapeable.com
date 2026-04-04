# blm.gov.R
# Self-contained Bureau of Land Management GIS client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public ArcGIS REST services)
# Docs: https://gis.blm.gov/arcgis/rest/services

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.blm_base <- "https://gis.blm.gov/arcgis/rest/services"

# -- Core ArcGIS query engine --------------------------------------------------

.blm_query <- function(service, layer = 0, where = "1=1", out_fields = "*",
                       result_count = 1000, result_offset = 0,
                       order_by = NULL) {
  url <- paste0(.blm_base, "/", service, "/FeatureServer/", layer, "/query")
  params <- list(
    where = where,
    outFields = out_fields,
    resultRecordCount = result_count,
    resultOffset = result_offset,
    f = "json"
  )
  if (!is.null(order_by)) params$orderByFields <- order_by

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# Paginated query returning tibble
.blm_fetch <- function(service, layer = 0, where = "1=1", out_fields = "*",
                       max_results = 500, order_by = NULL) {
  all_rows <- list()
  offset <- 0
  page <- min(max_results, 1000)

  repeat {
    raw <- .blm_query(service, layer, where, out_fields,
                      result_count = page, result_offset = offset,
                      order_by = order_by)
    feats <- raw$features
    if (is.null(feats) || length(feats) == 0) break

    rows <- lapply(feats, function(f) {
      attrs <- f$attributes
      lapply(attrs, function(v) {
        if (is.null(v)) NA_character_ else as.character(v)
      })
    })
    all_rows <- c(all_rows, rows)
    offset <- offset + length(feats)

    if (length(all_rows) >= max_results) break
    if (!isTRUE(raw$exceededTransferLimit) && length(feats) < page) break
  }

  if (length(all_rows) == 0) return(tibble())
  if (length(all_rows) > max_results) all_rows <- all_rows[seq_len(max_results)]
  bind_rows(lapply(all_rows, as_tibble))
}

# Count features
.blm_count <- function(service, layer = 0, where = "1=1") {
  url <- paste0(.blm_base, "/", service, "/FeatureServer/", layer, "/query")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(where = where, returnCountOnly = "true", f = "json") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  raw$count %||% 0L
}

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

# == Public functions ==========================================================

#' List available BLM GIS service categories
#'
#' Returns the folder structure of the BLM national ArcGIS REST services.
#'
#' @return A tibble with columns: folder (character)
#' @export
blm_list <- function() {
  url <- paste0(.blm_base, "?f=json")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  folders <- vapply(raw$folders, as.character, character(1))
  tibble(folder = folders)
}

#' List services within a BLM GIS folder
#'
#' @param folder Character. One of the folder names from blm_list().
#' @return A tibble with columns: name, type
#' @export
blm_services <- function(folder) {
  url <- paste0(.blm_base, "/", folder, "?f=json")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  if (length(raw$services) == 0) return(tibble(name = character(), type = character()))
  bind_rows(lapply(raw$services, function(s) {
    tibble(name = s$name %||% NA_character_, type = s$type %||% NA_character_)
  }))
}

#' Search BLM national recreation sites
#'
#' Queries the BLM National Recreation Sites FeatureServer.
#'
#' @param query Character. Search term to match against RecAreaName or State.
#'   Use NULL to list all sites.
#' @param state Character. Two-letter state abbreviation to filter by.
#' @param max_results Integer. Maximum number of results (default 100).
#' @return A tibble of recreation sites with columns including:
#'   RecAreaName, State, RecAreaDescription, ActivityNames, RecAreaLatitude,
#'   RecAreaLongitude, RecAreaPhone, RecAreaEmail, BLMRecURL
#' @export
blm_search <- function(query = NULL, state = NULL, max_results = 100) {
  clauses <- "1=1"
  if (!is.null(state)) {
    clauses <- paste0("State = '", toupper(state), "'")
  }
  if (!is.null(query)) {
    q_esc <- gsub("'", "''", query)
    clause <- paste0("RecAreaName LIKE '%", q_esc, "%'")
    clauses <- if (clauses == "1=1") clause else paste(clauses, "AND", clause)
  }

  .blm_fetch("recreation/BLM_Natl_Recreation_Offline", layer = 0,
             where = clauses, max_results = max_results,
             order_by = "RecAreaName ASC")
}

#' Get BLM National Landscape Conservation System areas
#'
#' Returns national monuments, conservation areas, wilderness, and other
#' NLCS-designated lands managed by BLM.
#'
#' @param state Character. Two-letter state abbreviation.
#' @param max_results Integer. Maximum results (default 200).
#' @return A tibble of NLCS areas with columns including:
#'   Label, NCA_NAME, sma_code, STATE_GEOG, WEBLINK, NLCS_ID
#' @export
blm_nlcs <- function(state = NULL, max_results = 200) {
  where <- "1=1"
  if (!is.null(state)) {
    where <- paste0("STATE_GEOG = '", toupper(state), "'")
  }
  .blm_fetch("lands/BLM_Natl_NLCS_Generalized", layer = 0,
             where = where, max_results = max_results)
}

#' Get BLM recreation site details by name
#'
#' Returns detailed information for a specific recreation site.
#'
#' @param name Character. Exact or partial name of the recreation area.
#' @return A tibble (usually 1 row) with all available fields.
#' @export
blm_recreation_site <- function(name) {
  q_esc <- gsub("'", "''", name)
  where <- paste0("RecAreaName LIKE '%", q_esc, "%'")
  .blm_fetch("recreation/BLM_Natl_Recreation_Offline", layer = 0,
             where = where, max_results = 10)
}

#' Count features in a BLM service layer
#'
#' @param service Character. Service path (e.g., "recreation/BLM_Natl_Recreation_Offline").
#' @param layer Integer. Layer index (default 0).
#' @param where Character. SQL where clause (default "1=1").
#' @return Integer count.
#' @export
blm_count <- function(service, layer = 0, where = "1=1") {
  .blm_count(service, layer, where)
}

#' Query any BLM FeatureServer layer
#'
#' Generic query function for any BLM ArcGIS FeatureServer endpoint.
#'
#' @param service Character. Service path (e.g., "recreation/BLM_Natl_Recreation_Offline").
#' @param layer Integer. Layer index (default 0).
#' @param where Character. SQL where clause (default "1=1").
#' @param fields Character. Comma-separated field names or "*" for all.
#' @param max_results Integer. Maximum results (default 500).
#' @return A tibble of feature attributes.
#' @export
blm_query <- function(service, layer = 0, where = "1=1", fields = "*",
                      max_results = 500) {
  .blm_fetch(service, layer, where = where, out_fields = fields,
             max_results = max_results)
}

#' Print blm.gov client context
#'
#' @return Character string of function signatures (invisibly).
#' @export
blm_context <- function() {
  src <- system.file("source", "blm.R", package = "blm.gov")
  if (src == "") {
    f <- sys.frame(sys.nframe())$ofile %||%
      attr(body(blm_context), "srcfile")$filename %||% ""
    if (nzchar(f)) src <- f
  }
  .build_context("blm.gov", src_file = if (nzchar(src)) src else NULL,
                 header_lines = c(
                   "# blm.gov — Bureau of Land Management GIS R client",
                   "# Base: https://gis.blm.gov/arcgis/rest/services",
                   "# Auth: none"
                 ))
}
