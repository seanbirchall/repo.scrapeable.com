# maricopa.gov.R - Maricopa County Municipal Annexation data client
#
# Data source: Maricopa County GIS ArcGIS MapServer
# Provides: municipal annexation boundary data for Maricopa County, AZ
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

.base_url <- "https://gis.maricopa.gov/arcgis/rest/services/IndividualService/MunicipalAnnexations/MapServer/0"

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

.schema_annexations <- tibble(
  object_id              = integer(),
  city_name              = character(),
  full_city_name         = character(),
  ordinance              = character(),
  ordinance_web_link     = character(),
  ordinance_date         = as.Date(character()),
  ordinance_effective_date = as.Date(character()),
  annexation_number      = character(),
  ward_number            = integer(),
  city_description       = character(),
  area_acres             = double()
)

# == Public functions ==========================================================

#' Get municipal annexation records for Maricopa County
#'
#' Fetches all annexation boundary records from the Maricopa County GIS
#' ArcGIS MapServer. Pagination is handled automatically so every matching
#' record is returned regardless of server page-size limits.
#'
#' @param city Character. Optional city name filter (case-insensitive
#'   partial match). Common values include \code{"PHOENIX"},
#'   \code{"MESA"}, \code{"SCOTTSDALE"}, \code{"TEMPE"},
#'   \code{"BUCKEYE"}, \code{"PEORIA"}. \code{NULL} (default) returns
#'   all cities. Use \code{\link{mcpa_cities}} to see all available names.
#' @return A tibble with columns:
#'   \describe{
#'     \item{object_id}{Integer. Unique GIS feature identifier.}
#'     \item{city_name}{Character. Uppercase city abbreviation
#'       (e.g. \code{"PHOENIX"}).}
#'     \item{full_city_name}{Character. Full municipal name
#'       (e.g. \code{"City of Phoenix"}).}
#'     \item{ordinance}{Character. Ordinance number.}
#'     \item{ordinance_web_link}{Character. URL to the ordinance document.}
#'     \item{ordinance_date}{Date. Date the ordinance was passed.}
#'     \item{ordinance_effective_date}{Date. Date the annexation took effect.}
#'     \item{annexation_number}{Character. County-assigned annexation number.}
#'     \item{ward_number}{Integer. Ward/district number, or \code{NA}.}
#'     \item{city_description}{Character. Description or notes.}
#'     \item{area_acres}{Double. Area of the annexation in acres.}
#'   }
#' @examples
#' mcpa_annexations()
#' mcpa_annexations(city = "PHOENIX")
#' @export
mcpa_annexations <- function(city = NULL) {
  where <- "1=1"
  if (!is.null(city)) {
    where <- sprintf("CityName LIKE '%%%s%%'", toupper(city))
  }
  raw <- .fetch_all_arcgis(.base_url, where = where)
  if (nrow(raw) == 0) return(.schema_annexations)
  tibble(
    object_id              = as.integer(raw$OBJECTID),
    city_name              = as.character(raw$CityName),
    full_city_name         = as.character(raw$FullCityName),
    ordinance              = as.character(raw$Ordinance),
    ordinance_web_link     = as.character(raw$OrdinanceWebLink),
    ordinance_date         = .epoch_to_date(raw$OrdinanceDate),
    ordinance_effective_date = .epoch_to_date(raw$OrdinanceEffectiveDate),
    annexation_number      = as.character(raw$AnnexationNumber),
    ward_number            = as.integer(raw$WardNumber),
    city_description       = as.character(raw$CityDescription),
    area_acres             = as.double(raw$Area_Acre)
  )
}

#' List cities with annexation records in Maricopa County
#'
#' Fetches all annexation records and summarises them by city, returning
#' a count of annexation records for each municipality. Results are
#' sorted by count descending (busiest cities first).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{city_name}{Character. Uppercase city abbreviation
#'       (e.g. \code{"PHOENIX"}, \code{"BUCKEYE"}).}
#'     \item{full_city_name}{Character. Full municipal name
#'       (e.g. \code{"City of Phoenix"}).}
#'     \item{n_annexations}{Integer. Number of annexation records
#'       for the city.}
#'   }
#' @examples
#' mcpa_cities()
#' @export
mcpa_cities <- function() {
  all <- mcpa_annexations()
  if (nrow(all) == 0) return(tibble(city_name = character(), full_city_name = character(), n_annexations = integer()))
  all |>
    count(city_name, full_city_name, name = "n_annexations") |>
    arrange(desc(n_annexations))
}

#' Search annexation records by keyword
#'
#' Performs a case-insensitive keyword search across city name,
#' full city name, and description fields of all Maricopa County
#' annexation records.
#'
#' @param query Character. Non-empty search string to match
#'   (e.g. \code{"Scottsdale"}, \code{"annex"}).
#' @return A tibble with the same columns as \code{\link{mcpa_annexations}}:
#'   \code{object_id}, \code{city_name}, \code{full_city_name},
#'   \code{ordinance}, \code{ordinance_web_link}, \code{ordinance_date},
#'   \code{ordinance_effective_date}, \code{annexation_number},
#'   \code{ward_number}, \code{city_description}, \code{area_acres}.
#' @examples
#' mcpa_search("Scottsdale")
#' @export
mcpa_search <- function(query) {
  stopifnot(is.character(query), length(query) == 1, nzchar(query))
  all <- mcpa_annexations()
  if (nrow(all) == 0) return(.schema_annexations)
  matches <- grepl(query, all$city_name, ignore.case = TRUE) |
    grepl(query, all$full_city_name, ignore.case = TRUE) |
    grepl(query, all$city_description, ignore.case = TRUE)
  all[matches, , drop = FALSE]
}

#' Get maricopa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mcpa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mcpa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/maricopa.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "maricopa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# maricopa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# maricopa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
