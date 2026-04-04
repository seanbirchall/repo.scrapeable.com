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
#' ArcGIS MapServer. Pagination is handled automatically.
#'
#' @param city Optional city name to filter results (case-insensitive partial match)
#' @return tibble with columns: object_id, city_name, full_city_name,
#'   ordinance, ordinance_web_link, ordinance_date, ordinance_effective_date,
#'   annexation_number, ward_number, city_description, area_acres
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
#' Returns a summary of all cities and number of annexation records.
#'
#' @return tibble with columns: city_name, full_city_name, n_annexations
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
#' Case-insensitive search across city name and description fields.
#'
#' @param query Character string to search for
#' @return tibble (same schema as mcpa_annexations)
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

#' Return context about the Maricopa County client functions
#'
#' @return Character string of function signatures and documentation
#' @export
mcpa_context <- function() {
  .build_context(
    pkg_name = "maricopa.gov",
    header_lines = c(
      "# Maricopa County Municipal Annexation Data Client",
      "# Source: ArcGIS MapServer",
      "# Base URL: https://gis.maricopa.gov/arcgis/rest/services/IndividualService/MunicipalAnnexations/MapServer/0"
    )
  )
}
