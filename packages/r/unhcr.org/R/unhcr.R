#' Fetch UNHCR population statistics
#'
#' Query the UNHCR population statistics API for refugees, asylum seekers,
#' IDPs, stateless persons, and others of concern.
#'
#' @param year Year (e.g. 2023). Required.
#' @param country_origin ISO3 country of origin code (e.g. "SYR", "AFG")
#' @param country_asylum ISO3 country of asylum code (e.g. "DEU", "TUR")
#' @param limit Max results (default 100, max 10000)
#' @param page Page number for pagination (default 1)
#' @return tibble: year, country_origin, country_origin_iso, country_asylum,
#'   country_asylum_iso, refugees, asylum_seekers, idps, stateless, oip, total
#' @export
unhcr_population <- function(year, country_origin = NULL,
                             country_asylum = NULL, limit = 100, page = 1) {
  params <- list(
    year = as.integer(year),
    limit = as.integer(limit),
    page = as.integer(page)
  )
  if (!is.null(country_origin)) params$coo <- country_origin
  if (!is.null(country_asylum)) params$coa <- country_asylum

  query_str <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/population/?%s", .unhcr_base, query_str)
  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_population)
  if (is.data.frame(items) && nrow(items) == 0) return(.schema_population)

  as_tibble(items) |>
    transmute(
      year = as.integer(year),
      country_origin = as.character(if ("coo_name" %in% names(items)) coo_name else NA),
      country_origin_iso = as.character(if ("coo_iso" %in% names(items)) coo_iso else if ("coo" %in% names(items)) coo else NA),
      country_asylum = as.character(if ("coa_name" %in% names(items)) coa_name else NA),
      country_asylum_iso = as.character(if ("coa_iso" %in% names(items)) coa_iso else if ("coa" %in% names(items)) coa else NA),
      refugees = as.integer(if ("refugees" %in% names(items)) refugees else NA),
      asylum_seekers = as.integer(if ("asylum_seekers" %in% names(items)) asylum_seekers else NA),
      idps = as.integer(if ("idps" %in% names(items)) idps else NA),
      stateless = as.integer(if ("stateless" %in% names(items)) stateless else NA),
      oip = as.integer(if ("oip" %in% names(items)) oip else NA),
      total = as.integer(if ("total_pop" %in% names(items)) total_pop else NA)
    )
}

# == Demographics ==============================================================

#' Fetch UNHCR demographic data
#'
#' @param year Year (e.g. 2023). Required.
#' @param country_origin ISO3 country of origin code
#' @param country_asylum ISO3 country of asylum code
#' @param limit Max results (default 100)
#' @return tibble: year, country_origin, country_asylum, sex, age_group, value
#' @export
unhcr_demographics <- function(year, country_origin = NULL,
                               country_asylum = NULL, limit = 100) {
  params <- list(
    year = as.integer(year),
    limit = as.integer(limit)
  )
  if (!is.null(country_origin)) params$coo <- country_origin
  if (!is.null(country_asylum)) params$coa <- country_asylum

  query_str <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/demographics/?%s", .unhcr_base, query_str)
  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_demographics)
  if (is.data.frame(items) && nrow(items) == 0) return(.schema_demographics)

  as_tibble(items) |>
    transmute(
      year = as.integer(year),
      country_origin = as.character(if ("coo_name" %in% names(items)) coo_name else NA),
      country_asylum = as.character(if ("coa_name" %in% names(items)) coa_name else NA),
      sex = as.character(if ("sex" %in% names(items)) sex else NA),
      age_group = as.character(if ("age_group" %in% names(items)) age_group else NA),
      value = as.integer(if ("value" %in% names(items)) value else NA)
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the unhcr.org package
#'
#' @return Character string (invisibly), also printed
#' @export
unhcr_context <- function() {
  .build_context("unhcr.org", header_lines = c(
    "# unhcr.org - UNHCR Refugee Statistics API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://api.unhcr.org/population/v1",
    "# All functions return tibbles with typed columns.",
    "# Country codes: ISO 3166-1 alpha-3 (e.g. SYR, AFG, UKR, DEU, TUR)"
  ))
}
