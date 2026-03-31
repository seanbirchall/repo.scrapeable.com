
#' Fetch historical emissions data from Climate Watch
#'
#' Returns yearly emissions data from the CAIT / PIK / other sources.
#' Data is pivoted from wide to long: one row per country-sector-gas-year.
#'
#' @param regions Character vector of ISO3 codes or region names
#'   (e.g. "USA", "WORLD", "CHN"). Default "WORLD".
#' @param source_ids Integer vector of data source IDs (default NULL = all).
#'   Use cw_sources() to see available sources.
#' @param gas_ids Integer vector of gas IDs (default NULL = all)
#' @param sector_ids Integer vector of sector IDs (default NULL = all)
#' @param per_page Max results per page (default 50)
#' @param page Page number (default 1)
#' @return tibble: iso_code3, country, data_source, sector, gas, unit, year, value
#' @export
cw_emissions <- function(regions = "WORLD", source_ids = NULL, gas_ids = NULL,
                         sector_ids = NULL, per_page = 50, page = 1) {
  url <- paste0(.cw_base, "/data/historical_emissions?per_page=", per_page, "&page=", page)
  for (r in regions) url <- paste0(url, "&regions[]=", utils::URLencode(r))
  if (!is.null(source_ids)) for (s in source_ids) url <- paste0(url, "&source_ids[]=", s)
  if (!is.null(gas_ids)) for (g in gas_ids) url <- paste0(url, "&gas_ids[]=", g)
  if (!is.null(sector_ids)) for (s in sector_ids) url <- paste0(url, "&sector_ids[]=", s)

  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_emissions)

  rows <- lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    em <- row$emissions[[1]]
    if (is.null(em) || length(em) == 0) return(NULL)
    tibble(
      iso_code3   = as.character(row$iso_code3),
      country     = as.character(row$country),
      data_source = as.character(row$data_source),
      sector      = as.character(row$sector),
      gas         = as.character(row$gas),
      unit        = as.character(row$unit),
      year        = as.integer(em$year),
      value       = as.numeric(em$value)
    )
  })
  bind_rows(rows)
}

#' List available data sources
#'
#' @return tibble: id, name
#' @export
cw_sources <- function() {
  url <- paste0(.cw_base, "/emissions/meta")
  raw <- .fetch_json(url)
  srcs <- raw$data_source
  if (is.null(srcs) || length(srcs) == 0) return(.schema_sources)
  tibble(
    id   = as.integer(srcs$id),
    name = as.character(srcs$name)
  )
}

#' Climate Watch context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
cw_context <- function() {
  .build_context("climatewatchdata.org", header_lines = c(
    "# climatewatchdata.org - Climate Watch Historical Emissions API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite",
    "#",
    "# Common regions: WORLD, USA, CHN, IND, EU, GBR, DEU, JPN, BRA",
    "# Use cw_sources() to discover available data source IDs"
  ))
}
