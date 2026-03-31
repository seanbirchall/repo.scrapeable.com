# == Data ======================================================================

#' Fetch World Bank indicator data
#'
#' Returns time series data for one indicator across one or more countries.
#' Supports ~29,500 indicators and 296 countries/regions.
#'
#' @param indicator Indicator code (e.g. "NY.GDP.MKTP.CD" for GDP,
#'   "SP.POP.TOTL" for population, "FP.CPI.TOTL.ZG" for inflation).
#'   Use wb_indicators() to find codes.
#' @param country ISO2 country code(s): "US", "GB", c("US","GB","DE").
#'   Use "all" for all countries. Default "all".
#' @param date_range Year range: "2000:2023" or single year "2023".
#'   Default: all available years.
#' @param per_page Results per page (default 1000)
#' @return tibble: country_iso3, country, indicator_id, indicator,
#'   date (character year), value (numeric)
#' @export
wb_data <- function(indicator, country = "all", date_range = NULL,
                    per_page = 1000) {
  country_str <- paste(country, collapse = ";")
  url <- sprintf("%s/country/%s/indicator/%s?format=json&per_page=%d",
                 .wb_base, country_str, indicator, per_page)
  if (!is.null(date_range)) url <- paste0(url, "&date=", date_range)

  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_data)

  # Check total pages and fetch remaining
  total_pages <- as.integer(result$meta$pages %||% 1)
  all_data <- result$data

  if (total_pages > 1) {
    for (pg in 2:min(total_pages, 20)) {
      pg_url <- paste0(url, "&page=", pg)
      pg_result <- tryCatch(.fetch_wb(pg_url), error = function(e) NULL)
      if (!is.null(pg_result$data)) all_data <- c(all_data, pg_result$data)
    }
  }

  tibble(
    country_iso3 = vapply(all_data, function(d) d$countryiso3code %||% NA_character_, character(1)),
    country      = vapply(all_data, function(d) d$country$value %||% NA_character_, character(1)),
    indicator_id = vapply(all_data, function(d) d$indicator$id %||% NA_character_, character(1)),
    indicator    = vapply(all_data, function(d) d$indicator$value %||% NA_character_, character(1)),
    date         = vapply(all_data, function(d) d$date %||% NA_character_, character(1)),
    value        = vapply(all_data, function(d) as.numeric(d$value %||% NA_real_), numeric(1))
  ) |>
    filter(!is.na(value)) |>
    arrange(country_iso3, date)
}


# == Countries =================================================================

#' List World Bank countries and regions
#'
#' @param per_page Results per page (default 300)
#' @return tibble: iso3, iso2, name, region, income_level, capital,
#'   longitude, latitude
#' @export
wb_countries <- function(per_page = 300) {
  url <- sprintf("%s/country?format=json&per_page=%d", .wb_base, per_page)
  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_countries)

  tibble(
    iso3         = vapply(result$data, function(c) c$id %||% NA_character_, character(1)),
    iso2         = vapply(result$data, function(c) c$iso2Code %||% NA_character_, character(1)),
    name         = vapply(result$data, function(c) c$name %||% NA_character_, character(1)),
    region       = vapply(result$data, function(c) c$region$value %||% NA_character_, character(1)),
    income_level = vapply(result$data, function(c) c$incomeLevel$value %||% NA_character_, character(1)),
    capital      = vapply(result$data, function(c) c$capitalCity %||% NA_character_, character(1)),
    longitude    = vapply(result$data, function(c) as.numeric(c$longitude %||% NA_real_), numeric(1)),
    latitude     = vapply(result$data, function(c) as.numeric(c$latitude %||% NA_real_), numeric(1))
  )
}


# == Indicators ================================================================

#' Search World Bank indicators
#'
#' @param query Search term (e.g. "GDP", "population", "inflation")
#' @param per_page Results per page (default 1000)
#' @param page Page number (default 1)
#' @return tibble: id, name, unit, source, source_note
#' @export
wb_indicators <- function(query = NULL, per_page = 1000, page = 1) {
  url <- sprintf("%s/indicator?format=json&per_page=%d&page=%d",
                 .wb_base, per_page, page)

  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_indicators)

  ind <- tibble(
    id          = vapply(result$data, function(i) i$id %||% NA_character_, character(1)),
    name        = vapply(result$data, function(i) i$name %||% NA_character_, character(1)),
    unit        = vapply(result$data, function(i) i$unit %||% NA_character_, character(1)),
    source      = vapply(result$data, function(i) i$source$value %||% NA_character_, character(1)),
    source_note = vapply(result$data, function(i) {
      n <- i$sourceNote %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )

  if (!is.null(query)) {
    pattern <- tolower(query)
    ind <- ind |> filter(grepl(pattern, tolower(name)) | grepl(pattern, tolower(id)))
  }
  ind
}


# == Context ===================================================================

#' Generate LLM-friendly context for the World Bank package
#'
#' @return Character string (invisibly), also printed
#' @export
wb_context <- function() {
  .build_context("data.worldbank.org", header_lines = c(
    "# data.worldbank.org - World Bank Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# 29,500+ indicators, 296 countries/regions",
    "#",
    "# Common indicators:",
    "#   NY.GDP.MKTP.CD = GDP (current US$)",
    "#   SP.POP.TOTL = Population",
    "#   FP.CPI.TOTL.ZG = Inflation (CPI %)",
    "#   SL.UEM.TOTL.ZS = Unemployment (%)",
    "#   NY.GDP.PCAP.CD = GDP per capita",
    "#",
    "# Country codes: ISO2 (US, GB, DE, JP, CN)"
  ))
}
