# == Public functions ==========================================================

#' Get all countries
#'
#' @return tibble: name, official, cca2, cca3, region, subregion, capital,
#'   population, area, currencies, languages, flag
#' @export
rc_all <- function() {
  url <- sprintf("%s/all?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag", .rc_base)
  raw <- .fetch_json(url)
  .parse_countries(raw)
}

#' Search countries by name
#'
#' @param name Country name (full or partial, e.g. "united", "brazil")
#' @return tibble of matching countries
#' @export
rc_name <- function(name) {
  url <- sprintf("%s/name/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag",
                 .rc_base, utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  .parse_countries(raw)
}

#' Get country by alpha code
#'
#' @param code ISO 3166-1 alpha-2 or alpha-3 code (e.g. "US", "USA", "BR")
#' @return tibble with one row
#' @export
rc_code <- function(code) {
  url <- sprintf("%s/alpha/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag",
                 .rc_base, utils::URLencode(code, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_countries)
  # single country returns object, not array
  if (!is.null(raw$name)) raw <- list(raw)
  .parse_countries(raw)
}

#' Get countries by region
#'
#' @param region Region name: "Africa", "Americas", "Asia", "Europe", "Oceania"
#' @return tibble of countries in that region
#' @export
rc_region <- function(region) {
  url <- sprintf("%s/region/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag",
                 .rc_base, utils::URLencode(region, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  .parse_countries(raw)
}

#' Show REST Countries package context for LLM use
#'
#' @return Invisible string with full context
#' @export
rc_context <- function() {
  .build_context("restcountries.com", header_lines = c(
    "# restcountries.com -- REST Countries v3.1 API",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Regions: Africa, Americas, Asia, Europe, Oceania",
    "# Codes: ISO 3166-1 alpha-2 (US, GB) or alpha-3 (USA, GBR)"
  ))
}
