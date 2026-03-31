# == Public Holidays ===========================================================

#' Fetch public holidays for a country and year
#'
#' @param country ISO 3166-1 alpha-2 country code (e.g. "US", "DE", "FR")
#' @param year Year (default current year)
#' @param language Language for holiday names (default "EN")
#' @return tibble: id, start_date, end_date, type, name, nationwide
#' @export
holiday_public <- function(country, year = format(Sys.Date(), "%Y"),
                           language = "EN") {
  url <- sprintf("%s/PublicHolidays?countryIsoCode=%s&languageIsoCode=%s&validFrom=%s-01-01&validTo=%s-12-31",
                 .holiday_base, country, language, year, year)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_holidays)

  rows <- lapply(raw, function(h) {
    tibble(
      id = as.character(h$id %||% NA),
      start_date = as.Date(h$startDate %||% NA),
      end_date = as.Date(h$endDate %||% NA),
      type = as.character(h$type %||% NA),
      name = .extract_name(h$name, language),
      nationwide = as.logical(h$nationwide %||% NA)
    )
  })
  bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fetch school holidays for a country and year
#'
#' @param country ISO 3166-1 alpha-2 country code (e.g. "DE", "FR", "AT")
#' @param year Year (default current year)
#' @param language Language for holiday names (default "EN")
#' @return tibble: id, start_date, end_date, type, name, nationwide
#' @export
holiday_school <- function(country, year = format(Sys.Date(), "%Y"),
                           language = "EN") {
  url <- sprintf("%s/SchoolHolidays?countryIsoCode=%s&languageIsoCode=%s&validFrom=%s-01-01&validTo=%s-12-31",
                 .holiday_base, country, language, year, year)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_holidays)

  rows <- lapply(raw, function(h) {
    tibble(
      id = as.character(h$id %||% NA),
      start_date = as.Date(h$startDate %||% NA),
      end_date = as.Date(h$endDate %||% NA),
      type = as.character(h$type %||% NA),
      name = .extract_name(h$name, language),
      nationwide = as.logical(h$nationwide %||% NA)
    )
  })
  bind_rows(rows)
}

#' Fetch list of supported countries
#'
#' @param language Language for country names (default "EN")
#' @return tibble: iso_code, name, official_languages
#' @export
holiday_countries <- function(language = "EN") {
  raw <- .fetch_json(sprintf("%s/Countries", .holiday_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_countries)

  rows <- lapply(raw, function(c) {
    tibble(
      iso_code = as.character(c$isoCode %||% NA),
      name = .extract_name(c$name, language),
      official_languages = paste(unlist(c$officialLanguages), collapse = ", ")
    )
  })
  bind_rows(rows)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the openholidays package
#'
#' @return Character string (invisibly), also printed
#' @export
holiday_context <- function() {
  .build_context("openholidaysapi.org", header_lines = c(
    "# openholidaysapi.org - Public and School Holiday Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Supported countries include: AD, AL, AT, BE, BG, BR, BY, CA, CH,",
    "#   CZ, DE, DK, EE, ES, FI, FR, GB, GR, HR, HU, IE, IT, LI, LT,",
    "#   LU, LV, MC, MD, ME, MK, MT, NL, NO, PL, PT, RO, RS, RU, SE,",
    "#   SI, SK, SM, TR, UA, US, VA"
  ))
}
