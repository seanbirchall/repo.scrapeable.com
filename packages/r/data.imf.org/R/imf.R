# == Data ======================================================================

#' Fetch IMF indicator data
#'
#' Returns annual data from the IMF World Economic Outlook and related datasets.
#' Covers ~133 macroeconomic indicators for 241 countries.
#'
#' @param indicator Indicator code (e.g. "NGDP_RPCH" for real GDP growth,
#'   "PCPIPCH" for inflation, "LUR" for unemployment).
#'   Use imf_indicators() to find codes.
#' @param countries Character vector of ISO country codes (e.g. "USA", "GBR").
#'   Default: all countries.
#' @param years Optional numeric vector of years to filter
#' @return tibble: country (character), indicator (character),
#'   year (integer), value (numeric)
#' @export
imf_data <- function(indicator, countries = NULL, years = NULL) {
  if (!is.null(countries)) {
    country_str <- paste(countries, collapse = "/")
    url <- sprintf("%s/%s/%s", .imf_base, indicator, country_str)
  } else {
    url <- sprintf("%s/%s", .imf_base, indicator)
  }

  if (!is.null(years)) {
    url <- paste0(url, "?periods=", paste(years, collapse = ","))
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("IMF API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_data)

  values <- raw$values
  if (is.null(values)) return(.schema_data)

  # Structure: values -> {INDICATOR} -> {COUNTRY} -> {YEAR: value}
  ind_data <- values[[indicator]]
  if (is.null(ind_data)) return(.schema_data)

  results <- lapply(names(ind_data), function(cty) {
    year_vals <- ind_data[[cty]]
    if (is.null(year_vals) || length(year_vals) == 0) return(NULL)
    tibble(
      country   = cty,
      indicator = indicator,
      year      = as.integer(names(year_vals)),
      value     = as.numeric(unlist(year_vals))
    )
  })

  result <- bind_rows(results)
  if (nrow(result) == 0) return(.schema_data)
  result |> filter(!is.na(value)) |> arrange(country, year)
}


# == Indicators ================================================================

#' List available IMF indicators
#'
#' Returns ~133 macroeconomic indicators from the DataMapper.
#'
#' @param query Optional search term to filter by name
#' @return tibble: id, label, description, unit, dataset
#' @export
imf_indicators <- function(query = NULL) {
  url <- paste0(.imf_base, "/indicators")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("IMF API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$indicators)) return(.schema_indicators)

  ind <- raw$indicators
  result <- tibble(
    id          = names(ind),
    label       = vapply(ind, function(i) i$label %||% NA_character_, character(1)),
    description = vapply(ind, function(i) {
      d <- i$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    unit        = vapply(ind, function(i) i$unit %||% NA_character_, character(1)),
    dataset     = vapply(ind, function(i) i$dataset %||% NA_character_, character(1))
  )

  if (!is.null(query)) {
    pattern <- tolower(query)
    result <- result |> filter(grepl(pattern, tolower(label)) | grepl(pattern, tolower(id)))
  }
  result
}


# == Countries =================================================================

#' List IMF country codes
#'
#' Returns ~241 countries and regions with ISO codes.
#'
#' @return tibble: iso (character), label (character)
#' @export
imf_countries <- function() {
  url <- paste0(.imf_base, "/countries")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("IMF API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$countries)) return(.schema_countries)

  ctys <- raw$countries
  tibble(
    iso   = names(ctys),
    label = vapply(ctys, function(c) c$label %||% NA_character_, character(1))
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the IMF package
#'
#' @return Character string (invisibly), also printed
#' @export
imf_context <- function() {
  .build_context("data.imf.org", header_lines = c(
    "# data.imf.org - IMF DataMapper API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# ~133 WEO macroeconomic indicators, 241 countries",
    "#",
    "# Common indicators:",
    "#   NGDP_RPCH = Real GDP growth (%)",
    "#   PCPIPCH = Inflation (CPI %)",
    "#   LUR = Unemployment rate (%)",
    "#   BCA_NGDPD = Current account (% GDP)",
    "#   GGXWDG_NGDP = Gov debt (% GDP)",
    "#",
    "# Country codes: ISO3 (USA, GBR, DEU, JPN, CHN)"
  ))
}
