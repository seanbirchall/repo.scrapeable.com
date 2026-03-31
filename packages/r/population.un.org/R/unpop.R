#' List available population indicators
#'
#' Returns a tibble of available demographic indicators from the UN Population
#' Division Data Portal (e.g., total population, fertility rate, life expectancy).
#'
#' @return tibble: id, name, short_name
#' @export
unpop_indicators <- function() {
  url <- paste0(.unpop_base, "/indicators")
  raw <- .unpop_fetch_paginated(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_indicators)

  nms <- names(raw)
  id_col <- if ("id" %in% nms) "id" else if ("indicatorId" %in% nms) "indicatorId" else nms[1]
  name_col <- if ("name" %in% nms) "name" else if ("indicatorName" %in% nms) "indicatorName" else nms[2]
  short_col <- if ("shortName" %in% nms) "shortName" else if ("short_name" %in% nms) "short_name" else name_col

  as_tibble(raw) |>
    transmute(
      id = as.integer(.data[[id_col]]),
      name = as.character(.data[[name_col]]),
      short_name = as.character(.data[[short_col]])
    )
}

#' List available locations (countries, regions)
#'
#' Returns a tibble of geographic locations available in the UN Population
#' Data Portal, including countries, regions, and income groups.
#'
#' @return tibble: id, name, iso3, type
#' @export
unpop_locations <- function() {
  url <- paste0(.unpop_base, "/locations")
  raw <- .unpop_fetch_paginated(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_locations)

  nms <- names(raw)
  id_col <- if ("id" %in% nms) "id" else if ("locationId" %in% nms) "locationId" else nms[1]
  name_col <- if ("name" %in% nms) "name" else nms[2]
  iso3_col <- if ("iso3" %in% nms) "iso3" else if ("iSO3_Alpha3" %in% nms) "iSO3_Alpha3" else NA
  type_col <- if ("locationType" %in% nms) "locationType" else if ("type" %in% nms) "type" else NA

  as_tibble(raw) |>
    transmute(
      id = as.integer(.data[[id_col]]),
      name = as.character(.data[[name_col]]),
      iso3 = if (is.na(iso3_col)) NA_character_ else as.character(.data[[iso3_col]]),
      type = if (is.na(type_col)) NA_character_ else as.character(.data[[type_col]])
    )
}

#' Fetch population data for a specific indicator and location
#'
#' @param indicator_id Numeric indicator ID (e.g., 49 = total population)
#' @param location_id Numeric location ID (e.g., 840 = United States)
#' @param start_year Start year (default 2000)
#' @param end_year End year (default 2025)
#' @return tibble: location_id, location, indicator_id, indicator, year,
#'   value, sex, variant
#' @export
unpop_data <- function(indicator_id, location_id, start_year = 2000,
                       end_year = 2025) {
  url <- sprintf(
    "%s/data/indicators/%d/locations/%d?startYear=%d&endYear=%d",
    .unpop_base, as.integer(indicator_id), as.integer(location_id),
    as.integer(start_year), as.integer(end_year)
  )
  raw <- .unpop_fetch_paginated(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_data)

  nms <- names(raw)

  as_tibble(raw) |>
    transmute(
      location_id = as.integer(if ("locationId" %in% nms) locationId else if ("locId" %in% nms) locId else NA_integer_),
      location = as.character(if ("location" %in% nms) location else if ("locName" %in% nms) locName else NA_character_),
      indicator_id = as.integer(if ("indicatorId" %in% nms) indicatorId else as.integer(indicator_id)),
      indicator = as.character(if ("indicator" %in% nms) indicator else if ("indicatorName" %in% nms) indicatorName else NA_character_),
      year = as.integer(if ("timeLabel" %in% nms) timeLabel else if ("year" %in% nms) year else NA_integer_),
      value = as.numeric(if ("value" %in% nms) value else NA_real_),
      sex = as.character(if ("sex" %in% nms) sex else NA_character_),
      variant = as.character(if ("variant" %in% nms) variant else if ("variantLabel" %in% nms) variantLabel else NA_character_)
    )
}

#' Print UN Population context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
unpop_context <- function() {
  .build_context(
    pkg_name = "population.un.org",
    header_lines = c(
      "# Package: population.un.org",
      "# UN World Population Prospects Data Portal API",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# Key indicator IDs:",
      "#   49 = Total population (thousands), 19 = Life expectancy at birth,",
      "#   54 = Fertility rate, 1 = Births (thousands), 69 = Infant mortality rate",
      "#",
      "# Key location IDs:",
      "#   840 = USA, 156 = China, 356 = India, 826 = UK, 276 = Germany"
    )
  )
}
