#' Get state-level drought severity statistics (percent area)
#'
#' Returns weekly drought severity percentages for a given state from the
#' US Drought Monitor. D0=Abnormally Dry through D4=Exceptional Drought.
#'
#' @param state Two-letter state abbreviation (e.g., "CA", "TX") or FIPS code
#' @param start Start date as "M/D/YYYY" (e.g., "1/1/2024")
#' @param end End date as "M/D/YYYY" (e.g., "12/31/2024")
#' @return tibble: date, state, none, d0, d1, d2, d3, d4
#' @export
drought_state <- function(state, start = "1/1/2024", end = "12/31/2024") {
  fips <- .to_fips(state)
  url <- sprintf(
    "%s/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=%s&startdate=%s&enddate=%s&statisticsType=2",
    .drought_base, fips, start, end
  )
  tmp <- .fetch(url, ext = ".csv")
  df <- .parse_drought_csv(tmp)
  if (is.null(df) || nrow(df) == 0) return(.schema_drought)

  as_tibble(df) |>
    transmute(
      date = as.Date(as.character(MapDate), format = "%Y%m%d"),
      state = as.character(StateAbbreviation),
      none = as.numeric(None),
      d0 = as.numeric(D0),
      d1 = as.numeric(D1),
      d2 = as.numeric(D2),
      d3 = as.numeric(D3),
      d4 = as.numeric(D4)
    )
}

#' Get county-level drought severity statistics (percent area)
#'
#' Returns weekly drought severity percentages for counties in a given state.
#'
#' @param state Two-letter state abbreviation (e.g., "CA", "TX") or FIPS code
#' @param start Start date as "M/D/YYYY" (e.g., "1/1/2024")
#' @param end End date as "M/D/YYYY" (e.g., "12/31/2024")
#' @return tibble: date, county, state, none, d0, d1, d2, d3, d4
#' @export
drought_county <- function(state, start = "1/1/2024", end = "12/31/2024") {
  fips <- .to_fips(state)
  url <- sprintf(
    "%s/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=%s&startdate=%s&enddate=%s&statisticsType=2",
    .drought_base, fips, start, end
  )
  tmp <- .fetch(url, ext = ".csv")
  df <- .parse_drought_csv(tmp)
  if (is.null(df) || nrow(df) == 0) return(.schema_drought_county)

  nms <- names(df)
  county_col <- if ("County" %in% nms) "County" else if ("CountyName" %in% nms) "CountyName" else if ("FIPS" %in% nms) "FIPS" else nms[2]

  as_tibble(df) |>
    transmute(
      date = as.Date(as.character(MapDate), format = "%Y%m%d"),
      county = as.character(.data[[county_col]]),
      state = as.character(if ("StateAbbreviation" %in% nms) StateAbbreviation else state),
      none = as.numeric(None),
      d0 = as.numeric(D0),
      d1 = as.numeric(D1),
      d2 = as.numeric(D2),
      d3 = as.numeric(D3),
      d4 = as.numeric(D4)
    )
}

#' Print US Drought Monitor context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
drought_context <- function() {
  .build_context(
    pkg_name = "droughtmonitor.unl.edu",
    header_lines = c(
      "# Package: droughtmonitor.unl.edu",
      "# US Drought Monitor - weekly drought severity statistics",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# Drought categories: None, D0 (Abnormally Dry), D1 (Moderate),",
      "#   D2 (Severe), D3 (Extreme), D4 (Exceptional)",
      "# Values are percent of area in each category",
      "# State param accepts abbreviation (CA) or FIPS code (06)",
      "# Date format: M/D/YYYY (e.g., '1/1/2024')"
    )
  )
}
