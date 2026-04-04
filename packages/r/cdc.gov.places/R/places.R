#' Query CDC PLACES data using Socrata SODA syntax
#'
#' @param where SoQL WHERE clause (e.g. "stateabbr='CA'")
#' @param select SoQL SELECT clause (default: all columns)
#' @param limit Max rows to return (default 100, max 50000)
#' @param offset Starting offset for pagination (default 0)
#' @param order SoQL ORDER BY clause (default NULL)
#' @return tibble: year, stateabbr, statedesc, locationname, category, measure,
#'   data_value, data_value_unit, low_confidence_limit, high_confidence_limit,
#'   totalpopulation, measureid
#' @export
places_query <- function(where = NULL, select = NULL, limit = 100,
                         offset = 0, order = NULL) {
  params <- list()
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  params[["$limit"]]  <- as.character(limit)
  params[["$offset"]] <- as.character(offset)

  query <- paste(names(params), utils::URLencode(params, reserved = TRUE),
                 sep = "=", collapse = "&")
  url <- paste0(.places_base, "?", query)

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_places)

  as_tibble(raw) |>
    mutate(
      data_value            = as.numeric(data_value),
      low_confidence_limit  = as.numeric(low_confidence_limit),
      high_confidence_limit = as.numeric(high_confidence_limit),
      totalpopulation       = as.integer(totalpopulation)
    ) |>
    select(any_of(c("year", "stateabbr", "statedesc", "locationname",
                     "category", "measure", "data_value", "data_value_unit",
                     "low_confidence_limit", "high_confidence_limit",
                     "totalpopulation", "measureid")))
}

#' Get CDC PLACES data by state for a specific measure
#'
#' @param measure Measure ID (e.g. "ARTHRITIS", "DIABETES", "OBESITY",
#'   "BPHIGH", "CASTHMA", "CHD", "COPD", "DEPRESSION")
#' @param state Optional state abbreviation filter (e.g. "CA", "NY")
#' @param limit Max rows (default 100)
#' @return tibble: same columns as places_query()
#' @export
places_states <- function(measure, state = NULL, limit = 100) {
  where <- sprintf("measureid='%s'", measure)
  if (!is.null(state)) where <- paste0(where, sprintf(" AND stateabbr='%s'", state))
  places_query(where = where, limit = limit)
}

#' Show CDC PLACES package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
places_context <- function() {
  .build_context(
    pkg_name = "cdc.places.gov",
    header_lines = c(
      "# cdc.places.gov",
      "# CDC PLACES API Client (county-level health data)",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Rate limits: Socrata default throttling",
      "#",
      "# Common measure IDs: ARTHRITIS, BPHIGH, CANCER, CASTHMA, CHD,",
      "#   COPD, CSMOKING, DEPRESSION, DIABETES, HIGHCHOL, KIDNEY,",
      "#   OBESITY, STROKE, TEETHLOST"
    )
  )
}
