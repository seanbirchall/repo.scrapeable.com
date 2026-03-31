# == Public functions ==========================================================

#' Decode a VIN (Vehicle Identification Number)
#'
#' @param vin 17-character Vehicle Identification Number
#' @param year Optional model year to improve accuracy
#' @return tibble: variable, value, variable_id, value_id
#' @export
vin_decode <- function(vin, year = NULL) {
  url <- sprintf("%s/DecodeVin/%s?format=json", .vin_base, as.character(vin))
  if (!is.null(year)) url <- sprintf("%s/DecodeVin/%s?format=json&modelyear=%s",
                                     .vin_base, vin, as.integer(year))
  raw <- .fetch_json(url)
  results <- raw$Results
  if (is.null(results) || nrow(results) == 0) return(.schema_decode)

  as_tibble(data.frame(
    variable    = as.character(results$Variable),
    value       = as.character(results$Value),
    variable_id = as.integer(results$VariableId),
    value_id    = as.character(results$ValueId),
    stringsAsFactors = FALSE
  )) |>
    filter(!is.na(value) & value != "")
}

#' Get all vehicle makes
#'
#' @return tibble: make_id, make_name
#' @export
vin_makes <- function() {
  url <- sprintf("%s/GetAllMakes?format=json", .vin_base)
  raw <- .fetch_json(url)
  results <- raw$Results
  if (is.null(results) || nrow(results) == 0) return(.schema_makes)

  as_tibble(data.frame(
    make_id   = as.integer(results$Make_ID),
    make_name = as.character(results$Make_Name),
    stringsAsFactors = FALSE
  ))
}

#' Get models for a make and year
#'
#' @param make Make name (e.g. "toyota", "ford", "honda")
#' @param year Model year (e.g. 2024)
#' @return tibble: make_id, make_name, model_id, model_name
#' @export
vin_models <- function(make, year) {
  url <- sprintf("%s/GetModelsForMakeYear/make/%s/modelyear/%s?format=json",
                 .vin_base,
                 utils::URLencode(as.character(make), reserved = TRUE),
                 as.integer(year))
  raw <- .fetch_json(url)
  results <- raw$Results
  if (is.null(results) || nrow(results) == 0) return(.schema_models)

  as_tibble(data.frame(
    make_id    = as.integer(results$Make_ID),
    make_name  = as.character(results$Make_Name),
    model_id   = as.integer(results$Model_ID),
    model_name = as.character(results$Model_Name),
    stringsAsFactors = FALSE
  ))
}

#' Show NHTSA vPIC package context for LLM use
#'
#' @return Invisible string with full context
#' @export
vin_context <- function() {
  .build_context("vpic.nhtsa.dot.gov", header_lines = c(
    "# vpic.nhtsa.dot.gov -- NHTSA Vehicle Product Information Catalog",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Example VINs: 1HGCM82633A004352 (Honda Accord),",
    "#   5YJSA1DG9DFP14705 (Tesla Model S)",
    "# Popular makes: Toyota, Ford, Honda, Chevrolet, BMW, Tesla"
  ))
}
