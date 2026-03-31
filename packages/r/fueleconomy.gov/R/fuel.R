# == Public functions ==========================================================

#' Get available model years
#'
#' @return tibble: text (display year), value (year string)
#' @export
fuel_years <- function() {
  url <- sprintf("%s/vehicle/menu/year", .fuel_base)
  .parse_menu(.fetch_xml(url))
}

#' Get makes available for a given year
#'
#' @param year Model year (e.g. 2024)
#' @return tibble: text (make name), value (make name)
#' @export
fuel_makes <- function(year) {
  url <- sprintf("%s/vehicle/menu/make?year=%s", .fuel_base, as.integer(year))
  .parse_menu(.fetch_xml(url))
}

#' Get models available for a given year and make
#'
#' @param year Model year (e.g. 2024)
#' @param make Make name (e.g. "Toyota", "Ford")
#' @return tibble: text (model name), value (model id)
#' @export
fuel_models <- function(year, make) {
  url <- sprintf("%s/vehicle/menu/model?year=%s&make=%s",
                 .fuel_base, as.integer(year),
                 utils::URLencode(as.character(make), reserved = TRUE))
  .parse_menu(.fetch_xml(url))
}

#' Get vehicle details by ID
#'
#' @param id Vehicle ID from fuel_models()
#' @return tibble with one row: id, year, make, model, trany, drive, cylinders,
#'   displ, fuel_type, city_mpg, highway_mpg, comb_mpg, co2
#' @export
fuel_vehicle <- function(id) {
  url <- sprintf("%s/vehicle/%s", .fuel_base, as.integer(id))
  doc <- .fetch_xml(url)

  .xml_val <- function(node, tag) {
    v <- xml2::xml_text(xml2::xml_find_first(node, tag))
    if (is.na(v) || v == "") return(NA_character_)
    v
  }

  as_tibble(data.frame(
    id          = as.integer(.xml_val(doc, ".//id")),
    year        = as.integer(.xml_val(doc, ".//year")),
    make        = as.character(.xml_val(doc, ".//make")),
    model       = as.character(.xml_val(doc, ".//model")),
    trany       = as.character(.xml_val(doc, ".//trany")),
    drive       = as.character(.xml_val(doc, ".//drive")),
    cylinders   = as.integer(.xml_val(doc, ".//cylinders")),
    displ       = as.numeric(.xml_val(doc, ".//displ")),
    fuel_type   = as.character(.xml_val(doc, ".//fuelType")),
    city_mpg    = as.numeric(.xml_val(doc, ".//city08")),
    highway_mpg = as.numeric(.xml_val(doc, ".//highway08")),
    comb_mpg    = as.numeric(.xml_val(doc, ".//comb08")),
    co2         = as.numeric(.xml_val(doc, ".//co2TailpipeGpm")),
    stringsAsFactors = FALSE
  ))
}

#' Show FuelEconomy.gov package context for LLM use
#'
#' @return Invisible string with full context
#' @export
fuel_context <- function() {
  .build_context("fueleconomy.gov", header_lines = c(
    "# fueleconomy.gov -- EPA Fuel Economy data",
    "# Deps: httr2, dplyr, tibble, xml2",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Workflow: fuel_years() -> fuel_makes(year) -> fuel_models(year, make)",
    "#   -> fuel_vehicle(id) for details",
    "# Years available: 1984-2026"
  ))
}
