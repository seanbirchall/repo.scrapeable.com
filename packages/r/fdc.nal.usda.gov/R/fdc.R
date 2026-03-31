# == Public functions ==========================================================

#' Search FoodData Central foods
#'
#' @param query Search term (e.g. "apple", "cheddar cheese")
#' @param page_size Results per page (default 25, max 200)
#' @param page_number Page number (default 1)
#' @param api_key API key (default DEMO_KEY). Register at
#'   https://fdc.nal.usda.gov/api-key-signup.html for higher limits.
#' @return tibble: fdc_id, description, data_type, brand_owner, score
#' @export
fdc_search <- function(query, page_size = 25, page_number = 1,
                       api_key = "DEMO_KEY") {
  url <- sprintf(
    "%s/foods/search?query=%s&pageSize=%d&pageNumber=%d&api_key=%s",
    .fdc_base, utils::URLencode(query, reserved = TRUE),
    as.integer(page_size), as.integer(page_number), api_key
  )
  raw <- .fetch_json(url)
  foods <- raw$foods
  if (is.null(foods) || length(foods) == 0 ||
      (is.data.frame(foods) && nrow(foods) == 0)) return(.schema_search)

  tibble(
    fdc_id = as.integer(foods$fdcId),
    description = as.character(foods$description),
    data_type = as.character(foods$dataType %||% NA_character_),
    brand_owner = as.character(foods$brandOwner %||% NA_character_),
    score = as.numeric(foods$score %||% NA_real_)
  )
}

#' Get detailed food info with nutrients
#'
#' @param fdc_id FoodData Central food ID (integer)
#' @param api_key API key (default DEMO_KEY)
#' @return tibble: fdc_id, description, data_type, nutrient_name, amount, unit
#'   (one row per nutrient)
#' @export
fdc_food <- function(fdc_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/food/%s?api_key=%s", .fdc_base, as.character(fdc_id), api_key)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_food)

  nutrients <- raw$foodNutrients
  if (is.null(nutrients) || length(nutrients) == 0) return(.schema_food)

  tibble(
    fdc_id = as.integer(raw$fdcId),
    description = as.character(raw$description),
    data_type = as.character(raw$dataType %||% NA_character_),
    nutrient_name = as.character(
      if (!is.null(nutrients$nutrient)) nutrients$nutrient$name
      else nutrients$nutrientName %||% NA_character_
    ),
    amount = as.numeric(nutrients$amount %||% NA_real_),
    unit = as.character(
      if (!is.null(nutrients$nutrient)) nutrients$nutrient$unitName
      else nutrients$unitName %||% NA_character_
    )
  )
}

#' Show FDC client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
fdc_context <- function() {
  .build_context(
    pkg_name = "fdc.nal.usda.gov",
    header_lines = c(
      "# fdc.nal.usda.gov -- USDA FoodData Central Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: api_key param (DEMO_KEY works, register for higher limits)",
      "# Register: https://fdc.nal.usda.gov/api-key-signup.html",
      "# Data types: Foundation, SR Legacy, Survey (FNDDS), Branded",
      "# Example queries: apple, chicken breast, cheddar cheese, brown rice"
    )
  )
}

