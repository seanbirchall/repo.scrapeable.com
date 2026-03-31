# == Public functions ==========================================================

#' Search meals by name
#'
#' @param name Meal name or partial name (e.g. "chicken", "pasta")
#' @return tibble: id, name, category, area, instructions, thumbnail, tags, youtube
#' @export
meal_search <- function(name) {
  url <- sprintf("%s/search.php?s=%s", .meal_base, utils::URLencode(name, reserved = TRUE))
  raw <- .fetch_json(url)
  .parse_meals(raw$meals)
}

#' List all meal categories
#'
#' @return tibble: id, name, description, thumbnail
#' @export
meal_categories <- function() {
  url <- sprintf("%s/categories.php", .meal_base)
  raw <- .fetch_json(url)
  cats <- raw$categories
  if (is.null(cats)) return(.schema_categories)
  as_tibble(data.frame(
    id          = as.integer(cats$idCategory),
    name        = as.character(cats$strCategory),
    description = as.character(cats$strCategoryDescription),
    thumbnail   = as.character(cats$strCategoryThumb),
    stringsAsFactors = FALSE
  ))
}

#' Get a random meal
#'
#' @return tibble with one row: id, name, category, area, instructions, thumbnail, tags, youtube
#' @export
meal_random <- function() {
  url <- sprintf("%s/random.php", .meal_base)
  raw <- .fetch_json(url)
  .parse_meals(raw$meals)
}

#' Filter meals by category
#'
#' @param category Category name (e.g. "Seafood", "Chicken", "Dessert")
#' @return tibble: id, name, thumbnail
#' @export
meal_by_category <- function(category) {
  url <- sprintf("%s/filter.php?c=%s", .meal_base, utils::URLencode(category, reserved = TRUE))
  raw <- .fetch_json(url)
  meals <- raw$meals
  if (is.null(meals)) return(.schema_filter)
  as_tibble(data.frame(
    id        = as.integer(meals$idMeal),
    name      = as.character(meals$strMeal),
    thumbnail = as.character(meals$strMealThumb),
    stringsAsFactors = FALSE
  ))
}

#' Show TheMealDB package context for LLM use
#'
#' @return Invisible string with full context
#' @export
meal_context <- function() {
  .build_context("themealdb.com", header_lines = c(
    "# themealdb.com -- Free meal recipe database",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none (free API key '1')",
    "# Rate limits: none documented",
    "#",
    "# Categories: Beef, Chicken, Dessert, Lamb, Miscellaneous, Pasta,",
    "#   Pork, Seafood, Side, Starter, Vegan, Vegetarian, Breakfast, Goat"
  ))
}
