# themealdb-com.R
# Self-contained TheMealDB API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (free tier uses key "1")
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.meal_base <- "https://www.themealdb.com/api/json/v1/1"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Parse a meal record from the API -----------------------------------------
.parse_meals <- function(meals) {
  if (is.null(meals)) return(.schema_meals)
  as_tibble(data.frame(
    id          = as.integer(meals$idMeal),
    name        = as.character(meals$strMeal),
    category    = as.character(meals$strCategory %||% NA_character_),
    area        = as.character(meals$strArea %||% NA_character_),
    instructions = as.character(meals$strInstructions %||% NA_character_),
    thumbnail   = as.character(meals$strMealThumb %||% NA_character_),
    tags        = as.character(meals$strTags %||% NA_character_),
    youtube     = as.character(meals$strYoutube %||% NA_character_),
    stringsAsFactors = FALSE
  ))
}

# == Schemas ===================================================================

.schema_meals <- tibble(
  id = integer(), name = character(), category = character(),
  area = character(), instructions = character(), thumbnail = character(),
  tags = character(), youtube = character()
)

.schema_categories <- tibble(
  id = integer(), name = character(), description = character(),
  thumbnail = character()
)

.schema_filter <- tibble(
  id = integer(), name = character(), thumbnail = character()
)

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
