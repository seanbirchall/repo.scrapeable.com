# themealdb.com.R - Self-contained themealdb.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# themealdb-com.R
# Self-contained TheMealDB API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (free tier uses key "1")
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.meal_base <- "https://www.themealdb.com/api/json/v1/1"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
#' Searches TheMealDB recipe database by meal name or partial name.
#' Returns full recipe metadata including cooking instructions, category,
#' cuisine area, and media links. The free API tier (key "1") is used.
#'
#' @param name Character. Meal name or partial name to search for
#'   (e.g. \code{"chicken"}, \code{"pasta"}, \code{"arrabiata"}).
#'   Case-insensitive.
#' @return A tibble with 8 columns:
#' \describe{
#'   \item{id}{Integer. TheMealDB recipe identifier.}
#'   \item{name}{Character. Full meal name.}
#'   \item{category}{Character. Meal category (e.g. "Chicken", "Seafood", "Dessert").}
#'   \item{area}{Character. Cuisine origin (e.g. "Italian", "Indian", "American").}
#'   \item{instructions}{Character. Full cooking instructions text.}
#'   \item{thumbnail}{Character. URL to meal thumbnail image.}
#'   \item{tags}{Character. Comma-separated tags, or \code{NA}.}
#'   \item{youtube}{Character. YouTube video URL, or empty string.}
#' }
#' @examples
#' \dontrun{
#' meal_search("chicken")
#' meal_search("pasta")
#' }
#' @seealso \code{\link{meal_by_category}} to browse by category,
#'   \code{\link{meal_random}} for a random recipe.
#' @export
meal_search <- function(name) {
  url <- sprintf("%s/search.php?s=%s", .meal_base, utils::URLencode(name, reserved = TRUE))
  raw <- .fetch_json(url)
  .parse_meals(raw$meals)
}

#' List all meal categories
#'
#' Returns every meal category available in TheMealDB with descriptions
#' and thumbnail images. Use category names with \code{\link{meal_by_category}}
#' to browse recipes within a category.
#'
#' @return A tibble with 4 columns:
#' \describe{
#'   \item{id}{Integer. Category identifier.}
#'   \item{name}{Character. Category name (e.g. "Beef", "Chicken", "Dessert", "Seafood").}
#'   \item{description}{Character. Multi-sentence description of the category.}
#'   \item{thumbnail}{Character. URL to category thumbnail image.}
#' }
#' @examples
#' \dontrun{
#' meal_categories()
#' }
#' @seealso \code{\link{meal_by_category}} to list meals in a category.
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
#' Returns a single randomly selected recipe from TheMealDB. Each call
#' returns a different meal. Useful for recipe inspiration or testing.
#'
#' @return A single-row tibble with 8 columns: id, name, category, area,
#'   instructions, thumbnail, tags, youtube. See \code{\link{meal_search}}
#'   for column descriptions.
#' @examples
#' \dontrun{
#' meal_random()
#' }
#' @export
meal_random <- function() {
  url <- sprintf("%s/random.php", .meal_base)
  raw <- .fetch_json(url)
  .parse_meals(raw$meals)
}

#' Filter meals by category
#'
#' Returns all meals within a given category. Provides meal IDs, names,
#' and thumbnails but not full recipe details. Use a returned \code{id}
#' with \code{\link{meal_search}} (by name) or a direct lookup to get
#' full instructions.
#'
#' @param category Character. Category name exactly as returned by
#'   \code{\link{meal_categories}} (e.g. \code{"Seafood"}, \code{"Chicken"},
#'   \code{"Dessert"}, \code{"Vegetarian"}, \code{"Beef"}).
#' @return A tibble with 3 columns:
#' \describe{
#'   \item{id}{Integer. TheMealDB recipe identifier.}
#'   \item{name}{Character. Meal name.}
#'   \item{thumbnail}{Character. URL to meal thumbnail image.}
#' }
#' @examples
#' \dontrun{
#' meal_by_category("Seafood")
#' meal_by_category("Vegetarian")
#' }
#' @seealso \code{\link{meal_categories}} to list all available categories.
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

# == Context ===================================================================

#' Get themealdb.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
meal_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(meal_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/themealdb.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "themealdb.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# themealdb.com context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# themealdb.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
