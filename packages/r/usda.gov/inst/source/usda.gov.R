# fdc.nal.usda.gov.R - Self-contained fdc.nal.usda.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# fdc.R
# Self-contained USDA FoodData Central API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key (DEMO_KEY works for testing)
# Rate limits: 30 req/hour (DEMO_KEY), 1000/hour (registered)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fdc_base <- "https://api.nal.usda.gov/fdc/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_search <- tibble(
  fdc_id = integer(), description = character(), data_type = character(),
  brand_owner = character(), score = numeric()
)

.schema_food <- tibble(
  fdc_id = integer(), description = character(), data_type = character(),
  nutrient_name = character(), amount = numeric(), unit = character()
)


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search FoodData Central foods
#'
#' Searches the USDA FoodData Central database for foods matching a
#' text query. Returns matches from all data types: Foundation,
#' SR Legacy, Survey (FNDDS), and Branded. Results are ranked by
#' relevance score.
#'
#' @param query Character. Search term (e.g. "apple", "cheddar cheese",
#'   "chicken breast").
#' @param page_size Integer. Results per page (default 25, max 200).
#' @param page_number Integer. Page number for pagination (default 1).
#' @param api_key Character. API key (default "DEMO_KEY" -- limited to 30
#'   requests/hour). Register at
#'   \url{https://fdc.nal.usda.gov/api-key-signup.html} for higher limits.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fdc_id}{Integer. FoodData Central unique food identifier.
#'       Pass to [fdc_food()] for full nutrient data.}
#'     \item{description}{Character. Food description.}
#'     \item{data_type}{Character. Data source: "Foundation", "SR Legacy",
#'       "Survey (FNDDS)", or "Branded".}
#'     \item{brand_owner}{Character. Brand name (Branded items only, NA otherwise).}
#'     \item{score}{Numeric. Relevance score (higher = better match).}
#'   }
#'
#' @examples
#' \dontrun{
#' fdc_search("apple")
#' fdc_search("cheddar cheese", page_size = 10)
#' }
#'
#' @seealso [fdc_food()], [fdc_list()], [fdc_compare()]
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
#' Fetches the complete nutrient profile for a single food item from
#' FoodData Central. Returns one row per nutrient, including energy,
#' macronutrients, vitamins, and minerals.
#'
#' @param fdc_id Integer. FoodData Central food ID (obtainable from
#'   [fdc_search()] or [fdc_list()]).
#' @param api_key Character. API key (default "DEMO_KEY").
#'
#' @return A tibble with one row per nutrient and columns:
#'   \describe{
#'     \item{fdc_id}{Integer. Food identifier.}
#'     \item{description}{Character. Food description.}
#'     \item{data_type}{Character. Data source type.}
#'     \item{nutrient_name}{Character. Nutrient name (e.g. "Protein",
#'       "Energy", "Vitamin C, total ascorbic acid").}
#'     \item{amount}{Numeric. Nutrient amount per 100g serving.}
#'     \item{unit}{Character. Unit of measure (e.g. "G", "MG", "KCAL").}
#'   }
#'
#' @examples
#' \dontrun{
#' fdc_food(454004)
#' }
#'
#' @seealso [fdc_search()], [fdc_compare()]
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

#' Compare nutrient profiles across multiple foods
#'
#' Fetches nutrient data for multiple foods and combines them into a
#' single tibble, filtered to a set of key nutrients. Useful for
#' side-by-side nutritional comparison.
#'
#' @param fdc_ids Integer vector. FoodData Central IDs to compare.
#' @param nutrients Character vector. Nutrient names to include in the
#'   comparison. Default: Energy, Protein, Total lipid (fat),
#'   Carbohydrate (by difference), and Fiber (total dietary).
#'   Names must match FDC nutrient names exactly.
#' @param api_key Character. API key (default "DEMO_KEY").
#'
#' @return A tibble with columns: fdc_id, description, nutrient_name,
#'   amount, unit. One row per food-nutrient combination.
#'
#' @examples
#' \dontrun{
#' fdc_compare(c(454004, 2709215))
#' }
#'
#' @seealso [fdc_food()], [fdc_search()]
#' @export
fdc_compare <- function(fdc_ids, nutrients = c("Energy", "Protein", "Total lipid (fat)",
                                                "Carbohydrate, by difference", "Fiber, total dietary"),
                        api_key = "DEMO_KEY") {
  rows <- lapply(fdc_ids, function(id) {
    food <- tryCatch(fdc_food(id, api_key = api_key), error = function(e) NULL)
    if (is.null(food) || nrow(food) == 0) return(NULL)
    food |> filter(nutrient_name %in% nutrients)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(.schema_food)
  bind_rows(rows)
}

#' List foods by data type
#'
#' Retrieves a paginated list of foods from a specific FoodData Central
#' data type. Useful for browsing the database without a search query.
#'
#' @param data_type Character. One of "Foundation", "SR Legacy",
#'   "Survey (FNDDS)", or "Branded" (default "Foundation").
#' @param page_size Integer. Results per page (default 25, max 200).
#' @param api_key Character. API key (default "DEMO_KEY").
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fdc_id}{Integer. Food identifier.}
#'     \item{description}{Character. Food description.}
#'     \item{data_type}{Character. Data source type.}
#'     \item{brand_owner}{Character. Brand name (Branded items only).}
#'   }
#'
#' @examples
#' \dontrun{
#' fdc_list()
#' fdc_list("Branded", page_size = 50)
#' }
#'
#' @seealso [fdc_search()], [fdc_food()]
#' @export
fdc_list <- function(data_type = "Foundation", page_size = 25,
                     api_key = "DEMO_KEY") {
  url <- sprintf(
    "%s/foods/list?dataType=%s&pageSize=%d&api_key=%s",
    .fdc_base, utils::URLencode(data_type, reserved = TRUE),
    as.integer(page_size), api_key
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_search)

  if (is.data.frame(raw)) {
    tibble(
      fdc_id = as.integer(raw$fdcId),
      description = as.character(raw$description),
      data_type = as.character(raw$dataType %||% NA_character_),
      brand_owner = as.character(raw$brandOwner %||% NA_character_)
    )
  } else {
    tibble(
      fdc_id = vapply(raw, function(x) as.integer(x$fdcId %||% NA_integer_), integer(1)),
      description = vapply(raw, function(x) x$description %||% NA_character_, character(1)),
      data_type = vapply(raw, function(x) x$dataType %||% NA_character_, character(1)),
      brand_owner = vapply(raw, function(x) x$brandOwner %||% NA_character_, character(1))
    )
  }
}

# == Context ===================================================================

#' Get usda.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fdc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fdc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/usda.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "usda.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# usda.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# usda.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
