# == Public functions ==========================================================

#' Search Open Food Facts products
#'
#' @param query Search query string
#' @param page_size Number of results (default 20, max 100)
#' @param page Page number (default 1)
#' @return tibble: code, product_name, brands, categories, nutriscore_grade,
#'   energy_kcal, fat, carbohydrates, proteins, image_url
#' @export
off_search <- function(query, page_size = 20, page = 1) {
  url <- sprintf(
    "%s/cgi/search.pl?search_terms=%s&page_size=%d&page=%d&search_simple=1&action=process&json=true",
    .off_base, utils::URLencode(query, reserved = TRUE),
    as.integer(page_size), as.integer(page)
  )
  raw <- .fetch_json(url)
  prods <- raw$products
  if (is.null(prods) || length(prods) == 0) return(.schema_products)

  nut <- prods$nutriments
  as_tibble(data.frame(
    code             = as.character(prods$code %||% NA_character_),
    product_name     = as.character(prods$product_name %||% NA_character_),
    brands           = as.character(prods$brands %||% NA_character_),
    categories       = as.character(prods$categories %||% NA_character_),
    nutriscore_grade = as.character(prods$nutriscore_grade %||% NA_character_),
    energy_kcal      = as.numeric(if (!is.null(nut[["energy-kcal_100g"]])) nut[["energy-kcal_100g"]] else NA_real_),
    fat              = as.numeric(if (!is.null(nut$fat_100g)) nut$fat_100g else NA_real_),
    carbohydrates    = as.numeric(if (!is.null(nut$carbohydrates_100g)) nut$carbohydrates_100g else NA_real_),
    proteins         = as.numeric(if (!is.null(nut$proteins_100g)) nut$proteins_100g else NA_real_),
    image_url        = as.character(prods$image_url %||% NA_character_),
    stringsAsFactors = FALSE
  ))
}

#' Get a specific product by barcode
#'
#' @param barcode Product barcode (EAN-13, UPC, etc.)
#' @return tibble with one row: code, product_name, brands, categories,
#'   ingredients_text, nutriscore_grade, energy_kcal, fat, carbohydrates,
#'   proteins, image_url
#' @export
off_product <- function(barcode) {
  url <- sprintf("%s/api/v2/product/%s.json", .off_base, as.character(barcode))
  raw <- .fetch_json(url)
  p <- raw$product
  if (is.null(p)) return(.schema_product)

  nut <- p$nutriments
  as_tibble(data.frame(
    code             = as.character(p$code %||% barcode),
    product_name     = as.character(p$product_name %||% NA_character_),
    brands           = as.character(p$brands %||% NA_character_),
    categories       = as.character(p$categories %||% NA_character_),
    ingredients_text = as.character(p$ingredients_text %||% NA_character_),
    nutriscore_grade = as.character(p$nutriscore_grade %||% NA_character_),
    energy_kcal      = as.numeric(if (!is.null(nut[["energy-kcal_100g"]])) nut[["energy-kcal_100g"]] else NA_real_),
    fat              = as.numeric(if (!is.null(nut$fat_100g)) nut$fat_100g else NA_real_),
    carbohydrates    = as.numeric(if (!is.null(nut$carbohydrates_100g)) nut$carbohydrates_100g else NA_real_),
    proteins         = as.numeric(if (!is.null(nut$proteins_100g)) nut$proteins_100g else NA_real_),
    image_url        = as.character(p$image_url %||% NA_character_),
    stringsAsFactors = FALSE
  ))
}

#' Show Open Food Facts package context for LLM use
#'
#' @return Invisible string with full context
#' @export
off_context <- function() {
  .build_context("openfoodfacts.org", header_lines = c(
    "# openfoodfacts.org -- Open Food Facts product database",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: be respectful, no hard limit",
    "#",
    "# Common barcodes: 3017620422003 (Nutella), 5449000000996 (Coca-Cola)",
    "# Search examples: 'chocolate', 'organic milk', 'pasta'"
  ))
}
