# openfoodfacts-org.R
# Self-contained Open Food Facts API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented (be respectful)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.off_base <- "https://world.openfoodfacts.org"

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

# == Schemas ===================================================================

.schema_products <- tibble(
  code = character(), product_name = character(), brands = character(),
  categories = character(), nutriscore_grade = character(),
  energy_kcal = numeric(), fat = numeric(), carbohydrates = numeric(),
  proteins = numeric(), image_url = character()
)

.schema_product <- tibble(
  code = character(), product_name = character(), brands = character(),
  categories = character(), ingredients_text = character(),
  nutriscore_grade = character(), energy_kcal = numeric(),
  fat = numeric(), carbohydrates = numeric(), proteins = numeric(),
  image_url = character()
)

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
