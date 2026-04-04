# world.openfoodfacts.org.R - Self-contained world.openfoodfacts.org client



.ua <- "support@scrapeable.com"
.base <- "https://world.openfoodfacts.org"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


#' Search Open Food Facts products
#'
#' Full-text search across the Open Food Facts database of food products.
#' Returns basic product info including brand, category, and Nutri-Score.
#'
#' @details
#' Open Food Facts is a free, open, collaborative database of food products
#' from around the world with over 3 million entries. The Nutri-Score grade
#' ranges from \code{"a"} (best) to \code{"e"} (worst) and summarises
#' nutritional quality. Not all products have a Nutri-Score assigned.
#'
#' @param query Character. Search term (e.g. \code{"nutella"}, \code{"organic yogurt"}).
#' @param page Integer. Page number for pagination (default 1).
#' @param page_size Integer. Results per page (default 25, max 100).
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{Character. Product barcode (EAN/UPC).}
#'   \item{product_name}{Character. Product name.}
#'   \item{brands}{Character. Brand name(s).}
#'   \item{categories}{Character. Product categories.}
#'   \item{nutriscore_grade}{Character. Nutri-Score grade (\code{"a"}--\code{"e"}).}
#' }
#' @export
#' @seealso \code{\link{off_product}}, \code{\link{off_category}},
#'   \code{\link{off_nutrition}}
#' @examples
#' off_search("chocolate", page_size = 5)
off_search <- function(query, page = 1L, page_size = 25L) {
  url <- sprintf("%s/cgi/search.pl?search_terms=%s&page=%d&page_size=%d&json=1",
                 .base, utils::URLencode(query, reserved = TRUE), page, page_size)
  raw <- .fetch_json(url)
  products <- raw$products
  if (length(products) == 0) return(tibble::tibble(code = character(), product_name = character()))
  tibble::tibble(
    code = vapply(products, function(x) x$code %||% NA_character_, character(1)),
    product_name = vapply(products, function(x) x$product_name %||% NA_character_, character(1)),
    brands = vapply(products, function(x) x$brands %||% NA_character_, character(1)),
    categories = vapply(products, function(x) x$categories %||% NA_character_, character(1)),
    nutriscore_grade = vapply(products, function(x) x$nutriscore_grade %||% NA_character_, character(1))
  )
}

#' Get a specific product by barcode
#'
#' Fetch detailed information for a single food product using its
#' barcode (EAN-13, UPC-A, or other formats).
#'
#' @details
#' Returns basic identification fields plus key macronutrient values
#' per 100 g. For a full nutrient breakdown, use
#' \code{\link{off_nutrition}}. Barcode examples: \code{"3017620422003"}
#' (Nutella), \code{"5000159484695"} (Heinz Ketchup).
#'
#' @param barcode Character. Product barcode.
#' @return A single-row tibble with columns:
#' \describe{
#'   \item{code}{Character. Product barcode.}
#'   \item{product_name}{Character. Product name.}
#'   \item{brands}{Character. Brand name(s).}
#'   \item{categories}{Character. Product categories.}
#'   \item{nutriscore_grade}{Character. Nutri-Score grade.}
#'   \item{energy_kcal}{Numeric. Energy per 100 g in kcal.}
#'   \item{fat_100g}{Numeric. Fat per 100 g in grams.}
#'   \item{carbs_100g}{Numeric. Carbohydrates per 100 g.}
#'   \item{proteins_100g}{Numeric. Protein per 100 g.}
#' }
#' @export
#' @seealso \code{\link{off_search}}, \code{\link{off_nutrition}},
#'   \code{\link{off_compare}}
#' @examples
#' off_product("3017620422003")
off_product <- function(barcode) {
  url <- sprintf("%s/api/v2/product/%s.json", .base, barcode)
  raw <- .fetch_json(url)
  p <- raw$product
  if (is.null(p)) return(tibble::tibble(code = character(), product_name = character()))
  tibble::tibble(
    code = p$code %||% NA_character_,
    product_name = p$product_name %||% NA_character_,
    brands = p$brands %||% NA_character_,
    categories = p$categories %||% NA_character_,
    nutriscore_grade = p$nutriscore_grade %||% NA_character_,
    energy_kcal = as.numeric(p$nutriments$`energy-kcal_100g` %||% NA),
    fat_100g = as.numeric(p$nutriments$fat_100g %||% NA),
    carbs_100g = as.numeric(p$nutriments$carbohydrates_100g %||% NA),
    proteins_100g = as.numeric(p$nutriments$proteins_100g %||% NA)
  )
}

#' Get detailed nutrition facts for a product
#'
#' Retrieve all available nutrient values per 100 g for a given product
#' barcode, returned in long (tidy) format.
#'
#' @details
#' The number of nutrients varies by product -- common entries include
#' energy, fat, saturated-fat, carbohydrates, sugars, fiber, proteins,
#' salt, and sodium. Units are typically \code{"g"}, \code{"mg"},
#' \code{"kcal"}, or \code{"kJ"}. Some products report dozens of
#' micronutrients (vitamins, minerals).
#'
#' @param barcode Character. Product barcode.
#' @return A tibble with columns:
#' \describe{
#'   \item{nutrient}{Character. Nutrient name (e.g. \code{"fat"}, \code{"sugars"}).}
#'   \item{value_100g}{Numeric. Amount per 100 g.}
#'   \item{unit}{Character. Unit of measure.}
#' }
#' @export
#' @seealso \code{\link{off_product}}, \code{\link{off_compare}}
#' @examples
#' off_nutrition("3017620422003")
off_nutrition <- function(barcode) {
  schema <- tibble(nutrient = character(), value_100g = numeric(), unit = character())
  url <- sprintf("%s/api/v2/product/%s.json?fields=nutriments", .base, barcode)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$product)) return(schema)

  n <- raw$product$nutriments
  if (is.null(n)) return(schema)

  # Extract all *_100g fields
  nms <- names(n)
  per100 <- nms[grepl("_100g$", nms)]
  if (length(per100) == 0) return(schema)

  tibble(
    nutrient = sub("_100g$", "", per100),
    value_100g = vapply(per100, function(k) suppressWarnings(as.numeric(n[[k]] %||% NA_real_)), numeric(1)),
    unit = vapply(per100, function(k) {
      unit_key <- sub("_100g$", "_unit", k)
      as.character(n[[unit_key]] %||% NA_character_)
    }, character(1))
  )
}

#' Browse products by category
#'
#' List food products belonging to a specific Open Food Facts category tag.
#'
#' @details
#' Category tags use lowercase, hyphenated English names. Common examples:
#' \code{"pizzas"}, \code{"chocolates"}, \code{"breakfast-cereals"},
#' \code{"yogurts"}, \code{"plant-based-beverages"}. Browse categories at
#' \url{https://world.openfoodfacts.org/categories}.
#'
#' @param category Character. Category tag (see Details).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{Character. Product barcode.}
#'   \item{product_name}{Character. Product name.}
#'   \item{brands}{Character. Brand name(s).}
#'   \item{nutriscore_grade}{Character. Nutri-Score grade.}
#'   \item{energy_kcal}{Numeric. Energy per 100 g in kcal.}
#' }
#' @export
#' @seealso \code{\link{off_search}}, \code{\link{off_product}}
#' @examples
#' off_category("pizzas", page = 1)
off_category <- function(category, page = 1L) {
  schema <- tibble(code = character(), product_name = character(),
                   brands = character(), nutriscore_grade = character(),
                   energy_kcal = numeric())
  url <- sprintf("%s/category/%s/%d.json", .base, utils::URLencode(category), as.integer(page))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)

  products <- raw$products
  if (is.null(products) || length(products) == 0) return(schema)

  tibble(
    code = vapply(products, function(x) x$code %||% NA_character_, character(1)),
    product_name = vapply(products, function(x) x$product_name %||% NA_character_, character(1)),
    brands = vapply(products, function(x) x$brands %||% NA_character_, character(1)),
    nutriscore_grade = vapply(products, function(x) x$nutriscore_grade %||% NA_character_, character(1)),
    energy_kcal = vapply(products, function(x) {
      suppressWarnings(as.numeric(x$nutriments$`energy-kcal_100g` %||% NA_real_))
    }, numeric(1))
  )
}

#' Compare nutrition between multiple products
#'
#' Fetch and align key macronutrient values for several products
#' side-by-side, making it easy to compare nutritional profiles.
#'
#' @details
#' Each barcode triggers a separate API call, so keep the list
#' reasonably short (10--20 products). Products that fail to load
#' are silently dropped. All nutrient columns are per 100 g.
#'
#' @param barcodes Character vector. One or more product barcodes.
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{Character. Product barcode.}
#'   \item{product_name}{Character. Product name.}
#'   \item{energy_kcal}{Numeric. Energy per 100 g (kcal).}
#'   \item{fat_100g}{Numeric. Fat per 100 g.}
#'   \item{carbs_100g}{Numeric. Carbohydrates per 100 g.}
#'   \item{proteins_100g}{Numeric. Protein per 100 g.}
#'   \item{fiber_100g}{Numeric. Fiber per 100 g.}
#'   \item{salt_100g}{Numeric. Salt per 100 g.}
#'   \item{nutriscore_grade}{Character. Nutri-Score grade.}
#' }
#' @export
#' @seealso \code{\link{off_product}}, \code{\link{off_nutrition}}
#' @examples
#' off_compare(c("3017620422003", "5000159484695"))
off_compare <- function(barcodes) {
  schema <- tibble(code = character(), product_name = character(),
                   energy_kcal = numeric(), fat_100g = numeric(),
                   carbs_100g = numeric(), proteins_100g = numeric(),
                   fiber_100g = numeric(), salt_100g = numeric(),
                   nutriscore_grade = character())

  rows <- lapply(barcodes, function(bc) {
    tryCatch({
      url <- sprintf("%s/api/v2/product/%s.json", .base, bc)
      raw <- .fetch_json(url)
      p <- raw$product
      if (is.null(p)) return(NULL)
      n <- p$nutriments %||% list()
      tibble(
        code = as.character(bc),
        product_name = as.character(p$product_name %||% NA_character_),
        energy_kcal = suppressWarnings(as.numeric(n$`energy-kcal_100g` %||% NA_real_)),
        fat_100g = suppressWarnings(as.numeric(n$fat_100g %||% NA_real_)),
        carbs_100g = suppressWarnings(as.numeric(n$carbohydrates_100g %||% NA_real_)),
        proteins_100g = suppressWarnings(as.numeric(n$proteins_100g %||% NA_real_)),
        fiber_100g = suppressWarnings(as.numeric(n$fiber_100g %||% NA_real_)),
        salt_100g = suppressWarnings(as.numeric(n$salt_100g %||% NA_real_)),
        nutriscore_grade = as.character(p$nutriscore_grade %||% NA_character_)
      )
    }, error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(schema)
  bind_rows(rows)
}

# == Context ===================================================================

#' Get openfoodfacts.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
off_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(off_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/openfoodfacts.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "openfoodfacts.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# openfoodfacts.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# openfoodfacts.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
