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


#' Search Open Food Facts products by keyword
#'
#' Searches the global Open Food Facts database of 3M+ food products
#' by name, brand, or ingredient. Returns basic product info and
#' Nutri-Score grade.
#'
#' @param query Character search term (e.g., \code{"nutella"},
#'   \code{"organic cereal"}).
#' @param page Integer page number for pagination (default 1).
#' @param page_size Integer results per page (default 25).
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Product barcode (EAN/UPC)}
#'     \item{product_name}{Product name}
#'     \item{brands}{Brand name(s)}
#'     \item{categories}{Product categories}
#'     \item{nutriscore_grade}{Nutri-Score letter grade (a--e), or NA}
#'   }
#' @export
#' @family Open Food Facts functions
#' @seealso \code{\link{off_product}} for full details on a single product,
#'   \code{\link{off_category}} to browse by category
#' @examples
#' \dontrun{
#' off_search("nutella")
#' off_search("organic yogurt", page_size = 10)
#' }
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
#' Fetches full product details including name, brand, categories,
#' Nutri-Score, and key macronutrient values per 100g.
#'
#' @param barcode Character product barcode (EAN/UPC, e.g.,
#'   \code{"3017620422003"} for Nutella).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{code}{Product barcode}
#'     \item{product_name}{Product name}
#'     \item{brands}{Brand name(s)}
#'     \item{categories}{Product categories}
#'     \item{nutriscore_grade}{Nutri-Score letter grade (a--e)}
#'     \item{energy_kcal}{Numeric energy per 100g in kcal}
#'     \item{fat_100g}{Numeric fat per 100g in grams}
#'     \item{carbs_100g}{Numeric carbohydrates per 100g in grams}
#'     \item{proteins_100g}{Numeric protein per 100g in grams}
#'   }
#' @export
#' @family Open Food Facts functions
#' @seealso \code{\link{off_nutrition}} for full nutrient breakdown,
#'   \code{\link{off_compare}} to compare multiple products
#' @examples
#' \dontrun{
#' off_product("3017620422003")
#' }
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
#' Returns all available nutrient values per 100g for a product,
#' including macros, vitamins, minerals, and more. The number of
#' nutrients varies by product completeness.
#'
#' @param barcode Character product barcode (e.g., \code{"3017620422003"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{nutrient}{Nutrient name (e.g., "fat", "sugars", "vitamin-c")}
#'     \item{value_100g}{Numeric value per 100g}
#'     \item{unit}{Unit of measurement (e.g., "g", "mg", "kJ")}
#'   }
#' @export
#' @family Open Food Facts functions
#' @seealso \code{\link{off_product}} for basic product info,
#'   \code{\link{off_compare}} to compare multiple products
#' @examples
#' \dontrun{
#' off_nutrition("3017620422003")
#' }
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
#' Returns products belonging to a specific food category with basic
#' nutrition and Nutri-Score data. Categories use hyphenated tag format.
#'
#' @param category Character category tag (e.g., \code{"pizzas"},
#'   \code{"chocolates"}, \code{"breakfast-cereals"}, \code{"plant-based-foods"}).
#' @param page Integer page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Product barcode}
#'     \item{product_name}{Product name}
#'     \item{brands}{Brand name(s)}
#'     \item{nutriscore_grade}{Nutri-Score letter grade (a--e)}
#'     \item{energy_kcal}{Numeric energy per 100g in kcal}
#'   }
#' @export
#' @family Open Food Facts functions
#' @seealso \code{\link{off_search}} for keyword search
#' @examples
#' \dontrun{
#' off_category("pizzas")
#' off_category("breakfast-cereals", page = 2)
#' }
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

#' Compare nutrition between multiple products side-by-side
#'
#' Fetches key nutritional data for multiple products and returns
#' them in a single tibble for easy comparison. Products that fail
#' to load are silently skipped.
#'
#' @param barcodes Character vector of product barcodes
#'   (e.g., \code{c("3017620422003", "5449000000996")}).
#' @return A tibble with one row per product and columns:
#'   \describe{
#'     \item{code}{Product barcode}
#'     \item{product_name}{Product name}
#'     \item{energy_kcal}{Energy per 100g in kcal}
#'     \item{fat_100g}{Fat per 100g in grams}
#'     \item{carbs_100g}{Carbohydrates per 100g in grams}
#'     \item{proteins_100g}{Protein per 100g in grams}
#'     \item{fiber_100g}{Fiber per 100g in grams}
#'     \item{salt_100g}{Salt per 100g in grams}
#'     \item{nutriscore_grade}{Nutri-Score letter grade (a--e)}
#'   }
#' @export
#' @family Open Food Facts functions
#' @seealso \code{\link{off_product}} for a single product,
#'   \code{\link{off_nutrition}} for full nutrient detail
#' @examples
#' \dontrun{
#' off_compare(c("3017620422003", "5449000000996"))
#' }
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

#' Get world.openfoodfacts.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/world.openfoodfacts.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "world.openfoodfacts.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# world.openfoodfacts.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# world.openfoodfacts.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
