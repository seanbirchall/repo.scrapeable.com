



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
#' @param query Search term
#' @param page Page number (default 1)
#' @param page_size Results per page (default 25)
#' @return tibble of matching products
#' @export
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
#' @param barcode Product barcode
#' @return tibble with one row of product data
#' @export
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

# == Context ===================================================================

#' Generate LLM-friendly context for world.openfoodfacts.org
#'
#' @return Character string with full function signatures and bodies
#' @export
off_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/world.openfoodfacts.org.R"
  if (!file.exists(src_file)) {
    cat("# world.openfoodfacts.org context - source not found\n")
    return(invisible("# world.openfoodfacts.org context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

