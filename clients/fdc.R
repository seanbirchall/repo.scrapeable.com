# fdc.R
# Self-contained USDA FoodData Central API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key (DEMO_KEY works for testing)
# Rate limits: 30 req/hour (DEMO_KEY), 1000/hour (registered)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fdc_base <- "https://api.nal.usda.gov/fdc/v1"

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
  cat(out, "\n"); invisible(out)
}

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

`%||%` <- function(x, y) if (is.null(x)) y else x
