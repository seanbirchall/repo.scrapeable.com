


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

# == Context ===================================================================

#' Generate LLM-friendly context for fdc.nal.usda.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
fdc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/fdc.nal.usda.gov.R"
  if (!file.exists(src_file)) {
    cat("# fdc.nal.usda.gov context - source not found\n")
    return(invisible("# fdc.nal.usda.gov context - source not found"))
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

