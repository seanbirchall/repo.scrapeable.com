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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

#' Search USDA FoodData Central foods
#'
#' Search the USDA FoodData Central database for foods matching a free-text
#' query. Returns basic food descriptions with relevance scores. Use
#' \code{fdc_food()} to get full nutrient breakdowns for a specific food.
#'
#' @param query Search term (e.g. \code{"apple"}, \code{"cheddar cheese"},
#'   \code{"chicken breast"}).
#' @param page_size Number of results per page (default 25, max 200).
#' @param page_number Page number for pagination (default 1).
#' @param api_key FDC API key (default \code{"DEMO_KEY"}). Register at
#'   \url{https://fdc.nal.usda.gov/api-key-signup.html} for higher limits
#'   (1000 req/hour vs 30 req/hour).
#' @return A tibble with one row per food:
#'   \describe{
#'     \item{fdc_id}{\code{integer} -- FoodData Central food ID (pass to \code{fdc_food()}).}
#'     \item{description}{\code{character} -- Food description.}
#'     \item{data_type}{\code{character} -- Data source type (e.g. \code{"Branded"}, \code{"SR Legacy"}).}
#'     \item{brand_owner}{\code{character} -- Brand owner (branded foods only).}
#'     \item{score}{\code{numeric} -- Search relevance score.}
#'   }
#' @examples
#' \dontrun{
#' fdc_search("apple", page_size = 10)
#' fdc_search("cheddar cheese", page_size = 5, page_number = 2)
#' }
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

#' Get detailed food nutrient data
#'
#' Retrieve full nutrient information for a single food item from FoodData
#' Central. Returns one row per nutrient, with the nutrient name, amount, and
#' unit. Use \code{fdc_search()} first to find the \code{fdc_id}.
#'
#' @param fdc_id FoodData Central food ID (integer). Obtain from the
#'   \code{fdc_id} column of \code{fdc_search()} results.
#' @param api_key FDC API key (default \code{"DEMO_KEY"}).
#' @return A tibble with one row per nutrient:
#'   \describe{
#'     \item{fdc_id}{\code{integer} -- The food's FDC identifier.}
#'     \item{description}{\code{character} -- Food description.}
#'     \item{data_type}{\code{character} -- Data source type.}
#'     \item{nutrient_name}{\code{character} -- Nutrient name (e.g. \code{"Protein"}, \code{"Calcium, Ca"}).}
#'     \item{amount}{\code{numeric} -- Nutrient amount per serving.}
#'     \item{unit}{\code{character} -- Unit of measure (e.g. \code{"g"}, \code{"mg"}, \code{"kcal"}).}
#'   }
#' @examples
#' \dontrun{
#' # Search for a food then get its nutrients
#' foods <- fdc_search("banana")
#' fdc_food(foods$fdc_id[1])
#' }
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

#' Get FDC client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' fdc_context()
#' }
#' @export
fdc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fdc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fdc.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fdc")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fdc context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fdc", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
