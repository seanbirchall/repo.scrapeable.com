# vpic.nhtsa.dot.gov.R - Self-contained vpic.nhtsa.dot.gov client



# vpic-nhtsa-dot-gov.R
# Self-contained NHTSA vPIC (Vehicle Product Information Catalog) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.vin_base <- "https://vpic.nhtsa.dot.gov/api/vehicles"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_decode <- tibble(
  variable = character(), value = character(), variable_id = integer(),
  value_id = character()
)

.schema_makes <- tibble(
  make_id = integer(), make_name = character()
)

.schema_models <- tibble(
  make_id = integer(), make_name = character(),
  model_id = integer(), model_name = character()
)


# == Public functions ==========================================================

#' Decode a VIN (Vehicle Identification Number)
#'
#' Uses the NHTSA vPIC API to decode a Vehicle Identification Number into
#' its component fields including make, model, year, engine, body type,
#' plant information, and safety features.
#'
#' @param vin Character. 17-character Vehicle Identification Number.
#' @param year Integer or NULL. Optional model year to improve decoding
#'   accuracy for VINs with ambiguous year codes.
#' @return A tibble with columns:
#'   \describe{
#'     \item{variable}{Character. Decoded field name (e.g. "Make", "Model").}
#'     \item{value}{Character. Decoded value. Rows with empty values are removed.}
#'     \item{variable_id}{Integer. NHTSA variable identifier.}
#'     \item{value_id}{Character. NHTSA value identifier.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' vin_decode("1HGBH41JXMN109186")
#' vin_decode("5YJSA1E26MF123456", year = 2021)
#' }
vin_decode <- function(vin, year = NULL) {
  url <- sprintf("%s/DecodeVin/%s?format=json", .vin_base, as.character(vin))
  if (!is.null(year)) url <- sprintf("%s/DecodeVin/%s?format=json&modelyear=%s",
                                     .vin_base, vin, as.integer(year))
  raw <- .fetch_json(url)
  results <- raw$Results
  if (is.null(results) || nrow(results) == 0) return(.schema_decode)

  as_tibble(data.frame(
    variable    = as.character(results$Variable),
    value       = as.character(results$Value),
    variable_id = as.integer(results$VariableId),
    value_id    = as.character(results$ValueId),
    stringsAsFactors = FALSE
  )) |>
    filter(!is.na(value) & value != "")
}

#' Get all vehicle makes
#'
#' Returns the complete list of vehicle manufacturers registered in the
#' NHTSA vPIC database. Includes both current and historical makes.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{make_id}{Integer. NHTSA make identifier.}
#'     \item{make_name}{Character. Manufacturer/make name.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' makes <- vin_makes()
#' makes[grep("Toyota", makes$make_name, ignore.case = TRUE), ]
#' }
vin_makes <- function() {
  url <- sprintf("%s/GetAllMakes?format=json", .vin_base)
  raw <- .fetch_json(url)
  results <- raw$Results
  if (is.null(results) || nrow(results) == 0) return(.schema_makes)

  as_tibble(data.frame(
    make_id   = as.integer(results$Make_ID),
    make_name = as.character(results$Make_Name),
    stringsAsFactors = FALSE
  ))
}

#' Get models for a make and year
#'
#' Returns all vehicle models for a given manufacturer and model year
#' from the NHTSA vPIC database.
#'
#' @param make Character. Make name (e.g. \code{"toyota"}, \code{"ford"},
#'   \code{"honda"}). Case-insensitive.
#' @param year Integer. Model year (e.g. \code{2024}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{make_id}{Integer. NHTSA make identifier.}
#'     \item{make_name}{Character. Make name.}
#'     \item{model_id}{Integer. NHTSA model identifier.}
#'     \item{model_name}{Character. Model name.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' vin_models("toyota", 2024)
#' vin_models("ford", 2023)
#' }
vin_models <- function(make, year) {
  url <- sprintf("%s/GetModelsForMakeYear/make/%s/modelyear/%s?format=json",
                 .vin_base,
                 utils::URLencode(as.character(make), reserved = TRUE),
                 as.integer(year))
  raw <- .fetch_json(url)
  results <- raw$Results
  if (is.null(results) || nrow(results) == 0) return(.schema_models)

  as_tibble(data.frame(
    make_id    = as.integer(results$Make_ID),
    make_name  = as.character(results$Make_Name),
    model_id   = as.integer(results$Model_ID),
    model_name = as.character(results$Model_Name),
    stringsAsFactors = FALSE
  ))
}

# == Context ===================================================================

#' Get nhtsa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
vin_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(vin_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nhtsa.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nhtsa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nhtsa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nhtsa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
