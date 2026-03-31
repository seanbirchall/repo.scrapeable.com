# vpic-nhtsa-dot-gov.R
# Self-contained NHTSA vPIC (Vehicle Product Information Catalog) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.vin_base <- "https://vpic.nhtsa.dot.gov/api/vehicles"

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
#' @param vin 17-character Vehicle Identification Number
#' @param year Optional model year to improve accuracy
#' @return tibble: variable, value, variable_id, value_id
#' @export
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
#' @return tibble: make_id, make_name
#' @export
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
#' @param make Make name (e.g. "toyota", "ford", "honda")
#' @param year Model year (e.g. 2024)
#' @return tibble: make_id, make_name, model_id, model_name
#' @export
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

#' Show NHTSA vPIC package context for LLM use
#'
#' @return Invisible string with full context
#' @export
vin_context <- function() {
  .build_context("vpic.nhtsa.dot.gov", header_lines = c(
    "# vpic.nhtsa.dot.gov -- NHTSA Vehicle Product Information Catalog",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Example VINs: 1HGCM82633A004352 (Honda Accord),",
    "#   5YJSA1DG9DFP14705 (Tesla Model S)",
    "# Popular makes: Toyota, Ford, Honda, Chevrolet, BMW, Tesla"
  ))
}
