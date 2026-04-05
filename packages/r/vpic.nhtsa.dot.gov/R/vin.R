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
#' Queries the NHTSA vPIC API to decode a 17-character VIN and return
#' vehicle attributes such as make, model, body class, engine type,
#' and safety equipment. Filters out empty/NA values automatically.
#'
#' @param vin Character string. A 17-character Vehicle Identification Number
#'   (e.g., \code{"1HGBH41JXMN109186"}).
#' @param year Optional integer model year to improve decoding accuracy
#'   when the VIN alone is ambiguous (e.g., \code{2020}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{variable}{Decoded attribute name (e.g., "Make", "Model", "Body Class")}
#'     \item{value}{Decoded value for the attribute}
#'     \item{variable_id}{Integer ID of the variable in the vPIC system}
#'     \item{value_id}{Character ID of the value (may be empty)}
#'   }
#' @export
#' @family vPIC functions
#' @seealso \code{\link{vin_makes}} for listing all makes,
#'   \code{\link{vin_models}} for models by make and year
#' @examples
#' \dontrun{
#' # Decode a Honda VIN
#' vin_decode("1HGBH41JXMN109186")
#'
#' # Decode with model year hint
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

#' Get all vehicle makes registered with NHTSA
#'
#' Returns every vehicle manufacturer (make) in the vPIC database,
#' including cars, trucks, motorcycles, trailers, and buses.
#' Typically returns 10,000+ makes.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{make_id}{Integer unique identifier for the make}
#'     \item{make_name}{Character name of the make (e.g., "TOYOTA", "FORD")}
#'   }
#' @export
#' @family vPIC functions
#' @seealso \code{\link{vin_models}} to list models for a specific make and year
#' @examples
#' \dontrun{
#' makes <- vin_makes()
#' # Filter to find a specific make
#' makes[grepl("TESLA", makes$make_name), ]
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

#' Get vehicle models for a specific make and year
#'
#' Lists all models produced by a given manufacturer in a given model year.
#' The make name is case-insensitive.
#'
#' @param make Character string. Manufacturer name (e.g., \code{"toyota"},
#'   \code{"ford"}, \code{"honda"}). Case-insensitive.
#' @param year Integer or numeric model year (e.g., \code{2024}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{make_id}{Integer unique identifier for the make}
#'     \item{make_name}{Character name of the make}
#'     \item{model_id}{Integer unique identifier for the model}
#'     \item{model_name}{Character name of the model (e.g., "Corolla", "Camry")}
#'   }
#' @export
#' @family vPIC functions
#' @seealso \code{\link{vin_makes}} for listing all makes,
#'   \code{\link{vin_decode}} to decode a specific VIN
#' @examples
#' \dontrun{
#' vin_models("toyota", 2024)
#' vin_models("tesla", 2023)
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

#' Get vpic.nhtsa.dot.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/vpic.nhtsa.dot.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "vpic.nhtsa.dot.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# vpic.nhtsa.dot.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# vpic.nhtsa.dot.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
