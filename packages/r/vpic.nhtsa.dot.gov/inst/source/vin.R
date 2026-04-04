


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

# == Context ===================================================================

#' Generate LLM-friendly context for vpic.nhtsa.dot.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
vin_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/vpic.nhtsa.dot.gov.R"
  if (!file.exists(src_file)) {
    cat("# vpic.nhtsa.dot.gov context - source not found\n")
    return(invisible("# vpic.nhtsa.dot.gov context - source not found"))
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

