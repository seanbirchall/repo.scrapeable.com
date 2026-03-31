# cdc-gov-places.R
# Self-contained CDC PLACES API client (Socrata SODA).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: Socrata default (throttled without app token)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.places_base <- "https://data.cdc.gov/resource/swc5-untb.json"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_places <- tibble(
  year = character(), stateabbr = character(), statedesc = character(),
  locationname = character(), category = character(), measure = character(),
  data_value = numeric(), data_value_unit = character(),
  low_confidence_limit = numeric(), high_confidence_limit = numeric(),
  totalpopulation = integer(), measureid = character()
)

# == Public functions ==========================================================

#' Query CDC PLACES data using Socrata SODA syntax
#'
#' @param where SoQL WHERE clause (e.g. "stateabbr='CA'")
#' @param select SoQL SELECT clause (default: all columns)
#' @param limit Max rows to return (default 100, max 50000)
#' @param offset Starting offset for pagination (default 0)
#' @param order SoQL ORDER BY clause (default NULL)
#' @return tibble: year, stateabbr, statedesc, locationname, category, measure,
#'   data_value, data_value_unit, low_confidence_limit, high_confidence_limit,
#'   totalpopulation, measureid
#' @export
places_query <- function(where = NULL, select = NULL, limit = 100,
                         offset = 0, order = NULL) {
  params <- list()
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  params[["$limit"]]  <- as.character(limit)
  params[["$offset"]] <- as.character(offset)

  query <- paste(names(params), utils::URLencode(params, reserved = TRUE),
                 sep = "=", collapse = "&")
  url <- paste0(.places_base, "?", query)

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_places)

  as_tibble(raw) |>
    mutate(
      data_value            = as.numeric(data_value),
      low_confidence_limit  = as.numeric(low_confidence_limit),
      high_confidence_limit = as.numeric(high_confidence_limit),
      totalpopulation       = as.integer(totalpopulation)
    ) |>
    select(any_of(c("year", "stateabbr", "statedesc", "locationname",
                     "category", "measure", "data_value", "data_value_unit",
                     "low_confidence_limit", "high_confidence_limit",
                     "totalpopulation", "measureid")))
}

#' Get CDC PLACES data by state for a specific measure
#'
#' @param measure Measure ID (e.g. "ARTHRITIS", "DIABETES", "OBESITY",
#'   "BPHIGH", "CASTHMA", "CHD", "COPD", "DEPRESSION")
#' @param state Optional state abbreviation filter (e.g. "CA", "NY")
#' @param limit Max rows (default 100)
#' @return tibble: same columns as places_query()
#' @export
places_states <- function(measure, state = NULL, limit = 100) {
  where <- sprintf("measureid='%s'", measure)
  if (!is.null(state)) where <- paste0(where, sprintf(" AND stateabbr='%s'", state))
  places_query(where = where, limit = limit)
}

#' Show CDC PLACES package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
places_context <- function() {
  .build_context(
    pkg_name = "cdc.places.gov",
    header_lines = c(
      "# cdc.places.gov",
      "# CDC PLACES API Client (county-level health data)",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Rate limits: Socrata default throttling",
      "#",
      "# Common measure IDs: ARTHRITIS, BPHIGH, CANCER, CASTHMA, CHD,",
      "#   COPD, CSMOKING, DEPRESSION, DIABETES, HIGHCHOL, KIDNEY,",
      "#   OBESITY, STROKE, TEETHLOST"
    )
  )
}
