# cdc-gov-places.R
# Self-contained CDC PLACES API client (Socrata SODA).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: Socrata default (throttled without app token)


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Sends a SoQL (Socrata Query Language) request to the CDC PLACES dataset
#' (\code{swc5-untb}), which provides county- and state-level estimates for
#' chronic disease risk factors, health outcomes, and preventive services from
#' the CDC's population-level analysis. Supports full SODA filtering,
#' selection, ordering, and pagination.
#'
#' @param where Character or \code{NULL}. SoQL WHERE clause for filtering rows
#'   (e.g. \code{"stateabbr='CA'"}, \code{"measureid='OBESITY' AND year='2023'"}).
#' @param select Character or \code{NULL}. SoQL SELECT clause to restrict
#'   returned columns (default: all columns).
#' @param limit Integer. Maximum rows to return (default 100, Socrata max 50 000).
#' @param offset Integer. Starting offset for pagination (default 0). Combine
#'   with \code{limit} to page through large result sets.
#' @param order Character or \code{NULL}. SoQL ORDER BY clause
#'   (e.g. \code{"data_value DESC"}).
#' @return A tibble with up to \code{limit} rows and columns:
#' \describe{
#'   \item{year}{Character. Survey year.}
#'   \item{stateabbr}{Character. Two-letter state abbreviation.}
#'   \item{statedesc}{Character. Full state name.}
#'   \item{locationname}{Character. County or locality name.}
#'   \item{category}{Character. Health topic category (e.g. \code{"Health Outcomes"}).}
#'   \item{measure}{Character. Full measure description.}
#'   \item{data_value}{Numeric. Estimated prevalence or value.}
#'   \item{data_value_unit}{Character. Unit (typically \code{"\%"}).}
#'   \item{low_confidence_limit}{Numeric. Lower 95\% confidence bound.}
#'   \item{high_confidence_limit}{Numeric. Upper 95\% confidence bound.}
#'   \item{totalpopulation}{Integer. Total population of the location.}
#'   \item{measureid}{Character. Short measure identifier (e.g. \code{"OBESITY"}).}
#' }
#' @examples
#' \dontrun{
#' places_query(where = "stateabbr='CA' AND measureid='OBESITY'", limit = 10)
#' places_query(where = "measureid='DIABETES'", order = "data_value DESC", limit = 5)
#' }
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
#' Convenience wrapper around \code{\link{places_query}} that filters to a
#' single health measure and optionally to one state. Common measure IDs
#' include \code{"ARTHRITIS"}, \code{"BPHIGH"}, \code{"CASTHMA"},
#' \code{"CHD"}, \code{"COPD"}, \code{"DEPRESSION"}, \code{"DIABETES"},
#' and \code{"OBESITY"}.
#'
#' @param measure Character. Measure ID string (e.g. \code{"OBESITY"},
#'   \code{"DIABETES"}).
#' @param state Character or \code{NULL}. Optional two-letter state abbreviation
#'   to restrict results (e.g. \code{"CA"}, \code{"NY"}).
#' @param limit Integer. Maximum rows to return (default 100).
#' @return A tibble with the same columns as \code{\link{places_query}}.
#' @examples
#' \dontrun{
#' places_states("OBESITY")
#' places_states("DIABETES", state = "NY", limit = 20)
#' }
#' @export
places_states <- function(measure, state = NULL, limit = 100) {
  where <- sprintf("measureid='%s'", measure)
  if (!is.null(state)) where <- paste0(where, sprintf(" AND stateabbr='%s'", state))
  places_query(where = where, limit = limit)
}

#' Get cdc-gov-places client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
places_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(places_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cdc-gov-places.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cdc-gov-places")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cdc-gov-places context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cdc-gov-places", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
