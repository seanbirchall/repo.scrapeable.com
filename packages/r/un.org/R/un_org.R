# population.un.org.R - Self-contained population.un.org client



# population-un-org.R
# Self-contained UN World Population Prospects client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.unpop_base <- "https://population.un.org/dataportalapi/api/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_indicators <- tibble(
  id = integer(), name = character(), short_name = character()
)

.schema_locations <- tibble(
  id = integer(), name = character(), iso3 = character(),
  type = character()
)

.schema_data <- tibble(
  location_id = integer(), location = character(),
  indicator_id = integer(), indicator = character(),
  year = integer(), value = numeric(), sex = character(),
  variant = character()
)

# == Private helpers ===========================================================

.unpop_fetch_paginated <- function(url, max_pages = 50) {
  all_data <- list()
  page <- 1
  while (page <= max_pages) {
    sep <- if (grepl("\\?", url)) "&" else "?"
    paged_url <- sprintf("%s%spageNumber=%d&pageSize=100", url, sep, page)
    raw <- tryCatch(.fetch_json(paged_url), error = function(e) NULL)
    if (is.null(raw)) break
    dat <- if (is.data.frame(raw)) raw else raw$data
    if (is.null(dat) || (is.data.frame(dat) && nrow(dat) == 0)) break
    if (!is.data.frame(dat)) break
    all_data[[page]] <- dat
    # Check if there are more pages
    total <- raw$totalPages %||% raw$pages %||% page
    if (page >= total) break
    page <- page + 1
  }
  if (length(all_data) == 0) return(NULL)
  bind_rows(all_data)
}

# == Public functions ==========================================================


#' List available population indicators
#'
#' Returns a tibble of available demographic indicators from the UN
#' Population Division World Population Prospects Data Portal. Indicators
#' include total population, fertility rates, life expectancy, mortality
#' rates, migration, and many more. Use the \code{id} column to query
#' data via [unpop_data()].
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Numeric indicator identifier for use with [unpop_data()].}
#'     \item{name}{Character. Full indicator name (e.g. "Total Population, as of
#'       1 January (thousands)").}
#'     \item{short_name}{Character. Abbreviated indicator label.}
#'   }
#'
#' @examples
#' \dontrun{
#' unpop_indicators()
#' }
#'
#' @seealso [unpop_data()], [unpop_locations()]
#' @export
unpop_indicators <- function() {
  url <- paste0(.unpop_base, "/indicators")
  raw <- .unpop_fetch_paginated(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_indicators)

  nms <- names(raw)
  id_col <- if ("id" %in% nms) "id" else if ("indicatorId" %in% nms) "indicatorId" else nms[1]
  name_col <- if ("name" %in% nms) "name" else if ("indicatorName" %in% nms) "indicatorName" else nms[2]
  short_col <- if ("shortName" %in% nms) "shortName" else if ("short_name" %in% nms) "short_name" else name_col

  as_tibble(raw) |>
    transmute(
      id = as.integer(.data[[id_col]]),
      name = as.character(.data[[name_col]]),
      short_name = as.character(.data[[short_col]])
    )
}

#' List available locations (countries, regions)
#'
#' Returns a tibble of geographic locations available in the UN Population
#' Data Portal, including individual countries, world regions, development
#' groups, and income classifications. Use the \code{id} column to
#' query data via [unpop_data()].
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Numeric location identifier for use with [unpop_data()].
#'       Common values: 840 (United States), 156 (China), 356 (India).}
#'     \item{name}{Character. Location name (country, region, or group).}
#'     \item{iso3}{Character. ISO 3166-1 alpha-3 code (NA for non-country
#'       locations like regions or income groups).}
#'     \item{type}{Character. Location type (e.g. "Country/Area",
#'       "Region", "Income Group").}
#'   }
#'
#' @examples
#' \dontrun{
#' unpop_locations()
#' }
#'
#' @seealso [unpop_data()], [unpop_indicators()]
#' @export
unpop_locations <- function() {
  url <- paste0(.unpop_base, "/locations")
  raw <- .unpop_fetch_paginated(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_locations)

  nms <- names(raw)
  id_col <- if ("id" %in% nms) "id" else if ("locationId" %in% nms) "locationId" else nms[1]
  name_col <- if ("name" %in% nms) "name" else nms[2]
  iso3_col <- if ("iso3" %in% nms) "iso3" else if ("iSO3_Alpha3" %in% nms) "iSO3_Alpha3" else NA
  type_col <- if ("locationType" %in% nms) "locationType" else if ("type" %in% nms) "type" else NA

  as_tibble(raw) |>
    transmute(
      id = as.integer(.data[[id_col]]),
      name = as.character(.data[[name_col]]),
      iso3 = if (is.na(iso3_col)) NA_character_ else as.character(.data[[iso3_col]]),
      type = if (is.na(type_col)) NA_character_ else as.character(.data[[type_col]])
    )
}

#' Fetch population data for a specific indicator and location
#'
#' Retrieves time-series demographic data from the UN World Population
#' Prospects for a given indicator and location combination. Data is
#' paginated automatically. Results include both historical estimates
#' and future projections (medium variant by default).
#'
#' @param indicator_id Integer. Indicator ID from [unpop_indicators()]
#'   (e.g. 49 = total population by sex, 19 = life expectancy at birth).
#' @param location_id Integer. Location ID from [unpop_locations()]
#'   (e.g. 840 = United States, 156 = China, 356 = India).
#' @param start_year Integer. First year of the time series (default 2000).
#' @param end_year Integer. Last year of the time series (default 2025).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{location_id}{Integer. Numeric location identifier.}
#'     \item{location}{Character. Location name.}
#'     \item{indicator_id}{Integer. Numeric indicator identifier.}
#'     \item{indicator}{Character. Indicator name.}
#'     \item{year}{Integer. Calendar year.}
#'     \item{value}{Numeric. Indicator value for this year (units vary
#'       by indicator -- thousands of persons, years, per-1000 rate, etc.).}
#'     \item{sex}{Character. Sex category (e.g. "Both sexes", "Male", "Female").}
#'     \item{variant}{Character. Projection variant (e.g. "Median",
#'       "Estimates" for historical data).}
#'   }
#'
#' @examples
#' \dontrun{
#' # US total population 2000-2025
#' unpop_data(indicator_id = 49, location_id = 840)
#'
#' # India life expectancy at birth
#' unpop_data(indicator_id = 19, location_id = 356, start_year = 1950, end_year = 2100)
#' }
#'
#' @seealso [unpop_indicators()], [unpop_locations()]
#' @export
unpop_data <- function(indicator_id, location_id, start_year = 2000,
                       end_year = 2025) {
  url <- sprintf(
    "%s/data/indicators/%d/locations/%d?startYear=%d&endYear=%d",
    .unpop_base, as.integer(indicator_id), as.integer(location_id),
    as.integer(start_year), as.integer(end_year)
  )
  raw <- .unpop_fetch_paginated(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_data)

  nms <- names(raw)

  as_tibble(raw) |>
    transmute(
      location_id = as.integer(if ("locationId" %in% nms) locationId else if ("locId" %in% nms) locId else NA_integer_),
      location = as.character(if ("location" %in% nms) location else if ("locName" %in% nms) locName else NA_character_),
      indicator_id = as.integer(if ("indicatorId" %in% nms) indicatorId else as.integer(indicator_id)),
      indicator = as.character(if ("indicator" %in% nms) indicator else if ("indicatorName" %in% nms) indicatorName else NA_character_),
      year = as.integer(if ("timeLabel" %in% nms) timeLabel else if ("year" %in% nms) year else NA_integer_),
      value = as.numeric(if ("value" %in% nms) value else NA_real_),
      sex = as.character(if ("sex" %in% nms) sex else NA_character_),
      variant = as.character(if ("variant" %in% nms) variant else if ("variantLabel" %in% nms) variantLabel else NA_character_)
    )
}

# == Context ===================================================================

#' Get un.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
unpop_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(unpop_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/un.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "un.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# un.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# un.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
