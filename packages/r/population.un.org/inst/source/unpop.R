


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
#' Returns a tibble of available demographic indicators from the UN Population
#' Division Data Portal (e.g., total population, fertility rate, life expectancy).
#'
#' @return tibble: id, name, short_name
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
#' Data Portal, including countries, regions, and income groups.
#'
#' @return tibble: id, name, iso3, type
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
#' @param indicator_id Numeric indicator ID (e.g., 49 = total population)
#' @param location_id Numeric location ID (e.g., 840 = United States)
#' @param start_year Start year (default 2000)
#' @param end_year End year (default 2025)
#' @return tibble: location_id, location, indicator_id, indicator, year,
#'   value, sex, variant
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

#' Generate LLM-friendly context for population.un.org
#'
#' @return Character string with full function signatures and bodies
#' @export
unpop_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/population.un.org.R"
  if (!file.exists(src_file)) {
    cat("# population.un.org context - source not found\n")
    return(invisible("# population.un.org context - source not found"))
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

