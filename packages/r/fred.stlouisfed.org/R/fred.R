# == Series data ===============================================================

#' Fetch FRED time series observations
#'
#' @param code FRED series code (e.g. "GDP", "UNRATE", "DGS10")
#' @param api_key FRED API key
#' @param start Optional start date (Date or "YYYY-MM-DD")
#' @param end Optional end date
#' @param frequency Optional frequency aggregation: "d", "w", "bw", "m",
#'   "q", "sa", "a" (daily through annual)
#' @param agg Optional aggregation method: "avg", "sum", "eop" (end of period)
#' @return tibble: date (Date), value (numeric), code (character)
#' @export
fred_series <- function(code, api_key, start = NULL, end = NULL,
                        frequency = NULL, agg = NULL) {
  url <- .fred_url("series/observations", api_key,
                   series_id = code,
                   observation_start = start,
                   observation_end = end,
                   frequency = frequency,
                   aggregation_method = agg)

  raw <- .fetch_json(url)
  obs <- raw$observations
  if (is.null(obs) || nrow(obs) == 0) return(.schema_series)

  as_tibble(obs) |>
    transmute(
      date  = as.Date(date),
      value = suppressWarnings(as.numeric(value)),
      code  = code
    )
}

#' Fetch multiple FRED series
#'
#' @param codes Character vector of FRED series codes
#' @param api_key FRED API key
#' @param sleep Seconds between requests (default 0.5)
#' @param ... Additional args passed to fred_series
#' @return tibble: date, value, code (stacked)
#' @export
fred_series_bulk <- function(codes, api_key, sleep = 0.5, ...) {
  n <- length(codes)
  results <- lapply(seq_along(codes), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] %s", i, n, codes[i]))
    tryCatch(fred_series(codes[i], api_key, ...), error = function(e) {
      message("  Failed: ", e$message)
      NULL
    })
  })
  bind_rows(results)
}


# == Series metadata ===========================================================

#' Fetch FRED series metadata via API
#'
#' Returns structured metadata for a series (title, units, frequency,
#' date range, notes). This uses the API — no web scraping.
#'
#' @param code FRED series code
#' @param api_key FRED API key
#' @return tibble: one row with id, title, frequency, units,
#'   seasonal_adjustment, observation_start, observation_end, last_updated, notes
#' @export
fred_series_info <- function(code, api_key) {
  url <- .fred_url("series", api_key, series_id = code)
  raw <- .fetch_json(url)
  s <- raw$seriess
  if (is.null(s) || nrow(s) == 0) return(.schema_series_info)

  as_tibble(s) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      frequency = as.character(frequency_short),
      units = as.character(units_short),
      seasonal_adjustment = as.character(seasonal_adjustment_short),
      observation_start = as.Date(observation_start),
      observation_end = as.Date(observation_end),
      last_updated = as.character(last_updated),
      notes = as.character(notes)
    )
}


# == Search ====================================================================

#' Search FRED series by keyword
#'
#' @param query Search term (e.g. "unemployment rate", "consumer price index")
#' @param api_key FRED API key
#' @param limit Max results (default 100, max 1000)
#' @param order_by Sort field: "search_rank" (default), "series_id",
#'   "title", "popularity", "observation_start", "observation_end"
#' @return tibble: id, title, frequency, units, seasonal_adjustment,
#'   observation_start, observation_end, popularity, notes
#' @export
fred_search <- function(query, api_key, limit = 100,
                        order_by = "search_rank") {
  url <- .fred_url("series/search", api_key,
                   search_text = utils::URLencode(query),
                   limit = limit,
                   order_by = order_by)
  raw <- .fetch_json(url)
  s <- raw$seriess
  if (is.null(s) || nrow(s) == 0) return(.schema_search)

  as_tibble(s) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      frequency = as.character(frequency_short),
      units = as.character(units_short),
      seasonal_adjustment = as.character(seasonal_adjustment_short),
      observation_start = as.Date(observation_start),
      observation_end = as.Date(observation_end),
      popularity = as.integer(popularity),
      notes = as.character(notes)
    )
}


# == Categories ================================================================

#' Fetch FRED category info
#'
#' @param category_id Category ID (default 0 = root)
#' @param api_key FRED API key
#' @return tibble: id, name, parent_id
#' @export
fred_category <- function(category_id = 0, api_key) {
  url <- .fred_url("category", api_key, category_id = category_id)
  raw <- .fetch_json(url)
  cats <- raw$categories
  if (is.null(cats) || nrow(cats) == 0) return(.schema_categories)

  as_tibble(cats) |>
    transmute(
      id = as.integer(id),
      name = as.character(name),
      parent_id = as.integer(parent_id)
    )
}

#' Fetch child categories
#'
#' @param category_id Parent category ID
#' @param api_key FRED API key
#' @return tibble: id, name, parent_id
#' @export
fred_category_children <- function(category_id, api_key) {
  url <- .fred_url("category/children", api_key, category_id = category_id)
  raw <- .fetch_json(url)
  cats <- raw$categories
  if (is.null(cats) || nrow(cats) == 0) return(.schema_categories)

  as_tibble(cats) |>
    transmute(
      id = as.integer(id),
      name = as.character(name),
      parent_id = as.integer(parent_id)
    )
}

#' Fetch series within a category
#'
#' @param category_id Category ID
#' @param api_key FRED API key
#' @param limit Max results (default 100)
#' @return tibble: same columns as fred_search
#' @export
fred_category_series <- function(category_id, api_key, limit = 100) {
  url <- .fred_url("category/series", api_key,
                   category_id = category_id, limit = limit)
  raw <- .fetch_json(url)
  s <- raw$seriess
  if (is.null(s) || nrow(s) == 0) return(.schema_search)

  as_tibble(s) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      frequency = as.character(frequency_short),
      units = as.character(units_short),
      seasonal_adjustment = as.character(seasonal_adjustment_short),
      observation_start = as.Date(observation_start),
      observation_end = as.Date(observation_end),
      popularity = as.integer(popularity),
      notes = as.character(notes)
    )
}


# == Releases ==================================================================

#' Fetch all FRED data releases
#'
#' @param api_key FRED API key
#' @return tibble: id, name, press_release, link, notes
#' @export
fred_releases <- function(api_key) {
  url <- .fred_url("releases", api_key)
  raw <- .fetch_json(url)
  r <- raw$releases
  if (is.null(r) || nrow(r) == 0) return(.schema_releases)

  as_tibble(r) |>
    transmute(
      id = as.integer(id),
      name = as.character(name),
      press_release = as.logical(press_release),
      link = as.character(link),
      notes = as.character(notes)
    )
}

#' Fetch series within a release
#'
#' @param release_id Release ID
#' @param api_key FRED API key
#' @param limit Max results (default 100)
#' @return tibble: same columns as fred_search
#' @export
fred_release_series <- function(release_id, api_key, limit = 100) {
  url <- .fred_url("release/series", api_key,
                   release_id = release_id, limit = limit)
  raw <- .fetch_json(url)
  s <- raw$seriess
  if (is.null(s) || nrow(s) == 0) return(.schema_search)

  as_tibble(s) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      frequency = as.character(frequency_short),
      units = as.character(units_short),
      seasonal_adjustment = as.character(seasonal_adjustment_short),
      observation_start = as.Date(observation_start),
      observation_end = as.Date(observation_end),
      popularity = as.integer(popularity),
      notes = as.character(notes)
    )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the fred package
#'
#' @return Character string (invisibly), also printed
#' @export
fred_context <- function() {
  .build_context("fred.stlouisfed.org", header_lines = c(
    "# fred.stlouisfed.org - Federal Reserve Economic Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: required API key as `api_key` param",
    "# Rate limit: 120 requests/minute",
    "# All functions return tibbles with typed columns."
  ))
}
