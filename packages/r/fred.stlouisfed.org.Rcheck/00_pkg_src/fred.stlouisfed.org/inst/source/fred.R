# fred.R
# Self-contained FRED (Federal Reserve Economic Data) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: required API key. Get one at https://fred.stlouisfed.org/docs/api/api_key.html
# Rate limits: 120 requests per minute.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fred_base <- "https://api.stlouisfed.org/fred"

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- FRED URL builder ----------------------------------------------------------

.fred_url <- function(endpoint, api_key, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  sprintf("%s/%s?%s&api_key=%s&file_type=json", .fred_base, endpoint, query, api_key)
}

# == Schemas ===================================================================

.schema_series <- tibble(
  date = as.Date(character()), value = numeric(), code = character()
)

.schema_series_info <- tibble(
  id = character(), title = character(), frequency = character(),
  units = character(), seasonal_adjustment = character(),
  observation_start = as.Date(character()), observation_end = as.Date(character()),
  last_updated = character(), notes = character()
)

.schema_search <- tibble(
  id = character(), title = character(), frequency = character(),
  units = character(), seasonal_adjustment = character(),
  observation_start = as.Date(character()), observation_end = as.Date(character()),
  popularity = integer(), notes = character()
)

.schema_categories <- tibble(
  id = integer(), name = character(), parent_id = integer()
)

.schema_releases <- tibble(
  id = integer(), name = character(), press_release = logical(),
  link = character(), notes = character()
)


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
#' @param compact If TRUE (default), concise output
#' @return Character string (invisibly), also printed
fred_context <- function(compact = TRUE) {
  fns <- list(
    list("fred_series", "(code, api_key, start, end, frequency, agg)",
         "Fetch time series observations. Returns date, value, code."),
    list("fred_series_bulk", "(codes, api_key, sleep, ...)",
         "Fetch multiple series. Stacked tibble with date, value, code."),
    list("fred_series_info", "(code, api_key)",
         "Series metadata: title, units, frequency, date range, notes."),
    list("fred_search", "(query, api_key, limit, order_by)",
         "Search series by keyword. Returns id, title, units, popularity."),
    list("fred_category", "(category_id, api_key)",
         "Fetch category info. category_id=0 for root."),
    list("fred_category_children", "(category_id, api_key)",
         "List child categories."),
    list("fred_category_series", "(category_id, api_key, limit)",
         "List series within a category."),
    list("fred_releases", "(api_key)",
         "List all FRED data releases."),
    list("fred_release_series", "(release_id, api_key, limit)",
         "List series within a specific release.")
  )

  lines <- c(
    "# fred.stlouisfed.org - Federal Reserve Economic Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: required API key as `api_key` param",
    "# Rate limit: 120 requests/minute",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common series codes:",
    "#   GDP        = Gross Domestic Product",
    "#   UNRATE     = Unemployment Rate",
    "#   CPIAUCSL   = Consumer Price Index (All Urban)",
    "#   DGS10      = 10-Year Treasury Constant Maturity",
    "#   FEDFUNDS   = Federal Funds Effective Rate",
    "#   M2SL       = M2 Money Stock",
    "#   HOUST      = Housing Starts",
    "#   PAYEMS     = Total Nonfarm Payrolls",
    "#",
    "# == Functions ==",
    "#"
  )

  for (fn in fns) {
    lines <- c(lines,
      sprintf("# %s%s", fn[[1]], fn[[2]]),
      sprintf("#   %s", fn[[3]]),
      sprintf("#   Run `%s` to view source or `?%s` for help.", fn[[1]], fn[[1]]),
      "#"
    )
  }

  lines <- c(lines,
    "# == Quick examples ==",
    "#",
    "# fred_series('UNRATE', api_key)",
    "# fred_series('GDP', api_key, start = '2020-01-01', frequency = 'q')",
    "# fred_series_bulk(c('UNRATE', 'GDP', 'CPIAUCSL'), api_key)",
    "# fred_search('unemployment', api_key)",
    "# fred_series_info('UNRATE', api_key)",
    "# fred_category_children(0, api_key)  # browse root categories",
    "#"
  )

  out <- paste(lines, collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
