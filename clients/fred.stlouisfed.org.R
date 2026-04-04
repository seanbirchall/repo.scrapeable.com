# fred.stlouisfed.org.R
# Self-contained FRED (Federal Reserve Economic Data) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: required API key. Get one at https://fred.stlouisfed.org/docs/api/api_key.html
# Rate limits: 120 requests per minute.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# fred.R
# Self-contained FRED (Federal Reserve Economic Data) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: required API key. Get one at https://fred.stlouisfed.org/docs/api/api_key.html
# Rate limits: 120 requests per minute.


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
#' Downloads observation data for a single FRED series. Returns a long-format
#' tibble with one row per observation period. Missing values (e.g. ".") are
#' converted to NA.
#'
#' @param code FRED series code (e.g. "GDP", "UNRATE", "DGS10"). Browse
#'   available series at \url{https://fred.stlouisfed.org} or use
#'   \code{\link{fred_search}}.
#' @param api_key FRED API key. Register free at
#'   \url{https://fred.stlouisfed.org/docs/api/api_key.html}.
#' @param start Optional start date (Date or "YYYY-MM-DD").
#' @param end Optional end date (Date or "YYYY-MM-DD").
#' @param frequency Optional frequency aggregation: "d" (daily), "w" (weekly),
#'   "bw" (biweekly), "m" (monthly), "q" (quarterly), "sa" (semiannual),
#'   "a" (annual).
#' @param agg Optional aggregation method: "avg" (average), "sum", "eop"
#'   (end of period).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Observation date}
#'     \item{value}{Observation value (numeric, NA for missing)}
#'     \item{code}{Series code}
#'   }
#' @examples
#' \dontrun{
#' # US unemployment rate
#' fred_series("UNRATE", api_key = "your_key")
#'
#' # GDP since 2010, annual frequency
#' fred_series("GDP", api_key = "your_key", start = "2010-01-01", frequency = "a")
#' }
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

#' Fetch multiple FRED series and stack into one tibble
#'
#' Sequentially downloads observation data for each series code, with a
#' configurable sleep between requests to respect rate limits (120 req/min).
#' Progress messages are printed to the console.
#'
#' @param codes Character vector of FRED series codes (e.g.
#'   \code{c("UNRATE", "GDP", "DGS10")}).
#' @param api_key FRED API key.
#' @param sleep Seconds to pause between requests (default 0.5).
#' @param ... Additional arguments passed to \code{\link{fred_series}}
#'   (e.g. \code{start}, \code{end}, \code{frequency}).
#' @return A tibble with columns: date, value, code -- rows stacked across
#'   all requested series.
#' @examples
#' \dontrun{
#' fred_series_bulk(c("UNRATE", "GDP"), api_key = "your_key",
#'                  start = "2020-01-01")
#' }
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

#' Fetch FRED series metadata
#'
#' Returns structured metadata for a single series including title, units,
#' frequency, date range, seasonal adjustment, and descriptive notes.
#'
#' @param code FRED series code (e.g. "UNRATE").
#' @param api_key FRED API key.
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{id}{Series code}
#'     \item{title}{Full series title}
#'     \item{frequency}{Observation frequency (e.g. "M" for monthly)}
#'     \item{units}{Units (e.g. "Percent", "Bil. of $")}
#'     \item{seasonal_adjustment}{Seasonal adjustment code (e.g. "SA", "NSA")}
#'     \item{observation_start}{First observation date}
#'     \item{observation_end}{Last observation date}
#'     \item{last_updated}{Timestamp of last update}
#'     \item{notes}{Descriptive notes about the series}
#'   }
#' @examples
#' \dontrun{
#' fred_series_info("UNRATE", api_key = "your_key")
#' }
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
#' Full-text search across all ~800,000 FRED series. Returns metadata for
#' matching series, sorted by relevance or popularity.
#'
#' @param query Search term (e.g. "unemployment rate", "consumer price index").
#' @param api_key FRED API key.
#' @param limit Maximum results to return (default 100, max 1000).
#' @param order_by Sort field: "search_rank" (default, relevance),
#'   "series_id", "title", "popularity", "observation_start",
#'   "observation_end".
#' @return A tibble with columns: id, title, frequency, units,
#'   seasonal_adjustment, observation_start, observation_end, popularity, notes.
#' @examples
#' \dontrun{
#' # Search for GDP-related series
#' fred_search("gross domestic product", api_key = "your_key", limit = 10)
#'
#' # Most popular inflation series
#' fred_search("inflation", api_key = "your_key", order_by = "popularity")
#' }
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
#' Returns metadata for a single FRED category. Use category_id = 0 for the
#' root category, then drill down with \code{\link{fred_category_children}}.
#'
#' @param category_id Integer category ID (default 0 = root).
#' @param api_key FRED API key.
#' @return A tibble with columns: id, name, parent_id.
#' @examples
#' \dontrun{
#' # Root category
#' fred_category(0, api_key = "your_key")
#' }
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

#' Fetch child categories of a FRED category
#'
#' Lists all direct child categories under a given parent. Use to browse the
#' FRED category tree starting from root (id = 0).
#'
#' @param category_id Integer parent category ID.
#' @param api_key FRED API key.
#' @return A tibble with columns: id, name, parent_id.
#' @examples
#' \dontrun{
#' # Top-level categories under root
#' fred_category_children(0, api_key = "your_key")
#'
#' # Sub-categories under "Money, Banking, & Finance" (category 32)
#' fred_category_children(32, api_key = "your_key")
#' }
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

#' Fetch series within a FRED category
#'
#' Lists all data series belonging to a specific category. Returns the same
#' metadata columns as \code{\link{fred_search}}.
#'
#' @param category_id Integer category ID.
#' @param api_key FRED API key.
#' @param limit Maximum results to return (default 100).
#' @return A tibble with columns: id, title, frequency, units,
#'   seasonal_adjustment, observation_start, observation_end, popularity, notes.
#' @examples
#' \dontrun{
#' # Series in the "Interest Rates" category (id = 22)
#' fred_category_series(22, api_key = "your_key")
#' }
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
#' Lists all release sources in FRED (e.g. "Employment Situation",
#' "Consumer Price Index"). Each release groups related series.
#'
#' @param api_key FRED API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Release ID}
#'     \item{name}{Release name}
#'     \item{press_release}{Whether this is a press release (logical)}
#'     \item{link}{URL to the release}
#'     \item{notes}{Description}
#'   }
#' @examples
#' \dontrun{
#' fred_releases(api_key = "your_key")
#' }
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
#' Lists all data series associated with a specific release. Useful for
#' discovering all series updated in a particular data release.
#'
#' @param release_id Integer release ID (from \code{\link{fred_releases}}).
#' @param api_key FRED API key.
#' @param limit Maximum results to return (default 100).
#' @return A tibble with columns: id, title, frequency, units,
#'   seasonal_adjustment, observation_start, observation_end, popularity, notes.
#' @examples
#' \dontrun{
#' # Series from the Employment Situation release (id = 50)
#' fred_release_series(50, api_key = "your_key")
#' }
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


# == Context ===================================================================

#' Get fred.stlouisfed.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fred_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fred_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fred.stlouisfed.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fred.stlouisfed.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fred.stlouisfed.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fred.stlouisfed.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
