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
#' Downloads observation data for a single FRED series. Returns a tibble
#' with date/value pairs and the series code. Supports optional date
#' filtering and frequency aggregation.
#'
#' @param code Character. FRED series identifier. Popular codes include:
#'   \code{"GDP"} (Gross Domestic Product), \code{"UNRATE"} (Unemployment Rate),
#'   \code{"DGS10"} (10-Year Treasury), \code{"CPIAUCSL"} (Consumer Price Index),
#'   \code{"FEDFUNDS"} (Federal Funds Rate), \code{"SP500"} (S&P 500).
#'   Find codes via \code{\link{fred_search}}.
#' @param api_key Character. FRED API key. Register free at
#'   \url{https://fred.stlouisfed.org/docs/api/api_key.html}.
#' @param start Date or character (\code{YYYY-MM-DD}). Start of observation
#'   window. \code{NULL} (default) returns from the beginning of the series.
#' @param end Date or character. End of observation window.
#'   \code{NULL} (default) returns through the latest observation.
#' @param frequency Character or \code{NULL}. Frequency aggregation:
#'   \code{"d"} (daily), \code{"w"} (weekly), \code{"bw"} (biweekly),
#'   \code{"m"} (monthly), \code{"q"} (quarterly), \code{"sa"} (semiannual),
#'   \code{"a"} (annual). \code{NULL} (default) uses the native frequency.
#' @param agg Character or \code{NULL}. Aggregation method when changing
#'   frequency: \code{"avg"} (average), \code{"sum"}, \code{"eop"}
#'   (end of period). \code{NULL} (default) uses average.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{value}{Numeric. Observation value, or \code{NA} for missing periods.}
#'     \item{code}{Character. The FRED series code.}
#'   }
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_search}} to find series codes,
#'   \code{\link{fred_series_info}} for series metadata
#' @examples
#' \dontrun{
#' fred_series("UNRATE", api_key = Sys.getenv("FRED_API_KEY"))
#' fred_series("GDP", api_key = key, start = "2020-01-01")
#' fred_series("DGS10", api_key = key, frequency = "m", agg = "avg")
#' }
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

#' Fetch multiple FRED series in bulk
#'
#' Convenience wrapper that fetches multiple FRED series sequentially and
#' stacks them into a single long-format tibble. Includes a configurable
#' sleep between requests to respect rate limits (120 req/min).
#'
#' @param codes Character vector. FRED series identifiers
#'   (e.g., \code{c("GDP", "UNRATE", "CPIAUCSL")}).
#' @param api_key Character. FRED API key.
#' @param sleep Numeric. Seconds to pause between requests (default 0.5).
#' @param ... Additional arguments passed to \code{\link{fred_series}}
#'   (e.g., \code{start}, \code{end}, \code{frequency}).
#' @return A tibble with the same columns as \code{\link{fred_series}}:
#'   \code{date}, \code{value}, \code{code}. Rows from all series are
#'   stacked (use the \code{code} column to distinguish series).
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_series}} for a single series
#' @examples
#' \dontrun{
#' fred_series_bulk(c("GDP", "UNRATE", "FEDFUNDS"), api_key = key)
#' }
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
#' Returns structured metadata for a FRED series including its title,
#' units, frequency, seasonal adjustment, date range, and notes.
#' Useful for understanding what a series code represents before
#' downloading observations.
#'
#' @param code Character. FRED series code (e.g., \code{"UNRATE"}).
#' @param api_key Character. FRED API key.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Character. Series identifier (same as \code{code}).}
#'     \item{title}{Character. Full series title (e.g.,
#'       \code{"Unemployment Rate"}).}
#'     \item{frequency}{Character. Native frequency code (\code{"M"} monthly,
#'       \code{"Q"} quarterly, \code{"A"} annual, etc.).}
#'     \item{units}{Character. Unit abbreviation (e.g., \code{"\%"},
#'       \code{"Bil. of $"}).}
#'     \item{seasonal_adjustment}{Character. Seasonal adjustment code
#'       (\code{"SA"} seasonally adjusted, \code{"NSA"} not seasonally adjusted).}
#'     \item{observation_start}{Date. Earliest available observation date.}
#'     \item{observation_end}{Date. Most recent observation date.}
#'     \item{last_updated}{Character. Timestamp of last data update.}
#'     \item{notes}{Character. Detailed series description and methodology notes.}
#'   }
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_series}} to download the actual data
#' @examples
#' \dontrun{
#' fred_series_info("UNRATE", api_key = key)
#' }
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
#' Performs a full-text search of the FRED database (400,000+ series).
#' Returns matching series with metadata. Use the \code{id} column to
#' pass to \code{\link{fred_series}} for actual data.
#'
#' @param query Character. Search terms (e.g., \code{"unemployment rate"},
#'   \code{"consumer price index"}, \code{"housing starts"}).
#' @param api_key Character. FRED API key.
#' @param limit Integer. Maximum results to return (default 100, max 1000).
#' @param order_by Character. Sort field: \code{"search_rank"} (default),
#'   \code{"series_id"}, \code{"title"}, \code{"popularity"},
#'   \code{"observation_start"}, \code{"observation_end"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. FRED series code (use with \code{\link{fred_series}}).}
#'     \item{title}{Character. Full series title.}
#'     \item{frequency}{Character. Native frequency code.}
#'     \item{units}{Character. Unit abbreviation.}
#'     \item{seasonal_adjustment}{Character. SA or NSA.}
#'     \item{observation_start}{Date. Earliest observation date.}
#'     \item{observation_end}{Date. Latest observation date.}
#'     \item{popularity}{Integer. FRED popularity ranking (higher = more popular).}
#'     \item{notes}{Character. Description and methodology notes.}
#'   }
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_series}} to download data for a found series
#' @examples
#' \dontrun{
#' fred_search("unemployment rate", api_key = key)
#' fred_search("GDP", api_key = key, order_by = "popularity")
#' }
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
#' Returns metadata for a single FRED category. The FRED database is
#' organized as a tree of categories; use this function to explore
#' that tree starting from the root (\code{category_id = 0}).
#'
#' @param category_id Integer. Category ID (default 0 = root).
#'   Find child IDs via \code{\link{fred_category_children}}.
#' @param api_key Character. FRED API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Category ID.}
#'     \item{name}{Character. Category name (e.g., \code{"Production & Business Activity"}).}
#'     \item{parent_id}{Integer. Parent category ID (0 for root categories).}
#'   }
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_category_children}} to list subcategories,
#'   \code{\link{fred_category_series}} to list series in a category
#' @examples
#' \dontrun{
#' fred_category(0, api_key = key)
#' }
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
#' Returns all immediate subcategories of a given FRED category. Use
#' recursively to navigate the full category tree.
#'
#' @param category_id Integer. Parent category ID. Start with \code{0}
#'   (root) and drill down.
#' @param api_key Character. FRED API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Child category ID.}
#'     \item{name}{Character. Category name.}
#'     \item{parent_id}{Integer. Parent category ID (= \code{category_id}).}
#'   }
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_category}} for a single category's info,
#'   \code{\link{fred_category_series}} for series in a category
#' @examples
#' \dontrun{
#' fred_category_children(0, api_key = key)
#' }
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
#' Returns all series that belong to a given FRED category. Output
#' format matches \code{\link{fred_search}}.
#'
#' @param category_id Integer. Category ID. Find via
#'   \code{\link{fred_category_children}}.
#' @param api_key Character. FRED API key.
#' @param limit Integer. Maximum results (default 100, max 1000).
#' @return A tibble with the same columns as \code{\link{fred_search}}:
#'   \code{id}, \code{title}, \code{frequency}, \code{units},
#'   \code{seasonal_adjustment}, \code{observation_start},
#'   \code{observation_end}, \code{popularity}, \code{notes}.
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_category_children}} to browse categories
#' @examples
#' \dontrun{
#' # Series in "Money, Banking, & Finance" (category 32)
#' fred_category_series(32, api_key = key)
#' }
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
#' Returns a list of all data releases published through FRED. Releases
#' group related series (e.g., "Employment Situation" contains UNRATE,
#' PAYEMS, etc.). Use the release ID with \code{\link{fred_release_series}}.
#'
#' @param api_key Character. FRED API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Release ID.}
#'     \item{name}{Character. Release name (e.g., \code{"Employment Situation"}).}
#'     \item{press_release}{Logical. Whether this release has an associated press release.}
#'     \item{link}{Character. URL to the release page.}
#'     \item{notes}{Character. Release description.}
#'   }
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_release_series}} for series in a release
#' @examples
#' \dontrun{
#' fred_releases(api_key = key)
#' }
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

#' Fetch series within a FRED release
#'
#' Returns all series associated with a given data release. For example,
#' the "Employment Situation" release (ID 50) contains series like UNRATE,
#' PAYEMS, and others.
#'
#' @param release_id Integer. Release ID. Find via \code{\link{fred_releases}}.
#' @param api_key Character. FRED API key.
#' @param limit Integer. Maximum results (default 100, max 1000).
#' @return A tibble with the same columns as \code{\link{fred_search}}:
#'   \code{id}, \code{title}, \code{frequency}, \code{units},
#'   \code{seasonal_adjustment}, \code{observation_start},
#'   \code{observation_end}, \code{popularity}, \code{notes}.
#' @export
#' @family FRED functions
#' @seealso \code{\link{fred_releases}} to list all releases
#' @examples
#' \dontrun{
#' # Series in "Employment Situation" release
#' fred_release_series(50, api_key = key)
#' }
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

#' Get fred client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fred.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fred")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fred context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fred", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
