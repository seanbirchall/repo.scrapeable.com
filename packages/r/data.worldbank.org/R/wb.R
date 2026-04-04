# data.worldbank.org.R - Self-contained data.worldbank.org client



# data-worldbank-org.R
# Self-contained World Bank Data API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://api.worldbank.org/v2

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wb_base <- "https://api.worldbank.org/v2"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# WB API returns [metadata, data] array
.fetch_wb <- function(url) {
  raw <- jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
  if (length(raw) < 2) return(list(meta = NULL, data = list()))
  list(meta = raw[[1]], data = raw[[2]])
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_data <- tibble(
  country_iso3 = character(), country = character(),
  indicator_id = character(), indicator = character(),
  date = character(), value = numeric()
)

.schema_countries <- tibble(
  iso3 = character(), iso2 = character(), name = character(),
  region = character(), income_level = character(),
  capital = character(), longitude = numeric(), latitude = numeric()
)

.schema_indicators <- tibble(
  id = character(), name = character(), unit = character(),
  source = character(), source_note = character()
)

# == Data ======================================================================

#' Fetch World Bank indicator data
#'
#' Returns time series data for one indicator across one or more countries.
#' The World Bank API covers approximately 29,500 indicators and 296
#' countries/regions. Automatically paginates up to 20 pages. Rows with
#' missing values are excluded.
#'
#' @param indicator Character. Indicator code (e.g. "NY.GDP.MKTP.CD" for GDP
#'   in current US$, "SP.POP.TOTL" for total population,
#'   "FP.CPI.TOTL.ZG" for CPI inflation). Use \code{wb_indicators()} to
#'   search for codes.
#' @param country Character. ISO2 country code(s): "US", "GB",
#'   \code{c("US","GB","DE")}. Use "all" for all countries. Default "all".
#' @param date_range Character or NULL. Year range: "2000:2023" or single
#'   year "2023". Default NULL returns all available years.
#' @param per_page Integer. Results per page (default 1000).
#' @return A tibble with columns:
#' \describe{
#'   \item{country_iso3}{Character. ISO 3166-1 alpha-3 country code (e.g. "USA").}
#'   \item{country}{Character. Country name (e.g. "United States").}
#'   \item{indicator_id}{Character. Indicator code (e.g. "NY.GDP.MKTP.CD").}
#'   \item{indicator}{Character. Indicator label (e.g. "GDP (current US$)").}
#'   \item{date}{Character. Year of observation (e.g. "2023").}
#'   \item{value}{Numeric. The indicator value. Rows with NA values are excluded.}
#' }
#' @export
#' @examples
#' \dontrun{
#' wb_data("NY.GDP.MKTP.CD", country = "US", date_range = "2020:2023")
#' wb_data("SP.POP.TOTL", country = c("US", "GB", "DE"))
#' }
wb_data <- function(indicator, country = "all", date_range = NULL,
                    per_page = 1000) {
  country_str <- paste(country, collapse = ";")
  url <- sprintf("%s/country/%s/indicator/%s?format=json&per_page=%d",
                 .wb_base, country_str, indicator, per_page)
  if (!is.null(date_range)) url <- paste0(url, "&date=", date_range)

  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_data)

  # Check total pages and fetch remaining
  total_pages <- as.integer(result$meta$pages %||% 1)
  all_data <- result$data

  if (total_pages > 1) {
    for (pg in 2:min(total_pages, 20)) {
      pg_url <- paste0(url, "&page=", pg)
      pg_result <- tryCatch(.fetch_wb(pg_url), error = function(e) NULL)
      if (!is.null(pg_result$data)) all_data <- c(all_data, pg_result$data)
    }
  }

  tibble(
    country_iso3 = vapply(all_data, function(d) d$countryiso3code %||% NA_character_, character(1)),
    country      = vapply(all_data, function(d) d$country$value %||% NA_character_, character(1)),
    indicator_id = vapply(all_data, function(d) d$indicator$id %||% NA_character_, character(1)),
    indicator    = vapply(all_data, function(d) d$indicator$value %||% NA_character_, character(1)),
    date         = vapply(all_data, function(d) d$date %||% NA_character_, character(1)),
    value        = vapply(all_data, function(d) as.numeric(d$value %||% NA_real_), numeric(1))
  ) |>
    filter(!is.na(value)) |>
    arrange(country_iso3, date)
}


# == Countries =================================================================

#' List World Bank countries and regions
#'
#' Returns metadata for all 296 countries, territories, and aggregate regions
#' in the World Bank database, including geographic coordinates and income
#' classification.
#'
#' @param per_page Integer. Results per page (default 300, sufficient for all
#'   entries in a single request).
#' @return A tibble with columns:
#' \describe{
#'   \item{iso3}{Character. ISO 3166-1 alpha-3 code (e.g. "USA", "GBR", "AFG").}
#'   \item{iso2}{Character. ISO 3166-1 alpha-2 code (e.g. "US", "GB").}
#'   \item{name}{Character. Country or region name (e.g. "United States", "Afghanistan").}
#'   \item{region}{Character. World Bank region (e.g. "North America", "Middle East & North Africa"). Empty string for aggregates.}
#'   \item{income_level}{Character. Income classification: "High income", "Upper middle income", "Lower middle income", "Low income", or "Aggregates".}
#'   \item{capital}{Character. Capital city name. Empty string for aggregates.}
#'   \item{longitude}{Numeric. Longitude of capital city. NA for aggregates.}
#'   \item{latitude}{Numeric. Latitude of capital city. NA for aggregates.}
#' }
#' @export
#' @examples
#' \dontrun{
#' countries <- wb_countries()
#' countries |> dplyr::filter(income_level == "Low income")
#' }
wb_countries <- function(per_page = 300) {
  url <- sprintf("%s/country?format=json&per_page=%d", .wb_base, per_page)
  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_countries)

  tibble(
    iso3         = vapply(result$data, function(c) c$id %||% NA_character_, character(1)),
    iso2         = vapply(result$data, function(c) c$iso2Code %||% NA_character_, character(1)),
    name         = vapply(result$data, function(c) c$name %||% NA_character_, character(1)),
    region       = vapply(result$data, function(c) c$region$value %||% NA_character_, character(1)),
    income_level = vapply(result$data, function(c) c$incomeLevel$value %||% NA_character_, character(1)),
    capital      = vapply(result$data, function(c) c$capitalCity %||% NA_character_, character(1)),
    longitude    = vapply(result$data, function(c) as.numeric(c$longitude %||% NA_real_), numeric(1)),
    latitude     = vapply(result$data, function(c) as.numeric(c$latitude %||% NA_real_), numeric(1))
  )
}


# == Indicators ================================================================

#' Search World Bank indicators
#'
#' Lists or searches the World Bank's catalogue of approximately 29,500
#' development indicators. When \code{query} is provided, filters results
#' client-side by matching against indicator names and IDs (case-insensitive).
#'
#' @param query Character or NULL. Search term (e.g. "GDP", "population",
#'   "inflation"). NULL returns all indicators on the requested page.
#' @param per_page Integer. Results per page (default 1000).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. Indicator code for use with \code{wb_data()} (e.g. "NY.GDP.MKTP.CD", "SP.POP.TOTL").}
#'   \item{name}{Character. Indicator name (e.g. "GDP (current US$)", "Population, total").}
#'   \item{unit}{Character. Unit of measurement (often empty string).}
#'   \item{source}{Character. Data source name (e.g. "World Development Indicators").}
#'   \item{source_note}{Character. Description of the indicator, truncated to 200 characters.}
#' }
#' @export
#' @examples
#' \dontrun{
#' wb_indicators(query = "GDP")
#' wb_indicators(query = "population")
#' }
wb_indicators <- function(query = NULL, per_page = 1000, page = 1) {
  url <- sprintf("%s/indicator?format=json&per_page=%d&page=%d",
                 .wb_base, per_page, page)

  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_indicators)

  ind <- tibble(
    id          = vapply(result$data, function(i) i$id %||% NA_character_, character(1)),
    name        = vapply(result$data, function(i) i$name %||% NA_character_, character(1)),
    unit        = vapply(result$data, function(i) i$unit %||% NA_character_, character(1)),
    source      = vapply(result$data, function(i) i$source$value %||% NA_character_, character(1)),
    source_note = vapply(result$data, function(i) {
      n <- i$sourceNote %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )

  if (!is.null(query)) {
    pattern <- tolower(query)
    ind <- ind |> filter(grepl(pattern, tolower(name)) | grepl(pattern, tolower(id)))
  }
  ind
}


# == Context ===================================================================

#' Get data.worldbank.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.worldbank.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.worldbank.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.worldbank.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.worldbank.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
