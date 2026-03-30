#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom utils URLencode
#' @import dplyr
#' @keywords internal
NULL

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


