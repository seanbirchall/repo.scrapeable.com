# Private utilities for hud.gov package

`%||%` <- function(a, b) if (is.null(a)) b else a

.hud_ua <- "hud.gov-R-client/0.1 (support@scrapeable.com)"

.hud_fetch <- function(url, ext = ".tmp") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .hud_ua) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

.hud_fetch_json <- function(url) {
  jsonlite::fromJSON(.hud_fetch(url, ext = ".json"), simplifyVector = TRUE)
}

.hud_fetch_xlsx <- function(url, sheet = 1, skip = 0) {
  tmp <- .hud_fetch(url, ext = ".xlsx")
  readxl::read_excel(tmp, sheet = sheet, skip = skip) |>
    tibble::as_tibble()
}

.safe_int <- function(x) suppressWarnings(as.integer(x))
.safe_num <- function(x) suppressWarnings(as.numeric(x))
.safe_chr <- function(x) as.character(x %||% NA_character_)

.hud_catalog_cache <- new.env(parent = emptyenv())

# Schemas
.schema_catalog <- tibble::tibble(
  title = character(), description = character(), keyword = character(),
  publisher = character(), modified = character(), identifier = character(),
  distribution_url = character(), media_type = character(),
  theme = character(), contact_name = character()
)

.schema_fmr <- tibble::tibble(
  state_abbr = character(), state_code = character(),
  hud_area_code = character(), county_name = character(),
  area_name = character(), metro = integer(), fips = character(),
  population = integer(),
  fmr_0br = integer(), fmr_1br = integer(), fmr_2br = integer(),
  fmr_3br = integer(), fmr_4br = integer()
)

.schema_income_limits <- tibble::tibble(
  fips = character(), state_abbr = character(), state_code = character(),
  state_name = character(), hud_area_code = character(),
  area_name = character(), county = character(), county_name = character(),
  metro = integer(), median_income = integer(),
  l50_1 = integer(), l50_2 = integer(), l50_3 = integer(), l50_4 = integer(),
  l80_1 = integer(), l80_2 = integer(), l80_3 = integer(), l80_4 = integer(),
  eli_1 = integer(), eli_2 = integer(), eli_3 = integer(), eli_4 = integer()
)

.schema_inspections <- tibble::tibble(
  inspection_id = character(), property_id = character(),
  property_name = character(), address = character(),
  city = character(), state_abbr = character(), state_fips = character(),
  zip = character(), county_name = character(),
  cbsa_name = character(), cbsa_code = character(),
  latitude = numeric(), longitude = numeric(),
  inspection_score = numeric(), inspection_date = character()
)

.schema_contracts <- tibble::tibble(
  property_id = integer(), contract_number = character(),
  bedroom_count = integer(), unit_count = integer(),
  contract_rent = numeric(), fair_market_rent = numeric(),
  utility_allowance = numeric()
)

.schema_homeless <- tibble::tibble(
  state = character(),
  total_yr_beds = integer(), total_yr_beds_es = integer(),
  total_yr_beds_th = integer(), total_yr_beds_sh = integer(),
  units_hh_children = integer(), beds_hh_children = integer()
)
