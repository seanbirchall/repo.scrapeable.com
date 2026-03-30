#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom utils URLencode
#' @import dplyr
#' @import tidyr
#' @keywords internal
NULL

# census-gov.R
# Self-contained US Census Bureau client.
# All public functions return tibbles. All columns return as character —
# caller decides types (Census uses "-", "N", "(X)" for suppressed data).
#
# Dependencies: httr2, jsonlite, dplyr, tidyr, tibble
# Auth: optional API key (query param). Get one at api.census.gov/data/key_signup.html


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.census_cache <- new.env(parent = emptyenv())
.census_base <- "https://api.census.gov/data"

# Max variables per Census API request (hard limit is 50)
.census_var_limit <- 49

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Census response parser ----------------------------------------------------
# Census API returns JSON array-of-arrays: [[headers], [row1], [row2], ...]
# Returns ALL columns as character — no auto-type guessing.

.parse_census_response <- function(raw) {
  if (is.matrix(raw)) {
    if (nrow(raw) < 2) return(tibble())
    headers <- tolower(raw[1, ])
    mat <- raw[-1, , drop = FALSE]
    colnames(mat) <- headers
    as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
  } else if (is.list(raw) && length(raw) >= 2) {
    headers <- tolower(unlist(raw[[1]]))
    rows <- raw[-1]
    if (length(rows) == 0)
      return(tibble(!!!setNames(rep(list(character()), length(headers)), headers)))
    mat <- do.call(rbind, lapply(rows, function(r) {
      r[vapply(r, is.null, logical(1))] <- NA_character_
      unlist(r)
    }))
    colnames(mat) <- headers
    as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
  } else {
    tibble()
  }
}

# -- Variable caching ----------------------------------------------------------

.cached_variables <- function(dataset, year, key = NULL) {
  cache_key <- paste0("vars_", dataset, "_", year)
  if (!is.null(.census_cache[[cache_key]])) return(.census_cache[[cache_key]])
  result <- census_variables(dataset, year, key)
  .census_cache[[cache_key]] <- result
  result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  title = character(), description = character(),
  dataset = character(), year = integer()
)

.schema_variables <- tibble(
  name = character(), label = character(), concept = character(),
  predicate_type = character(), group = character()
)

.schema_groups <- tibble(name = character(), description = character())

.schema_geography <- tibble(
  name = character(), geo_level = character(),
  requires = character(), wildcard = character()
)


# == Geography helpers =========================================================

#' Build a state-level geography spec
#' @param code FIPS code or "*" for all (default)
#' @return list(for_geo, in_geo) for use with census_get
census_geo_state <- function(code = "*") {
  list(for_geo = paste0("state:", code), in_geo = NULL)
}

#' Build a county-level geography spec
#' @param state State FIPS code (required)
#' @param code County FIPS or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_county <- function(state, code = "*") {
  list(for_geo = paste0("county:", code),
       in_geo = paste0("state:", state))
}

#' Build a tract-level geography spec
#' @param state State FIPS code (required)
#' @param county County FIPS code (required)
#' @param code Tract code or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_tract <- function(state, county, code = "*") {
  list(for_geo = paste0("tract:", code),
       in_geo = paste("state:", state, " county:", county, sep = ""))
}

#' Build a place-level geography spec
#' @param state State FIPS code (required)
#' @param code Place FIPS or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_place <- function(state, code = "*") {
  list(for_geo = paste0("place:", code),
       in_geo = paste0("state:", state))
}

#' Build a ZCTA geography spec
#' @param code ZCTA code or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_zcta <- function(code = "*") {
  list(for_geo = paste0("zip code tabulation area:", code), in_geo = NULL)
}

#' Build a block group geography spec
#' @param state State FIPS (required)
#' @param county County FIPS (required)
#' @param tract Tract code (required)
#' @param code Block group code or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_blockgroup <- function(state, county, tract, code = "*") {
  list(for_geo = paste0("block group:", code),
       in_geo = paste0("state:", state, " county:", county, " tract:", tract))
}


