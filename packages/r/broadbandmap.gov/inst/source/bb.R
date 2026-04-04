# broadbandmap.gov.R
# Self-contained broadbandmap.gov client (NTIA National Broadband Map).
#
# STATUS: UNREACHABLE
# The original NTIA National Broadband Map API at www.broadbandmap.gov was
# decommissioned by the FCC in December 2018. The domain now redirects (301)
# to https://broadbandmap.fcc.gov, which is the new FCC National Broadband Map.
# The new FCC broadband map API requires authentication (API token + username)
# and all public endpoints return HTTP 405 "Method Not Available".
#
# All 29 original endpoints (broadband summary, wireline/wireless providers,
# speed tests, geography lookup, demographics, community anchor institutions,
# census block lookup) are defunct.
#
# See: https://www.fcc.gov/news-events/blog/2018/12/07/decommissioning-national-broadband-map-and-its-apis
# New API docs: https://www.fcc.gov/sites/default/files/bdc-public-data-api-spec.pdf
#
# Dependencies: httr2, jsonlite, dplyr, tibble

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"

# == Endpoint catalog ==========================================================

.bb_endpoints <- tibble::tribble(
  ~category,               ~endpoint_name,                                            ~description,
  "Broadband Summary",     "broadband-summary-api-nation",                             "Broadband summary data for the entire nation",
  "Broadband Summary",     "broadband-summary-api-by-geography-type",                  "Broadband summary data by geography type",
  "Broadband Summary",     "broadband-summary-api-by-geography-type-and-geography-id", "Broadband summary data by geography type and ID",

  "Wireline Broadband",    "wireline-broadband-api",                                   "Wireline providers within a census block by lat/lon",
  "Wireless Broadband",    "wireless-broadband-api",                                   "Wireless providers within a census block by lat/lon",

  "Speed Test",            "speed-test-api-nation",                                    "Speed test results for the entire US",
  "Speed Test",            "speed-test-api-minimum-and-maximum-quartile-speeds-by-geography-type", "Min/max quartile speeds by geography type",
  "Speed Test",            "speed-test-api-by-geography-type-and-geography-name",       "Speed test results by geography type and name",
  "Speed Test",            "speed-test-api-by-geography-type-and-geography-id",         "Speed test results by geography type and ID",

  "Geography Lookup",      "geography-lookup-api-by-geography-type",                    "All geographies of a specified type",
  "Geography Lookup",      "geography-lookup-api-by-geography-id",                      "Geography by type and ID",
  "Geography Lookup",      "geography-lookup-api-by-geography-type-and-geography-name", "Geographies by type and name",
  "Geography Lookup",      "geography-lookup-api-by-geography-type-within-a-state",     "All geographies of a type within a state",
  "Geography Lookup",      "geography-lookup-api-by-name-of-specific-geography-type-within-a-state", "Geographies by name within a state",

  "Demographics",          "demographics-api-nation",                                   "Demographics for the whole nation",
  "Demographics",          "demographics-api-by-geography-type-and-geography-name",     "Demographics by geography type and name",
  "Demographics",          "demographics-api-by-geography-type-and-geography-id",       "Demographics by geography type and ID",
  "Demographics",          "demographics-api-by-coordinates",                           "Demographics by lat/lon coordinates",

  "Anchor Institutions",   "community-anchor-institutions-api-nation",                  "Broadband at anchor institutions for the US",
  "Anchor Institutions",   "community-anchor-institutions-api-by-geography-type-and-geography-name", "Anchor institutions by geography type and name",
  "Anchor Institutions",   "community-anchor-institutions-api-by-geography-type-and-geography-id",   "Anchor institutions by geography type and ID",
  "Anchor Institutions",   "community-anchor-institutions-api-by-coordinates",          "Anchor institutions near lat/lon coordinates",

  "Census",                "census-api-by-coordinates",                                 "Census block FIPS for lat/lon coordinates",
  "Census",                "census-api-number-of-census-blocks-by-geography-type",      "Number of census blocks by geography type",
  "Census",                "search-api-census-block-within-a-state",                    "Search census blocks within a state",

  "Broadband Summary",     "broadband-summary-api-by-geography-type-and-geography-name", "Broadband summary by geography type and name",
  "Broadband Summary",     "broadband-summary-api-by-coordinates",                      "Broadband summary by lat/lon coordinates",

  "Search",                "search-api-provider-by-name",                               "Search broadband providers by name",
  "Speed Test",            "speed-test-api-by-geography-type-and-geography-name-tech",  "Speed tests by geography, name, and technology"
)

# == Public functions ==========================================================

#' List all known broadbandmap.gov API endpoints (defunct)
#'
#' Returns a catalog of the 29 original API endpoints, now all defunct.
#' @return A tibble with category, endpoint_name, and description.
bb_list <- function() {
  .bb_endpoints
}

#' Search the broadbandmap.gov endpoint catalog
#'
#' @param query Character string to search for (case-insensitive, matches
#'   category, endpoint name, or description).
#' @return A tibble of matching endpoints.
bb_search <- function(query) {
  pattern <- tolower(query)
  .bb_endpoints |>
    dplyr::filter(
      grepl(pattern, tolower(category)) |
      grepl(pattern, tolower(endpoint_name)) |
      grepl(pattern, tolower(description))
    )
}

#' Return source code context for broadbandmap.gov client
#'
#' Reads this source file and returns the full text for use as LLM context.
#' @return A character string with the full source code.
bb_context <- function() {
  src <- tryCatch(
    readLines(sys.frame(0)$ofile %||%
      attr(body(bb_context), "srcfile")$filename %||%
      "clients/broadbandmap.gov.R"),
    error = function(e) "(source not available)"
  )
  paste(src, collapse = "\n")
}

#' Check availability of the broadbandmap.gov API
#'
#' Probes the old and new broadband map endpoints and reports their status.
#' @return A tibble with url, http_status, and notes.
bb_status <- function() {
  urls <- c(
    old_api     = "http://www.broadbandmap.gov/broadbandmap/broadband/jun2014/summary/api/nation?format=json",
    new_api     = "https://broadbandmap.fcc.gov/api/public/map/listAvailableNBMVintages",
    new_home    = "https://broadbandmap.fcc.gov/home"
  )
  results <- lapply(names(urls), function(nm) {
    status <- tryCatch({
      resp <- httr2::request(urls[[nm]]) |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_timeout(10) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
      httr2::resp_status(resp)
    }, error = function(e) NA_integer_)
    tibble::tibble(name = nm, url = urls[[nm]], http_status = status)
  })
  dplyr::bind_rows(results)
}

cat("broadbandmap.gov client loaded (API DEFUNCT since 2018).\n")
cat("Use bb_list() to see the 29 original endpoints.\n")
cat("Use bb_search(query) to search the endpoint catalog.\n")
cat("Use bb_status() to probe current endpoint availability.\n")
