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
#' Returns a catalog of the 29 original NTIA National Broadband Map API
#' endpoints. All endpoints were decommissioned by the FCC in December 2018.
#' This function serves as a reference for the original API surface.
#'
#' @return A tibble with 29 rows and columns:
#'   \describe{
#'     \item{category}{Character. Endpoint category: \code{"Broadband Summary"},
#'       \code{"Wireline Broadband"}, \code{"Wireless Broadband"},
#'       \code{"Speed Test"}, \code{"Geography Lookup"},
#'       \code{"Demographics"}, \code{"Anchor Institutions"},
#'       \code{"Census"}, or \code{"Search"}.}
#'     \item{endpoint_name}{Character. Original API endpoint name
#'       (e.g. \code{"broadband-summary-api-nation"}).}
#'     \item{description}{Character. Brief description of the endpoint's
#'       original function.}
#'   }
#' @examples
#' bb_list()
#' bb_list() |> dplyr::count(category)
#' @export
bb_list <- function() {
  .bb_endpoints
}

#' Search the broadbandmap.gov endpoint catalog
#'
#' Filters the catalog of 29 defunct NTIA endpoints by a keyword search
#' across category, endpoint name, and description fields. Matching is
#' case-insensitive.
#'
#' @param query Character. Search term (e.g. \code{"speed"}, \code{"census"},
#'   \code{"wireless"}).
#' @return A tibble with the same columns as \code{\link{bb_list}}: category,
#'   endpoint_name, description. Only rows matching the query are returned.
#' @examples
#' bb_search("speed")
#' bb_search("demographics")
#' @export
bb_search <- function(query) {
  pattern <- tolower(query)
  .bb_endpoints |>
    dplyr::filter(
      grepl(pattern, tolower(category)) |
      grepl(pattern, tolower(endpoint_name)) |
      grepl(pattern, tolower(description))
    )
}

#' Get broadbandmap.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/broadbandmap.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "broadbandmap.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# broadbandmap.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# broadbandmap.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
