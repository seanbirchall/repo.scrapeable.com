#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom httr2 req_timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# govinfo.gov.R - Self-contained GovInfo API client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: uses DEMO_KEY by default; set GOVINFO_API_KEY env var for higher limits
# Rate limits: DEMO_KEY = 40/hr, registered key = 1000/hr


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.govinfo_base <- "https://api.govinfo.gov"

`%||%` <- function(a, b) if (is.null(a)) b else a

.govinfo_key <- function() {
  Sys.getenv("GOVINFO_API_KEY", unset = "DEMO_KEY")
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 2, max_seconds = 10) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_collection <- tibble(
  collectionCode = character(), collectionName = character(),
  packageCount = integer(), granuleCount = integer()
)

.schema_package <- tibble(
  packageId = character(), title = character(),
  collectionCode = character(), dateIssued = as.Date(character()),
  lastModified = character()
)

.schema_search <- tibble(
  title = character(), packageId = character(),
  collectionCode = character(), dateIssued = as.Date(character()),
  governmentAuthor = character(), category = character()
)

