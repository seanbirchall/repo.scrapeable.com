#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom httr2 req_timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# githubusercontent.com.R - Self-contained GitHub raw content client
# Fetches open government datasets hosted on raw.githubusercontent.com
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: standard GitHub rate limits


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ghraw_base <- "https://raw.githubusercontent.com"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.fetch_csv <- function(url) {
  tmp <- .fetch(url, ext = ".csv")
  utils::read.csv(tmp, stringsAsFactors = FALSE) |> tibble::as_tibble()
}

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_nasa_project <- tibble(
  name = character(), description = character(),
  organization = character(), repositoryURL = character(),
  homepageURL = character(), tags = character(),
  languages = character(), metadataLastUpdated = as.Date(character())
)

.schema_gsa_standard <- tibble(
  id = character(), name = character(), category = character(),
  status = character(), description = character(),
  deployment_type = character()
)

