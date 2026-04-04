#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# gbif.org.R - Self-contained GBIF (Global Biodiversity Information Facility) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gbif_base <- "https://api.gbif.org/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.build_url <- function(path, params = list()) {
  params <- params[!vapply(params, is.null, logical(1))]
  url <- paste0(.gbif_base, path)
  if (length(params) > 0) {
    pairs <- paste0(names(params), "=", vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), character(1)))
    url <- paste0(url, "?", paste(pairs, collapse = "&"))
  }
  url
}

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_occurrence <- tibble(
  key = numeric(), scientificName = character(), species = character(),
  family = character(), order = character(), class = character(),
  phylum = character(), kingdom = character(),
  decimalLatitude = numeric(), decimalLongitude = numeric(),
  country = character(), year = integer(), month = integer(),
  basisOfRecord = character(), datasetName = character(),
  institutionCode = character()
)

.schema_species <- tibble(
  key = integer(), scientificName = character(), canonicalName = character(),
  rank = character(), taxonomicStatus = character(),
  kingdom = character(), phylum = character(), class = character(),
  order = character(), family = character(), genus = character(),
  numOccurrences = integer()
)

.schema_dataset <- tibble(
  key = character(), title = character(), type = character(),
  description = character(), publishingCountry = character(),
  license = character(), recordCount = integer()
)

