#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom httr2 req_timeout
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# sciencebase.gov.R - Self-contained USGS ScienceBase catalog client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public catalog)
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sb_base <- "https://www.sciencebase.gov/catalog"

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

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

.collapse_tags <- function(tags_list) {
  if (is.null(tags_list)) return(NA_character_)
  if (is.data.frame(tags_list)) return(paste(tags_list$name, collapse = "; "))
  if (is.list(tags_list)) {
    return(vapply(tags_list, function(x) {
      if (is.data.frame(x)) paste(x$name, collapse = "; ")
      else if (is.list(x)) paste(vapply(x, function(z) z$name %||% "", character(1)), collapse = "; ")
      else NA_character_
    }, character(1)))
  }
  NA_character_
}

# == Schemas ===================================================================

.schema_item <- tibble(
  id = character(), title = character(), summary = character(),
  hasChildren = logical(), dateCreated = as.POSIXct(character()),
  lastUpdated = as.POSIXct(character()), tags = character()
)

.schema_item_detail <- tibble(
  id = character(), title = character(), summary = character(),
  hasChildren = logical(), dateCreated = as.POSIXct(character()),
  lastUpdated = as.POSIXct(character()), tags = character(),
  contacts = character(), files = character()
)

