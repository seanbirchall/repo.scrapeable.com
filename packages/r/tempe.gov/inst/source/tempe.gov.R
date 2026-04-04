# tempe.gov.R - City of Tempe AZ open data client (ArcGIS Hub)
#
# Data source: data.tempe.gov (ArcGIS Hub)
# Datasets: ~599 (police, crime, addresses, wastewater, fire, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tempe_base <- "https://data.tempe.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.fetch_csv <- function(url) {
  tmp <- tryCatch(.fetch(url, ext = ".csv"), error = function(e) NULL)
  if (is.null(tmp)) return(tibble())
  tryCatch(as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE)),
           error = function(e) tibble())
}

# == Known datasets ============================================================

.tempe_datasets <- list(
  arrests_90d    = list(id = "8931f2aae8f44e9786d6398c4bdc6151", layer = 0, name = "Police Arrests - Last 90 Days"),
  arrests_all    = list(id = "aeb0328cb1d6412b9f7a1fc65b056148", layer = 0, name = "Police Arrests - All Data"),
  offenses       = list(id = "1563be5b343b4f78b1163e97a9a503ad", layer = 0, name = "General Offenses"),
  addresses      = list(id = "9250a8606ddb4b0c869b58ed1525cd9e", layer = 0, name = "Addresses"),
  zip_codes      = list(id = "9e56f81e8af8469eb89a9d464d6ce59f", layer = 0, name = "Zip Code Boundaries"),
  covid_ww       = list(id = "c921c8af868f4e17b857c390543c5250", layer = 0, name = "Wastewater COVID-19 Results"),
  fire_survey    = list(id = "744963c36de8447195a0461b8beb435a", layer = 0, name = "Fire Services Survey")
)

# == Data access ===============================================================

#' List available Tempe datasets
#'
#' Returns a catalog of pre-configured City of Tempe AZ open data
#' datasets that can be downloaded with \code{\link{tempe_data}}. These
#' are curated datasets hosted on the ArcGIS Hub portal at
#' \url{https://data.tempe.gov}.
#'
#' @return A tibble with 4 columns:
#' \describe{
#'   \item{key}{Character. Short key for use with \code{\link{tempe_data}} (e.g. "arrests_90d").}
#'   \item{name}{Character. Human-readable dataset name.}
#'   \item{item_id}{Character. ArcGIS Hub item identifier.}
#'   \item{layer}{Integer. Layer number within the dataset.}
#' }
#' @examples
#' \dontrun{
#' tempe_datasets()
#' }
#' @seealso \code{\link{tempe_data}} to download a dataset by key,
#'   \code{\link{tempe_search}} to search the full portal.
#' @export
tempe_datasets <- function() {
  tibble(
    key     = names(.tempe_datasets),
    name    = vapply(.tempe_datasets, function(x) x$name, character(1)),
    item_id = vapply(.tempe_datasets, function(x) x$id, character(1)),
    layer   = vapply(.tempe_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Tempe open data portal
#'
#' Searches all datasets on the City of Tempe ArcGIS Hub portal by
#' keyword. Returns matching dataset metadata including titles,
#' descriptions, and direct URLs. The portal hosts approximately 599
#' datasets covering police, crime, addresses, wastewater, fire, and
#' other municipal data.
#'
#' @param q Character. Search query string (e.g. \code{"police"},
#'   \code{"water"}, \code{"crime"}). Default \code{""} returns all.
#' @return A tibble with 5 columns:
#' \describe{
#'   \item{id}{Character. Dataset or layer identifier.}
#'   \item{type}{Character. Resource type (e.g. "dataset").}
#'   \item{title}{Character. Human-readable dataset title.}
#'   \item{description}{Character. Brief description of the dataset.}
#'   \item{url}{Character. Direct URL to the dataset page.}
#' }
#' @examples
#' \dontrun{
#' tempe_search("police")
#' tempe_search("wastewater")
#' }
#' @export
tempe_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .tempe_base, utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(tibble())
  d <- raw$data
  tibble(
    id          = as.character(d$id),
    type        = as.character(d$type %||% NA_character_),
    title       = as.character(d$attributes$name %||% NA_character_),
    description = as.character(d$attributes$searchDescription %||% NA_character_),
    url         = as.character(d$attributes$url %||% NA_character_)
  )
}

#' Download a Tempe dataset as CSV
#'
#' Downloads a City of Tempe dataset from the ArcGIS Hub portal as CSV
#' and returns it as a tibble. Accepts either a short key from the
#' curated list (see \code{\link{tempe_datasets}}) or a raw ArcGIS Hub
#' item ID for any dataset on the portal.
#'
#' @param key Character. Either a dataset key from \code{\link{tempe_datasets}}
#'   (e.g. \code{"arrests_90d"}, \code{"offenses"}) or a raw ArcGIS Hub
#'   item ID (32-character hex string).
#' @param layer Integer or \code{NULL}. Layer number within the dataset.
#'   Only needed when using a raw item ID; ignored when using a curated key.
#'   Default \code{NULL} (uses layer 0).
#' @return A tibble with all records and fields from the dataset. Column
#'   names and types vary by dataset.
#' @examples
#' \dontrun{
#' tempe_data("arrests_90d")
#' tempe_data("offenses")
#' tempe_data("9250a8606ddb4b0c869b58ed1525cd9e")  # Addresses by item ID
#' }
#' @seealso \code{\link{tempe_datasets}} for available dataset keys.
#' @export
tempe_data <- function(key, layer = NULL) {
  if (key %in% names(.tempe_datasets)) {
    ds <- .tempe_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .tempe_base, item_id, lyr)
  .fetch_csv(url)
}

#' Get Tempe police arrest data (last 90 days)
#'
#' Convenience wrapper around \code{\link{tempe_data}} that downloads
#' police arrest records from the last 90 days. Includes arrest type,
#' location, charges, officer and arrestee demographics, and geo-coordinates.
#'
#' @return A tibble with approximately 40 columns including arrest_type,
#'   arrest_dt, location, district, zone, charge, severity_code,
#'   arrestee_age_range, and spatial coordinates. Column names vary as
#'   the dataset schema evolves.
#' @examples
#' \dontrun{
#' tempe_arrests()
#' }
#' @seealso \code{\link{tempe_offenses}} for general offense data.
#' @export
tempe_arrests <- function() {
  tempe_data("arrests_90d")
}

#' Get Tempe general offense crime data
#'
#' Convenience wrapper around \code{\link{tempe_data}} that downloads
#' general offense/crime records from the City of Tempe. Includes offense
#' type, location, date/time, and case details.
#'
#' @return A tibble with offense records. Column names and types vary as
#'   the dataset schema evolves.
#' @examples
#' \dontrun{
#' tempe_offenses()
#' }
#' @seealso \code{\link{tempe_arrests}} for arrest data.
#' @export
tempe_offenses <- function() {
  tempe_data("offenses")
}

# == Context ===================================================================

#' Get tempe.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
tempe_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(tempe_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/tempe.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "tempe.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# tempe.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# tempe.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
