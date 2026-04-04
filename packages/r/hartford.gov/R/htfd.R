# hartford.gov.R - City of Hartford CT open data client (ArcGIS Hub)
#
# Data source: data.hartford.gov (ArcGIS Hub)
# Datasets: ~105 (food licenses, service requests, zoning, buildings, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.htfd_base <- "https://data.hartford.gov"

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

# == Known datasets (item_id, layer, name) =====================================

.htfd_datasets <- list(
  food_licenses    = list(id = "27139a92097e4e6a957646fb633e1e71", layer = 2, name = "Food Establishments Licenses"),
  service_requests = list(id = "2185af186dda46caa1e59323407d1daf", layer = 9, name = "Service Requests Current Year"),
  zoning           = list(id = "0b899d5a56bc4791a66688a366ce9ea6", layer = 4, name = "Zoning Districts"),
  city_property    = list(id = "8e4ef8c879594a6f8f01805e7d0b0235", layer = 0, name = "City Owned Property"),
  buildings        = list(id = "493ed7fa65414636bb497504d3db1c80", layer = 29, name = "Buildings"),
  parking_lots     = list(id = "5272d58a79c7452b94b0c27e4a50032d", layer = 27, name = "Parking Lots")
)

# == Data access ===============================================================

#' List available Hartford datasets
#'
#' Returns the catalog of pre-configured Hartford open data datasets
#' with their ArcGIS Hub item IDs and layer numbers.
#'
#' @return A tibble with columns: \code{key} (character, short name for use
#'   with \code{\link{htfd_data}}), \code{name} (character, human-readable
#'   title), \code{item_id} (character, ArcGIS Hub UUID), \code{layer}
#'   (integer, layer index).
#' @examples
#' \dontrun{
#' htfd_datasets()
#' }
#' @export
htfd_datasets <- function() {
  tibble(
    key     = names(.htfd_datasets),
    name    = vapply(.htfd_datasets, function(x) x$name, character(1)),
    item_id = vapply(.htfd_datasets, function(x) x$id, character(1)),
    layer   = vapply(.htfd_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Hartford open data portal
#'
#' Searches the Hartford ArcGIS Hub portal for datasets matching a query.
#'
#' @param q Character. Search query (e.g. \code{"food"}, \code{"zoning"},
#'   \code{"parking"}, \code{"property"}).
#' @return A tibble with columns: \code{id} (character, dataset UUID),
#'   \code{type} (character, e.g. \code{"dataset"}), \code{title} (character),
#'   \code{description} (character), \code{url} (character, link to dataset page).
#' @examples
#' \dontrun{
#' htfd_search("food")
#' htfd_search("zoning")
#' }
#' @export
htfd_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .htfd_base, utils::URLencode(q))
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

#' Download a Hartford dataset as CSV
#'
#' Downloads and parses a Hartford open data dataset. Can use a short key
#' from \code{\link{htfd_datasets}} or a raw ArcGIS Hub item ID.
#'
#' @param key Character. Dataset key from \code{htfd_datasets()} (e.g.
#'   \code{"food_licenses"}, \code{"service_requests"}, \code{"zoning"},
#'   \code{"city_property"}, \code{"buildings"}, \code{"parking_lots"}),
#'   or a raw ArcGIS Hub item ID (UUID string).
#' @param layer Integer or NULL. Layer number (only needed if using a raw
#'   item_id; ignored when using a known key).
#' @return A tibble with dataset-specific columns. For example,
#'   \code{"food_licenses"} returns: \code{OBJECTID}, \code{Record_ID},
#'   \code{ParcelID}, \code{Full_Address}, \code{Unit}, \code{DBA},
#'   \code{Record_Status}, \code{Classification}, \code{Seating_Capacity},
#'   \code{GlobalID}.
#' @examples
#' \dontrun{
#' htfd_data("food_licenses")
#' htfd_data("city_property")
#' }
#' @export
htfd_data <- function(key, layer = NULL) {
  if (key %in% names(.htfd_datasets)) {
    ds <- .htfd_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .htfd_base, item_id, lyr)
  .fetch_csv(url)
}

#' Download Hartford dataset as GeoJSON
#'
#' Downloads a Hartford open data dataset in GeoJSON format, suitable
#' for spatial analysis. Uses the same keys as \code{\link{htfd_data}}.
#'
#' @param key Character. Dataset key from \code{htfd_datasets()} (e.g.
#'   \code{"zoning"}, \code{"buildings"}, \code{"parking_lots"})
#'   or a raw ArcGIS Hub item ID.
#' @param layer Integer or NULL. Layer number (only needed if using a raw
#'   item_id).
#' @return A list representing parsed GeoJSON with \code{type},
#'   \code{features}, etc. Returns \code{NULL} on error.
#' @examples
#' \dontrun{
#' htfd_geojson("zoning")
#' htfd_geojson("buildings")
#' }
#' @export
htfd_geojson <- function(key, layer = NULL) {
  if (key %in% names(.htfd_datasets)) {
    ds <- .htfd_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/geojson?layers=%d", .htfd_base, item_id, lyr)
  tryCatch(.fetch_json(url), error = function(e) NULL)
}

# == Context ===================================================================

#' Get hartford.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
htfd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(htfd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/hartford.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "hartford.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# hartford.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# hartford.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
