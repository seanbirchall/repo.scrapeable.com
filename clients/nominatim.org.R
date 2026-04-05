# nominatim.org.R - Self-contained nominatim.org client

library(httr2)
library(jsonlite)
library(tibble)


# nominatim-org.R
# Self-contained Nominatim geocoding client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent required per TOS)
# API: https://nominatim.openstreetmap.org


.ua <- "scrapeable.com (support@scrapeable.com)"
.nom_base <- "https://nominatim.openstreetmap.org"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |> httr2::req_headers(`User-Agent` = .ua) |> httr2::req_perform(path = tmp)
  tmp
}
.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.schema_geocode <- tibble(
  place_id = integer(), osm_id = character(), display_name = character(),
  latitude = numeric(), longitude = numeric(), type = character(),
  importance = numeric()
)


#' Forward geocode -- search for places by name
#'
#' Converts a place name or address into geographic coordinates using the
#' OpenStreetMap Nominatim geocoding service. Returns multiple candidates
#' ranked by relevance.
#'
#' @param query Character. Place name or address (e.g., \code{"New York"},
#'   \code{"Eiffel Tower"}, \code{"1600 Pennsylvania Ave, Washington DC"}).
#' @param limit Integer. Maximum number of results (default 5, max 50).
#' @param countrycodes Character. Comma-separated ISO 3166-1 alpha-2 country
#'   codes to restrict results (e.g., \code{"us"}, \code{"us,ca"},
#'   \code{"gb,ie"}). Default NULL (worldwide).
#' @return A tibble with columns:
#'   \describe{
#'     \item{place_id}{integer -- Nominatim internal place ID}
#'     \item{osm_id}{character -- OpenStreetMap element ID}
#'     \item{display_name}{character -- full formatted address}
#'     \item{latitude}{numeric -- WGS84 latitude}
#'     \item{longitude}{numeric -- WGS84 longitude}
#'     \item{type}{character -- place type (e.g., \code{"administrative"}, \code{"city"}, \code{"attraction"})}
#'     \item{importance}{numeric -- relevance score between 0 and 1}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nom_search("Eiffel Tower")
#' nom_search("coffee shop", countrycodes = "us", limit = 10)
#' }
nom_search <- function(query, limit = 5, countrycodes = NULL) {
  url <- sprintf("%s/search?q=%s&format=json&limit=%d",
                 .nom_base, utils::URLencode(query, reserved = TRUE), limit)
  if (!is.null(countrycodes)) url <- paste0(url, "&countrycodes=", countrycodes)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Nominatim error: ", e$message); NULL })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_geocode)
  tibble(place_id = as.integer(raw$place_id), osm_id = as.character(raw$osm_id),
         display_name = as.character(raw$display_name),
         latitude = as.numeric(raw$lat), longitude = as.numeric(raw$lon),
         type = as.character(raw$type), importance = as.numeric(raw$importance))
}

#' Reverse geocode -- coordinates to address
#'
#' Converts geographic coordinates into a human-readable address using the
#' OpenStreetMap Nominatim reverse geocoding service.
#'
#' @param latitude Numeric. WGS84 latitude in decimal degrees (e.g., \code{40.7128}).
#' @param longitude Numeric. WGS84 longitude in decimal degrees (e.g., \code{-74.0060}).
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{place_id}{integer -- Nominatim internal place ID}
#'     \item{osm_id}{character -- OpenStreetMap element ID}
#'     \item{display_name}{character -- full formatted address}
#'     \item{latitude}{numeric -- WGS84 latitude (echoed back)}
#'     \item{longitude}{numeric -- WGS84 longitude (echoed back)}
#'     \item{type}{character -- place type (e.g., \code{"town_hall"}, \code{"residential"})}
#'     \item{importance}{numeric -- relevance score between 0 and 1}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nom_reverse(40.7128, -74.0060)
#' }
nom_reverse <- function(latitude, longitude) {
  url <- sprintf("%s/reverse?lat=%s&lon=%s&format=json", .nom_base, latitude, longitude)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Nominatim error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_geocode)
  tibble(place_id = as.integer(raw$place_id), osm_id = as.character(raw$osm_id),
         display_name = as.character(raw$display_name),
         latitude = as.numeric(raw$lat), longitude = as.numeric(raw$lon),
         type = as.character(raw$type %||% NA_character_), importance = as.numeric(raw$importance %||% NA_real_))
}

#' Get nominatim.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nom_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nom_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nominatim.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nominatim.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nominatim.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nominatim.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
