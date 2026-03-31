# nominatim-org.R
# Self-contained Nominatim geocoding client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent required per TOS)
# API: https://nominatim.openstreetmap.org

library(dplyr, warn.conflicts = FALSE)
library(tibble)

.ua <- "scrapeable.com (support@scrapeable.com)"
.nom_base <- "https://nominatim.openstreetmap.org"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE); n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi]); if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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

#' Forward geocode — search for places by name
#'
#' @param query Place name (e.g. "New York", "Eiffel Tower")
#' @param limit Max results (default 5, max 50)
#' @param countrycodes Limit to countries (e.g. "us,ca")
#' @return tibble: place_id, osm_id, display_name, latitude, longitude, type, importance
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

#' Reverse geocode — coordinates to address
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @return tibble: one row with place_id, osm_id, display_name, latitude, longitude, type, importance
nom_reverse <- function(latitude, longitude) {
  url <- sprintf("%s/reverse?lat=%s&lon=%s&format=json", .nom_base, latitude, longitude)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("Nominatim error: ", e$message); NULL })
  if (is.null(raw)) return(.schema_geocode)
  tibble(place_id = as.integer(raw$place_id), osm_id = as.character(raw$osm_id),
         display_name = as.character(raw$display_name),
         latitude = as.numeric(raw$lat), longitude = as.numeric(raw$lon),
         type = as.character(raw$type %||% NA_character_), importance = as.numeric(raw$importance %||% NA_real_))
}

#' Generate context
#' @return Character string (invisibly)
nom_context <- function() {
  .build_context("nominatim.org", header_lines = c(
    "# nominatim.org - OpenStreetMap Geocoding Client for R",
    "# Auth: none (User-Agent required per TOS)",
    "# Forward + reverse geocoding. Rate limit: 1 req/sec."
  ))
}
