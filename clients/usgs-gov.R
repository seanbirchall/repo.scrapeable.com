# usgs-gov.R
# Self-contained USGS earthquake data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://earthquake.usgs.gov/fdsnws/event/1

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.usgs_base <- "https://earthquake.usgs.gov/fdsnws/event/1"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
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
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_quakes <- tibble(
  id = character(), time = as.POSIXct(character()), mag = numeric(),
  place = character(), longitude = numeric(), latitude = numeric(),
  depth = numeric(), type = character(), status = character(),
  url = character()
)

# == Earthquakes ===============================================================

#' Query USGS earthquake data
#'
#' Returns earthquake events from the USGS FDSN web service.
#'
#' @param starttime Start date (Date or "YYYY-MM-DD")
#' @param endtime End date (Date or "YYYY-MM-DD"). Default: today.
#' @param minmagnitude Minimum magnitude (default 2.5)
#' @param maxmagnitude Maximum magnitude
#' @param limit Max results (default 100, max 20000)
#' @param latitude Center latitude for radius search
#' @param longitude Center longitude for radius search
#' @param maxradiuskm Max radius in km (requires lat/lon)
#' @param orderby Sort: "time" (default), "time-asc", "magnitude", "magnitude-asc"
#' @return tibble: id, time (POSIXct), mag, place, longitude, latitude,
#'   depth, type, status, url
usgs_quakes <- function(starttime = NULL, endtime = NULL,
                        minmagnitude = 2.5, maxmagnitude = NULL,
                        limit = 100, latitude = NULL, longitude = NULL,
                        maxradiuskm = NULL, orderby = "time") {
  params <- list(format = "geojson", limit = limit,
                 minmagnitude = minmagnitude, orderby = orderby)
  if (!is.null(starttime)) params$starttime <- as.character(starttime)
  if (!is.null(endtime))   params$endtime <- as.character(endtime)
  if (!is.null(maxmagnitude)) params$maxmagnitude <- maxmagnitude
  if (!is.null(latitude))   params$latitude <- latitude
  if (!is.null(longitude))  params$longitude <- longitude
  if (!is.null(maxradiuskm)) params$maxradiuskm <- maxradiuskm

  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.usgs_base, "/query?", query)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("USGS API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_quakes)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_quakes)

  tibble(
    id        = vapply(features, function(f) f$id %||% NA_character_, character(1)),
    time      = as.POSIXct(vapply(features, function(f) {
      (f$properties$time %||% NA_real_) / 1000
    }, numeric(1)), origin = "1970-01-01", tz = "UTC"),
    mag       = vapply(features, function(f) as.numeric(f$properties$mag %||% NA_real_), numeric(1)),
    place     = vapply(features, function(f) f$properties$place %||% NA_character_, character(1)),
    longitude = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    latitude  = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    depth     = vapply(features, function(f) as.numeric(f$geometry$coordinates[[3]] %||% NA_real_), numeric(1)),
    type      = vapply(features, function(f) f$properties$type %||% NA_character_, character(1)),
    status    = vapply(features, function(f) f$properties$status %||% NA_character_, character(1)),
    url       = vapply(features, function(f) f$properties$url %||% NA_character_, character(1))
  )
}


#' Get significant earthquakes in the last 30 days
#'
#' @return tibble: same columns as usgs_quakes
usgs_significant <- function() {
  url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/significant_month.geojson"
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("USGS API error: ", e$message); NULL
  })
  if (is.null(raw)) return(.schema_quakes)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_quakes)

  tibble(
    id        = vapply(features, function(f) f$id %||% NA_character_, character(1)),
    time      = as.POSIXct(vapply(features, function(f) (f$properties$time %||% NA_real_) / 1000, numeric(1)),
                           origin = "1970-01-01", tz = "UTC"),
    mag       = vapply(features, function(f) as.numeric(f$properties$mag %||% NA_real_), numeric(1)),
    place     = vapply(features, function(f) f$properties$place %||% NA_character_, character(1)),
    longitude = vapply(features, function(f) as.numeric(f$geometry$coordinates[[1]] %||% NA_real_), numeric(1)),
    latitude  = vapply(features, function(f) as.numeric(f$geometry$coordinates[[2]] %||% NA_real_), numeric(1)),
    depth     = vapply(features, function(f) as.numeric(f$geometry$coordinates[[3]] %||% NA_real_), numeric(1)),
    type      = vapply(features, function(f) f$properties$type %||% NA_character_, character(1)),
    status    = vapply(features, function(f) f$properties$status %||% NA_character_, character(1)),
    url       = vapply(features, function(f) f$properties$url %||% NA_character_, character(1))
  )
}


#' Generate context
#' @return Character string (invisibly)
usgs_context <- function() {
  .build_context("usgs.gov", header_lines = c(
    "# usgs.gov - USGS Earthquake Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# GeoJSON format, magnitude/location/time filters"
  ))
}
