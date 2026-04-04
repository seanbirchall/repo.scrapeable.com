# project-osrm-org.R
# Self-contained OSRM routing client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://router.project-osrm.org

library(dplyr, warn.conflicts = FALSE)
library(tibble)

.ua <- "support@scrapeable.com"
.osrm_base <- "https://router.project-osrm.org"

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
.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.schema_route <- tibble(
  distance_m = numeric(), duration_s = numeric(),
  origin = character(), destination = character()
)

.schema_table <- tibble(
  origin_index = integer(), dest_index = integer(),
  duration_s = numeric(), distance_m = numeric()
)

#' Calculate driving route between points
#'
#' @param coords Matrix or data frame with columns longitude, latitude.
#'   Or a list of c(lon, lat) vectors. Minimum 2 points.
#' @param profile "driving" (default), "walking", "cycling"
#' @param overview Route geometry: "full", "simplified" (default), or "false"
#' @return tibble: distance_m, duration_s, origin, destination
osrm_route <- function(coords, profile = "driving", overview = "simplified") {
  if (is.data.frame(coords) || is.matrix(coords)) {
    coord_str <- paste(sprintf("%s,%s", coords[, 1], coords[, 2]), collapse = ";")
  } else {
    coord_str <- paste(vapply(coords, function(c) paste(c, collapse = ","), character(1)), collapse = ";")
  }
  url <- sprintf("%s/route/v1/%s/%s?overview=%s&steps=false",
                 .osrm_base, profile, coord_str, overview)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("OSRM error: ", e$message); NULL })
  if (is.null(raw) || raw$code != "Ok") return(.schema_route)

  routes <- raw$routes
  if (is.null(routes) || length(routes) == 0) return(.schema_route)

  tibble(
    distance_m  = vapply(routes, function(r) as.numeric(r$distance %||% NA_real_), numeric(1)),
    duration_s  = vapply(routes, function(r) as.numeric(r$duration %||% NA_real_), numeric(1)),
    origin      = coord_str,
    destination = coord_str
  )
}

#' Calculate distance/duration matrix
#'
#' @param coords Matrix or data frame with lon, lat columns
#' @param profile "driving" (default)
#' @return tibble: origin_index, dest_index, duration_s, distance_m
osrm_table <- function(coords, profile = "driving") {
  if (is.data.frame(coords) || is.matrix(coords)) {
    coord_str <- paste(sprintf("%s,%s", coords[, 1], coords[, 2]), collapse = ";")
  } else {
    coord_str <- paste(vapply(coords, function(c) paste(c, collapse = ","), character(1)), collapse = ";")
  }
  url <- sprintf("%s/table/v1/%s/%s?annotations=duration,distance",
                 .osrm_base, profile, coord_str)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("OSRM error: ", e$message); NULL })
  if (is.null(raw) || raw$code != "Ok") return(.schema_table)

  durations <- raw$durations
  distances <- raw$distances
  n <- length(durations)
  results <- list()
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      results[[length(results) + 1]] <- tibble(
        origin_index = as.integer(i), dest_index = as.integer(j),
        duration_s = as.numeric(durations[[i]][[j]] %||% NA_real_),
        distance_m = as.numeric(distances[[i]][[j]] %||% NA_real_)
      )
    }
  }
  bind_rows(results)
}

#' Get nearest road point
#'
#' @param longitude Longitude
#' @param latitude Latitude
#' @param profile "driving" (default)
#' @return tibble: one row with snapped lon/lat, distance, name
osrm_nearest <- function(longitude, latitude, profile = "driving") {
  url <- sprintf("%s/nearest/v1/%s/%s,%s", .osrm_base, profile, longitude, latitude)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("OSRM error: ", e$message); NULL })
  if (is.null(raw) || raw$code != "Ok") return(tibble(longitude = numeric(), latitude = numeric(), distance = numeric(), name = character()))
  w <- raw$waypoints[[1]]
  tibble(longitude = as.numeric(w$location[[1]]), latitude = as.numeric(w$location[[2]]),
         distance = as.numeric(w$distance), name = w$name %||% NA_character_)
}

#' Generate context
#' @return Character string (invisibly)
osrm_context <- function() {
  .build_context("project-osrm.org", header_lines = c(
    "# project-osrm.org - OSRM Routing Client for R",
    "# Auth: none required. Profiles: driving, walking, cycling.",
    "# Coords as lon,lat (NOT lat,lon). Demo server - for production use own instance."
  ))
}
