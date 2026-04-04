# project-osrm-org.R
# Self-contained OSRM routing client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://router.project-osrm.org


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Queries the OSRM (Open Source Routing Machine) demo server to calculate
#' the shortest path between two or more waypoints on the road network.
#'
#' @param coords Matrix or data frame with columns longitude, latitude.
#'   Or a list of c(lon, lat) vectors. Minimum 2 points.
#' @param profile Routing profile: "driving" (default), "walking", "cycling"
#' @param overview Route geometry detail: "full", "simplified" (default),
#'   or "false" (no geometry)
#' @return A tibble with columns:
#'   \describe{
#'     \item{distance_m}{Total route distance in meters (numeric)}
#'     \item{duration_s}{Estimated travel time in seconds (numeric)}
#'     \item{origin}{Coordinate string for origin (character)}
#'     \item{destination}{Coordinate string for destination (character)}
#'   }
#' @examples
#' osrm_route(list(c(-73.99, 40.74), c(-73.96, 40.78)))
#' @seealso [osrm_table()], [osrm_nearest()], [osrm_context()]
#' @source <https://router.project-osrm.org>
#' @export
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

#' Calculate distance/duration matrix between points
#'
#' Computes a full origin-destination matrix of travel times and distances
#' between all pairs of input coordinates using OSRM.
#'
#' @param coords Matrix or data frame with lon, lat columns, or a list
#'   of c(lon, lat) vectors
#' @param profile Routing profile: "driving" (default)
#' @return A tibble with one row per origin-destination pair and columns:
#'   \describe{
#'     \item{origin_index}{1-based index of origin point (integer)}
#'     \item{dest_index}{1-based index of destination point (integer)}
#'     \item{duration_s}{Travel time in seconds (numeric)}
#'     \item{distance_m}{Distance in meters (numeric)}
#'   }
#' @examples
#' pts <- list(c(-73.99, 40.74), c(-73.96, 40.78), c(-74.01, 40.71))
#' osrm_table(pts)
#' @seealso [osrm_route()], [osrm_nearest()], [osrm_context()]
#' @source <https://router.project-osrm.org>
#' @export
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

#' Snap a coordinate to the nearest road
#'
#' Finds the nearest point on the road network to a given coordinate.
#' Useful for snapping GPS coordinates to the street network.
#'
#' @param longitude Longitude in decimal degrees (numeric)
#' @param latitude Latitude in decimal degrees (numeric)
#' @param profile Routing profile: "driving" (default)
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{longitude}{Snapped longitude on road (numeric)}
#'     \item{latitude}{Snapped latitude on road (numeric)}
#'     \item{distance}{Distance from input to snapped point in meters (numeric)}
#'     \item{name}{Street name at snapped location (character)}
#'   }
#' @examples
#' osrm_nearest(-73.99, 40.74)
#' @seealso [osrm_route()], [osrm_table()], [osrm_context()]
#' @source <https://router.project-osrm.org>
#' @export
osrm_nearest <- function(longitude, latitude, profile = "driving") {
  url <- sprintf("%s/nearest/v1/%s/%s,%s", .osrm_base, profile, longitude, latitude)
  raw <- tryCatch(.fetch_json(url), error = function(e) { warning("OSRM error: ", e$message); NULL })
  if (is.null(raw) || raw$code != "Ok") return(tibble(longitude = numeric(), latitude = numeric(), distance = numeric(), name = character()))
  w <- raw$waypoints[[1]]
  tibble(longitude = as.numeric(w$location[[1]]), latitude = as.numeric(w$location[[2]]),
         distance = as.numeric(w$distance), name = w$name %||% NA_character_)
}

#' Get project-osrm-org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
osrm_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(osrm_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/project-osrm-org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "project-osrm-org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# project-osrm-org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# project-osrm-org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
