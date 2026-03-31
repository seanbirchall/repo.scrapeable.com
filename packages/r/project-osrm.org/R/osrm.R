#' Calculate driving route between points
#'
#' @param coords Matrix or data frame with columns longitude, latitude.
#'   Or a list of c(lon, lat) vectors. Minimum 2 points.
#' @param profile "driving" (default), "walking", "cycling"
#' @param overview Route geometry: "full", "simplified" (default), or "false"
#' @return tibble: distance_m, duration_s, origin, destination
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

#' Calculate distance/duration matrix
#'
#' @param coords Matrix or data frame with lon, lat columns
#' @param profile "driving" (default)
#' @return tibble: origin_index, dest_index, duration_s, distance_m
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

#' Get nearest road point
#'
#' @param longitude Longitude
#' @param latitude Latitude
#' @param profile "driving" (default)
#' @return tibble: one row with snapped lon/lat, distance, name
#' @export
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
#' @export
osrm_context <- function() {
  .build_context("project-osrm.org", header_lines = c(
    "# project-osrm.org - OSRM Routing Client for R",
    "# Auth: none required. Profiles: driving, walking, cycling.",
    "# Coords as lon,lat (NOT lat,lon). Demo server - for production use own instance."
  ))
}
