# philart.net.R - Philadelphia Public Art API client
# Self-contained. All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API docs: http://philart.net/api.html

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.philart_base <- "https://www.philart.net/api"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_art <- tibble(
  id = integer(), name = character()
)

.schema_art_detail <- tibble(
  id = integer(), title = character(), artists = character(),
  year = character(), latitude = double(), longitude = double(),
  location_description = character(), architecture = character(),
  description = character(), image_url = character()
)

.schema_artists <- tibble(
  id = integer(), name = character()
)

# == Public functions ==========================================================

#' List all Philadelphia public art titles
#'
#' Returns a tibble of all artworks with ID and name.
#'
#' @return tibble: id, name
#' @export
philart_list <- function() {
  url <- paste0(.philart_base, "/art.json")
  raw <- .fetch_json(url)
  items <- raw$body$list
  if (is.null(items) || length(items) == 0) return(.schema_art)

  tibble(
    id   = vapply(items, function(x) {
      href <- x$links[[1]]$href %||% ""
      as.integer(sub(".*/(\\d+)\\.json$", "\\1", href))
    }, integer(1)),
    name = vapply(items, function(x) as.character(x$name %||% NA), character(1))
  )
}

#' Get detail for a single artwork by ID
#'
#' @param id Artwork ID (integer)
#' @return tibble: single row with id, title, artists, year, latitude,
#'   longitude, location_description, architecture, description, image_url
#' @export
philart_detail <- function(id) {
  url <- sprintf("%s/art/%d.json", .philart_base, as.integer(id))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$body)) return(.schema_art_detail)

  b <- raw$body
  artists <- if (!is.null(b$artists) && length(b$artists) > 0) {
    paste(vapply(b$artists, function(a) a$name %||% NA_character_, character(1)),
          collapse = "; ")
  } else NA_character_

  year <- if (!is.null(b$years) && length(b$years) > 0) {
    paste(vapply(b$years, function(y) y$year %||% NA_character_, character(1)),
          collapse = ", ")
  } else NA_character_

  loc <- b$location
  arch <- if (!is.null(b$architecture)) b$architecture$description else NA_character_

  # Extract description from body items
  desc <- NA_character_
  if (!is.null(b$description)) {
    desc <- as.character(b$description)
  }

  # Image URL
  img <- NA_character_
  if (!is.null(b$images) && length(b$images) > 0) {
    img <- b$images[[1]]$src %||% b$images[[1]]$url %||% NA_character_
  }

  tibble(
    id                   = as.integer(id),
    title                = as.character(b$title$display %||% b$title %||% NA),
    artists              = artists,
    year                 = year,
    latitude             = as.double(loc$latitude %||% NA),
    longitude            = as.double(loc$longitude %||% NA),
    location_description = as.character(loc$description %||% NA),
    architecture         = arch,
    description          = desc,
    image_url            = img
  )
}

#' Browse multiple artworks with detail
#'
#' Fetches details for a batch of artworks. By default fetches the first 20.
#'
#' @param limit Number of artworks to fetch (default 20, max ~1600)
#' @param offset Number of artworks to skip (default 0)
#' @return tibble: id, title, artists, year, latitude, longitude,
#'   location_description, architecture, description, image_url
#' @export
philart_artworks <- function(limit = 20, offset = 0) {
  all_art <- philart_list()
  if (nrow(all_art) == 0) return(.schema_art_detail)

  subset <- all_art[(offset + 1):min(offset + limit, nrow(all_art)), ]
  results <- lapply(subset$id, function(aid) {
    tryCatch(philart_detail(aid), error = function(e) NULL)
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) return(.schema_art_detail)
  bind_rows(results)
}

#' List all artists in Philadelphia public art collection
#'
#' @return tibble: id, name
#' @export
philart_artists <- function() {
  url <- paste0(.philart_base, "/artists.json")
  raw <- .fetch_json(url)
  items <- raw$body$list
  if (is.null(items) || length(items) == 0) return(.schema_artists)

  tibble(
    id   = vapply(items, function(x) {
      href <- x$links[[1]]$href %||% ""
      as.integer(sub(".*/(\\d+)\\.json$", "\\1", href))
    }, integer(1)),
    name = vapply(items, function(x) as.character(x$name %||% NA), character(1))
  )
}

#' Search artworks by keyword
#'
#' Filters the artwork list by name matching.
#'
#' @param query Search string (case-insensitive)
#' @return tibble: id, name
#' @export
philart_search <- function(query) {
  all_art <- philart_list()
  if (nrow(all_art) == 0) return(all_art)
  q <- tolower(query)
  all_art[grepl(q, tolower(all_art$name), fixed = TRUE), ]
}

#' Find artworks near a location
#'
#' Uses the philart.net geo endpoint to find nearby artworks.
#'
#' @param lat Latitude (default 39.9524, Center City)
#' @param lon Longitude (default -75.1657, Center City)
#' @param radius_deg Bounding box half-size in degrees (default 0.005)
#' @return tibble: id, name
#' @export
philart_nearby <- function(lat = 39.9524, lon = -75.1657, radius_deg = 0.005) {
  bb <- sprintf("%.6f,%.6f,%.6f,%.6f",
                lat - radius_deg, lon - radius_deg,
                lat + radius_deg, lon + radius_deg)
  url <- sprintf("%s/geo.json?ll=%.6f,%.6f&bb=%s", .philart_base, lat, lon, bb)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$body) || is.null(raw$body$list)) return(.schema_art)

  items <- raw$body$list
  if (length(items) == 0) return(.schema_art)

  tibble(
    id   = vapply(items, function(x) {
      href <- x$links[[1]]$href %||% ""
      as.integer(sub(".*/(\\d+)\\.json$", "\\1", href))
    }, integer(1)),
    name = vapply(items, function(x) as.character(x$name %||% NA), character(1))
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for philart.net
#'
#' @return Character string with full function signatures and bodies
#' @export
philart_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/philart.net.R"
  if (!file.exists(src_file)) {
    cat("# philart.net context - source not found\n")
    return(invisible("# philart.net context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
