# philart.net.R - Philadelphia Public Art API client
# Self-contained. All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API docs: http://philart.net/api.html


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
#' Returns a complete catalog of all public artworks registered in the
#' Philadelphia Public Art collection (~1,255 works). Each artwork has a
#' unique integer ID that can be passed to \code{philart_detail()} for
#' full metadata including artist, year, location, and image.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Unique artwork identifier.}
#'     \item{name}{Character. Artwork title.}
#'   }
#'
#' @examples
#' arts <- philart_list()
#' nrow(arts)   # ~1255
#'
#' @seealso \code{\link{philart_detail}}, \code{\link{philart_search}},
#'   \code{\link{philart_artworks}}
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
#' Fetches full metadata for one Philadelphia public artwork from the
#' philart.net JSON API. Returns a single-row tibble with artist names,
#' year(s), geographic coordinates, textual description, and an image URL
#' when available.
#'
#' @param id Integer. Artwork ID (obtain from \code{philart_list()} or
#'   \code{philart_search()}).
#'
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Artwork ID.}
#'     \item{title}{Character. Display title.}
#'     \item{artists}{Character. Semicolon-separated artist names.}
#'     \item{year}{Character. Year(s) of creation, comma-separated.}
#'     \item{latitude}{Numeric. Latitude (WGS 84).}
#'     \item{longitude}{Numeric. Longitude (WGS 84).}
#'     \item{location_description}{Character. Textual location description.}
#'     \item{architecture}{Character. Architectural context, if any.}
#'     \item{description}{Character. Description of the artwork.}
#'     \item{image_url}{Character. URL to a representative image.}
#'   }
#'
#' @examples
#' philart_detail(111)
#'
#' @seealso \code{\link{philart_list}}, \code{\link{philart_artworks}}
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

#' Browse multiple artworks with full detail
#'
#' Fetches detailed metadata for a batch of artworks by iterating over the
#' artwork catalog and calling the per-artwork detail endpoint. Each
#' artwork requires a separate HTTP request, so large batches will be
#' slow; keep \code{limit} reasonable (20--50).
#'
#' @param limit Integer. Number of artworks to fetch (default 20).
#'   The full catalog contains ~1,255 works.
#' @param offset Integer. Number of artworks to skip from the beginning
#'   of the sorted catalog (default 0). Use for pagination.
#'
#' @return A tibble with the same columns as \code{philart_detail()}:
#'   \code{id}, \code{title}, \code{artists}, \code{year}, \code{latitude},
#'   \code{longitude}, \code{location_description}, \code{architecture},
#'   \code{description}, \code{image_url}.
#'
#' @examples
#' # First 5 artworks with full detail
#' philart_artworks(limit = 5)
#'
#' @seealso \code{\link{philart_detail}}, \code{\link{philart_list}}
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

#' List all artists in the Philadelphia public art collection
#'
#' Returns a tibble of every artist represented in the Philadelphia Public
#' Art database, with a unique integer ID and full name. The list is
#' fetched from the \code{/api/artists.json} endpoint.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Unique artist identifier.}
#'     \item{name}{Character. Artist name.}
#'   }
#'
#' @examples
#' artists <- philart_artists()
#' head(artists, 10)
#'
#' @seealso \code{\link{philart_list}}, \code{\link{philart_detail}}
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

#' Search Philadelphia public artworks by keyword
#'
#' Performs a case-insensitive substring match against artwork titles.
#' Downloads the full artwork catalog via \code{philart_list()} and
#' filters locally. Use \code{philart_detail()} on matching IDs to
#' retrieve full metadata.
#'
#' @param query Character. Search string matched against artwork names
#'   (case-insensitive, substring match).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Artwork identifier.}
#'     \item{name}{Character. Artwork title.}
#'   }
#'
#' @examples
#' philart_search("eagle")
#' philart_search("fountain")
#'
#' @seealso \code{\link{philart_list}}, \code{\link{philart_detail}}
#' @export
philart_search <- function(query) {
  all_art <- philart_list()
  if (nrow(all_art) == 0) return(all_art)
  q <- tolower(query)
  all_art[grepl(q, tolower(all_art$name), fixed = TRUE), ]
}

#' Find artworks near a geographic location
#'
#' Queries the philart.net geo endpoint to discover public artworks within
#' a bounding box around the specified coordinates. The default location is
#' Center City Philadelphia. Increase \code{radius_deg} to widen the search
#' area (0.01 degrees is roughly 1.1 km).
#'
#' @param lat Numeric. Latitude in decimal degrees (default 39.9524,
#'   Center City Philadelphia).
#' @param lon Numeric. Longitude in decimal degrees (default -75.1657,
#'   Center City Philadelphia).
#' @param radius_deg Numeric. Half-size of the bounding box in degrees
#'   (default 0.005, approximately 550 meters).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Artwork identifier.}
#'     \item{name}{Character. Artwork title.}
#'   }
#'
#' @examples
#' # Art near Philadelphia Museum of Art
#' philart_nearby(lat = 39.9656, lon = -75.1810, radius_deg = 0.01)
#'
#' @seealso \code{\link{philart_detail}}, \code{\link{philart_search}}
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

#' Get philart.net client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
philart_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(philart_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/philart.net.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "philart.net")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# philart.net context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# philart.net", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
