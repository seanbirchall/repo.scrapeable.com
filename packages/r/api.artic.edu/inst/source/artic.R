# api-artic-edu.R
# Self-contained Art Institute of Chicago API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: 60 requests per minute

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.artic_base <- "https://api.artic.edu/api/v1"

`%||%` <- function(x, y) if (is.null(x)) y else x

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
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_artworks <- tibble(
  id = integer(), title = character(), artist = character(),
  date_display = character(), medium = character(), department = character(),
  place_of_origin = character(), is_public_domain = logical(),
  image_id = character()
)

# == Public functions ==========================================================

#' Browse Art Institute of Chicago artworks
#'
#' @param limit Number of artworks per page (default 20, max 100)
#' @param page Page number (1-indexed, default 1)
#' @param fields Optional comma-separated fields to return
#' @return tibble: id, title, artist, date_display, medium, department,
#'   place_of_origin, is_public_domain, image_id
artic_artworks <- function(limit = 20, page = 1, fields = NULL) {
  url <- sprintf("%s/artworks?limit=%d&page=%d", .artic_base, limit, page)
  if (!is.null(fields)) url <- paste0(url, "&fields=", utils::URLencode(fields))
  else url <- paste0(url, "&fields=id,title,artist_display,date_display,medium_display,department_title,place_of_origin,is_public_domain,image_id")

  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_artworks)

  as_tibble(data) |>
    transmute(
      id = as.integer(id),
      title = as.character(if ("title" %in% names(data)) title else NA),
      artist = as.character(if ("artist_display" %in% names(data)) artist_display else NA),
      date_display = as.character(if ("date_display" %in% names(data)) date_display else NA),
      medium = as.character(if ("medium_display" %in% names(data)) medium_display else NA),
      department = as.character(if ("department_title" %in% names(data)) department_title else NA),
      place_of_origin = as.character(if ("place_of_origin" %in% names(data)) place_of_origin else NA),
      is_public_domain = as.logical(if ("is_public_domain" %in% names(data)) is_public_domain else NA),
      image_id = as.character(if ("image_id" %in% names(data)) image_id else NA)
    )
}

#' Search Art Institute of Chicago artworks
#'
#' @param query Search query string
#' @param limit Maximum results (default 20, max 100)
#' @return tibble: id, title, artist, date_display, medium, department,
#'   place_of_origin, is_public_domain, image_id
artic_search <- function(query, limit = 20) {
  url <- sprintf("%s/artworks/search?q=%s&limit=%d&fields=id,title,artist_display,date_display,medium_display,department_title,place_of_origin,is_public_domain,image_id",
                 .artic_base, utils::URLencode(query), limit)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_artworks)

  as_tibble(data) |>
    transmute(
      id = as.integer(id),
      title = as.character(if ("title" %in% names(data)) title else NA),
      artist = as.character(if ("artist_display" %in% names(data)) artist_display else NA),
      date_display = as.character(if ("date_display" %in% names(data)) date_display else NA),
      medium = as.character(if ("medium_display" %in% names(data)) medium_display else NA),
      department = as.character(if ("department_title" %in% names(data)) department_title else NA),
      place_of_origin = as.character(if ("place_of_origin" %in% names(data)) place_of_origin else NA),
      is_public_domain = as.logical(if ("is_public_domain" %in% names(data)) is_public_domain else NA),
      image_id = as.character(if ("image_id" %in% names(data)) image_id else NA)
    )
}

#' Fetch a single artwork by ID
#'
#' @param id Artwork ID (integer)
#' @return tibble: single row with artwork details
artic_artwork <- function(id) {
  url <- sprintf("%s/artworks/%d?fields=id,title,artist_display,date_display,medium_display,department_title,place_of_origin,is_public_domain,image_id",
                 .artic_base, as.integer(id))
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data)) return(.schema_artworks)

  tibble(
    id = as.integer(data$id),
    title = as.character(data$title %||% NA),
    artist = as.character(data$artist_display %||% NA),
    date_display = as.character(data$date_display %||% NA),
    medium = as.character(data$medium_display %||% NA),
    department = as.character(data$department_title %||% NA),
    place_of_origin = as.character(data$place_of_origin %||% NA),
    is_public_domain = as.logical(data$is_public_domain %||% NA),
    image_id = as.character(data$image_id %||% NA)
  )
}

#' Art Institute of Chicago package context for LLM integration
#'
#' @return Invisibly returns the context string
artic_context <- function() {
  .build_context("api.artic.edu", header_lines = c(
    "# api.artic.edu - Art Institute of Chicago API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: 60 requests per minute",
    "# Departments: Painting and Sculpture of Europe, American Art, Asian Art, etc.",
    "# IIIF images: https://www.artic.edu/iiif/2/{image_id}/full/843,/0/default.jpg"
  ))
}
