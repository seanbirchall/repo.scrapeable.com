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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
