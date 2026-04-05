# api.artic.edu.R - Self-contained api.artic.edu client



# api-artic-edu.R
# Self-contained Art Institute of Chicago API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: 60 requests per minute


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.artic_base <- "https://api.artic.edu/api/v1"

`%||%` <- function(x, y) if (is.null(x)) y else x

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
#' Retrieves a paginated listing of artworks from the Art Institute of Chicago
#' collection. By default returns a curated set of nine fields; pass a custom
#' \code{fields} string to request additional API fields. Combine with
#' \code{\link{artic_search}} for keyword-based discovery.
#'
#' @param limit Integer. Number of artworks per page (default 20, max 100).
#' @param page Integer. Page number, 1-indexed (default 1).
#' @param fields Character or \code{NULL}. Comma-separated API field names to
#'   return. When \code{NULL} (the default), a sensible set of nine fields is
#'   requested. See the AIC API documentation for the full list of available
#'   fields.
#' @return A tibble with one row per artwork and columns:
#' \describe{
#'   \item{id}{Integer. Unique artwork identifier.}
#'   \item{title}{Character. Title of the artwork.}
#'   \item{artist}{Character. Artist display name (may include life dates).}
#'   \item{date_display}{Character. Human-readable date string (e.g. \code{"1884--86"}).}
#'   \item{medium}{Character. Medium description (e.g. \code{"Oil on canvas"}).}
#'   \item{department}{Character. Curatorial department (e.g. \code{"Painting and Sculpture of Europe"}).}
#'   \item{place_of_origin}{Character. Country or region of origin.}
#'   \item{is_public_domain}{Logical. \code{TRUE} if the artwork image is in the public domain.}
#'   \item{image_id}{Character. IIIF image identifier; build URLs with \code{https://www.artic.edu/iiif/2/{image_id}/full/843,/0/default.jpg}.}
#' }
#' @examples
#' \dontrun{
#' artic_artworks()
#' artic_artworks(limit = 50, page = 2)
#' }
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
#' Performs a full-text search against the AIC collection using Elasticsearch.
#' Returns the same column schema as \code{\link{artic_artworks}} but ranked
#' by relevance to the query.
#'
#' @param query Character. Search query string (e.g. \code{"monet"},
#'   \code{"impressionism"}, \code{"still life"}).
#' @param limit Integer. Maximum results to return (default 20, max 100).
#' @return A tibble with one row per matching artwork and the same columns as
#'   \code{\link{artic_artworks}}: id, title, artist, date_display, medium,
#'   department, place_of_origin, is_public_domain, image_id.
#' @examples
#' \dontrun{
#' artic_search("monet")
#' artic_search("japanese prints", limit = 50)
#' }
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
#' Retrieves full metadata for a single artwork from the Art Institute of
#' Chicago by its numeric identifier. Useful for drilling into a specific
#' result from \code{\link{artic_search}} or \code{\link{artic_artworks}}.
#'
#' @param id Integer. Artwork identifier (e.g. \code{27992} for Seurat's
#'   "A Sunday on La Grande Jatte").
#' @return A one-row tibble with the same columns as \code{\link{artic_artworks}}:
#'   id, title, artist, date_display, medium, department, place_of_origin,
#'   is_public_domain, image_id. Returns a zero-row schema tibble if the ID
#'   is not found.
#' @examples
#' \dontrun{
#' artic_artwork(27992)
#' }
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

#' Browse artists at the Art Institute of Chicago
#'
#' Retrieves a paginated list of artists represented in the AIC collection.
#' The \code{title} column contains the artist's display name. Birth and death
#' dates are returned as integers (year only); \code{NA} when unknown.
#'
#' @param limit Integer. Number of artists per page (default 20).
#' @param page Integer. Page number, 1-indexed (default 1).
#' @return A tibble with one row per artist and columns:
#' \describe{
#'   \item{id}{Integer. Unique artist identifier.}
#'   \item{title}{Character. Artist display name.}
#'   \item{birth_date}{Integer. Birth year, or \code{NA}.}
#'   \item{death_date}{Integer. Death year, or \code{NA}.}
#'   \item{description}{Character. Brief biographical note, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' artic_artists()
#' artic_artists(limit = 100, page = 3)
#' }
#' @export
artic_artists <- function(limit = 20, page = 1) {
  schema <- tibble(id = integer(), title = character(), birth_date = integer(),
                   death_date = integer(), description = character())
  url <- sprintf("%s/artists?limit=%d&page=%d&fields=id,title,birth_date,death_date,description",
                 .artic_base, limit, page)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(schema)

  data <- raw$data
  if (is.data.frame(data) && nrow(data) > 0) {
    nms <- names(data)
    as_tibble(data) |>
      transmute(
        id = as.integer(id),
        title = as.character(if ("title" %in% nms) title else NA_character_),
        birth_date = as.integer(if ("birth_date" %in% nms) birth_date else NA_integer_),
        death_date = as.integer(if ("death_date" %in% nms) death_date else NA_integer_),
        description = as.character(if ("description" %in% nms) description else NA_character_)
      )
  } else {
    schema
  }
}

#' Search or browse Art Institute exhibitions
#'
#' When \code{query} is supplied, performs a full-text search across exhibition
#' records. When \code{NULL}, returns a paginated browse listing. Useful for
#' finding past, current, or upcoming shows.
#'
#' @param query Character or \code{NULL}. Search string (e.g. \code{"impressionism"}).
#'   Pass \code{NULL} (the default) to browse without filtering.
#' @param limit Integer. Maximum results to return (default 20).
#' @return A tibble with one row per exhibition and columns:
#' \describe{
#'   \item{id}{Integer. Unique exhibition identifier.}
#'   \item{title}{Character. Exhibition title.}
#'   \item{status}{Character. Exhibition status (e.g. \code{"Closed"}, \code{"Open"}).}
#'   \item{aic_start_at}{Character. ISO 8601 start timestamp.}
#'   \item{aic_end_at}{Character. ISO 8601 end timestamp.}
#' }
#' @examples
#' \dontrun{
#' artic_exhibitions()
#' artic_exhibitions("impressionism", limit = 10)
#' }
#' @export
artic_exhibitions <- function(query = NULL, limit = 20) {
  schema <- tibble(id = integer(), title = character(), status = character(),
                   aic_start_at = character(), aic_end_at = character())
  url <- if (!is.null(query)) {
    sprintf("%s/exhibitions/search?q=%s&limit=%d&fields=id,title,status,aic_start_at,aic_end_at",
            .artic_base, utils::URLencode(query), limit)
  } else {
    sprintf("%s/exhibitions?limit=%d&fields=id,title,status,aic_start_at,aic_end_at",
            .artic_base, limit)
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(schema)

  data <- raw$data
  if (is.data.frame(data) && nrow(data) > 0) {
    nms <- names(data)
    as_tibble(data) |>
      transmute(
        id = as.integer(id),
        title = as.character(if ("title" %in% nms) title else NA_character_),
        status = as.character(if ("status" %in% nms) status else NA_character_),
        aic_start_at = as.character(if ("aic_start_at" %in% nms) aic_start_at else NA_character_),
        aic_end_at = as.character(if ("aic_end_at" %in% nms) aic_end_at else NA_character_)
      )
  } else {
    schema
  }
}

# == Context ===================================================================

#' Get artic.edu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
artic_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(artic_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/artic.edu.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "artic.edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# artic.edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# artic.edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
