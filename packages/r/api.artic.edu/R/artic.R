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

#' @title Browse Art Institute of Chicago artworks
#'
#' Returns a paginated tibble of artworks from the Art Institute of Chicago's
#' permanent collection. Use \code{artic_search()} for keyword queries, or
#' this function to browse sequentially. Supports pagination for traversing
#' the full collection of 120,000+ works.
#'
#' @param limit Number of artworks per page. Default \code{20}. Maximum \code{100}.
#' @param page Page number (1-indexed). Default \code{1}. Use with \code{limit}
#'   to paginate through results.
#' @param fields Optional comma-separated string of API field names to return.
#'   Default \code{NULL} returns the standard set. Example:
#'   \code{"id,title,date_display"}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (integer): Unique artwork identifier (use with \code{artic_artwork()})
#'     \item \code{title} (character): Artwork title, e.g. "Water Lilies"
#'     \item \code{artist} (character): Artist name with nationality and dates, e.g. "Claude Monet (French, 1840-1926)"
#'     \item \code{date_display} (character): Display date string, e.g. "1906", "c. 1865"
#'     \item \code{medium} (character): Medium description, e.g. "Oil on canvas"
#'     \item \code{department} (character): Curatorial department, e.g. "Painting and Sculpture of Europe"
#'     \item \code{place_of_origin} (character): Country or region of origin, e.g. "France"
#'     \item \code{is_public_domain} (logical): Whether the artwork image is in the public domain
#'     \item \code{image_id} (character): IIIF image identifier for constructing image URLs
#'   }
#' @examples
#' \dontrun{
#' # First 20 artworks
#' artic_artworks()
#'
#' # Page 3, 50 artworks per page
#' artic_artworks(limit = 50, page = 3)
#'
#' # Only return id and title
#' artic_artworks(fields = "id,title")
#' }
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

#' @title Search Art Institute of Chicago artworks
#'
#' Full-text search across the Art Institute of Chicago's artwork collection.
#' Matches against titles, artist names, descriptions, and other metadata.
#' Returns the same columns as \code{artic_artworks()} but ranked by
#' relevance to the query.
#'
#' @param query Search query string. Examples: \code{"monet"}, \code{"water lilies"},
#'   \code{"impressionism"}, \code{"Japanese prints"}.
#' @param limit Maximum number of results. Default \code{20}. Maximum \code{100}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (integer): Unique artwork identifier
#'     \item \code{title} (character): Artwork title
#'     \item \code{artist} (character): Artist name with nationality and dates
#'     \item \code{date_display} (character): Display date string
#'     \item \code{medium} (character): Medium description
#'     \item \code{department} (character): Curatorial department
#'     \item \code{place_of_origin} (character): Country or region of origin
#'     \item \code{is_public_domain} (logical): Public domain status
#'     \item \code{image_id} (character): IIIF image identifier
#'   }
#' @examples
#' \dontrun{
#' # Search for Monet paintings
#' artic_search("monet")
#'
#' # Search for Japanese woodblock prints
#' artic_search("Japanese woodblock", limit = 50)
#'
#' # Search for a specific artwork
#' artic_search("Sunday on La Grande Jatte")
#' }
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

#' @title Fetch a single artwork by ID
#'
#' Returns detailed metadata for a single artwork from the Art Institute of
#' Chicago. Use artwork IDs obtained from \code{artic_artworks()} or
#' \code{artic_search()}. Returns the same column structure as the browse
#' and search functions but always a single row.
#'
#' @param id Artwork ID (integer). Example: \code{27992} (A Sunday on La Grande
#'   Jatte). Find IDs using \code{artic_search()} or \code{artic_artworks()}.
#' @return A tibble with one row and columns:
#'   \itemize{
#'     \item \code{id} (integer): Artwork identifier
#'     \item \code{title} (character): Artwork title
#'     \item \code{artist} (character): Artist name with nationality and dates
#'     \item \code{date_display} (character): Display date string
#'     \item \code{medium} (character): Medium description
#'     \item \code{department} (character): Curatorial department
#'     \item \code{place_of_origin} (character): Country or region of origin
#'     \item \code{is_public_domain} (logical): Public domain status
#'     \item \code{image_id} (character): IIIF image identifier
#'   }
#' @examples
#' \dontrun{
#' # Fetch A Sunday on La Grande Jatte
#' artic_artwork(27992)
#'
#' # Fetch Water Lilies
#' artic_artwork(16568)
#' }
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

#' @title Browse artists at the Art Institute of Chicago
#'
#' Returns a paginated tibble of artists represented in the Art Institute
#' of Chicago's collection. Includes birth/death dates and optional
#' descriptions. Use pagination to traverse the full artist list.
#'
#' @param limit Number of artists per page. Default \code{20}.
#' @param page Page number (1-indexed). Default \code{1}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (integer): Unique artist identifier
#'     \item \code{title} (character): Artist name, e.g. "Claude Monet"
#'     \item \code{birth_date} (integer): Birth year (may be \code{NA})
#'     \item \code{death_date} (integer): Death year (may be \code{NA} if living)
#'     \item \code{description} (character): Biographical description (often \code{NA})
#'   }
#' @examples
#' \dontrun{
#' # First page of artists
#' artic_artists()
#'
#' # Page 5, 50 per page
#' artic_artists(limit = 50, page = 5)
#' }
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

#' @title Browse or search Art Institute exhibitions
#'
#' Returns exhibition data from the Art Institute of Chicago. Without a query,
#' browses all exhibitions; with a query, performs full-text search. Includes
#' exhibition dates and status (open/closed).
#'
#' @param query Optional search query string. Examples: \code{"Picasso"},
#'   \code{"modern art"}. \code{NULL} browses all exhibitions. Default \code{NULL}.
#' @param limit Number of results to return. Default \code{20}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (integer): Unique exhibition identifier
#'     \item \code{title} (character): Exhibition title
#'     \item \code{status} (character): Exhibition status, e.g. "Closed", "Open"
#'     \item \code{aic_start_at} (character): Exhibition start date (ISO 8601 timestamp)
#'     \item \code{aic_end_at} (character): Exhibition end date (ISO 8601 timestamp)
#'   }
#' @examples
#' \dontrun{
#' # Browse recent exhibitions
#' artic_exhibitions()
#'
#' # Search for Picasso exhibitions
#' artic_exhibitions("Picasso")
#'
#' # Get 50 exhibitions
#' artic_exhibitions(limit = 50)
#' }
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

#' Get api.artic.edu client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.artic.edu.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.artic.edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.artic.edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.artic.edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
