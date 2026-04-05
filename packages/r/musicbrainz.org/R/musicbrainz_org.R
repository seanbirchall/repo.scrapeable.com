# musicbrainz.org.R - Self-contained musicbrainz.org client



# musicbrainz-org.R
# Self-contained MusicBrainz API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent header required)
# Rate limits: 1 request per second


# == Private utilities =========================================================

.ua <- "scrapeable.com/0.1.0 ( support@scrapeable.com )"
.mb_base <- "https://musicbrainz.org/ws/2"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_artists <- tibble(
  id = character(), name = character(), sort_name = character(),
  type = character(), country = character(), score = integer(),
  disambiguation = character()
)

.schema_releases <- tibble(
  id = character(), title = character(), status = character(),
  date = character(), country = character(), score = integer(),
  artist = character()
)

.schema_recordings <- tibble(
  id = character(), title = character(), length_ms = integer(),
  score = integer(), artist = character(), releases = character()
)


# == Artist search =============================================================

#' Search MusicBrainz for artists
#'
#' Searches the MusicBrainz database for artists matching a query string.
#' Results are ranked by relevance score. MusicBrainz is the open music
#' encyclopedia with data on over 2 million artists.
#'
#' @param query Character. Search query (e.g. \code{"radiohead"},
#'   \code{"miles davis"}).
#' @param limit Integer. Number of results (default 25, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. MusicBrainz artist UUID.}
#'     \item{name}{Character. Artist name.}
#'     \item{sort_name}{Character. Name used for sorting.}
#'     \item{type}{Character. Artist type (e.g. "Person", "Group").}
#'     \item{country}{Character. ISO 3166-1 country code.}
#'     \item{score}{Integer. Relevance score (0--100).}
#'     \item{disambiguation}{Character. Disambiguation comment.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mb_artists("radiohead")
#' mb_artists("miles davis", limit = 5)
#' }
mb_artists <- function(query, limit = 25) {
  url <- sprintf("%s/artist/?query=%s&fmt=json&limit=%d",
                 .mb_base, utils::URLencode(query), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("MusicBrainz artist search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_artists)

  artists <- raw$artists
  if (is.null(artists) || length(artists) == 0) return(.schema_artists)

  as_tibble(artists) |>
    transmute(
      id             = as.character(id),
      name           = as.character(name %||% NA_character_),
      sort_name      = as.character(`sort-name` %||% NA_character_),
      type           = as.character(type %||% NA_character_),
      country        = as.character(country %||% NA_character_),
      score          = as.integer(score %||% NA_integer_),
      disambiguation = as.character(disambiguation %||% NA_character_)
    )
}

# == Release search ============================================================

#' Search MusicBrainz for releases (albums)
#'
#' Searches the MusicBrainz database for releases (albums, singles, EPs, etc.)
#' matching a query string. Results include basic release metadata and the
#' credited artist.
#'
#' @param query Character. Search query (e.g. \code{"ok computer"},
#'   \code{"kind of blue"}).
#' @param limit Integer. Number of results (default 25, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. MusicBrainz release UUID.}
#'     \item{title}{Character. Release title.}
#'     \item{status}{Character. Release status (e.g. "Official", "Bootleg").}
#'     \item{date}{Character. Release date (YYYY, YYYY-MM, or YYYY-MM-DD).}
#'     \item{country}{Character. Country of release.}
#'     \item{score}{Integer. Relevance score (0--100).}
#'     \item{artist}{Character. Credited artist name(s).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mb_releases("ok computer")
#' mb_releases("kind of blue", limit = 10)
#' }
mb_releases <- function(query, limit = 25) {
  url <- sprintf("%s/release/?query=%s&fmt=json&limit=%d",
                 .mb_base, utils::URLencode(query), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("MusicBrainz release search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_releases)

  releases <- raw$releases
  if (is.null(releases) || length(releases) == 0) return(.schema_releases)

  artist_names <- vapply(seq_len(nrow(releases)), function(i) {
    ac <- releases$`artist-credit`
    if (is.null(ac)) return(NA_character_)
    tryCatch({
      entry <- ac[[i]]
      if (is.data.frame(entry)) {
        paste(entry$name %||% entry$artist$name, collapse = " & ")
      } else if (is.list(entry) && length(entry) > 0) {
        paste(vapply(entry, function(x) x$name %||% x$artist$name %||% "", character(1)), collapse = " & ")
      } else NA_character_
    }, error = function(e) NA_character_)
  }, character(1))

  tibble(
    id      = as.character(releases$id),
    title   = as.character(releases$title %||% NA_character_),
    status  = as.character(releases$status %||% NA_character_),
    date    = as.character(releases$date %||% NA_character_),
    country = as.character(releases$country %||% NA_character_),
    score   = as.integer(releases$score %||% NA_integer_),
    artist  = artist_names
  )
}

# == Recording search ==========================================================

#' Search MusicBrainz for recordings (songs/tracks)
#'
#' Searches the MusicBrainz database for recordings (individual songs or
#' tracks) matching a query string. Returns track duration, artist, and
#' associated releases.
#'
#' @param query Character. Search query (e.g. \code{"bohemian rhapsody"},
#'   \code{"so what"}).
#' @param limit Integer. Number of results (default 25, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. MusicBrainz recording UUID.}
#'     \item{title}{Character. Recording title.}
#'     \item{length_ms}{Integer. Duration in milliseconds.}
#'     \item{score}{Integer. Relevance score (0--100).}
#'     \item{artist}{Character. Credited artist name(s).}
#'     \item{releases}{Character. Semicolon-separated release titles.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mb_recordings("bohemian rhapsody")
#' mb_recordings("so what", limit = 5)
#' }
mb_recordings <- function(query, limit = 25) {
  url <- sprintf("%s/recording/?query=%s&fmt=json&limit=%d",
                 .mb_base, utils::URLencode(query), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("MusicBrainz recording search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_recordings)

  recs <- raw$recordings
  if (is.null(recs) || length(recs) == 0) return(.schema_recordings)

  artist_names <- vapply(seq_len(nrow(recs)), function(i) {
    ac <- recs$`artist-credit`
    if (is.null(ac)) return(NA_character_)
    tryCatch({
      entry <- ac[[i]]
      if (is.data.frame(entry)) {
        paste(entry$name %||% entry$artist$name, collapse = " & ")
      } else if (is.list(entry) && length(entry) > 0) {
        paste(vapply(entry, function(x) x$name %||% x$artist$name %||% "", character(1)), collapse = " & ")
      } else NA_character_
    }, error = function(e) NA_character_)
  }, character(1))

  release_titles <- vapply(seq_len(nrow(recs)), function(i) {
    rels <- recs$releases
    if (is.null(rels)) return(NA_character_)
    tryCatch({
      entry <- rels[[i]]
      if (is.data.frame(entry)) paste(entry$title, collapse = "; ") else NA_character_
    }, error = function(e) NA_character_)
  }, character(1))

  tibble(
    id        = as.character(recs$id),
    title     = as.character(recs$title %||% NA_character_),
    length_ms = as.integer(recs$length %||% NA_integer_),
    score     = as.integer(recs$score %||% NA_integer_),
    artist    = artist_names,
    releases  = release_titles
  )
}

# == Context ===================================================================

#' Get musicbrainz.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/musicbrainz.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "musicbrainz.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# musicbrainz.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# musicbrainz.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
