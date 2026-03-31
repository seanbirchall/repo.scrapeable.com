# musicbrainz-org.R
# Self-contained MusicBrainz API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent header required)
# Rate limits: 1 request per second

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "scrapeable.com/0.1.0 ( support@scrapeable.com )"
.mb_base <- "https://musicbrainz.org/ws/2"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
  cat(out, "\n"); invisible(out)
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
#' @param query Search query (e.g. "radiohead", "miles davis")
#' @param limit Number of results (default 25, max 100)
#' @return tibble: id, name, sort_name, type, country, score, disambiguation
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
#' @param query Search query (e.g. "ok computer", "kind of blue")
#' @param limit Number of results (default 25, max 100)
#' @return tibble: id, title, status, date, country, score, artist
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
#' @param query Search query (e.g. "bohemian rhapsody", "so what")
#' @param limit Number of results (default 25, max 100)
#' @return tibble: id, title, length_ms, score, artist, releases
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the musicbrainz.org package
#'
#' @return Character string (invisibly), also printed
mb_context <- function() {
  .build_context("musicbrainz.org", header_lines = c(
    "# musicbrainz.org - MusicBrainz API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (User-Agent header sent per API policy)",
    "# Rate limit: 1 request per second",
    "# All functions return tibbles with typed columns."
  ))
}
