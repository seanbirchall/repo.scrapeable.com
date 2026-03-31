# archive-org.R
# Self-contained Internet Archive client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ia_base <- "https://archive.org"

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
    j <- fi - 1
    rox_start <- fi
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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_search <- tibble(
  identifier = character(), title = character(), description = character(),
  mediatype = character(), date = character(), creator = character(),
  downloads = integer(), collection = character()
)

.schema_metadata <- tibble(
  field = character(), value = character()
)

# == Public functions ==========================================================

#' Search Internet Archive items
#'
#' Queries the Internet Archive's advanced search API.
#'
#' @param query Search query string
#' @param rows Number of results per page. Default 10, max 1000.
#' @param page Page number. Default 1.
#' @param mediatype Filter by media type: "texts", "movies", "audio",
#'   "software", "image", "collection". NULL for all.
#' @return tibble: identifier, title, description, mediatype, date, creator,
#'   downloads, collection
#' @export
ia_search <- function(query, rows = 10, page = 1, mediatype = NULL) {
  q <- query
  if (!is.null(mediatype)) q <- paste0(q, " AND mediatype:", mediatype)

  url <- sprintf(
    "%s/advancedsearch.php?q=%s&output=json&rows=%d&page=%d&fl[]=identifier&fl[]=title&fl[]=description&fl[]=mediatype&fl[]=date&fl[]=creator&fl[]=downloads&fl[]=collection",
    .ia_base, utils::URLencode(q, reserved = TRUE),
    as.integer(rows), as.integer(page)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_search)

  docs <- raw$response$docs
  if (is.null(docs) || length(docs) == 0) return(.schema_search)

  .collapse <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.list(x)) return(paste(unlist(x), collapse = "; "))
    as.character(x)
  }

  rows <- lapply(docs, function(d) {
    tibble(
      identifier = .collapse(d$identifier),
      title = .collapse(d$title),
      description = .collapse(d$description),
      mediatype = .collapse(d$mediatype),
      date = .collapse(d$date),
      creator = .collapse(d$creator),
      downloads = as.integer(d$downloads %||% NA_integer_),
      collection = .collapse(d$collection)
    )
  })
  bind_rows(rows)
}

#' Get metadata for an Internet Archive item
#'
#' Returns metadata fields for a specific item by its identifier.
#'
#' @param identifier The item identifier (e.g., "gov.uscourts.dcd.123456")
#' @return tibble: field, value
#' @export
ia_metadata <- function(identifier) {
  url <- sprintf("%s/metadata/%s", .ia_base, identifier)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_metadata)

  meta <- raw$metadata
  if (is.null(meta)) return(.schema_metadata)

  fields <- names(meta)
  values <- vapply(meta, function(x) {
    if (is.list(x)) paste(unlist(x), collapse = "; ")
    else if (length(x) > 1) paste(x, collapse = "; ")
    else as.character(x)
  }, character(1))

  tibble(field = fields, value = values)
}

#' Print Internet Archive context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
ia_context <- function() {
  .build_context(
    pkg_name = "archive.org",
    header_lines = c(
      "# Package: archive.org",
      "# Internet Archive Advanced Search & Metadata API",
      "# Auth: none",
      "# Rate limits: be respectful, no hard limit documented",
      "#",
      "# Media types: texts, movies, audio, software, image, collection",
      "# Search syntax: field:value, AND/OR operators",
      "# Identifiers look like: 'gov.uscourts.dcd.123456', 'nasa_photos'"
    )
  )
}
