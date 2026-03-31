# opendata-cern-ch.R
# Self-contained CERN Open Data Portal client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cern_base <- "https://opendata.cern.ch/api"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_records <- tibble(
  recid = integer(), title = character(), type = character(),
  experiment = character(), date_published = character(),
  abstract = character()
)

.schema_record <- tibble(
  recid = integer(), title = character(), type = character(),
  experiment = character(), date_published = character(),
  abstract = character(), availability = character(),
  publisher = character()
)

# == Public functions ==========================================================

#' Search CERN Open Data records
#'
#' @param type Record type filter, e.g. "Dataset", "Software" (default "Dataset")
#' @param size Number of records to return (default 10, max 100)
#' @param page Page number (default 1)
#' @param query Optional search query string
#' @return tibble: recid, title, type, experiment, date_published, abstract
#' @export
cern_records <- function(type = "Dataset", size = 10, page = 1, query = NULL) {
  url <- sprintf("%s/records/?page=%d&size=%d&type=%s",
                 .cern_base, page, size, utils::URLencode(type))
  if (!is.null(query)) url <- paste0(url, "&q=", utils::URLencode(query))
  raw <- .fetch_json(url)
  hits <- raw$hits$hits
  if (is.null(hits) || length(hits) == 0) return(.schema_records)

  tibble(
    recid         = as.integer(vapply(hits, function(x) as.character(x$metadata$recid %||% NA), character(1))),
    title         = vapply(hits, function(x) x$metadata$title %||% NA_character_, character(1)),
    type          = vapply(hits, function(x) {
      t <- x$metadata$type
      if (is.null(t)) NA_character_
      else if (is.list(t)) paste(unlist(t$primary %||% t), collapse = ", ")
      else as.character(t[1])
    }, character(1)),
    experiment    = vapply(hits, function(x) {
      e <- x$metadata$experiment
      if (is.null(e)) NA_character_ else as.character(e[1])
    }, character(1)),
    date_published = vapply(hits, function(x) x$metadata$date_published %||% NA_character_, character(1)),
    abstract      = vapply(hits, function(x) {
      a <- x$metadata$abstract
      if (is.null(a)) NA_character_
      else if (is.list(a)) a$description %||% NA_character_
      else as.character(a)
    }, character(1))
  )
}

#' Fetch a single CERN Open Data record by ID
#'
#' @param recid Record ID (integer)
#' @return tibble: one row with recid, title, type, experiment, date_published,
#'   abstract, availability, publisher
#' @export
cern_record <- function(recid) {
  url <- sprintf("%s/records/%d", .cern_base, as.integer(recid))
  raw <- .fetch_json(url)
  m <- raw$metadata
  if (is.null(m)) return(.schema_record)

  tibble(
    recid          = as.integer(m$recid %||% recid),
    title          = as.character(m$title %||% NA_character_),
    type           = if (is.null(m$type)) NA_character_
                     else if (is.list(m$type)) paste(unlist(m$type$primary %||% m$type), collapse = ", ")
                     else as.character(m$type[1]),
    experiment     = if (is.null(m$experiment)) NA_character_ else as.character(m$experiment[1]),
    date_published = as.character(m$date_published %||% NA_character_),
    abstract       = if (is.null(m$abstract)) NA_character_
                     else if (is.list(m$abstract)) m$abstract$description %||% NA_character_
                     else as.character(m$abstract),
    availability   = as.character(m$availability %||% NA_character_),
    publisher      = as.character(m$publisher %||% NA_character_)
  )
}

#' Show CERN Open Data package context for LLM use
#'
#' Prints all public function signatures and documentation.
#'
#' @return Invisibly returns the context string
#' @export
cern_context <- function() {
  .build_context(
    pkg_name = "opendata.cern.ch",
    header_lines = c(
      "# opendata.cern.ch",
      "# CERN Open Data Portal API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required",
      "# Rate limits: unknown (be polite)"
    )
  )
}
