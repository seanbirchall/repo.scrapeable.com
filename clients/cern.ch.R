# opendata.cern.ch.R - Self-contained opendata.cern.ch client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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
#' Searches the CERN Open Data Portal for datasets, software, and other
#' records from high-energy physics experiments (CMS, ALICE, ATLAS, LHCb,
#' etc.). Supports filtering by record type and free-text queries.
#'
#' @param type Character. Record type filter: \code{"Dataset"} (default),
#'   \code{"Software"}, \code{"Environment"}, or \code{"Documentation"}.
#' @param size Integer. Number of records to return (default 10, max 100).
#' @param page Integer. Page number for pagination (default 1).
#' @param query Character or \code{NULL}. Optional free-text search
#'   (e.g. \code{"Higgs"}, \code{"muon"}, \code{"CMS 2012"}).
#' @return A tibble with one row per record and 6 columns:
#' \describe{
#'   \item{recid}{Integer. Unique record ID. Use with \code{cern_record()} or \code{cern_files()}.}
#'   \item{title}{Character. Record title.}
#'   \item{type}{Character. Record type (e.g. \code{"Dataset"}).}
#'   \item{experiment}{Character. Experiment name (e.g. \code{"CMS"}, \code{"ALICE"}), or \code{NA}.}
#'   \item{date_published}{Character. Publication year or date.}
#'   \item{abstract}{Character. Record description/abstract.}
#' }
#' @examples
#' cern_records()
#' cern_records(query = "Higgs", size = 5)
#' cern_records(type = "Software", size = 5)
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
#' Retrieves full metadata for a specific CERN Open Data record,
#' including availability status and publisher information.
#'
#' @param recid Integer. Record ID obtained from \code{cern_records()}
#'   (e.g. \code{1}, \code{203}).
#' @return A tibble with 1 row and 8 columns:
#' \describe{
#'   \item{recid}{Integer. Record ID.}
#'   \item{title}{Character. Record title.}
#'   \item{type}{Character. Record type (e.g. \code{"Dataset"}).}
#'   \item{experiment}{Character. Experiment name, or \code{NA}.}
#'   \item{date_published}{Character. Publication year or date.}
#'   \item{abstract}{Character. Description (may contain HTML markup).}
#'   \item{availability}{Character. Access status (e.g. \code{"online"}).}
#'   \item{publisher}{Character. Publisher name (e.g. \code{"CERN Open Data Portal"}).}
#' }
#' @examples
#' cern_record(1)
#' cern_record(203)
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

#' List files in a CERN Open Data record
#'
#' Returns the list of downloadable files attached to a CERN Open Data
#' record, with sizes and checksums. Note: not all records have files
#' exposed through this endpoint.
#'
#' @param recid Integer. Record ID (e.g. \code{1}, \code{203}).
#' @return A tibble with one row per file and 4 columns:
#' \describe{
#'   \item{filename}{Character. File name/key.}
#'   \item{size}{Integer. File size in bytes.}
#'   \item{checksum}{Character. File checksum (typically MD5).}
#'   \item{type}{Character. File extension (e.g. \code{"root"}, \code{"csv"}), or \code{NA}.}
#' }
#' @examples
#' cern_files(1)
#' cern_files(203)
#' @export
cern_files <- function(recid) {
  schema <- tibble(filename = character(), size = integer(), checksum = character(), type = character())
  url <- sprintf("%s/records/%d", .cern_base, as.integer(recid))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$metadata)) return(schema)

  files <- raw$metadata$files
  if (is.null(files) || length(files) == 0) return(schema)

  tibble(
    filename = vapply(files, function(f) f$key %||% f$filename %||% NA_character_, character(1)),
    size = vapply(files, function(f) as.integer(f$size %||% NA_integer_), integer(1)),
    checksum = vapply(files, function(f) f$checksum %||% NA_character_, character(1)),
    type = vapply(files, function(f) {
      ext <- tools::file_ext(f$key %||% f$filename %||% "")
      if (nchar(ext) == 0) NA_character_ else ext
    }, character(1))
  )
}

#' List available experiments in CERN Open Data
#'
#' Scans recent dataset records and extracts distinct experiment names
#' with their frequency counts. Useful for discovering which experiments
#' have data available on the portal.
#'
#' @param size Integer. Number of dataset records to scan (default 100).
#'   Larger values give a more complete picture but take longer.
#' @return A tibble with one row per experiment and 2 columns:
#' \describe{
#'   \item{experiment}{Character. Experiment name (e.g. \code{"CMS"}, \code{"ALICE"}).}
#'   \item{count}{Integer. Number of dataset records from this experiment.}
#' }
#' @examples
#' cern_experiments()
#' cern_experiments(size = 50)
#' @export
cern_experiments <- function(size = 100) {
  schema <- tibble(experiment = character(), count = integer())
  recs <- tryCatch(cern_records(type = "Dataset", size = size), error = function(e) NULL)
  if (is.null(recs) || nrow(recs) == 0) return(schema)

  exps <- recs$experiment[!is.na(recs$experiment)]
  if (length(exps) == 0) return(schema)
  tbl <- sort(table(exps), decreasing = TRUE)
  tibble(experiment = names(tbl), count = as.integer(tbl))
}

# == Context ===================================================================

#' Get cern.ch client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
opendata_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(opendata_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cern.ch.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cern.ch")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cern.ch context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cern.ch", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
