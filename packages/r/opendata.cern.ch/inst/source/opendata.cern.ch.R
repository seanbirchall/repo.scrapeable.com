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
#' Browse or search the CERN Open Data Portal for datasets, software,
#' documentation, and other record types from LHC experiments and beyond.
#'
#' @details
#' The CERN Open Data portal hosts petabytes of collision data from the
#' Large Hadron Collider experiments (CMS, ATLAS, ALICE, LHCb) as well
#' as derived datasets, simulation data, and analysis software. Record
#' types include \code{"Dataset"}, \code{"Software"},
#' \code{"Documentation"}, and \code{"Environment"}.
#'
#' @param type Character. Record type filter (default \code{"Dataset"}).
#' @param size Integer. Number of records to return (default 10, max 100).
#' @param page Integer. Page number for pagination (default 1).
#' @param query Character or \code{NULL}. Optional free-text search query.
#' @return A tibble with columns:
#' \describe{
#'   \item{recid}{Integer. CERN Open Data record ID.}
#'   \item{title}{Character. Record title.}
#'   \item{type}{Character. Record type.}
#'   \item{experiment}{Character. Experiment name (e.g. \code{"CMS"}).}
#'   \item{date_published}{Character. Publication year or date.}
#'   \item{abstract}{Character. Brief description of the record.}
#' }
#' @export
#' @seealso \code{\link{cern_record}}, \code{\link{cern_files}},
#'   \code{\link{cern_experiments}}
#' @examples
#' cern_records(type = "Dataset", size = 5)
#' cern_records(query = "Higgs", size = 5)
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
#' Retrieve full metadata for one record from the CERN Open Data portal,
#' including availability status and publisher information.
#'
#' @details
#' The \code{recid} is the integer identifier visible in the portal URL,
#' e.g. \code{https://opendata.cern.ch/record/203}. Use
#' \code{\link{cern_files}} with the same ID to list downloadable files.
#'
#' @param recid Integer. CERN Open Data record identifier.
#' @return A single-row tibble with columns:
#' \describe{
#'   \item{recid}{Integer. Record ID.}
#'   \item{title}{Character. Record title.}
#'   \item{type}{Character. Record type.}
#'   \item{experiment}{Character. Experiment name.}
#'   \item{date_published}{Character. Publication date.}
#'   \item{abstract}{Character. Description.}
#'   \item{availability}{Character. Data availability note.}
#'   \item{publisher}{Character. Publisher name.}
#' }
#' @export
#' @seealso \code{\link{cern_records}}, \code{\link{cern_files}}
#' @examples
#' cern_record(203)
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
#' Retrieve the list of downloadable files associated with a CERN Open
#' Data record, including file sizes and checksums.
#'
#' @details
#' Files may be ROOT files (\code{.root}), CSV, JSON, or other formats.
#' The \code{size} column is in bytes. Large datasets can contain thousands
#' of files totalling terabytes. The \code{type} column is the file
#' extension inferred from the filename.
#'
#' @param recid Integer. CERN Open Data record identifier.
#' @return A tibble with columns:
#' \describe{
#'   \item{filename}{Character. File name (key).}
#'   \item{size}{Integer. File size in bytes.}
#'   \item{checksum}{Character. File checksum.}
#'   \item{type}{Character. File extension (e.g. \code{"root"}, \code{"csv"}).}
#' }
#' @export
#' @seealso \code{\link{cern_record}}, \code{\link{cern_records}}
#' @examples
#' cern_files(203)
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
#' Scan recent dataset records to identify which LHC experiments are
#' represented and how many records each has.
#'
#' @details
#' This function fetches \code{size} dataset records and tabulates the
#' \code{experiment} field. Increase \code{size} for a more complete
#' census. Major experiments include CMS, ATLAS, ALICE, and LHCb.
#'
#' @param size Integer. Number of dataset records to scan (default 100).
#' @return A tibble with columns:
#' \describe{
#'   \item{experiment}{Character. Experiment name.}
#'   \item{count}{Integer. Number of records.}
#' }
#' @export
#' @seealso \code{\link{cern_records}}
#' @examples
#' cern_experiments(size = 50)
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

#' Get opendata.cern.ch client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/opendata.cern.ch.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "opendata.cern.ch")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# opendata.cern.ch context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# opendata.cern.ch", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
