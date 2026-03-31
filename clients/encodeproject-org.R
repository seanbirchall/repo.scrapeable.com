# encodeproject-org.R
# Self-contained ENCODE Project API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: be polite (no official limit published)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.encode_base <- "https://www.encodeproject.org"

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
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  accession = character(), type = character(), description = character(),
  assay_title = character(), biosample_summary = character(),
  lab = character(), status = character(), date_released = as.Date(character())
)

.schema_experiment <- tibble(
  accession = character(), type = character(), description = character(),
  assay_title = character(), biosample_summary = character(),
  target = character(), lab = character(), status = character(),
  date_released = as.Date(character()), files_count = integer()
)

.schema_files <- tibble(
  accession = character(), file_format = character(),
  output_type = character(), assembly = character(),
  file_size = numeric(), href = character(), status = character()
)

# == Search ====================================================================

#' Search ENCODE experiments and other objects
#'
#' Full-text search across the ENCODE portal. Returns experiments,
#' biosamples, antibodies, and other object types.
#'
#' @param type Object type: "Experiment", "Biosample", "AntibodyLot",
#'   "GeneticModification", etc. (default "Experiment")
#' @param query Free-text search query (e.g. "CTCF", "ChIP-seq")
#' @param assay Assay type (e.g. "ChIP-seq", "RNA-seq", "ATAC-seq")
#' @param organism Organism (e.g. "Homo sapiens", "Mus musculus")
#' @param limit Max results (default 25, max 200)
#' @return tibble: accession, type, description, assay_title,
#'   biosample_summary, lab, status, date_released
encode_search <- function(type = "Experiment", query = NULL, assay = NULL,
                          organism = NULL, limit = 25) {
  params <- list(
    type = type,
    searchTerm = query,
    assay_title = assay,
    replicates.library.biosample.donor.organism.scientific_name = organism,
    limit = as.integer(limit),
    format = "json"
  )
  params <- params[!vapply(params, is.null, logical(1))]
  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/search/?%s", .encode_base, query_str)
  raw <- .fetch_json(url)
  g <- raw[["@graph"]]
  if (is.null(g) || length(g) == 0) return(.schema_search)

  rows <- lapply(g, function(x) {
    tibble(
      accession = as.character(x$accession %||% NA),
      type = as.character(if (!is.null(x[["@type"]])) x[["@type"]][1] else NA),
      description = as.character(x$description %||% NA),
      assay_title = as.character(x$assay_title %||% NA),
      biosample_summary = as.character(x$biosample_summary %||% NA),
      lab = as.character(if (!is.null(x$lab)) x$lab$title %||% x$lab else NA),
      status = as.character(x$status %||% NA),
      date_released = tryCatch(as.Date(x$date_released), error = function(e) NA_real_)
    )
  })
  bind_rows(rows)
}

# == Experiment detail =========================================================

#' Fetch a single ENCODE experiment by accession
#'
#' @param accession ENCODE accession (e.g. "ENCSR000AEG")
#' @return tibble: one row with accession, type, description, assay_title,
#'   biosample_summary, target, lab, status, date_released, files_count
encode_experiment <- function(accession) {
  url <- sprintf("%s/experiments/%s/?format=json", .encode_base, accession)
  x <- .fetch_json(url)
  if (is.null(x) || is.null(x$accession)) return(.schema_experiment)

  tibble(
    accession = as.character(x$accession),
    type = as.character(if (!is.null(x[["@type"]])) x[["@type"]][1] else NA),
    description = as.character(x$description %||% NA),
    assay_title = as.character(x$assay_title %||% NA),
    biosample_summary = as.character(x$biosample_summary %||% NA),
    target = as.character(if (!is.null(x$target)) x$target$label %||% x$target else NA),
    lab = as.character(if (!is.null(x$lab)) x$lab$title %||% x$lab else NA),
    status = as.character(x$status %||% NA),
    date_released = tryCatch(as.Date(x$date_released), error = function(e) NA_real_),
    files_count = as.integer(length(x$files %||% list()))
  )
}

# == Experiment files ==========================================================

#' List files for an ENCODE experiment
#'
#' @param accession ENCODE experiment accession
#' @param file_format Filter by format: "bam", "bigWig", "fastq", "bed", etc.
#' @return tibble: accession, file_format, output_type, assembly, file_size, href, status
encode_files <- function(accession, file_format = NULL) {
  params <- list(
    type = "File",
    dataset = sprintf("/experiments/%s/", accession),
    limit = "all",
    format = "json"
  )
  if (!is.null(file_format)) params$file_format <- file_format
  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/search/?%s", .encode_base, query_str)
  raw <- .fetch_json(url)
  g <- raw[["@graph"]]
  if (is.null(g) || length(g) == 0) return(.schema_files)

  rows <- lapply(g, function(x) {
    tibble(
      accession = as.character(x$accession %||% NA),
      file_format = as.character(x$file_format %||% NA),
      output_type = as.character(x$output_type %||% NA),
      assembly = as.character(x$assembly %||% NA),
      file_size = as.numeric(x$file_size %||% NA),
      href = as.character(x$href %||% NA),
      status = as.character(x$status %||% NA)
    )
  })
  bind_rows(rows)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the encodeproject.org package
#'
#' @return Character string (invisibly), also printed
encode_context <- function() {
  .build_context("encodeproject.org", header_lines = c(
    "# encodeproject.org - ENCODE Project API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://www.encodeproject.org",
    "# All functions return tibbles with typed columns.",
    "# Common types: Experiment, Biosample, AntibodyLot",
    "# Common assays: ChIP-seq, RNA-seq, ATAC-seq, Hi-C, WGBS"
  ))
}
