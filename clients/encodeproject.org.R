# encodeproject.org.R - Self-contained encodeproject.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# encodeproject-org.R
# Self-contained ENCODE Project API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: be polite (no official limit published)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.encode_base <- "https://www.encodeproject.org"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

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


#' Search the ENCODE portal for experiments and other objects
#'
#' Performs a full-text search across the ENCODE Project data portal.
#' By default searches for Experiment objects, but can also retrieve
#' Biosamples, AntibodyLots, and other ENCODE object types. Results
#' include core metadata such as assay type, biosample summary, lab,
#' and release date.
#'
#' @param type ENCODE object type to search (default \code{"Experiment"}).
#'   Other common values: \code{"Biosample"}, \code{"AntibodyLot"},
#'   \code{"GeneticModification"}, \code{"File"}.
#' @param query Optional free-text search term (e.g. \code{"CTCF"},
#'   \code{"p53"}, \code{"ChIP-seq K562"}).
#' @param assay Optional assay type filter (e.g. \code{"ChIP-seq"},
#'   \code{"RNA-seq"}, \code{"ATAC-seq"}).
#' @param organism Optional organism filter (e.g. \code{"Homo sapiens"},
#'   \code{"Mus musculus"}).
#' @param limit Maximum number of results to return (default 25, max 200).
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{ENCODE accession, e.g. \code{"ENCSR000AEG"} (character).}
#'     \item{type}{Object type (character).}
#'     \item{description}{Experiment description (character).}
#'     \item{assay_title}{Assay type label (character).}
#'     \item{biosample_summary}{Human-readable biosample summary (character).}
#'     \item{lab}{Lab that produced the data (character).}
#'     \item{status}{Release status, e.g. \code{"released"} (character).}
#'     \item{date_released}{Public release date (Date).}
#'   }
#' @export
#' @seealso \code{\link{encode_experiment}}, \code{\link{encode_files}},
#'   \code{\link{encode_biosamples}}
#' @examples
#' \dontrun{
#' encode_search(query = "CTCF", limit = 10)
#' encode_search(assay = "ATAC-seq", organism = "Mus musculus")
#' }
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
      date_released = tryCatch(as.Date(x$date_released %||% NA_character_), error = function(e) as.Date(NA))
    )
  })
  bind_rows(rows)
}

# == Experiment detail =========================================================

#' Fetch detailed metadata for a single ENCODE experiment
#'
#' Retrieves the full record for one ENCODE experiment by its accession
#' number. Includes the target protein/gene (for ChIP-seq and similar
#' assays), the number of associated data files, and all metadata
#' returned by \code{\link{encode_search}}.
#'
#' @param accession ENCODE experiment accession (e.g.
#'   \code{"ENCSR000AEG"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{accession}{ENCODE accession (character).}
#'     \item{type}{Object type (character).}
#'     \item{description}{Experiment description (character).}
#'     \item{assay_title}{Assay type label (character).}
#'     \item{biosample_summary}{Human-readable biosample summary (character).}
#'     \item{target}{Target protein or gene label, if applicable (character).}
#'     \item{lab}{Lab that produced the data (character).}
#'     \item{status}{Release status (character).}
#'     \item{date_released}{Public release date (Date).}
#'     \item{files_count}{Number of data files attached to this experiment (integer).}
#'   }
#' @export
#' @seealso \code{\link{encode_search}}, \code{\link{encode_files}}
#' @examples
#' \dontrun{
#' encode_experiment("ENCSR000AEG")
#' }
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
    date_released = tryCatch(as.Date(x$date_released %||% NA_character_), error = function(e) as.Date(NA)),
    files_count = as.integer(length(x$files %||% list()))
  )
}

# == Experiment files ==========================================================

#' List data files for an ENCODE experiment
#'
#' Searches the ENCODE portal for all File objects associated with a
#' given experiment accession. Optionally filters by file format.
#' The \code{href} column contains a relative path that can be appended
#' to \code{https://www.encodeproject.org} to construct a download URL.
#'
#' @param accession ENCODE experiment accession (e.g.
#'   \code{"ENCSR000AEG"}).
#' @param file_format Optional file format filter. Common values:
#'   \code{"bam"}, \code{"bigWig"}, \code{"fastq"}, \code{"bed"},
#'   \code{"tsv"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{ENCODE file accession (character).}
#'     \item{file_format}{File format, e.g. \code{"bam"} (character).}
#'     \item{output_type}{Output type label (character).}
#'     \item{assembly}{Genome assembly, e.g. \code{"GRCh38"} (character).}
#'     \item{file_size}{File size in bytes (numeric).}
#'     \item{href}{Relative download path on the ENCODE portal (character).}
#'     \item{status}{File status, e.g. \code{"released"} (character).}
#'   }
#' @export
#' @seealso \code{\link{encode_experiment}}, \code{\link{encode_search}}
#' @examples
#' \dontrun{
#' encode_files("ENCSR000AEG")
#' encode_files("ENCSR000AEG", file_format = "bam")
#' }
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

# == Biosamples ================================================================

#' Search ENCODE biosamples
#'
#' Queries the ENCODE portal for Biosample objects, optionally filtered
#' by organism and a free-text search term. Each biosample represents a
#' biological sample (cell line, tissue, primary cell, etc.) used in
#' ENCODE experiments.
#'
#' @param organism Scientific name for organism filter (default
#'   \code{"Homo sapiens"}). Use \code{"Mus musculus"} for mouse, etc.
#' @param query Optional free-text search term (e.g. \code{"K562"},
#'   \code{"liver"}).
#' @param limit Maximum number of results (default 25).
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{ENCODE biosample accession (character).}
#'     \item{biosample_type}{Ontology classification, e.g. \code{"cell line"}
#'       (character).}
#'     \item{organism}{Scientific name of the organism (character).}
#'     \item{summary}{Human-readable biosample summary (character).}
#'     \item{source}{Source lab or vendor (character).}
#'     \item{status}{Biosample status (character).}
#'   }
#' @export
#' @seealso \code{\link{encode_search}}, \code{\link{encode_experiment}}
#' @examples
#' \dontrun{
#' encode_biosamples(query = "K562")
#' encode_biosamples(organism = "Mus musculus", limit = 10)
#' }
encode_biosamples <- function(organism = "Homo sapiens", query = NULL, limit = 25) {
  schema <- tibble(accession = character(), biosample_type = character(),
                   organism = character(), summary = character(),
                   source = character(), status = character())
  params <- list(
    type = "Biosample",
    `organism.scientific_name` = organism,
    searchTerm = query,
    limit = as.integer(limit),
    format = "json"
  )
  params <- params[!vapply(params, is.null, logical(1))]
  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/search/?%s", .encode_base, query_str)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)
  g <- raw[["@graph"]]
  if (is.null(g) || length(g) == 0) return(schema)

  rows <- lapply(g, function(x) {
    tibble(
      accession = as.character(x$accession %||% NA),
      biosample_type = as.character(if (!is.null(x$biosample_ontology)) x$biosample_ontology$classification %||% NA else NA),
      organism = as.character(if (!is.null(x$organism)) x$organism$scientific_name %||% NA else NA),
      summary = as.character(x$summary %||% NA),
      source = as.character(if (!is.null(x$source)) x$source$title %||% x$source else NA),
      status = as.character(x$status %||% NA)
    )
  })
  bind_rows(rows)
}

# == Context ===================================================================

#' Get encodeproject.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
encode_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(encode_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/encodeproject.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "encodeproject.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# encodeproject.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# encodeproject.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
