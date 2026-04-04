# dataverse.R
# Self-contained Harvard Dataverse API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: generous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dv_base <- "https://dataverse.harvard.edu/api"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_search <- tibble(
  name = character(), type = character(), url = character(),
  description = character(), published_at = character(),
  citation = character()
)

.schema_dataset <- tibble(
  name = character(), doi = character(), description = character(),
  publisher = character(), published_at = character(),
  file_name = character(), file_id = integer(), file_size = integer(),
  file_type = character()
)

# == Public functions ==========================================================

#' Search Harvard Dataverse
#'
#' Performs a full-text search across Harvard Dataverse, the world's largest
#' open-source research data repository. Can search for datasets, individual
#' data files, or sub-dataverses. Pagination is offset-based; increment
#' \code{start} by \code{per_page} to page through results.
#'
#' @param query Character. Search query (e.g. \code{"climate"}, \code{"elections"}).
#' @param type Character. Result type: \code{"dataset"} (default), \code{"file"},
#'   or \code{"dataverse"}.
#' @param per_page Integer. Results per page (default 10).
#' @param start Integer. Offset for pagination (default 0).
#' @return A tibble with one row per result and columns:
#' \describe{
#'   \item{name}{Character. Dataset or file name.}
#'   \item{type}{Character. Result type (\code{"dataset"}, \code{"file"}, or \code{"dataverse"}).}
#'   \item{url}{Character. Persistent URL, or \code{NA}.}
#'   \item{description}{Character. Description text, or \code{NA}.}
#'   \item{published_at}{Character. ISO 8601 publication timestamp, or \code{NA}.}
#'   \item{citation}{Character. Full citation string, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' dv_search("climate")
#' dv_search("elections", type = "file", per_page = 20)
#' }
#' @export
dv_search <- function(query, type = "dataset", per_page = 10, start = 0) {
  url <- sprintf(
    "%s/search?q=%s&type=%s&per_page=%d&start=%d",
    .dv_base, utils::URLencode(query, reserved = TRUE),
    type, as.integer(per_page), as.integer(start)
  )
  raw <- .fetch_json(url)
  items <- raw$data$items
  if (is.null(items) || length(items) == 0 ||
      (is.data.frame(items) && nrow(items) == 0)) return(.schema_search)

  tibble(
    name = as.character(items$name),
    type = as.character(items$type),
    url = as.character(items$url %||% NA_character_),
    description = as.character(items$description %||% NA_character_),
    published_at = as.character(items$published_at %||% NA_character_),
    citation = as.character(items$citation %||% NA_character_)
  )
}

#' Get dataset details by DOI
#'
#' Fetches full metadata for a single dataset from Harvard Dataverse using its
#' persistent identifier (DOI). Returns one row per file contained in the
#' dataset, with dataset-level metadata repeated across rows. Useful for
#' discovering downloadable files after a \code{\link{dv_search}}.
#'
#' @param doi Character. Dataset persistent ID, including the \code{"doi:"}
#'   prefix (e.g. \code{"doi:10.7910/DVN/EXAMPLE"}).
#' @return A tibble with one row per file in the dataset and columns:
#' \describe{
#'   \item{name}{Character. Dataset title extracted from citation metadata.}
#'   \item{doi}{Character. The DOI passed in.}
#'   \item{description}{Character. Dataset description, or \code{NA}.}
#'   \item{publisher}{Character. Publishing institution, or \code{NA}.}
#'   \item{published_at}{Character. Publication date string, or \code{NA}.}
#'   \item{file_name}{Character. Filename of the data file, or \code{NA}.}
#'   \item{file_id}{Integer. Numeric file identifier for download, or \code{NA}.}
#'   \item{file_size}{Integer. File size in bytes, or \code{NA}.}
#'   \item{file_type}{Character. MIME content type (e.g. \code{"text/csv"}), or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' dv_dataset("doi:10.7910/DVN/26721")
#' }
#' @export
dv_dataset <- function(doi) {
  url <- sprintf("%s/datasets/:persistentId?persistentId=%s",
                 .dv_base, utils::URLencode(doi, reserved = TRUE))
  raw <- .fetch_json(url)
  ds <- raw$data
  if (is.null(ds)) return(.schema_dataset)

  version <- ds$latestVersion
  if (is.null(version)) return(.schema_dataset)

  meta <- version$metadataBlocks$citation$fields
  title <- NA_character_
  desc <- NA_character_
  if (is.data.frame(meta)) {
    title_row <- meta[meta$typeName == "title", ]
    if (nrow(title_row) > 0) title <- as.character(title_row$value[1])
    desc_row <- meta[meta$typeName == "dsDescription", ]
    if (nrow(desc_row) > 0) {
      dval <- desc_row$value[[1]]
      if (is.data.frame(dval) && "dsDescriptionValue" %in% names(dval)) {
        desc <- as.character(dval$dsDescriptionValue$value[1])
      }
    }
  }

  files <- version$files
  if (is.null(files) || length(files) == 0) {
    return(tibble(
      name = title, doi = doi, description = desc,
      publisher = as.character(ds$publisher %||% NA_character_),
      published_at = as.character(ds$publicationDate %||% NA_character_),
      file_name = NA_character_, file_id = NA_integer_,
      file_size = NA_integer_, file_type = NA_character_
    ))
  }

  tibble(
    name = title,
    doi = doi,
    description = desc,
    publisher = as.character(ds$publisher %||% NA_character_),
    published_at = as.character(ds$publicationDate %||% NA_character_),
    file_name = as.character(files$dataFile$filename %||% files$label %||% NA_character_),
    file_id = as.integer(files$dataFile$id %||% NA_integer_),
    file_size = as.integer(files$dataFile$filesize %||% NA_integer_),
    file_type = as.character(files$dataFile$contentType %||% NA_character_)
  )
}

#' Get dataverse client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dv_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dv_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/dataverse.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "dataverse")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# dataverse context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# dataverse", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
