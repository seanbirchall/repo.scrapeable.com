# dataverse.harvard.edu.R - Self-contained dataverse.harvard.edu client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search Harvard Dataverse
#'
#' Searches the Harvard Dataverse repository for datasets, files, or
#' dataverses matching a query string. Supports pagination via
#' \code{start} offset.
#'
#' @param query Character. Search query (e.g. \code{"climate"},
#'   \code{"elections"}, \code{"GDP"}).
#' @param type Character. Result type: \code{"dataset"} (default),
#'   \code{"file"}, or \code{"dataverse"}.
#' @param per_page Integer. Results per page (default 10).
#' @param start Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Dataset or file name.}
#'     \item{type}{Character. Result type (e.g. "dataset").}
#'     \item{url}{Character. Persistent URL to the resource.}
#'     \item{description}{Character. Description text.}
#'     \item{published_at}{Character. ISO-8601 publication timestamp.}
#'     \item{citation}{Character. Formatted citation string.}
#'   }
#' @examples
#' dv_search("climate", per_page = 3)
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
#' Retrieves full metadata and file listing for a Harvard Dataverse dataset
#' identified by its persistent DOI. Returns one row per file in the dataset.
#'
#' @param doi Character. Dataset persistent identifier
#'   (e.g. \code{"doi:10.7910/DVN/EXAMPLE"}).
#' @return A tibble with one row per file and columns:
#'   \describe{
#'     \item{name}{Character. Dataset title.}
#'     \item{doi}{Character. Persistent identifier (echoed back).}
#'     \item{description}{Character. Dataset description.}
#'     \item{publisher}{Character. Publishing institution.}
#'     \item{published_at}{Character. Publication date.}
#'     \item{file_name}{Character. Name of the file in the dataset.}
#'     \item{file_id}{Integer. Dataverse file identifier.}
#'     \item{file_size}{Integer. File size in bytes.}
#'     \item{file_type}{Character. MIME content type.}
#'   }
#' @examples
#' dv_dataset("doi:10.7910/DVN/EXAMPLE")
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

# == Dataverse info =============================================================

#' Get info about a specific dataverse collection
#'
#' Retrieves metadata for a Dataverse collection by its alias. Returns the
#' collection's ID, name, description, and creation date.
#'
#' @param alias Character. Dataverse alias (e.g. \code{"harvard"},
#'   \code{"iqss"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Integer. Dataverse internal identifier.}
#'     \item{alias}{Character. Dataverse alias.}
#'     \item{name}{Character. Dataverse display name.}
#'     \item{description}{Character. Dataverse description (may contain HTML).}
#'     \item{creation_date}{Character. ISO-8601 creation timestamp.}
#'   }
#' @examples
#' dv_dataverse("harvard")
#' @export
dv_dataverse <- function(alias) {
  schema <- tibble(id = integer(), alias = character(), name = character(),
                   description = character(), creation_date = character())
  url <- paste0(.dv_base, "/dataverses/", utils::URLencode(alias))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(schema)
  d <- raw$data

  tibble(
    id = as.integer(d$id %||% NA_integer_),
    alias = as.character(d$alias %||% NA_character_),
    name = as.character(d$name %||% NA_character_),
    description = as.character(d$description %||% NA_character_),
    creation_date = as.character(d$creationDate %||% NA_character_)
  )
}

#' List files in a Harvard Dataverse dataset
#'
#' Lists all files in the latest version of a dataset, returning file
#' identifiers, names, sizes, and content types. Use the \code{file_id}
#' with the Dataverse access API to download individual files.
#'
#' @param doi Character. Dataset persistent identifier
#'   (e.g. \code{"doi:10.7910/DVN/EXAMPLE"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{file_id}{Integer. Dataverse file identifier.}
#'     \item{file_name}{Character. File name.}
#'     \item{file_size}{Integer. File size in bytes.}
#'     \item{content_type}{Character. MIME content type.}
#'     \item{description}{Character. File description (\code{NA} if none).}
#'   }
#' @examples
#' dv_files("doi:10.7910/DVN/EXAMPLE")
#' @export
dv_files <- function(doi) {
  schema <- tibble(file_id = integer(), file_name = character(),
                   file_size = integer(), content_type = character(),
                   description = character())
  url <- sprintf("%s/datasets/:persistentId/versions/:latest/files?persistentId=%s",
                 .dv_base, utils::URLencode(doi, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(schema)
  d <- raw$data
  if (length(d) == 0) return(schema)
  if (!is.data.frame(d)) return(schema)

  tibble(
    file_id = as.integer(d$dataFile$id %||% NA_integer_),
    file_name = as.character(d$dataFile$filename %||% d$label %||% NA_character_),
    file_size = as.integer(d$dataFile$filesize %||% NA_integer_),
    content_type = as.character(d$dataFile$contentType %||% NA_character_),
    description = as.character(d$description %||% NA_character_)
  )
}

# == Context ===================================================================

#' Get harvard.edu client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/harvard.edu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "harvard.edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# harvard.edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# harvard.edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
