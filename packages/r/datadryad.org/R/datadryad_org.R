# datadryad.org.R - Self-contained datadryad.org client



# dryad.R
# Self-contained Dryad API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dryad_base <- "https://datadryad.org/api/v2"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = integer(), doi = character(), title = character(),
  abstract = character(), published_date = as.Date(character()),
  storage_size = numeric()
)

.schema_dataset_detail <- tibble(
  id = integer(), doi = character(), title = character(),
  abstract = character(), published_date = as.Date(character()),
  storage_size = numeric(), authors = character(),
  keywords = character(), license = character()
)

# == Dataset listing ===========================================================

#' List Dryad datasets
#'
#' Retrieves a paginated list of datasets published on Dryad, a curated
#' repository for open research data. Sorted by most recently published.
#'
#' @param per_page Integer. Number of results per page (default 10, max 100).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Internal Dryad dataset identifier.}
#'     \item{doi}{Character. Dataset DOI (e.g., "doi:10.5061/dryad.abc123").}
#'     \item{title}{Character. Title of the dataset.}
#'     \item{abstract}{Character. Dataset abstract or description (may contain HTML).}
#'     \item{published_date}{Date. Date the dataset was published.}
#'     \item{storage_size}{Numeric. Total storage size in bytes.}
#'   }
#' @examples
#' dryad_datasets(per_page = 5)
#' dryad_datasets(per_page = 20, page = 2)
dryad_datasets <- function(per_page = 10, page = 1) {
  url <- paste0(.dryad_base, "/datasets?per_page=", per_page, "&page=", page)
  raw <- .fetch_json(url)
  d <- raw$`_embedded`$`stash:datasets`
  if (is.null(d) || length(d) == 0) return(.schema_datasets)

  as_tibble(d) |>
    transmute(
      id = as.integer(identifier),
      doi = as.character(if ("doi" %in% names(d)) doi else NA_character_),
      title = as.character(title),
      abstract = as.character(if ("abstract" %in% names(d)) abstract else NA_character_),
      published_date = tryCatch(as.Date(if ("publicationDate" %in% names(d)) publicationDate else NA_character_), error = function(e) as.Date(NA)),
      storage_size = as.numeric(if ("storageSize" %in% names(d)) storageSize else NA_real_)
    )
}

# == Dataset detail ============================================================

#' Fetch a single Dryad dataset by DOI
#'
#' Returns full metadata for one dataset including authors, keywords, and
#' license information not available in the listing endpoint.
#'
#' @param doi Character. Dataset DOI, e.g. \code{"doi:10.5061/dryad.1234"}.
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Internal Dryad dataset identifier.}
#'     \item{doi}{Character. The dataset DOI.}
#'     \item{title}{Character. Title of the dataset.}
#'     \item{abstract}{Character. Dataset abstract or description.}
#'     \item{published_date}{Date. Date the dataset was published.}
#'     \item{storage_size}{Numeric. Total storage size in bytes.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'     \item{keywords}{Character. Semicolon-separated keywords.}
#'     \item{license}{Character. License URL or identifier (e.g., CC0 1.0).}
#'   }
#' @examples
#' dryad_dataset("doi:10.5061/dryad.1234")
dryad_dataset <- function(doi) {
  url <- paste0(.dryad_base, "/datasets/", utils::URLencode(doi, reserved = TRUE))
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$identifier)) return(.schema_dataset_detail)

  auth_str <- if (!is.null(raw$authors) && is.data.frame(raw$authors)) {
    fn <- if ("firstName" %in% names(raw$authors)) raw$authors$firstName else ""
    ln <- if ("lastName" %in% names(raw$authors)) raw$authors$lastName else ""
    paste(trimws(paste(fn, ln)), collapse = "; ")
  } else NA_character_

  kw_str <- if (!is.null(raw$keywords)) {
    paste(raw$keywords, collapse = "; ")
  } else NA_character_

  tibble(
    id = as.integer(raw$identifier %||% NA_integer_),
    doi = as.character(raw$doi %||% NA_character_),
    title = as.character(raw$title %||% NA_character_),
    abstract = as.character(raw$abstract %||% NA_character_),
    published_date = tryCatch(as.Date(raw$publicationDate), error = function(e) as.Date(NA)),
    storage_size = as.numeric(raw$storageSize %||% NA_real_),
    authors = auth_str,
    keywords = kw_str,
    license = as.character(raw$license %||% NA_character_)
  )
}

# == Dataset search =============================================================

#' Search Dryad datasets by query
#'
#' Full-text search across Dryad dataset titles, abstracts, and keywords.
#'
#' @param query Character. Search query string (e.g., \code{"genomics"},
#'   \code{"ecology"}, \code{"climate change"}).
#' @param per_page Integer. Number of results per page (default 10, max 100).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Internal Dryad dataset identifier.}
#'     \item{doi}{Character. Dataset DOI.}
#'     \item{title}{Character. Title of the dataset.}
#'     \item{abstract}{Character. Dataset abstract or description.}
#'     \item{published_date}{Date. Date the dataset was published.}
#'     \item{storage_size}{Numeric. Total storage size in bytes.}
#'   }
#' @examples
#' dryad_search("ecology", per_page = 5)
#' dryad_search("genomics", per_page = 20, page = 2)
dryad_search <- function(query, per_page = 10, page = 1) {
  url <- sprintf("%s/search?q=%s&per_page=%d&page=%d",
                 .dryad_base, utils::URLencode(query, reserved = TRUE),
                 as.integer(per_page), as.integer(page))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_datasets)
  d <- raw$`_embedded`$`stash:datasets`
  if (is.null(d) || length(d) == 0) return(.schema_datasets)

  as_tibble(d) |>
    transmute(
      id = as.integer(identifier),
      doi = as.character(if ("doi" %in% names(d)) doi else NA_character_),
      title = as.character(title),
      abstract = as.character(if ("abstract" %in% names(d)) abstract else NA_character_),
      published_date = tryCatch(as.Date(if ("publicationDate" %in% names(d)) publicationDate else NA_character_), error = function(e) as.Date(NA)),
      storage_size = as.numeric(if ("storageSize" %in% names(d)) storageSize else NA_real_)
    )
}

#' List files in a Dryad dataset
#'
#' Retrieves the file manifest for a specific dataset version,
#' showing file paths, sizes, MIME types, and processing status.
#'
#' @param doi Character. Dataset DOI, e.g. \code{"doi:10.5061/dryad.1234"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{path}{Character. File path within the dataset.}
#'     \item{size}{Numeric. File size in bytes.}
#'     \item{mime_type}{Character. MIME content type (e.g., "text/csv", "application/zip").}
#'     \item{status}{Character. Processing status (e.g., "created", "copied").}
#'     \item{description}{Character. File description, if provided.}
#'   }
#' @examples
#' dryad_files("doi:10.5061/dryad.1234")
dryad_files <- function(doi) {
  schema <- tibble(path = character(), size = numeric(), mime_type = character(),
                   status = character(), description = character())
  url <- paste0(.dryad_base, "/datasets/", utils::URLencode(doi, reserved = TRUE),
                "/versions/1/files")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)
  d <- raw$`_embedded`$`stash:files`
  if (is.null(d) || length(d) == 0) return(schema)

  if (is.data.frame(d)) {
    as_tibble(d) |>
      transmute(
        path = as.character(if ("path" %in% names(d)) path else NA_character_),
        size = as.numeric(if ("size" %in% names(d)) size else NA_real_),
        mime_type = as.character(if ("mimeType" %in% names(d)) mimeType else NA_character_),
        status = as.character(if ("status" %in% names(d)) status else NA_character_),
        description = as.character(if ("description" %in% names(d)) description else NA_character_)
      )
  } else {
    tibble(
      path = vapply(d, function(x) x$path %||% NA_character_, character(1)),
      size = vapply(d, function(x) as.numeric(x$size %||% NA_real_), numeric(1)),
      mime_type = vapply(d, function(x) x$mimeType %||% NA_character_, character(1)),
      status = vapply(d, function(x) x$status %||% NA_character_, character(1)),
      description = vapply(d, function(x) x$description %||% NA_character_, character(1))
    )
  }
}

# == Context ===================================================================

#' Get datadryad.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dryad_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dryad_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/datadryad.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "datadryad.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# datadryad.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# datadryad.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
