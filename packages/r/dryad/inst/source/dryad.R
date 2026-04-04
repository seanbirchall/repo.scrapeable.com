# dryad.R
# Self-contained Dryad API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dryad_base <- "https://datadryad.org/api/v2"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

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
#' Returns a paginated listing of publicly available datasets on the Dryad
#' Digital Repository (\url{https://datadryad.org}), a curated resource for
#' scientific and medical research data. Datasets are ordered by the API's
#' default sort (typically newest first).
#'
#' @param per_page Integer. Number of results per page (default 10, max 100).
#' @param page Integer. Page number, 1-indexed (default 1).
#' @return A tibble with one row per dataset and columns:
#' \describe{
#'   \item{id}{Integer. Internal Dryad dataset identifier, or \code{NA}.}
#'   \item{doi}{Character. Dataset DOI, or \code{NA}.}
#'   \item{title}{Character. Dataset title.}
#'   \item{abstract}{Character. Dataset abstract / description, or \code{NA}.}
#'   \item{published_date}{Date. Publication date, or \code{NA}.}
#'   \item{storage_size}{Numeric. Total storage size in bytes, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' dryad_datasets()
#' dryad_datasets(per_page = 50, page = 2)
#' }
#' @export
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
#' Retrieves detailed metadata for a single Dryad dataset including its
#' authors, keywords, and license information. The \code{authors} and
#' \code{keywords} columns are semicolon-delimited strings.
#'
#' @param doi Character. Dataset DOI with prefix (e.g.
#'   \code{"doi:10.5061/dryad.1234"}).
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{id}{Integer. Internal dataset identifier, or \code{NA}.}
#'   \item{doi}{Character. Dataset DOI.}
#'   \item{title}{Character. Dataset title.}
#'   \item{abstract}{Character. Full abstract, or \code{NA}.}
#'   \item{published_date}{Date. Publication date, or \code{NA}.}
#'   \item{storage_size}{Numeric. Total size in bytes, or \code{NA}.}
#'   \item{authors}{Character. Semicolon-separated author names, or \code{NA}.}
#'   \item{keywords}{Character. Semicolon-separated keywords, or \code{NA}.}
#'   \item{license}{Character. License URL or identifier, or \code{NA}.}
#' }
#' @examples
#' \dontrun{
#' dryad_dataset("doi:10.5061/dryad.1234")
#' }
#' @export
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

# == Context (LLM injection) ==================================================

#' Get dryad client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/dryad.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "dryad")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# dryad context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# dryad", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
