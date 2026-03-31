# pypi-org.R
# Self-contained PyPI (Python Package Index) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Note: PyPI has no search API; only package metadata endpoint exists.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pypi_base <- "https://pypi.org"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_package <- tibble(
  name = character(), version = character(), summary = character(),
  author = character(), author_email = character(), license = character(),
  home_page = character(), package_url = character(),
  requires_python = character(), keywords = character()
)

.schema_releases <- tibble(
  version = character(), upload_time = as.POSIXct(character()),
  python_version = character(), size = integer(),
  packagetype = character(), filename = character(), url = character()
)

# == Package metadata ==========================================================

#' Get PyPI package metadata
#'
#' Returns metadata for a Python package from PyPI, including the latest
#' version info, author, license, and requirements.
#'
#' @param name Package name (e.g. "requests", "numpy", "pandas")
#' @return tibble: name, version, summary, author, author_email, license,
#'   home_page, package_url, requires_python, keywords
pypi_package <- function(name) {
  url <- sprintf("%s/pypi/%s/json", .pypi_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("PyPI fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_package)

  info <- raw$info
  tibble(
    name            = as.character(info$name %||% NA_character_),
    version         = as.character(info$version %||% NA_character_),
    summary         = as.character(info$summary %||% NA_character_),
    author          = as.character(info$author %||% NA_character_),
    author_email    = as.character(info$author_email %||% NA_character_),
    license         = as.character(info$license %||% NA_character_),
    home_page       = as.character(info$home_page %||% NA_character_),
    package_url     = as.character(info$package_url %||% NA_character_),
    requires_python = as.character(info$requires_python %||% NA_character_),
    keywords        = as.character(info$keywords %||% NA_character_)
  )
}

#' Get PyPI package release history
#'
#' Returns all releases (versions) for a package with upload timestamps
#' and file details.
#'
#' @param name Package name (e.g. "requests", "numpy")
#' @param limit Maximum number of versions to return (default 20)
#' @return tibble: version, upload_time, python_version, size,
#'   packagetype, filename, url
pypi_releases <- function(name, limit = 20) {
  url <- sprintf("%s/pypi/%s/json", .pypi_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("PyPI fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_releases)

  rels <- raw$releases
  if (is.null(rels) || length(rels) == 0) return(.schema_releases)

  rows <- list()
  ver_names <- names(rels)
  # Take last N versions (most recent)
  ver_names <- tail(ver_names, limit)
  for (v in ver_names) {
    files <- rels[[v]]
    if (is.null(files) || length(files) == 0 || (is.data.frame(files) && nrow(files) == 0)) {
      rows[[length(rows) + 1]] <- tibble(
        version = v, upload_time = as.POSIXct(NA),
        python_version = NA_character_, size = NA_integer_,
        packagetype = NA_character_, filename = NA_character_, url = NA_character_
      )
    } else {
      f <- if (is.data.frame(files)) files[1, ] else files[[1]]
      rows[[length(rows) + 1]] <- tibble(
        version        = v,
        upload_time    = as.POSIXct(f$upload_time %||% NA_character_),
        python_version = as.character(f$python_version %||% NA_character_),
        size           = as.integer(f$size %||% NA_integer_),
        packagetype    = as.character(f$packagetype %||% NA_character_),
        filename       = as.character(f$filename %||% NA_character_),
        url            = as.character(f$url %||% NA_character_)
      )
    }
  }
  bind_rows(rows)
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the pypi.org package
#'
#' @return Character string (invisibly), also printed
pypi_context <- function() {
  .build_context("pypi.org", header_lines = c(
    "# pypi.org - Python Package Index API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Note: PyPI has no search API. Use pypi_package() for metadata.",
    "# All functions return tibbles with typed columns."
  ))
}
