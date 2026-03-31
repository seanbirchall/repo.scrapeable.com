
# == Package metadata ==========================================================

#' Get PyPI package metadata
#'
#' Returns metadata for a Python package from PyPI, including the latest
#' version info, author, license, and requirements.
#'
#' @param name Package name (e.g. "requests", "numpy", "pandas")
#' @return tibble: name, version, summary, author, author_email, license,
#'   home_page, package_url, requires_python, keywords
#' @export
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
#' @export
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
#' @export
pypi_context <- function() {
  .build_context("pypi.org", header_lines = c(
    "# pypi.org - Python Package Index API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Note: PyPI has no search API. Use pypi_package() for metadata.",
    "# All functions return tibbles with typed columns."
  ))
}
