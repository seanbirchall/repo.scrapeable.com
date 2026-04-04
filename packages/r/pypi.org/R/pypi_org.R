# pypi.org.R - Self-contained pypi.org client



# pypi-org.R
# Self-contained PyPI (Python Package Index) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Note: PyPI has no search API; only package metadata endpoint exists.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pypi_base <- "https://pypi.org"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
#' Fetches the latest release metadata for a Python package from the PyPI
#' JSON API. Returns author information, version, license, Python version
#' requirements, and links to the project homepage.
#'
#' @param name Character string. Package name on PyPI (e.g., \code{"requests"},
#'   \code{"numpy"}, \code{"pandas"}). Case-insensitive.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{name}{Character. Canonical package name.}
#'     \item{version}{Character. Latest release version string.}
#'     \item{summary}{Character. One-line package description.}
#'     \item{author}{Character. Package author name.}
#'     \item{author_email}{Character. Author contact email.}
#'     \item{license}{Character. License identifier (e.g., \code{"MIT"}).}
#'     \item{home_page}{Character. Project homepage URL.}
#'     \item{package_url}{Character. PyPI page URL.}
#'     \item{requires_python}{Character. Minimum Python version (e.g., \code{">=3.8"}).}
#'     \item{keywords}{Character. Comma-separated keywords.}
#'   }
#'   Returns an empty tibble if the package is not found.
#' @examples
#' pypi_package("requests")
#' pypi_package("numpy")
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
#' Returns the release history for a Python package, with one row per version.
#' Each row includes the upload timestamp, file size, Python version tag,
#' and direct download URL for the first distribution file of that release.
#' Versions are returned in chronological order (oldest first).
#'
#' @param name Character string. Package name on PyPI (e.g., \code{"requests"}).
#' @param limit Integer. Maximum number of versions to return (default 20).
#'   Returns the most recent \code{limit} versions.
#' @return A tibble with columns:
#'   \describe{
#'     \item{version}{Character. Version string (e.g., \code{"2.28.1"}).}
#'     \item{upload_time}{POSIXct. UTC timestamp of upload.}
#'     \item{python_version}{Character. Python version tag (e.g., \code{"py3"}).}
#'     \item{size}{Integer. File size in bytes.}
#'     \item{packagetype}{Character. Distribution type (\code{"bdist_wheel"},
#'       \code{"sdist"}).}
#'     \item{filename}{Character. Distribution filename.}
#'     \item{url}{Character. Direct download URL.}
#'   }
#'   Returns an empty tibble if the package is not found.
#' @examples
#' pypi_releases("requests", limit = 5)
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

# == Context ===================================================================

#' Get pypi.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pypi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pypi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/pypi.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "pypi.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# pypi.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# pypi.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
