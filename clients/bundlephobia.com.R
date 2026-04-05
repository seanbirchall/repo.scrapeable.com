# bundlephobia.com.R - Self-contained bundlephobia.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# bundlephobia-com.R
# Self-contained Bundlephobia API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: unknown — be polite.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bundlephobia_base <- "https://bundlephobia.com/api"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))


# == Schemas ===================================================================

.schema_size <- tibble(
  name = character(), version = character(), description = character(),
  size = integer(), gzip = integer(), dependency_count = integer(),
  has_side_effects = logical(), repository = character()
)

# == Size ======================================================================

#' Get npm package bundle size from Bundlephobia
#'
#' Fetches the minified and gzipped bundle size for the latest version
#' of an npm package. Includes dependency count, side-effect flags, and
#' repository URL.
#'
#' @param package Character. npm package name (e.g. \code{"react"},
#'   \code{"lodash"}, \code{"express"}, \code{"vue"}).
#' @return A tibble with 1 row and 8 columns:
#' \describe{
#'   \item{name}{Character. Package name (e.g. \code{"react"}).}
#'   \item{version}{Character. Latest version analyzed (e.g. \code{"19.2.4"}).}
#'   \item{description}{Character. Package description from npm.}
#'   \item{size}{Integer. Minified bundle size in bytes.}
#'   \item{gzip}{Integer. Gzipped bundle size in bytes.}
#'   \item{dependency_count}{Integer. Number of direct dependencies.}
#'   \item{has_side_effects}{Logical. Whether the package has side effects (affects tree-shaking).}
#'   \item{repository}{Character. Git repository URL.}
#' }
#' @examples
#' bundlephobia_size("react")
#' bundlephobia_size("lodash")
#' @export
bundlephobia_size <- function(package) {
  url <- paste0(.bundlephobia_base, "/size?package=", utils::URLencode(package))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Failed to fetch bundle size for '", package, "': ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_size)

  tibble(
    name             = as.character(raw$name %||% NA_character_),
    version          = as.character(raw$version %||% NA_character_),
    description      = as.character(raw$description %||% NA_character_),
    size             = as.integer(raw$size %||% NA_integer_),
    gzip             = as.integer(raw$gzip %||% NA_integer_),
    dependency_count = as.integer(raw$dependencyCount %||% NA_integer_),
    has_side_effects = as.logical(raw$hasSideEffects %||% NA),
    repository       = as.character(raw$repository %||% NA_character_)
  )
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# == History ==================================================================

#' Get bundle size history across versions for an npm package
#'
#' Returns bundle size data for every published version of a package,
#' useful for tracking how bundle size has changed over time. Early
#' versions may have \code{NA} sizes when Bundlephobia has no data.
#'
#' @param package Character. npm package name (e.g. \code{"react"},
#'   \code{"lodash"}).
#' @return A tibble with one row per version and 4 columns:
#' \describe{
#'   \item{name}{Character. Package name.}
#'   \item{version}{Character. Semver version string (e.g. \code{"4.17.21"}).}
#'   \item{size}{Integer. Minified bundle size in bytes, or \code{NA}.}
#'   \item{gzip}{Integer. Gzipped bundle size in bytes, or \code{NA}.}
#' }
#' @examples
#' bundlephobia_history("lodash")
#' @export
bundlephobia_history <- function(package) {
  url <- paste0(.bundlephobia_base, "/package-history?package=", utils::URLencode(package))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) {
    return(tibble(name = character(), version = character(),
                  size = integer(), gzip = integer()))
  }

  # raw is a named list: version -> {size, gzip}
  if (is.list(raw) && !is.data.frame(raw)) {
    versions <- names(raw)
    rows <- lapply(seq_along(raw), function(i) {
      v <- raw[[i]]
      tibble(
        name = package,
        version = versions[i],
        size = as.integer(v$size %||% NA_integer_),
        gzip = as.integer(v$gzip %||% NA_integer_)
      )
    })
    bind_rows(rows)
  } else {
    tibble(name = character(), version = character(),
           size = integer(), gzip = integer())
  }
}

#' Compare bundle sizes of multiple npm packages
#'
#' Fetches bundle size data for multiple packages and returns them in
#' a single tibble for easy comparison. Calls \code{bundlephobia_size()}
#' for each package; failed lookups are silently dropped.
#'
#' @param packages Character vector. npm package names to compare
#'   (e.g. \code{c("react", "vue", "svelte")}).
#' @return A tibble with one row per package and 8 columns:
#' \describe{
#'   \item{name}{Character. Package name.}
#'   \item{version}{Character. Latest version analyzed.}
#'   \item{description}{Character. Package description from npm.}
#'   \item{size}{Integer. Minified bundle size in bytes.}
#'   \item{gzip}{Integer. Gzipped bundle size in bytes.}
#'   \item{dependency_count}{Integer. Number of direct dependencies.}
#'   \item{has_side_effects}{Logical. Whether the package has side effects.}
#'   \item{repository}{Character. Git repository URL.}
#' }
#' @examples
#' bundlephobia_compare(c("react", "vue", "svelte"))
#' @export
bundlephobia_compare <- function(packages) {
  rows <- lapply(packages, function(pkg) {
    tryCatch(bundlephobia_size(pkg), error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(.schema_size)
  bind_rows(rows)
}

# == Context ===================================================================

#' Get bundlephobia.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bundlephobia_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bundlephobia_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bundlephobia.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bundlephobia.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bundlephobia.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bundlephobia.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
