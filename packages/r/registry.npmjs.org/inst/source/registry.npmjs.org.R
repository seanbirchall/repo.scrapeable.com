# registry.npmjs.org.R - Self-contained registry.npmjs.org client

library(httr2)
library(jsonlite)
library(tibble)


# registry-npmjs-org.R
# Self-contained npm registry API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.npm_base <- "https://registry.npmjs.org"

`%||%` <- function(a, b) if (is.null(a)) b else a
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

.schema_search <- tibble(
  name = character(), version = character(), description = character(),
  keywords = character(), date = as.POSIXct(character()),
  author = character(), publisher = character(), links_npm = character()
)

.schema_package <- tibble(
  name = character(), version = character(), description = character(),
  license = character(), homepage = character(), repository = character(),
  created = as.POSIXct(character()), modified = as.POSIXct(character()),
  dependencies = character()
)


# == Search ====================================================================

#' Search the npm registry for packages
#'
#' Queries the npm registry search endpoint for JavaScript / Node.js
#' packages matching a free-text query. Results are ranked by npm's
#' internal relevance scoring (popularity, quality, maintenance).
#'
#' @param text Free-text search query (e.g. \code{"express"},
#'   \code{"react state management"}).
#' @param size Number of results to return (default 20, maximum 250).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Package name on npm (character).}
#'     \item{version}{Latest published version string (character).}
#'     \item{description}{Short package description (character).}
#'     \item{keywords}{Comma-separated keywords (character).}
#'     \item{date}{Date/time of the latest publish (POSIXct).}
#'     \item{author}{Author name, if declared (character).}
#'     \item{publisher}{npm username of the latest publisher (character).}
#'     \item{links_npm}{URL to the npm package page (character).}
#'   }
#' @export
#' @seealso \code{\link{npm_package}}
#' @examples
#' \dontrun{
#' npm_search("express", size = 5)
#' npm_search("react state management")
#' }
npm_search <- function(text, size = 20) {
  url <- sprintf("%s/-/v1/search?text=%s&size=%d",
                 .npm_base, utils::URLencode(text), as.integer(size))
  raw <- .fetch_json(url)
  objects <- raw$objects
  if (is.null(objects) || length(objects) == 0) return(.schema_search)

  pkg <- objects$package
  tibble(
    name        = as.character(pkg$name %||% NA_character_),
    version     = as.character(pkg$version %||% NA_character_),
    description = as.character(pkg$description %||% NA_character_),
    keywords    = vapply(pkg$keywords %||% list(), function(x) paste(x, collapse = ", "), character(1)),
    date        = as.POSIXct(pkg$date %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    author      = as.character(pkg$author$name %||% NA_character_),
    publisher   = as.character(pkg$publisher$username %||% NA_character_),
    links_npm   = as.character(pkg$links$npm %||% NA_character_)
  )
}

# == Package metadata ==========================================================

#' Get detailed metadata for an npm package
#'
#' Fetches the full registry document for a single npm package and
#' extracts metadata from the latest published version. Includes the
#' license, homepage, source repository URL, creation and modification
#' timestamps, and a comma-separated list of runtime dependencies.
#'
#' @param name Exact package name on npm (e.g. \code{"express"},
#'   \code{"react"}, \code{"lodash"}). Scoped packages use the form
#'   \code{"@scope/name"}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{name}{Package name (character).}
#'     \item{version}{Latest published version (character).}
#'     \item{description}{Package description (character).}
#'     \item{license}{SPDX license identifier (character).}
#'     \item{homepage}{Project homepage URL (character).}
#'     \item{repository}{Source repository URL (character).}
#'     \item{created}{Date/time the package was first published (POSIXct).}
#'     \item{modified}{Date/time of the most recent publish (POSIXct).}
#'     \item{dependencies}{Comma-separated runtime dependency names (character).}
#'   }
#' @export
#' @seealso \code{\link{npm_search}}
#' @examples
#' \dontrun{
#' npm_package("express")
#' npm_package("@anthropic-ai/sdk")
#' }
npm_package <- function(name) {
  url <- sprintf("%s/%s", .npm_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("npm package fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_package)

  latest_ver <- raw$`dist-tags`$latest %||% NA_character_
  latest <- raw$versions[[latest_ver]]
  deps <- if (!is.null(latest$dependencies)) paste(names(latest$dependencies), collapse = ", ") else NA_character_
  repo_url <- if (is.list(raw$repository)) raw$repository$url %||% NA_character_ else raw$repository %||% NA_character_

  tibble(
    name         = as.character(raw$name %||% NA_character_),
    version      = as.character(latest_ver),
    description  = as.character(raw$description %||% NA_character_),
    license      = as.character(raw$license %||% NA_character_),
    homepage     = as.character(raw$homepage %||% NA_character_),
    repository   = as.character(repo_url),
    created      = as.POSIXct(raw$time$created %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    modified     = as.POSIXct(raw$time$modified %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    dependencies = as.character(deps)
  )
}

# == Context ===================================================================

#' Get registry.npmjs.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
npm_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(npm_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/registry.npmjs.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "registry.npmjs.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# registry.npmjs.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# registry.npmjs.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
