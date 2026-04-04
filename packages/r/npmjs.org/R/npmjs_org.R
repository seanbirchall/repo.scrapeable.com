# registry.npmjs.org.R - Self-contained registry.npmjs.org client



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

#' Search npm registry for packages
#'
#' Full-text search across the npm package registry. Returns package metadata
#' including version, description, and download links.
#'
#' @param text Character. Search query string (e.g., \code{"express"},
#'   \code{"react"}, \code{"typescript linter"}).
#' @param size Integer. Number of results to return (default 20, max 250).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- package name (e.g., \code{"express"})}
#'     \item{version}{character -- latest published version}
#'     \item{description}{character -- package description}
#'     \item{keywords}{character -- comma-separated keywords}
#'     \item{date}{POSIXct -- date of latest release}
#'     \item{author}{character -- author name}
#'     \item{publisher}{character -- npm publisher username}
#'     \item{links_npm}{character -- URL to the npm package page}
#'   }
#' @export
#' @examples
#' \dontrun{
#' npm_search("express", size = 5)
#' npm_search("typescript linter")
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

#' Get npm package metadata
#'
#' Fetches full metadata for a specific npm package, including its latest
#' version, license, homepage, repository URL, and dependency list.
#'
#' @param name Character. Package name exactly as published on npm (e.g.,
#'   \code{"express"}, \code{"react"}, \code{"lodash"}, \code{"@types/node"}).
#'   Scoped packages use the \code{"@scope/name"} format.
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{name}{character -- package name}
#'     \item{version}{character -- latest version string}
#'     \item{description}{character -- package description}
#'     \item{license}{character -- SPDX license identifier (e.g., \code{"MIT"}, \code{"ISC"})}
#'     \item{homepage}{character -- project homepage URL}
#'     \item{repository}{character -- git repository URL}
#'     \item{created}{POSIXct -- package creation timestamp}
#'     \item{modified}{POSIXct -- last modification timestamp}
#'     \item{dependencies}{character -- comma-separated runtime dependency names}
#'   }
#' @export
#' @examples
#' \dontrun{
#' npm_package("express")
#' npm_package("@types/node")
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

#' Get npmjs.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/npmjs.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "npmjs.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# npmjs.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# npmjs.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
