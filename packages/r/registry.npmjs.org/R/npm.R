
# == Search ====================================================================

#' Search npm registry for packages
#'
#' @param text Search query string
#' @param size Number of results to return (default 20, max 250)
#' @return tibble: name, version, description, keywords, date, author,
#'   publisher, links_npm
#' @export
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
#' Returns metadata for a specific npm package including latest version,
#' description, license, homepage, and dependencies.
#'
#' @param name Package name (e.g. "express", "react", "lodash")
#' @return tibble: name, version, description, license, homepage,
#'   repository, created, modified, dependencies
#' @export
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the registry.npmjs.org package
#'
#' @return Character string (invisibly), also printed
#' @export
npm_context <- function() {
  .build_context("registry.npmjs.org", header_lines = c(
    "# registry.npmjs.org - npm Registry API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns."
  ))
}
