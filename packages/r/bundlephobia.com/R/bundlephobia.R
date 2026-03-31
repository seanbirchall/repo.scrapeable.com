# == Schemas ===================================================================

.schema_size <- tibble(
  name = character(), version = character(), description = character(),
  size = integer(), gzip = integer(), dependency_count = integer(),
  has_side_effects = logical(), repository = character()
)

# == Size ======================================================================

#' Get npm package bundle size from Bundlephobia
#'
#' Returns the minified and gzipped bundle size for an npm package,
#' plus dependency count and metadata.
#'
#' @param package npm package name (e.g. "react", "lodash", "express")
#' @return tibble: name (character), version (character), description (character),
#'   size (integer), gzip (integer), dependency_count (integer),
#'   has_side_effects (logical), repository (character)
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the bundlephobia.com package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
#' @export
bundlephobia_context <- function() {
  .build_context("bundlephobia.com", header_lines = c(
    "# bundlephobia.com - npm Package Bundle Size Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: unknown, be polite",
    "# All functions return tibbles with typed columns."
  ))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
