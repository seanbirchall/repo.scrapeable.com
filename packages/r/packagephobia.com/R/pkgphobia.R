# == Schemas ===================================================================

.schema_size <- tibble(
  name = character(), version = character(), publish_size = integer(),
  install_size = integer()
)

# == Size ======================================================================

#' Get npm package install size from Packagephobia
#'
#' Returns the publish and install size for an npm package.
#' Note: packagephobia.com uses Vercel bot protection which may
#' block automated requests.
#'
#' @param package npm package name (e.g. "express", "lodash")
#' @return tibble: name (character), version (character),
#'   publish_size (integer), install_size (integer)
#' @export
pkgphobia_size <- function(package) {
  url <- paste0(.pkgphobia_base, "/api.json?p=", utils::URLencode(package))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Failed to fetch package size for '", package, "': ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_size)

  tibble(
    name         = as.character(raw$name %||% package),
    version      = as.character(raw$version %||% NA_character_),
    publish_size = as.integer(raw$publishSize %||% NA_integer_),
    install_size = as.integer(raw$installSize %||% NA_integer_)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the packagephobia.com package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
#' @export
pkgphobia_context <- function() {
  .build_context("packagephobia.com", header_lines = c(
    "# packagephobia.com - npm Package Install Size Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (but Vercel bot protection may block requests)",
    "# All functions return tibbles with typed columns."
  ))
}
