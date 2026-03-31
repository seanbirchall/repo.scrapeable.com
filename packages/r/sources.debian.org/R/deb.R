# == Public functions ==========================================================

#' Get information about a Debian source package
#'
#' @param name Package name (e.g. "bash", "python3", "nginx", "curl")
#' @return tibble: package, version, area, suites (one row per version)
#' @export
deb_package <- function(name) {
  url <- sprintf("%s/src/%s/", .deb_base,
                 utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$versions) || length(raw$versions) == 0) {
    return(.schema_package)
  }

  rows <- lapply(raw$versions, function(v) {
    suites_str <- if (!is.null(v$suites) && length(v$suites) > 0) {
      paste(unlist(v$suites), collapse = ", ")
    } else NA_character_
    tibble(
      package = as.character(raw$package %||% name),
      version = as.character(v$version %||% NA),
      area    = as.character(v$area %||% NA),
      suites  = suites_str
    )
  })
  bind_rows(rows)
}

#' Search for Debian source packages by name
#'
#' @param query Package name or partial name (e.g. "python", "lib", "nginx")
#' @return tibble: name, match_type ("exact" or "other")
#' @export
deb_search <- function(query) {
  url <- sprintf("%s/search/%s/", .deb_base,
                 utils::URLencode(query, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results)) return(.schema_search)

  rows <- list()
  if (!is.null(raw$results$exact) && !is.null(raw$results$exact$name)) {
    rows[[1]] <- tibble(
      name = as.character(raw$results$exact$name),
      match_type = "exact"
    )
  }
  if (!is.null(raw$results$other) && length(raw$results$other) > 0) {
    others <- lapply(raw$results$other, function(x) {
      tibble(
        name = as.character(x$name %||% NA),
        match_type = "other"
      )
    })
    rows <- c(rows, others)
  }
  if (length(rows) == 0) return(.schema_search)
  bind_rows(rows)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the sources.debian.org package
#'
#' @return Character string (invisibly), also printed
#' @export
deb_context <- function() {
  .build_context("sources.debian.org", header_lines = c(
    "# sources.debian.org - Debian Source Packages Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: none documented",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common packages: bash, python3, nginx, curl, linux, gcc, systemd",
    "# Suites: sid (unstable), trixie (testing), bookworm (stable),",
    "#   bullseye (oldstable), buster (oldoldstable)"
  ))
}
