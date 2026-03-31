# bundlephobia-com.R
# Self-contained Bundlephobia API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: unknown — be polite.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bundlephobia_base <- "https://bundlephobia.com/api"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
#' Returns the minified and gzipped bundle size for an npm package,
#' plus dependency count and metadata.
#'
#' @param package npm package name (e.g. "react", "lodash", "express")
#' @return tibble: name (character), version (character), description (character),
#'   size (integer), gzip (integer), dependency_count (integer),
#'   has_side_effects (logical), repository (character)
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
