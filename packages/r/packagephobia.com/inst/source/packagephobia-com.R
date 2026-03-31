# packagephobia-com.R
# Self-contained Packagephobia API client.
# NOTE: packagephobia.com is behind Vercel bot protection.
# This client may not work from automated environments.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: Vercel bot protection may block requests.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pkgphobia_base <- "https://packagephobia.com"

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

`%||%` <- function(a, b) if (is.null(a)) b else a

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
pkgphobia_context <- function() {
  .build_context("packagephobia.com", header_lines = c(
    "# packagephobia.com - npm Package Install Size Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (but Vercel bot protection may block requests)",
    "# All functions return tibbles with typed columns."
  ))
}
