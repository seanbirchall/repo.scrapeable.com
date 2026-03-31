# registry-npmjs-org.R
# Self-contained npm registry API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.npm_base <- "https://registry.npmjs.org"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
#' @param text Search query string
#' @param size Number of results to return (default 20, max 250)
#' @return tibble: name, version, description, keywords, date, author,
#'   publisher, links_npm
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
npm_context <- function() {
  .build_context("registry.npmjs.org", header_lines = c(
    "# registry.npmjs.org - npm Registry API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns."
  ))
}
