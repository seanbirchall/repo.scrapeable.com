# sources-debian-org.R
# Self-contained Debian Sources API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.deb_base <- "https://sources.debian.org/api"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
    j <- fi - 1; rox_start <- fi
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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_package <- tibble(
  package = character(), version = character(), area = character(),
  suites = character()
)

.schema_search <- tibble(
  name = character(), match_type = character()
)

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
