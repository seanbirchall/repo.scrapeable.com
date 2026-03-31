# osv-dev.R
# Self-contained OSV (Open Source Vulnerabilities) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osv_base <- "https://api.osv.dev/v1"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

.post_json <- function(url, body) {
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_vulns <- tibble(
  id = character(), summary = character(), published = character(),
  modified = character(), severity = character()
)

.schema_vuln_detail <- tibble(
  id = character(), summary = character(), details = character(),
  published = character(), modified = character(),
  aliases = character(), severity = character()
)

# == Query vulnerabilities =====================================================

#' Query OSV for vulnerabilities affecting a package
#'
#' Searches the OSV database for known vulnerabilities affecting a specific
#' package in a given ecosystem (npm, PyPI, crates.io, etc.).
#'
#' @param package Package name (e.g. "lodash", "requests", "log4j-core")
#' @param ecosystem Package ecosystem: "npm", "PyPI", "crates.io", "Maven",
#'   "Go", "NuGet", "RubyGems", "Packagist", "Hex", "Pub"
#' @return tibble: id (character), summary (character), published (character),
#'   modified (character), severity (character)
osv_query <- function(package, ecosystem = "npm") {
  url <- paste0(.osv_base, "/query")
  body <- list(package = list(name = package, ecosystem = ecosystem))
  raw <- tryCatch(.post_json(url, body), error = function(e) {
    warning("OSV query failed for '", package, "': ", e$message)
    return(list())
  })

  vulns <- raw$vulns
  if (is.null(vulns) || length(vulns) == 0) return(.schema_vulns)

  as_tibble(data.frame(
    id        = vapply(vulns, function(v) v$id %||% NA_character_, character(1)),
    summary   = vapply(vulns, function(v) v$summary %||% NA_character_, character(1)),
    published = vapply(vulns, function(v) v$published %||% NA_character_, character(1)),
    modified  = vapply(vulns, function(v) v$modified %||% NA_character_, character(1)),
    severity  = vapply(vulns, function(v) {
      db <- v$database_specific
      if (!is.null(db) && !is.null(db$severity)) db$severity else NA_character_
    }, character(1)),
    stringsAsFactors = FALSE
  ))
}

# == Get vulnerability detail ==================================================

#' Fetch detailed information about a specific vulnerability
#'
#' @param id OSV vulnerability ID (e.g. "GHSA-jfh8-c2jp-5v3q", "CVE-2021-44228")
#' @return tibble: id, summary, details, published, modified, aliases, severity
osv_vuln <- function(id) {
  url <- paste0(.osv_base, "/vulns/", utils::URLencode(id))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("OSV vuln fetch failed for '", id, "': ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_vuln_detail)

  tibble(
    id        = as.character(raw$id %||% NA_character_),
    summary   = as.character(raw$summary %||% NA_character_),
    details   = as.character(raw$details %||% NA_character_),
    published = as.character(raw$published %||% NA_character_),
    modified  = as.character(raw$modified %||% NA_character_),
    aliases   = paste(unlist(raw$aliases %||% list()), collapse = ", "),
    severity  = as.character((raw$database_specific$severity) %||% NA_character_)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the osv.dev package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
osv_context <- function() {
  .build_context("osv.dev", header_lines = c(
    "# osv.dev - Open Source Vulnerabilities API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Ecosystems: npm, PyPI, crates.io, Maven, Go, NuGet, RubyGems, etc.",
    "# All functions return tibbles with typed columns."
  ))
}
