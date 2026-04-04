# osv.dev.R - Self-contained osv.dev client

library(httr2)
library(jsonlite)
library(tibble)


# osv-dev.R
# Self-contained OSV (Open Source Vulnerabilities) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osv_base <- "https://api.osv.dev/v1"
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
#' Searches the Open Source Vulnerabilities (OSV) database for known
#' security vulnerabilities affecting a specific package. Covers CVEs,
#' GitHub Security Advisories (GHSA), and ecosystem-specific advisories.
#'
#' @param package Character. Package name as listed in its ecosystem registry
#'   (e.g., \code{"lodash"}, \code{"requests"}, \code{"log4j-core"},
#'   \code{"github.com/gin-gonic/gin"}).
#' @param ecosystem Character. Package ecosystem identifier. Valid values:
#'   \code{"npm"}, \code{"PyPI"}, \code{"crates.io"}, \code{"Maven"},
#'   \code{"Go"}, \code{"NuGet"}, \code{"RubyGems"}, \code{"Packagist"},
#'   \code{"Hex"}, \code{"Pub"}, \code{"CRAN"}, \code{"Bioconductor"}.
#'   Default \code{"npm"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- vulnerability ID (e.g., \code{"GHSA-xxjr-mmjv-4gpg"}, \code{"CVE-2021-44228"})}
#'     \item{summary}{character -- one-line vulnerability summary}
#'     \item{published}{character -- ISO 8601 publication timestamp}
#'     \item{modified}{character -- ISO 8601 last-modified timestamp}
#'     \item{severity}{character -- severity level: \code{"CRITICAL"}, \code{"HIGH"}, \code{"MODERATE"}, or \code{"LOW"}}
#'   }
#' @export
#' @examples
#' \dontrun{
#' osv_query("lodash", "npm")
#' osv_query("requests", "PyPI")
#' }
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
#' Retrieves the full advisory record for a single vulnerability, including
#' a detailed Markdown description, aliases (cross-references to CVE / GHSA),
#' and severity rating.
#'
#' @param id Character. OSV vulnerability identifier (e.g.,
#'   \code{"GHSA-jfh8-c2jp-5v3q"}, \code{"CVE-2021-44228"},
#'   \code{"RUSTSEC-2021-0078"}).
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{id}{character -- vulnerability ID}
#'     \item{summary}{character -- one-line summary}
#'     \item{details}{character -- full Markdown description}
#'     \item{published}{character -- ISO 8601 publication timestamp}
#'     \item{modified}{character -- ISO 8601 last-modified timestamp}
#'     \item{aliases}{character -- comma-separated alias IDs (e.g., CVE cross-references)}
#'     \item{severity}{character -- severity level: \code{"CRITICAL"}, \code{"HIGH"}, \code{"MODERATE"}, or \code{"LOW"}}
#'   }
#' @export
#' @examples
#' \dontrun{
#' osv_vuln("GHSA-jfh8-c2jp-5v3q")
#' osv_vuln("CVE-2021-44228")
#' }
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

# == Context ===================================================================

#' Get osv.dev client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
osv_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(osv_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/osv.dev.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "osv.dev")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# osv.dev context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# osv.dev", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
