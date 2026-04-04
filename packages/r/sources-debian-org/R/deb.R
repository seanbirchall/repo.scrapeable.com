# sources-debian-org.R
# Self-contained Debian Sources API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Queries the Debian Sources API for all available versions of a source
#' package across Debian suites (stable, testing, unstable, etc.).
#'
#' @param name Package name (e.g. "bash", "python3", "nginx", "curl")
#' @return A tibble with columns:
#'   \describe{
#'     \item{package}{Source package name (character)}
#'     \item{version}{Debian package version string (character)}
#'     \item{area}{Repository area: "main", "contrib", or "non-free" (character)}
#'     \item{suites}{Comma-separated suite names, e.g. "bookworm, sid" (character)}
#'   }
#' @examples
#' deb_package("curl")
#' deb_package("nginx")
#' @seealso [deb_search()], [deb_context()]
#' @source <https://sources.debian.org/doc/api/>
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
#' Searches the Debian Sources index for packages matching a name or prefix.
#' Returns exact matches (if any) and partial matches.
#'
#' @param query Package name or partial name (e.g. "python", "lib", "nginx")
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Package name (character)}
#'     \item{match_type}{"exact" for exact name match or "other" for partial (character)}
#'   }
#' @examples
#' deb_search("python")
#' deb_search("nginx")
#' @seealso [deb_package()], [deb_context()]
#' @source <https://sources.debian.org/doc/api/>
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

#' Get sources-debian-org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
deb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(deb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/sources-debian-org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "sources-debian-org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# sources-debian-org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# sources-debian-org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
