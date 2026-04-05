# packages.debian.org.R - Self-contained packages.debian.org client

library(httr2)
library(jsonlite)
library(tibble)



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://sources.debian.org/api"

#' Search Debian packages
#'
#' Searches the Debian Sources API for packages matching a name query.
#' Returns exact matches first, followed by partial matches.
#'
#' @param query Character. Package name or partial name to search for
#'   (e.g., \code{"python"}, \code{"bash"}, \code{"linux"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{package}{Character. Debian source package name.}
#'   }
#' @examples
#' dpkg_search("python")
#' dpkg_search("curl")
dpkg_search <- function(query) {
  url <- sprintf("%s/search/%s/", .base, URLencode(query, TRUE))
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble::tibble(package = character()))
  if (is.list(results) && !is.null(results$exact)) {
    pkgs <- c(if (!is.null(results$exact)) list(results$exact) else list(),
             if (!is.null(results$other)) results$other else list())
  } else {
    pkgs <- results
  }
  tibble::tibble(
    package = vapply(pkgs, function(x) {
      if (is.character(x)) x else if (is.list(x) && !is.null(x$name)) x$name else NA_character_
    }, character(1))
  )
}

#' Get Debian package info
#'
#' Retrieves version history and suite (release) availability for a
#' specific Debian source package.
#'
#' @param package Character. Exact Debian source package name
#'   (e.g., \code{"bash"}, \code{"python3"}, \code{"curl"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{version}{Character. Package version string (e.g., "5.2.37-2").}
#'     \item{suites}{Character. Comma-separated Debian suite names where this
#'       version is available (e.g., "bookworm", "trixie, sid").}
#'   }
#' @examples
#' dpkg_info("bash")
#' dpkg_info("curl")
dpkg_info <- function(package) {
  url <- sprintf("%s/src/%s/", .base, URLencode(package, TRUE))
  raw <- .fetch_json(url)
  versions <- raw$versions
  if (length(versions) == 0) return(tibble::tibble(version = character(), suites = character()))
  tibble::tibble(
    version = vapply(versions, function(x) x$version %||% NA_character_, character(1)),
    suites = vapply(versions, function(x) paste(unlist(x$suites), collapse = ", "), character(1))
  )
}

#' Get debian.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dpkg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dpkg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/debian.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "debian.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# debian.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# debian.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
