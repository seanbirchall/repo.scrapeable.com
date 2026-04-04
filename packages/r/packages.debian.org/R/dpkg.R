# packages.debian.org.R - Self-contained packages.debian.org client




.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://sources.debian.org/api"

#' Search Debian source packages by name
#'
#' Queries the Debian Sources API (\code{sources.debian.org}) for source
#' packages whose name matches the given query. Returns exact and partial
#' matches as a simple list of package names. Use \code{\link{dpkg_info}}
#' to retrieve version and suite details for a specific package.
#'
#' @param query Package name or partial name to search for (e.g.
#'   \code{"nginx"}, \code{"python3"}).
#' @return A tibble with one column:
#'   \describe{
#'     \item{package}{Matching source package name (character).}
#'   }
#' @export
#' @seealso \code{\link{dpkg_info}}
#' @examples
#' \dontrun{
#' dpkg_search("nginx")
#' dpkg_search("linux")
#' }
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

#' Get version and suite information for a Debian source package
#'
#' Fetches version details for a named Debian source package from the
#' Debian Sources API. Each row represents a distinct version of the
#' package and the Debian suite(s) (e.g. \code{"bookworm"},
#' \code{"trixie"}, \code{"sid"}) in which that version is available.
#'
#' @param package Exact source package name (e.g. \code{"nginx"},
#'   \code{"coreutils"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{version}{Package version string (character).}
#'     \item{suites}{Comma-separated Debian suite names where this version
#'       is published (character).}
#'   }
#' @export
#' @seealso \code{\link{dpkg_search}}
#' @examples
#' \dontrun{
#' dpkg_info("nginx")
#' dpkg_info("coreutils")
#' }
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

#' Get packages.debian.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/packages.debian.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "packages.debian.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# packages.debian.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# packages.debian.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
