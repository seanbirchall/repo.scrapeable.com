# cran.r-project.org.R - Self-contained cran.r-project.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


#' Search CRAN packages by name
#'
#' Queries the Posit Package Manager API for CRAN packages whose name
#' matches the search term. Returns basic metadata for each match.
#'
#' @param query Character. Search term matched against package names
#'   (e.g. \code{"dplyr"}, \code{"ggplot"}).
#' @param size Integer. Maximum number of results (default 30).
#' @return A tibble with columns:
#'   \describe{
#'     \item{package}{Character. Package name.}
#'     \item{version}{Character. Latest CRAN version string.}
#'     \item{title}{Character. One-line package title (may be empty).}
#'     \item{description}{Character. Longer description (may be empty).}
#'     \item{maintainer}{Character. Package maintainer (may be empty).}
#'   }
#' @examples
#' cran_search("dplyr", size = 5)
#' cran_search("machine learning")
#' @export
cran_search <- function(query, size = 30L) {
  schema <- tibble::tibble(package = character(), version = character(),
                           title = character(), description = character(),
                           maintainer = character())
  url <- sprintf(
    "https://packagemanager.posit.co/__api__/repos/2/packages?_sort=name&_limit=%d&name_like=%s",
    as.integer(size), utils::URLencode(query, reserved = TRUE)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)

  if (is.list(raw) && !is.data.frame(raw)) {
    rows <- lapply(raw, function(x) {
      tibble::tibble(
        package = as.character(x$name %||% NA_character_),
        version = as.character(x$version %||% NA_character_),
        title = as.character(x$title %||% NA_character_),
        description = as.character(x$description %||% NA_character_),
        maintainer = as.character(x$maintainer %||% NA_character_)
      )
    })
    bind_rows(rows)
  } else if (is.data.frame(raw)) {
    nms <- names(raw)
    as_tibble(raw) |>
      transmute(
        package = as.character(name),
        version = as.character(version),
        title = as.character(if ("title" %in% nms) title else NA_character_),
        description = as.character(if ("description" %in% nms) description else NA_character_),
        maintainer = as.character(if ("maintainer" %in% nms) maintainer else NA_character_)
      )
  } else {
    schema
  }
}

#' Get detailed CRAN package metadata
#'
#' Fetches comprehensive metadata for a single CRAN package from the
#' crandb API, including dependencies, license, and description.
#'
#' @param package Character. Exact CRAN package name (e.g. \code{"ggplot2"}).
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{package}{Character. Package name.}
#'     \item{version}{Character. Latest version (e.g. \code{"4.0.2"}).}
#'     \item{title}{Character. One-line title.}
#'     \item{description}{Character. Full description text.}
#'     \item{date}{Character. Last publication date (ISO 8601).}
#'     \item{maintainer}{Character. Current maintainer name and email.}
#'     \item{license}{Character. License string (e.g. \code{"MIT + file LICENSE"}).}
#'     \item{url}{Character. Package homepage URL(s).}
#'     \item{depends}{Character. Comma-separated Depends packages.}
#'     \item{imports}{Character. Comma-separated Imports packages.}
#'   }
#' @examples
#' cran_package("ggplot2")
#' cran_package("dplyr")
#' @export
cran_package <- function(package) {
  url <- sprintf("https://crandb.r-pkg.org/%s", utils::URLencode(package))
  raw <- .fetch_json(url)
  tibble::tibble(
    package = raw$Package %||% NA_character_,
    version = raw$Version %||% NA_character_,
    title = raw$Title %||% NA_character_,
    description = raw$Description %||% NA_character_,
    date = raw$date %||% NA_character_,
    maintainer = raw$Maintainer %||% NA_character_,
    license = raw$License %||% NA_character_,
    url = if (!is.null(raw$URL)) raw$URL else NA_character_,
    depends = if (!is.null(raw$Depends)) paste(names(raw$Depends), collapse = ", ") else NA_character_,
    imports = if (!is.null(raw$Imports)) paste(names(raw$Imports), collapse = ", ") else NA_character_
  )
}

#' Get daily download counts for a CRAN package
#'
#' Retrieves daily download statistics from the cranlogs API (RStudio
#' CRAN mirror). Useful for tracking package popularity over time.
#'
#' @param package Character. CRAN package name (e.g. \code{"dplyr"}).
#' @param period Character. Time window: \code{"last-day"},
#'   \code{"last-week"}, \code{"last-month"} (default), or a custom
#'   date range as \code{"YYYY-MM-DD:YYYY-MM-DD"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Calendar date of the download count.}
#'     \item{downloads}{Integer. Number of downloads on that day.}
#'   }
#' @examples
#' cran_downloads("dplyr", period = "last-week")
#' cran_downloads("ggplot2", period = "2024-01-01:2024-01-31")
#' @export
cran_downloads <- function(package, period = "last-month") {
  url <- sprintf("https://cranlogs.r-pkg.org/downloads/daily/%s/%s",
                 period, utils::URLencode(package))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble(date = as.Date(character()), downloads = integer()))

  downloads <- raw[[1]]$downloads
  if (is.null(downloads) || length(downloads) == 0) return(tibble(date = as.Date(character()), downloads = integer()))

  tibble(
    date = as.Date(vapply(downloads, function(d) d$day %||% NA_character_, character(1))),
    downloads = vapply(downloads, function(d) as.integer(d$downloads %||% 0L), integer(1))
  )
}

#' Get the most downloaded CRAN packages
#'
#' Returns a ranked list of the most downloaded packages on the RStudio
#' CRAN mirror for a given time period.
#'
#' @param count Integer. Number of top packages to return (default 25).
#' @param period Character. Time window: \code{"last-day"},
#'   \code{"last-week"}, or \code{"last-month"} (default).
#' @return A tibble with columns:
#'   \describe{
#'     \item{rank}{Integer. Rank position (1 = most downloaded).}
#'     \item{package}{Character. Package name.}
#'     \item{downloads}{Integer. Total downloads in the period.}
#'   }
#' @examples
#' cran_top(count = 10)
#' cran_top(count = 5, period = "last-week")
#' @export
cran_top <- function(count = 25, period = "last-month") {
  url <- sprintf("https://cranlogs.r-pkg.org/top/%s/%d", period, as.integer(count))
  raw <- .fetch_json(url)
  if (is.null(raw)) return(tibble(rank = integer(), package = character(), downloads = integer()))

  pkgs <- raw$downloads
  if (is.null(pkgs) || length(pkgs) == 0) return(tibble(rank = integer(), package = character(), downloads = integer()))

  tibble(
    rank = seq_along(pkgs),
    package = vapply(pkgs, function(p) p$package %||% NA_character_, character(1)),
    downloads = vapply(pkgs, function(p) as.integer(p$downloads %||% 0L), integer(1))
  )
}

#' Get reverse dependencies for a CRAN package
#'
#' Returns all CRAN packages that depend on, import, suggest, or enhance
#' the specified package. Useful for assessing a package's ecosystem impact.
#'
#' @param package Character. CRAN package name (e.g. \code{"ggplot2"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{package}{Character. Name of the reverse-dependent package.}
#'     \item{type}{Character. Dependency type: \code{"Depends"},
#'       \code{"Imports"}, \code{"Suggests"}, or \code{"Enhances"}.}
#'   }
#' @examples
#' cran_deps("ggplot2")
#' cran_deps("Rcpp")
#' @export
cran_deps <- function(package) {
  url <- sprintf("https://crandb.r-pkg.org/-/revdeps/%s", utils::URLencode(package))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble(package = character(), type = character()))

  pkg_data <- raw[[1]]
  if (is.null(pkg_data)) return(tibble(package = character(), type = character()))

  rows <- list()
  for (dep_type in c("Depends", "Imports", "Suggests", "Enhances")) {
    deps <- pkg_data[[dep_type]]
    if (!is.null(deps) && length(deps) > 0) {
      dep_pkgs <- unlist(deps)
      if (length(dep_pkgs) > 0) {
        rows[[length(rows) + 1]] <- tibble(
          package = as.character(dep_pkgs),
          type = dep_type
        )
      }
    }
  }
  if (length(rows) == 0) return(tibble(package = character(), type = character()))
  bind_rows(rows)
}

# == Context ===================================================================

#' Get r-project.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cran_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cran_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/r-project.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "r-project.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# r-project.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# r-project.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
