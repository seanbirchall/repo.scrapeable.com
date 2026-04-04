# crates.io.R - Self-contained crates.io client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# crates-io.R
# Self-contained crates.io (Rust package registry) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent header required)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.crates_base <- "https://crates.io/api/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
  name = character(), description = character(), max_version = character(),
  downloads = integer(), created_at = as.POSIXct(character()),
  updated_at = as.POSIXct(character()), homepage = character(),
  repository = character(), categories = character()
)

.schema_crate <- tibble(
  name = character(), description = character(), max_version = character(),
  downloads = integer(), created_at = as.POSIXct(character()),
  updated_at = as.POSIXct(character()), homepage = character(),
  repository = character(), license = character(),
  max_stable_version = character()
)


# == Search ====================================================================

#' Search crates.io for Rust crates
#'
#' Searches the crates.io registry for Rust packages matching a query string.
#' Results are ranked by relevance and include download counts and metadata.
#'
#' @param query Character. Search query string (e.g., \code{"serde"},
#'   \code{"async runtime"}, \code{"web framework"}).
#' @param per_page Integer. Number of results per page (default 20, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{\code{character} -- Crate name (e.g., "serde")}
#'     \item{description}{\code{character} -- Short crate description}
#'     \item{max_version}{\code{character} -- Latest published version (e.g., "1.0.228")}
#'     \item{downloads}{\code{integer} -- Total all-time download count}
#'     \item{created_at}{\code{POSIXct} -- Date/time crate was first published}
#'     \item{updated_at}{\code{POSIXct} -- Date/time of most recent update}
#'     \item{homepage}{\code{character} -- Homepage URL (may be NA)}
#'     \item{repository}{\code{character} -- Source repository URL (e.g., GitHub)}
#'     \item{categories}{\code{character} -- Category tags (may be NA)}
#'   }
#' @examples
#' crates_search("serde")
#' crates_search("async runtime", per_page = 5)
#' @export
crates_search <- function(query, per_page = 20) {
  url <- sprintf("%s/crates?q=%s&per_page=%d",
                 .crates_base, utils::URLencode(query), as.integer(per_page))
  raw <- .fetch_json(url)
  crates <- raw$crates
  if (is.null(crates) || nrow(crates) == 0) return(.schema_search)

  as_tibble(crates) |>
    transmute(
      name        = as.character(name),
      description = as.character(description %||% NA_character_),
      max_version = as.character(max_version %||% NA_character_),
      downloads   = as.integer(downloads %||% NA_integer_),
      created_at  = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%OS"),
      updated_at  = as.POSIXct(updated_at, format = "%Y-%m-%dT%H:%M:%OS"),
      homepage    = as.character(homepage %||% NA_character_),
      repository  = as.character(repository %||% NA_character_),
      categories  = as.character(NA)
    )
}

# == Crate detail ==============================================================

#' Get detailed crate metadata from crates.io
#'
#' Fetches comprehensive metadata for a single Rust crate, including
#' license information and stable version details.
#'
#' @param name Character. Exact crate name (e.g., \code{"serde"}, \code{"tokio"},
#'   \code{"rand"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{name}{\code{character} -- Crate name}
#'     \item{description}{\code{character} -- Crate description}
#'     \item{max_version}{\code{character} -- Latest version (including pre-releases)}
#'     \item{downloads}{\code{integer} -- Total all-time downloads}
#'     \item{created_at}{\code{POSIXct} -- First publication date/time}
#'     \item{updated_at}{\code{POSIXct} -- Most recent update date/time}
#'     \item{homepage}{\code{character} -- Homepage URL (may be NA)}
#'     \item{repository}{\code{character} -- Source repository URL}
#'     \item{license}{\code{character} -- SPDX license expression (e.g., "MIT OR Apache-2.0")}
#'     \item{max_stable_version}{\code{character} -- Latest non-pre-release version}
#'   }
#' @examples
#' crates_crate("tokio")
#' crates_crate("serde")
#' @export
crates_crate <- function(name) {
  url <- sprintf("%s/crates/%s", .crates_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("crates.io fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_crate)

  cr <- raw$crate
  tibble(
    name               = as.character(cr$name %||% NA_character_),
    description        = as.character(cr$description %||% NA_character_),
    max_version        = as.character(cr$max_version %||% NA_character_),
    downloads          = as.integer(cr$downloads %||% NA_integer_),
    created_at         = as.POSIXct(cr$created_at %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    updated_at         = as.POSIXct(cr$updated_at %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    homepage           = as.character(cr$homepage %||% NA_character_),
    repository         = as.character(cr$repository %||% NA_character_),
    license            = as.character(cr$license %||% NA_character_),
    max_stable_version = as.character(cr$max_stable_version %||% NA_character_)
  )
}

#' List all published versions of a crate
#'
#' Returns the complete version history for a crate, including download
#' counts per version, yank status, and license for each release.
#'
#' @param name Character. Crate name (e.g., \code{"serde"}, \code{"tokio"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{crate}{\code{character} -- Crate name}
#'     \item{version}{\code{character} -- Semver version string (e.g., "1.0.228")}
#'     \item{downloads}{\code{integer} -- Downloads for this specific version}
#'     \item{created_at}{\code{POSIXct} -- Publication date/time of this version}
#'     \item{yanked}{\code{logical} -- Whether this version has been yanked}
#'     \item{license}{\code{character} -- SPDX license for this version}
#'   }
#' @examples
#' crates_versions("serde")
#' crates_versions("tokio")
#' @export
crates_versions <- function(name) {
  url <- sprintf("%s/crates/%s/versions", .crates_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(crate = character(), version = character(),
                                   downloads = integer(), created_at = as.POSIXct(character())))

  vers <- raw$versions
  if (is.null(vers) || nrow(vers) == 0) return(tibble(crate = character(), version = character(),
                                                        downloads = integer()))

  as_tibble(vers) |>
    transmute(
      crate = as.character(name),
      version = as.character(num),
      downloads = as.integer(downloads %||% NA_integer_),
      created_at = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%OS"),
      yanked = as.logical(yanked %||% NA),
      license = as.character(license %||% NA_character_)
    )
}

#' Get daily download statistics for a crate
#'
#' Returns aggregated daily download counts across all versions of a crate
#' for approximately the last 90 days. Useful for tracking popularity trends.
#'
#' @param name Character. Crate name (e.g., \code{"serde"}, \code{"tokio"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{\code{Date} -- Calendar date}
#'     \item{downloads}{\code{integer} -- Total downloads across all versions on that date}
#'   }
#' @examples
#' crates_downloads("serde")
#' @export
crates_downloads <- function(name) {
  url <- sprintf("%s/crates/%s/downloads", .crates_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(date = as.Date(character()), downloads = integer()))

  dl <- raw$version_downloads
  if (is.null(dl) || nrow(dl) == 0) return(tibble(date = as.Date(character()), downloads = integer()))

  as_tibble(dl) |>
    group_by(date = as.Date(date)) |>
    summarise(downloads = sum(as.integer(downloads), na.rm = TRUE), .groups = "drop") |>
    arrange(date)
}

#' Get reverse dependencies (crates that depend on this one)
#'
#' Lists crates that declare a dependency on the specified crate.
#' Useful for understanding ecosystem impact and downstream users.
#'
#' @param name Character. Crate name (e.g., \code{"serde"}, \code{"tokio"}).
#' @param per_page Integer. Number of results to return (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{\code{character} -- Name of the dependent crate}
#'     \item{downloads}{\code{integer} -- Total downloads of the dependent crate}
#'     \item{description}{\code{character} -- Description of the dependent crate}
#'   }
#' @examples
#' crates_reverse_deps("serde", per_page = 10)
#' @export
crates_reverse_deps <- function(name, per_page = 20) {
  url <- sprintf("%s/crates/%s/reverse_dependencies?per_page=%d",
                 .crates_base, utils::URLencode(name), as.integer(per_page))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(name = character(), downloads = integer(), description = character()))

  deps <- raw$versions
  if (is.null(deps) || nrow(deps) == 0) return(tibble(name = character(), downloads = integer()))

  # deps has crate_id but we need more details from the crates list
  crate_ids <- unique(deps$crate_id)
  crates_info <- raw$crates
  if (!is.null(crates_info) && nrow(crates_info) > 0) {
    tibble(
      name = as.character(crates_info$name %||% crates_info$id),
      downloads = as.integer(crates_info$downloads %||% NA_integer_),
      description = as.character(crates_info$description %||% NA_character_)
    )
  } else {
    tibble(name = as.character(crate_ids), downloads = NA_integer_, description = NA_character_)
  }
}

# == Context ===================================================================

#' Get crates.io client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
crates_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(crates_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/crates.io.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "crates.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# crates.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# crates.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
