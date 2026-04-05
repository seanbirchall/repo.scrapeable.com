# libio.R
# Self-contained Libraries.io API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key optional for some endpoints, required for search.
#   Get one at https://libraries.io/account
# Rate limits: 60 requests per minute.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.libio_base <- "https://libraries.io/api"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_platforms <- tibble(
  name = character(), project_count = integer(), homepage = character(),
  default_language = character()
)

.schema_package <- tibble(
  name = character(), platform = character(), description = character(),
  homepage = character(), repository_url = character(),
  stars = integer(), forks = integer(), rank = integer(),
  latest_release_number = character(), language = character(),
  licenses = character(), dependent_repos_count = integer()
)

.schema_search <- tibble(
  name = character(), platform = character(), description = character(),
  stars = integer(), rank = integer(), latest_release_number = character()
)

# == Platforms =================================================================

#' List all package manager platforms on Libraries.io
#'
#' Returns metadata for every package manager platform tracked by
#' Libraries.io (npm, PyPI, CRAN, Maven, NuGet, etc.).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Platform name, e.g. "npm", "pypi", "cran" (character)}
#'     \item{project_count}{Number of packages tracked (integer)}
#'     \item{homepage}{Platform homepage URL (character)}
#'     \item{default_language}{Primary programming language (character)}
#'   }
#' @examples
#' libio_platforms()
#' @seealso [libio_package()], [libio_search()], [libio_context()]
#' @source <https://libraries.io/api>
#' @export
libio_platforms <- function() {
  url <- paste0(.libio_base, "/platforms")
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_platforms)

  as_tibble(raw) |>
    transmute(
      name = as.character(name),
      project_count = as.integer(project_count),
      homepage = as.character(homepage),
      default_language = as.character(if ("default_language" %in% names(raw)) default_language else NA_character_)
    )
}

# == Package info ==============================================================

#' Fetch package details from Libraries.io
#'
#' Returns comprehensive metadata for a single package including GitHub
#' stars, forks, SourceRank, license, and dependency counts.
#'
#' @param platform Package manager platform (e.g. "npm", "pypi", "cran",
#'   "maven", "nuget", "rubygems")
#' @param name Package name (e.g. "express", "pandas", "dplyr")
#' @param api_key API key (optional for this endpoint). Get one at
#'   <https://libraries.io/account>.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{name}{Package name (character)}
#'     \item{platform}{Package manager (character)}
#'     \item{description}{Package description (character)}
#'     \item{homepage}{Package homepage URL (character)}
#'     \item{repository_url}{Source code repository URL (character)}
#'     \item{stars}{GitHub stars count (integer)}
#'     \item{forks}{GitHub forks count (integer)}
#'     \item{rank}{Libraries.io SourceRank score (integer)}
#'     \item{latest_release_number}{Latest version string (character)}
#'     \item{language}{Primary language (character)}
#'     \item{licenses}{License identifier (character)}
#'     \item{dependent_repos_count}{Repos depending on this package (integer)}
#'   }
#' @examples
#' libio_package("npm", "express")
#' libio_package("pypi", "pandas")
#' @seealso [libio_platforms()], [libio_search()], [libio_context()]
#' @source <https://libraries.io/api>
#' @export
libio_package <- function(platform, name, api_key = NULL) {
  url <- paste0(.libio_base, "/", utils::URLencode(platform), "/",
                utils::URLencode(name))
  if (!is.null(api_key)) url <- paste0(url, "?api_key=", api_key)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$name)) return(.schema_package)

  tibble(
    name = as.character(raw$name %||% NA_character_),
    platform = as.character(raw$platform %||% NA_character_),
    description = as.character(raw$description %||% NA_character_),
    homepage = as.character(raw$homepage %||% NA_character_),
    repository_url = as.character(raw$repository_url %||% NA_character_),
    stars = as.integer(raw$stars %||% NA_integer_),
    forks = as.integer(raw$forks %||% NA_integer_),
    rank = as.integer(raw$rank %||% NA_integer_),
    latest_release_number = as.character(raw$latest_release_number %||% NA_character_),
    language = as.character(raw$language %||% NA_character_),
    licenses = as.character(raw$licenses %||% NA_character_),
    dependent_repos_count = as.integer(raw$dependent_repos_count %||% NA_integer_)
  )
}

# == Search ====================================================================

#' Search packages on Libraries.io
#'
#' Searches across all package managers (or a specific one) for packages
#' matching a query string. Requires a free API key.
#'
#' @param query Search query string (e.g. "http client", "csv parser")
#' @param platforms Optional platform filter (e.g. "npm", "pypi")
#' @param sort Sort by: "rank" (default), "stars", "dependents_count",
#'   "dependent_repos_count", "latest_release_published_at",
#'   "contributions_count", "created_at"
#' @param api_key API key (required). Register free at
#'   <https://libraries.io/account>.
#' @param page Page number for pagination (default 1)
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Package name (character)}
#'     \item{platform}{Package manager name (character)}
#'     \item{description}{Package description (character)}
#'     \item{stars}{GitHub stars (integer)}
#'     \item{rank}{SourceRank score (integer)}
#'     \item{latest_release_number}{Latest version string (character)}
#'   }
#' @examples
#' \dontrun{
#' libio_search("http client", api_key = "YOUR_KEY")
#' libio_search("csv parser", platforms = "pypi", api_key = "YOUR_KEY")
#' }
#' @seealso [libio_platforms()], [libio_package()], [libio_context()]
#' @source <https://libraries.io/api>
#' @export
libio_search <- function(query, platforms = NULL, sort = "rank",
                         api_key = NULL, page = 1) {
  if (is.null(api_key)) stop(
    "api_key is required for search. Register free at https://libraries.io/account"
  )
  url <- paste0(.libio_base, "/search?q=", utils::URLencode(query),
                "&api_key=", api_key, "&sort=", sort, "&page=", page)
  if (!is.null(platforms)) url <- paste0(url, "&platforms=", utils::URLencode(platforms))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_search)

  as_tibble(raw) |>
    transmute(
      name = as.character(name),
      platform = as.character(platform),
      description = as.character(if ("description" %in% names(raw)) description else NA_character_),
      stars = as.integer(if ("stars" %in% names(raw)) stars else NA_integer_),
      rank = as.integer(if ("rank" %in% names(raw)) rank else NA_integer_),
      latest_release_number = as.character(if ("latest_release_number" %in% names(raw)) latest_release_number else NA_character_)
    )
}

# == Context (LLM injection) ==================================================

#' Get libio client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
libio_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(libio_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/libio.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "libio")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# libio context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# libio", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
