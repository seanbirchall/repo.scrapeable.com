# libio.R
# Self-contained Libraries.io API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key optional for some endpoints, required for search.
#   Get one at https://libraries.io/account
# Rate limits: 60 requests per minute.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
#' @return tibble: name, project_count, homepage, default_language
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
#' @param platform Package manager platform (e.g. "npm", "pypi", "cran")
#' @param name Package name (e.g. "express", "pandas", "dplyr")
#' @param api_key API key (optional for this endpoint)
#' @return tibble: one row with name, platform, description, homepage,
#'   repository_url, stars, forks, rank, latest_release_number, language,
#'   licenses, dependent_repos_count
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
#' @param query Search query string
#' @param platforms Optional platform filter (e.g. "npm")
#' @param sort Sort by: "rank", "stars", "dependents_count",
#'   "dependent_repos_count", "latest_release_published_at",
#'   "contributions_count", "created_at"
#' @param api_key API key (required for search). Get one at https://libraries.io/account
#' @param page Page number (default 1)
#' @return tibble: name, platform, description, stars, rank, latest_release_number
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

#' Generate LLM-friendly context for the Libraries.io package
#'
#' @return Character string (invisibly), also printed
libio_context <- function() {
  .build_context("libraries.io", header_lines = c(
    "# libraries.io - Package Manager API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: API key required for search, optional for platforms/package lookup",
    "# Rate limit: 60 requests per minute",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common platforms: npm, pypi, cran, rubygems, maven, nuget, go, cargo"
  ))
}
