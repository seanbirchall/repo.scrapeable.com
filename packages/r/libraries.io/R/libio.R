# == Platforms =================================================================

#' List all package manager platforms on Libraries.io
#'
#' @return tibble: name, project_count, homepage, default_language
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
#' @param platform Package manager platform (e.g. "npm", "pypi", "cran")
#' @param name Package name (e.g. "express", "pandas", "dplyr")
#' @param api_key API key (optional for this endpoint)
#' @return tibble: one row with name, platform, description, homepage,
#'   repository_url, stars, forks, rank, latest_release_number, language,
#'   licenses, dependent_repos_count
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
#' @param query Search query string
#' @param platforms Optional platform filter (e.g. "npm")
#' @param sort Sort by: "rank", "stars", "dependents_count",
#'   "dependent_repos_count", "latest_release_published_at",
#'   "contributions_count", "created_at"
#' @param api_key API key (required for search). Get one at https://libraries.io/account
#' @param page Page number (default 1)
#' @return tibble: name, platform, description, stars, rank, latest_release_number
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

#' Generate LLM-friendly context for the Libraries.io package
#'
#' @return Character string (invisibly), also printed
#' @export
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
