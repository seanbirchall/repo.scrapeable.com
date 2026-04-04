# libraries.io.R - Self-contained libraries.io client



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
#' Returns every package manager platform tracked by Libraries.io
#' (e.g., NPM, PyPI, CRAN, Maven, NuGet) with project counts and
#' homepage URLs. No authentication required.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Platform name (e.g., \code{"NPM"}, \code{"Pypi"}).}
#'     \item{project_count}{Integer. Number of packages tracked on this platform.}
#'     \item{homepage}{Character. Platform homepage URL.}
#'     \item{default_language}{Character. Primary programming language, or \code{NA}.}
#'   }
#' @examples
#' libio_platforms()
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
#' Retrieves comprehensive metadata for a single package including its
#' description, repository link, star/fork counts, SourceRank score,
#' latest version, license, and dependent repository count.
#'
#' @param platform Character. Package manager name (case-insensitive).
#'   Common values: \code{"npm"}, \code{"pypi"}, \code{"cran"},
#'   \code{"maven"}, \code{"nuget"}, \code{"rubygems"}, \code{"go"}.
#'   See \code{\link{libio_platforms}} for the full list.
#' @param name Character. Package name (e.g., \code{"express"},
#'   \code{"pandas"}, \code{"dplyr"}).
#' @param api_key Character or \code{NULL}. Libraries.io API key
#'   (optional for this endpoint). Register free at
#'   \url{https://libraries.io/account}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{name}{Character. Package name.}
#'     \item{platform}{Character. Platform name.}
#'     \item{description}{Character. Package description.}
#'     \item{homepage}{Character. Project homepage URL.}
#'     \item{repository_url}{Character. Source repository URL.}
#'     \item{stars}{Integer. GitHub star count.}
#'     \item{forks}{Integer. GitHub fork count.}
#'     \item{rank}{Integer. Libraries.io SourceRank score.}
#'     \item{latest_release_number}{Character. Latest version string.}
#'     \item{language}{Character. Primary programming language.}
#'     \item{licenses}{Character. License identifier.}
#'     \item{dependent_repos_count}{Integer. Number of repositories depending on this package.}
#'   }
#' @examples
#' libio_package("npm", "express")
#' libio_package("pypi", "pandas")
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
#' Performs a full-text search across all platforms (or a single platform)
#' on Libraries.io. Requires an API key. Results are paginated with 30
#' results per page.
#'
#' @param query Character. Search term (e.g., \code{"http client"},
#'   \code{"machine learning"}).
#' @param platforms Character or \code{NULL}. Restrict results to a single
#'   platform (e.g., \code{"npm"}, \code{"pypi"}). If \code{NULL}
#'   (default), searches all platforms.
#' @param sort Character. Sort field. One of \code{"rank"} (default),
#'   \code{"stars"}, \code{"dependents_count"},
#'   \code{"dependent_repos_count"}, \code{"latest_release_published_at"},
#'   \code{"contributions_count"}, \code{"created_at"}.
#' @param api_key Character. Libraries.io API key (required). Register
#'   free at \url{https://libraries.io/account}.
#' @param page Integer. Page number for pagination (default \code{1}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Package name.}
#'     \item{platform}{Character. Platform name.}
#'     \item{description}{Character. Package description, or \code{NA}.}
#'     \item{stars}{Integer. GitHub star count, or \code{NA}.}
#'     \item{rank}{Integer. Libraries.io SourceRank score, or \code{NA}.}
#'     \item{latest_release_number}{Character. Latest version string, or \code{NA}.}
#'   }
#' @examples
#' \dontrun{
#' libio_search("http client", platforms = "npm", api_key = "YOUR_KEY")
#' }
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

# == Package dependencies =====================================================

#' Get dependencies for a package version on Libraries.io
#'
#' Returns the declared dependencies for a specific version of a package,
#' including runtime, development, and optional dependency kinds.
#'
#' @param platform Character. Package manager name (e.g., \code{"npm"},
#'   \code{"pypi"}, \code{"cran"}).
#' @param name Character. Package name.
#' @param version Character. Version string (default \code{"latest"}).
#'   Use a specific version like \code{"1.0.0"} or \code{"latest"}.
#' @param api_key Character or \code{NULL}. Libraries.io API key (optional).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Dependency package name.}
#'     \item{platform}{Character. Dependency platform.}
#'     \item{kind}{Character. Dependency type (e.g., \code{"runtime"},
#'       \code{"Development"}, \code{"optional"}).}
#'     \item{requirements}{Character. Version constraint (e.g., \code{">=1.0"}).}
#'   }
#' @examples
#' libio_dependencies("pypi", "requests")
#' libio_dependencies("npm", "express", version = "4.18.2")
libio_dependencies <- function(platform, name, version = "latest", api_key = NULL) {
  schema <- tibble(name = character(), platform = character(),
                   kind = character(), requirements = character())
  url <- paste0(.libio_base, "/", utils::URLencode(platform), "/",
                utils::URLencode(name), "/", utils::URLencode(version),
                "/dependencies")
  if (!is.null(api_key)) url <- paste0(url, "?api_key=", api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$dependencies)) return(schema)
  deps <- raw$dependencies
  if (!is.data.frame(deps) || nrow(deps) == 0) return(schema)

  as_tibble(deps) |>
    transmute(
      name = as.character(if ("name" %in% names(deps)) name else NA_character_),
      platform = as.character(if ("platform" %in% names(deps)) platform else NA_character_),
      kind = as.character(if ("kind" %in% names(deps)) kind else NA_character_),
      requirements = as.character(if ("requirements" %in% names(deps)) requirements else NA_character_)
    )
}

#' Get contributors for a GitHub repository via Libraries.io
#'
#' Returns contributor metadata for a GitHub repository as tracked by
#' Libraries.io, including login name, contribution count, and profile
#' information.
#'
#' @param owner Character. GitHub repository owner (e.g., \code{"tidyverse"}).
#' @param repo Character. GitHub repository name (e.g., \code{"dplyr"}).
#' @param api_key Character or \code{NULL}. Libraries.io API key (optional).
#' @return A tibble with columns:
#'   \describe{
#'     \item{github_id}{Integer. GitHub user numeric ID.}
#'     \item{login}{Character. GitHub username.}
#'     \item{contributions}{Integer. Number of contributions to this repository.}
#'     \item{name}{Character. Display name, or \code{NA}.}
#'     \item{blog}{Character. Personal website URL, or \code{NA}.}
#'   }
#' @examples
#' libio_contributors("tidyverse", "dplyr")
libio_contributors <- function(owner, repo, api_key = NULL) {
  schema <- tibble(github_id = integer(), login = character(),
                   contributions = integer(), name = character(),
                   blog = character())
  url <- sprintf("%s/github/%s/%s/contributors",
                 .libio_base, utils::URLencode(owner), utils::URLencode(repo))
  if (!is.null(api_key)) url <- paste0(url, "?api_key=", api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)
  if (!is.data.frame(raw)) return(schema)

  as_tibble(raw) |>
    transmute(
      github_id = as.integer(if ("github_id" %in% names(raw)) github_id else NA_integer_),
      login = as.character(if ("login" %in% names(raw)) login else NA_character_),
      contributions = as.integer(if ("contributions" %in% names(raw)) contributions else NA_integer_),
      name = as.character(if ("name" %in% names(raw)) name else NA_character_),
      blog = as.character(if ("blog" %in% names(raw)) blog else NA_character_)
    )
}

# == Context ===================================================================

#' Get libraries.io client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/libraries.io.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "libraries.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# libraries.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# libraries.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
