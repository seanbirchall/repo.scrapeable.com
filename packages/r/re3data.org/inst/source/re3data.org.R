# re3data.org.R - Self-contained re3data.org client

library(httr2)
library(jsonlite)
library(xml2)
library(tibble)
library(dplyr)


.ua <- "support@scrapeable.com"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) {
  jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

.re3_base <- "https://www.re3data.org/api/beta"

#' Search the re3data registry of research data repositories
#'
#' Queries the re3data.org beta API for research data repositories
#' matching a free-text term. re3data indexes over 3 000 repositories
#' worldwide across all scientific disciplines.
#'
#' @param query Search term (e.g. \code{"genomics"}, \code{"climate"},
#'   \code{"astronomy"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{re3data repository ID, e.g. \code{"r3d100010468"} (character).}
#'     \item{name}{Repository name (character).}
#'     \item{link}{URL to the re3data landing page (character).}
#'   }
#' @export
#' @seealso \code{\link{re3_repository}}, \code{\link{re3_list}}
#' @examples
#' \dontrun{
#' re3_search("genomics")
#' re3_search("ocean temperature")
#' }
re3_search <- function(query) {
  schema <- tibble(id = character(), name = character(), link = character())
  url <- sprintf("%s/repositories?query=%s", .re3_base, utils::URLencode(query, reserved = TRUE))
  tmp <- .fetch(url, ext = ".xml")
  doc <- tryCatch(xml2::read_xml(tmp), error = function(e) NULL)
  if (is.null(doc)) return(schema)

  repos <- xml2::xml_find_all(doc, ".//repository")
  if (length(repos) == 0) return(schema)
  tibble(
    id = xml2::xml_text(xml2::xml_find_first(repos, ".//id")),
    name = xml2::xml_text(xml2::xml_find_first(repos, ".//name")),
    link = xml2::xml_text(xml2::xml_find_first(repos, ".//link"))
  )
}

#' Get detailed metadata for a research data repository
#'
#' Fetches rich metadata for a single repository identified by its
#' re3data ID. The response includes the repository URL, a prose
#' description, subject classifications, content types, provider types,
#' and the countries of the hosting institutions.
#'
#' @param repo_id Repository identifier from \code{\link{re3_search}} or
#'   \code{\link{re3_list}} (e.g. \code{"r3d100010468"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{re3data repository ID (character).}
#'     \item{name}{Repository name (character).}
#'     \item{url}{Repository home URL (character).}
#'     \item{description}{Prose description of the repository (character).}
#'     \item{subjects}{Semicolon-separated subject classifications (character).}
#'     \item{content_types}{Semicolon-separated content type labels (character).}
#'     \item{provider_types}{Semicolon-separated provider types (character).}
#'     \item{countries}{Semicolon-separated ISO country codes of hosting
#'       institutions (character).}
#'   }
#' @export
#' @seealso \code{\link{re3_search}}, \code{\link{re3_list}}
#' @examples
#' \dontrun{
#' re3_repository("r3d100010468")
#' }
re3_repository <- function(repo_id) {
  schema <- tibble(id = character(), name = character(), url = character(),
                   description = character(), subjects = character(),
                   content_types = character(), provider_types = character(),
                   countries = character())
  api_url <- sprintf("%s/repository/%s", .re3_base, repo_id)
  tmp <- .fetch(api_url, ext = ".xml")
  doc <- tryCatch(xml2::read_xml(tmp), error = function(e) NULL)
  if (is.null(doc)) return(schema)

  ns <- xml2::xml_ns(doc)
  get_text <- function(xpath) {
    nodes <- xml2::xml_find_all(doc, xpath)
    if (length(nodes) == 0) return(NA_character_)
    paste(xml2::xml_text(nodes), collapse = "; ")
  }

  tibble(
    id = repo_id,
    name = get_text(".//r3d:repositoryName"),
    url = get_text(".//r3d:repositoryURL"),
    description = get_text(".//r3d:description"),
    subjects = get_text(".//r3d:subject"),
    content_types = get_text(".//r3d:contentType"),
    provider_types = get_text(".//r3d:providerType"),
    countries = get_text(".//r3d:institutionCountry")
  )
}

#' List all research data repositories in re3data
#'
#' Returns every repository registered in re3data.org (currently over
#' 3 000 entries). The result contains only summary fields; call
#' \code{\link{re3_repository}} with an \code{id} to get full metadata.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{re3data repository ID (character).}
#'     \item{name}{Repository name (character).}
#'     \item{link}{URL to the re3data landing page (character).}
#'   }
#' @export
#' @seealso \code{\link{re3_search}}, \code{\link{re3_repository}}
#' @examples
#' \dontrun{
#' all_repos <- re3_list()
#' nrow(all_repos)
#' }
re3_list <- function() {
  schema <- tibble(id = character(), name = character(), link = character())
  url <- sprintf("%s/repositories", .re3_base)
  tmp <- .fetch(url, ext = ".xml")
  doc <- tryCatch(xml2::read_xml(tmp), error = function(e) NULL)
  if (is.null(doc)) return(schema)

  repos <- xml2::xml_find_all(doc, ".//repository")
  if (length(repos) == 0) return(schema)
  tibble(
    id = xml2::xml_text(xml2::xml_find_first(repos, ".//id")),
    name = xml2::xml_text(xml2::xml_find_first(repos, ".//name")),
    link = xml2::xml_text(xml2::xml_find_first(repos, ".//link"))
  )
}

# == Context ===================================================================

#' Get re3data.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
re3data_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(re3data_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/re3data.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "re3data.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# re3data.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# re3data.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
