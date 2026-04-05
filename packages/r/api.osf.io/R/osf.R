# api.osf.io.R - Self-contained api.osf.io client



# api-osf-io.R
# Self-contained Open Science Framework (OSF) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: none documented for read-only access


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osf_base <- "https://api.osf.io/v2"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_preprints <- tibble(
  id = character(), title = character(), description = character(),
  date_created = as.POSIXct(character()), date_published = as.POSIXct(character()),
  doi = character(), provider = character()
)

.schema_nodes <- tibble(
  id = character(), title = character(), description = character(),
  category = character(), date_created = as.POSIXct(character()),
  date_modified = as.POSIXct(character()), public = logical(),
  tags = character()
)



# == Preprints =================================================================

#' Fetch preprints from the Open Science Framework
#'
#' Returns recent preprints from OSF and its community preprint servers
#' (SocArXiv, PsyArXiv, etc.). Each row is one preprint with title,
#' abstract, dates, and DOI. No authentication required for public
#' preprints. Use \code{osf_nodes()} to search for research projects
#' and their associated files/data.
#'
#' @param provider Character. Preprint server to query. Default
#'   \code{"osf"} (OSF Preprints). Other providers:
#'   \code{"socarxiv"} (social sciences), \code{"psyarxiv"} (psychology),
#'   \code{"engrxiv"} (engineering), \code{"biohackrxiv"} (bioinformatics),
#'   \code{"africarxiv"} (Africa-focused research).
#' @param page_size Integer. Number of preprints per page, default
#'   \code{25}, maximum \code{100}.
#' @return A tibble with one row per preprint:
#'   \describe{
#'     \item{id}{\code{character} -- OSF preprint ID (e.g. "6ae4x", usable in osf.io URLs)}
#'     \item{title}{\code{character} -- Preprint title}
#'     \item{description}{\code{character} -- Abstract / description text}
#'     \item{date_created}{\code{POSIXct} -- UTC timestamp when the preprint was created}
#'     \item{date_published}{\code{POSIXct} -- UTC timestamp when published/made public}
#'     \item{doi}{\code{character} -- DOI if assigned, or NA}
#'     \item{provider}{\code{character} -- Preprint server name (matches the \code{provider} argument)}
#'   }
#' @examples
#' \dontrun{
#' # Recent OSF preprints
#' osf_preprints("osf", page_size = 10)
#'
#' # Psychology preprints from PsyArXiv
#' osf_preprints("psyarxiv", page_size = 25)
#'
#' # Social science preprints with DOIs
#' osf_preprints("socarxiv") |> dplyr::filter(!is.na(doi))
#' }
osf_preprints <- function(provider = "osf", page_size = 25) {
  url <- sprintf("%s/preprints/?filter[provider]=%s&page[size]=%d",
                 .osf_base, provider, page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_preprints)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tibble(
      id             = as.character(item$id %||% NA),
      title          = as.character(a$title %||% NA),
      description    = as.character(a$description %||% NA),
      date_created   = as.POSIXct(a$date_created %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_published = as.POSIXct(a$date_published %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      doi            = as.character(a$doi %||% NA),
      provider       = provider
    )
  })
  bind_rows(rows)
}


# == Nodes (projects) ==========================================================

#' Search OSF nodes (projects and components)
#'
#' Searches public OSF projects and components by title. Returns
#' metadata including description, category, dates, and tags. Each
#' OSF "node" is a project or component that may contain files,
#' preregistrations, data, and wiki pages. Use the returned \code{id}
#' to construct URLs like \code{https://osf.io/{id}}.
#'
#' @param query Character. Title search filter, e.g. \code{"climate"},
#'   \code{"machine learning"}, \code{"replication"}. Matches against
#'   project/component titles (case-insensitive substring match).
#' @param page_size Integer. Number of results per page, default
#'   \code{25}, maximum \code{100}.
#' @return A tibble with one row per node:
#'   \describe{
#'     \item{id}{\code{character} -- 5-character OSF node ID (e.g. "wne6t"), usable in \code{https://osf.io/{id}}}
#'     \item{title}{\code{character} -- Project/component title}
#'     \item{description}{\code{character} -- Project description / abstract}
#'     \item{category}{\code{character} -- Node type: "project", "analysis", "data", "methods", etc.}
#'     \item{date_created}{\code{POSIXct} -- UTC timestamp when the node was created}
#'     \item{date_modified}{\code{POSIXct} -- UTC timestamp of last modification}
#'     \item{public}{\code{logical} -- TRUE if the node is publicly visible}
#'     \item{tags}{\code{character} -- Comma-separated tags (e.g. "climate, temperature, modeling")}
#'   }
#' @examples
#' \dontrun{
#' # Search for climate research projects
#' osf_nodes("climate", page_size = 10)
#'
#' # Find replication studies
#' osf_nodes("replication", page_size = 25)
#'
#' # Machine learning projects, recently modified
#' osf_nodes("machine learning") |> dplyr::arrange(desc(date_modified))
#' }
osf_nodes <- function(query, page_size = 25) {
  url <- sprintf("%s/nodes/?filter[title]=%s&page[size]=%d",
                 .osf_base, utils::URLencode(query), page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_nodes)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tags_str <- paste(unlist(a$tags %||% list()), collapse = ", ")
    tibble(
      id            = as.character(item$id %||% NA),
      title         = as.character(a$title %||% NA),
      description   = as.character(a$description %||% NA),
      category      = as.character(a$category %||% NA),
      date_created  = as.POSIXct(a$date_created %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_modified = as.POSIXct(a$date_modified %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      public        = as.logical(a$public %||% NA),
      tags          = tags_str
    )
  })
  bind_rows(rows)
}


# == Context ===================================================================

#' Get api.osf.io client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
osf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(osf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.osf.io.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.osf.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.osf.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.osf.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
