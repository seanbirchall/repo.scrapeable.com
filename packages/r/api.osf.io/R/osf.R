

# == Preprints =================================================================

#' Fetch preprints from the Open Science Framework
#'
#' Returns recent preprints, optionally filtered by provider.
#' Uses the JSON:API format from OSF v2 API.
#'
#' @param provider Preprint provider filter (default "osf"). Other values:
#'   "socarxiv", "psyarxiv", "engrxiv", "biohackrxiv"
#' @param page_size Number of results per page (default 25, max 100)
#' @return tibble: id, title, description, date_created, date_published,
#'   doi, provider
#' @export
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
#' Returns public OSF nodes matching a title filter.
#'
#' @param query Title search filter
#' @param page_size Number of results per page (default 25, max 100)
#' @return tibble: id, title, description, category, date_created,
#'   date_modified, public, tags
#' @export
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

#' Show OSF package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
#' @export
osf_context <- function() {
  header <- c(
    "# api.osf.io - Open Science Framework API Client",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for public data",
    "# Rate limits: none documented for read-only",
    "#",
    "# Preprint providers: osf, socarxiv, psyarxiv, engrxiv, biohackrxiv",
    "# Node categories: project, component, data, analysis"
  )
  .build_context("api.osf.io", header_lines = header)
}
