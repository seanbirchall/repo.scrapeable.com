# == Public functions ==========================================================

#' Search the EU Open Data Portal
#'
#' @param query Search term (e.g. "climate", "transport", "energy")
#' @param limit Number of results (default 10, max 100)
#' @param page Page number (default 0, zero-indexed)
#' @return tibble: id, title, description, country, catalog, modified, format
#' @export
eudata_search <- function(query, limit = 10, page = 0) {
  url <- sprintf("%s/search?q=%s&limit=%d&page=%d",
                 .eudata_base,
                 utils::URLencode(query, reserved = TRUE),
                 as.integer(limit), as.integer(page))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$result) || is.null(raw$result$results) ||
      length(raw$result$results) == 0) {
    return(.schema_search)
  }

  rows <- lapply(raw$result$results, function(x) {
    formats <- tryCatch({
      dists <- x$distributions
      if (!is.null(dists) && length(dists) > 0) {
        fmts <- vapply(dists, function(d) as.character(d$format$label %||% d$mediaType %||% ""), character(1))
        paste(unique(fmts[fmts != ""]), collapse = ", ")
      } else NA_character_
    }, error = function(e) NA_character_)

    tibble(
      id          = as.character(x$identifier[[1]] %||% NA),
      title       = .safe_text(x$title),
      description = .safe_text(x$description),
      country     = as.character(x$country$label %||% NA),
      catalog     = as.character(x$catalog$title[[1]] %||% NA),
      modified    = as.character(x$modified %||% NA),
      format      = formats
    )
  })
  bind_rows(rows)
}

#' Get details for a specific EU dataset
#'
#' @param id Dataset identifier (e.g. "groenklimaatassen-gent")
#' @return tibble with one row: id, title, description, country, catalog,
#'   publisher, modified, issued, format, resource_url
#' @export
eudata_dataset <- function(id) {
  url <- sprintf("%s/datasets/%s", .eudata_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$result)) return(.schema_dataset)

  x <- raw$result
  formats <- tryCatch({
    dists <- x$distributions
    if (!is.null(dists) && length(dists) > 0) {
      fmts <- vapply(dists, function(d) as.character(d$format$label %||% d$mediaType %||% ""), character(1))
      paste(unique(fmts[fmts != ""]), collapse = ", ")
    } else NA_character_
  }, error = function(e) NA_character_)

  resource_url <- tryCatch({
    dists <- x$distributions
    if (!is.null(dists) && length(dists) > 0) {
      as.character(dists[[1]]$accessUrl %||% dists[[1]]$downloadUrl %||% NA)
    } else NA_character_
  }, error = function(e) NA_character_)

  tibble(
    id           = as.character(x$identifier[[1]] %||% NA),
    title        = .safe_text(x$title),
    description  = .safe_text(x$description),
    country      = as.character(x$country$label %||% NA),
    catalog      = as.character(x$catalog$title[[1]] %||% NA),
    publisher    = as.character(x$catalog$publisher$name %||% NA),
    modified     = as.character(x$modified %||% NA),
    issued       = as.character(x$issued %||% NA),
    format       = formats,
    resource_url = resource_url
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the data.europa.eu package
#'
#' @return Character string (invisibly), also printed
#' @export
eudata_context <- function() {
  .build_context("data.europa.eu", header_lines = c(
    "# data.europa.eu - EU Open Data Portal Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: none documented",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Search topics: climate, energy, transport, health, agriculture,",
    "#   environment, economy, education, science, population"
  ))
}
