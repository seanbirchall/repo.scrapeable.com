# == Public functions ==========================================================

#' List ONS datasets
#'
#' Returns available datasets from the ONS API.
#'
#' @param limit Maximum number of datasets to return (default 20, max 1000)
#' @param offset Offset for pagination (default 0)
#' @return tibble: id, title, description, release_frequency, state,
#'   last_updated, unit_of_measure
#' @export
ons_datasets <- function(limit = 20, offset = 0) {
  url <- sprintf("%s/datasets?limit=%d&offset=%d", .ons_base, limit, offset)
  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_datasets)

  as_tibble(items) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      description = as.character(if ("description" %in% names(items)) description else NA_character_),
      release_frequency = as.character(if ("release_frequency" %in% names(items)) release_frequency else NA_character_),
      state = as.character(if ("state" %in% names(items)) state else NA_character_),
      last_updated = as.character(if ("last_updated" %in% names(items)) last_updated else NA_character_),
      unit_of_measure = as.character(if ("unit_of_measure" %in% names(items)) unit_of_measure else NA_character_)
    )
}

#' Get ONS dataset details
#'
#' Returns metadata for a specific dataset including available editions.
#'
#' @param id Dataset ID (e.g. "wellbeing-quarterly", "cpih01")
#' @return tibble: edition, state, links_self, links_latest_version
#' @export
ons_dataset <- function(id) {
  url <- sprintf("%s/datasets/%s/editions", .ons_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) list(items = NULL))
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_editions)

  as_tibble(items) |>
    transmute(
      edition = as.character(edition),
      state = as.character(if ("state" %in% names(items)) state else NA_character_),
      links_self = as.character(links$self$href),
      links_latest_version = as.character(links$latest_version$href)
    )
}

#' Get ONS dataset observations
#'
#' Returns observation data for a specific dataset version.
#' The version endpoint must be fully specified.
#'
#' @param id Dataset ID (e.g. "cpih01")
#' @param edition Edition name (e.g. "time-series")
#' @param version Version number (integer, e.g. 1)
#' @param limit Maximum observations to return (default 100)
#' @return tibble with dimension and observation columns (varies by dataset)
#' @export
ons_observations <- function(id, edition, version, limit = 100) {
  url <- sprintf("%s/datasets/%s/editions/%s/versions/%s/observations?limit=%d",
                 .ons_base, id, edition, version, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) list(observations = NULL))
  obs <- raw$observations
  if (is.null(obs) || length(obs) == 0) return(.schema_observations)

  # observations structure varies by dataset, flatten to tibble
  rows <- lapply(obs, function(o) {
    dims <- if (!is.null(o$dimensions)) {
      setNames(
        vapply(o$dimensions, function(d) as.character(d$id %||% d$label %||% NA), character(1)),
        names(o$dimensions)
      )
    } else character(0)
    c(dims, observation = as.character(o$observation %||% NA))
  })
  tryCatch(
    bind_rows(lapply(rows, function(r) as_tibble(as.list(r)))),
    error = function(e) .schema_observations
  )
}

#' ONS API context for LLM use
#'
#' Prints package overview, auth info, and function signatures.
#' @return Invisible string with context info
#' @export
ons_context <- function() {
  header <- c(
    "# api.beta.ons.gov.uk - Office for National Statistics API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: not documented",
    "#",
    "# Example datasets: 'wellbeing-quarterly', 'cpih01', 'mid-year-pop-est'",
    "# Typical flow: ons_datasets() -> ons_dataset(id) -> ons_observations(id, edition, version)"
  )
  .build_context("api.beta.ons.gov.uk", header_lines = header)
}

# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
