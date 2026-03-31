# == Launches ==================================================================

#' Fetch SpaceX launches
#'
#' Returns all SpaceX launches (past and upcoming).
#'
#' @param limit Optional maximum number of launches to return (most recent first)
#' @return tibble: id, name, flight_number, date_utc, success, details,
#'   rocket, launchpad, upcoming
#' @export
spacex_launches <- function(limit = NULL) {
  raw <- .fetch_json(sprintf("%s/launches", .spacex_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_launches)

  result <- .parse_launches(raw)
  result <- result |> arrange(desc(date_utc))
  if (!is.null(limit)) result <- head(result, limit)
  result
}

#' Fetch the latest SpaceX launch
#'
#' @return tibble: single row with same columns as spacex_launches
#' @export
spacex_latest <- function() {
  raw <- .fetch_json(sprintf("%s/launches/latest", .spacex_base))
  if (is.null(raw)) return(.schema_launches)
  .parse_launches(list(raw))
}

#' Fetch SpaceX rockets
#'
#' @return tibble: id, name, type, active, stages, boosters,
#'   cost_per_launch, first_flight, country, company, description
#' @export
spacex_rockets <- function() {
  raw <- .fetch_json(sprintf("%s/rockets", .spacex_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_rockets)

  rows <- lapply(raw, function(r) {
    tibble(
      id = as.character(r$id %||% NA),
      name = as.character(r$name %||% NA),
      type = as.character(r$type %||% NA),
      active = as.logical(r$active %||% NA),
      stages = as.integer(r$stages %||% NA),
      boosters = as.integer(r$boosters %||% NA),
      cost_per_launch = as.numeric(r$cost_per_launch %||% NA),
      first_flight = as.Date(r$first_flight %||% NA),
      country = as.character(r$country %||% NA),
      company = as.character(r$company %||% NA),
      description = as.character(r$description %||% NA)
    )
  })
  bind_rows(rows)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the spacexdata package
#'
#' @return Character string (invisibly), also printed
#' @export
spacex_context <- function() {
  .build_context("spacexdata.com", header_lines = c(
    "# spacexdata.com - SpaceX Launch and Rocket Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns."
  ))
}
