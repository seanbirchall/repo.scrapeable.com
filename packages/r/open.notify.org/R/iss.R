# == ISS Position ==============================================================

#' Fetch current ISS position
#'
#' Returns the current latitude and longitude of the International Space Station.
#'
#' @return tibble: timestamp (POSIXct), latitude (numeric), longitude (numeric)
#' @export
iss_position <- function() {
  raw <- .fetch_json(sprintf("%s/iss-now.json", .iss_base))
  if (is.null(raw) || raw$message != "success") return(.schema_position)

  tibble(
    timestamp = as.POSIXct(as.numeric(raw$timestamp), origin = "1970-01-01", tz = "UTC"),
    latitude = as.numeric(raw$iss_position$latitude),
    longitude = as.numeric(raw$iss_position$longitude)
  )
}

#' Fetch current astronauts in space
#'
#' Returns all people currently in space, including their spacecraft.
#'
#' @return tibble: name (character), craft (character)
#' @export
iss_astronauts <- function() {
  raw <- .fetch_json(sprintf("%s/astros.json", .iss_base))
  if (is.null(raw) || raw$message != "success") return(.schema_astronauts)

  people <- raw$people
  if (is.null(people) || nrow(people) == 0) return(.schema_astronauts)

  as_tibble(people) |>
    transmute(
      name = as.character(name),
      craft = as.character(craft)
    )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the open-notify package
#'
#' @return Character string (invisibly), also printed
#' @export
iss_context <- function() {
  .build_context("open.notify.org", header_lines = c(
    "# open.notify.org - ISS Tracking and Astronaut Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns."
  ))
}
