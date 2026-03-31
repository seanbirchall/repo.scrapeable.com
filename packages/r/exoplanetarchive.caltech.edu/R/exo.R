# == Public functions ==========================================================

#' Run an ADQL query against the Exoplanet Archive TAP service
#'
#' @param sql ADQL query string (e.g. "select pl_name, hostname from ps
#'   where disc_year = 2024")
#' @param format Output format: "json" (default), "csv", "votable"
#' @return tibble with query results
#' @export
exo_query <- function(sql, format = "json") {
  url <- sprintf("%s?query=%s&format=%s",
                 .exo_base,
                 utils::URLencode(sql, reserved = TRUE),
                 format)
  if (format == "json") {
    raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0) return(tibble())
    as_tibble(raw)
  } else {
    path <- .fetch(url, ext = paste0(".", format))
    if (format == "csv") {
      as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
    } else {
      message("File saved to: ", path)
      tibble(file = path)
    }
  }
}

#' Get confirmed exoplanets, optionally filtered by discovery year
#'
#' Returns one row per unique planet (default solution) from the Planetary
#' Systems (ps) table.
#'
#' @param year Discovery year filter (optional, e.g. 2024)
#' @param limit Max rows (default 100)
#' @return tibble: pl_name, hostname, disc_year, pl_orbper, pl_rade,
#'   pl_bmassj, pl_eqt, disc_facility, discoverymethod
#' @export
exo_planets <- function(year = NULL, limit = 100) {
  where <- if (!is.null(year)) sprintf(" where disc_year = %d", as.integer(year)) else ""
  sql <- sprintf(
    "select distinct pl_name, hostname, disc_year, pl_orbper, pl_rade, pl_bmassj, pl_eqt, disc_facility, discoverymethod from ps%s order by disc_year desc",
    where
  )
  if (!is.null(limit)) sql <- paste0(sql, sprintf(" top %d", as.integer(limit)))

  url <- sprintf("%s?query=%s&format=json",
                 .exo_base, utils::URLencode(sql, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_planets)

  as_tibble(raw) |>
    mutate(
      pl_name         = as.character(pl_name),
      hostname        = as.character(hostname),
      disc_year       = as.integer(disc_year),
      pl_orbper       = as.numeric(pl_orbper),
      pl_rade         = as.numeric(pl_rade),
      pl_bmassj       = as.numeric(pl_bmassj),
      pl_eqt          = as.numeric(pl_eqt),
      disc_facility   = as.character(disc_facility),
      discoverymethod = as.character(discoverymethod)
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the exoplanetarchive.caltech.edu package
#'
#' @return Character string (invisibly), also printed
#' @export
exo_context <- function() {
  .build_context("exoplanetarchive.caltech.edu", header_lines = c(
    "# exoplanetarchive.caltech.edu - NASA Exoplanet Archive Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: none documented",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Main table: ps (Planetary Systems) - one row per planet-reference pair",
    "# Key columns: pl_name, hostname, disc_year, pl_orbper (orbital period days),",
    "#   pl_rade (Earth radii), pl_bmassj (Jupiter masses), pl_eqt (equilibrium temp K),",
    "#   disc_facility, discoverymethod",
    "# Discovery methods: Transit, Radial Velocity, Imaging, Microlensing",
    "# Use exo_query() for custom ADQL queries"
  ))
}
