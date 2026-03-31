# == Public functions ==========================================================

#' List IBGE aggregate tables
#'
#' Returns all available aggregate surveys/tables from the IBGE SIDRA system.
#'
#' @return tibble: research_id, research_name, aggregate_id, aggregate_name
#' @export
ibge_aggregates <- function() {
  url <- sprintf("%s/agregados", .ibge_base)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_aggregates)

  rows <- lapply(raw, function(research) {
    rid <- as.character(research$id)
    rname <- as.character(research$nome)
    aggs <- research$agregados
    if (is.null(aggs) || length(aggs) == 0) return(NULL)
    lapply(aggs, function(a) {
      tibble(
        research_id = rid,
        research_name = rname,
        aggregate_id = as.character(a$id),
        aggregate_name = as.character(a$nome)
      )
    })
  })
  bind_rows(unlist(rows, recursive = FALSE))
}

#' Get metadata for an IBGE aggregate
#'
#' Returns variable definitions for a specific aggregate table.
#'
#' @param aggregate_id Aggregate table ID (character or integer, e.g. "4714")
#' @return tibble: variable_id, variable_name, unit
#' @export
ibge_metadata <- function(aggregate_id) {
  url <- sprintf("%s/agregados/%s/metadados", .ibge_base, aggregate_id)
  raw <- .fetch_json(url)
  vars <- raw$variaveis
  if (is.null(vars) || length(vars) == 0) return(.schema_metadata)

  rows <- lapply(vars, function(v) {
    tibble(
      variable_id = as.integer(v$id),
      variable_name = as.character(v$nome),
      unit = as.character(v$unidade %||% NA)
    )
  })
  bind_rows(rows)
}

#' Fetch IBGE aggregate data
#'
#' Returns data for a specific aggregate, variable, period, and geography.
#'
#' @param aggregate_id Aggregate table ID (e.g. "4714" for population)
#' @param variable_id Variable ID (e.g. "93" for resident population)
#' @param period Period specification: use "-1" for latest, or specific like "2022"
#' @param geo Geographic level and filter. Format: "N1\[all\]" for Brazil,
#'   "N3\[all\]" for all states, "N6\[3304557\]" for a specific municipality.
#'   Common levels: N1=Brazil, N2=Region, N3=State, N6=Municipality
#' @return tibble: variable_id, variable_name, unit, location_id,
#'   location_level, location_name, period, value
#' @export
ibge_data <- function(aggregate_id, variable_id, period = "-1",
                      geo = "N1[all]") {
  url <- sprintf("%s/agregados/%s/periodos/%s/variaveis/%s?localidades=%s",
                 .ibge_base, aggregate_id, period, variable_id, geo)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_data)

  rows <- lapply(raw, function(v) {
    vid <- as.character(v$id)
    vname <- as.character(v$variavel)
    vunit <- as.character(v$unidade)
    results <- v$resultados
    if (is.null(results) || length(results) == 0) return(NULL)

    result_rows <- lapply(results, function(r) {
      series <- r$series
      if (is.null(series) || length(series) == 0) return(NULL)
      lapply(series, function(s) {
        loc <- s$localidade
        serie <- s$serie
        if (is.null(serie) || length(serie) == 0) return(NULL)
        periods <- names(serie)
        values <- vapply(serie, as.character, character(1))
        tibble(
          variable_id = vid,
          variable_name = vname,
          unit = vunit,
          location_id = as.character(loc$id),
          location_level = as.character(loc$nivel$nome),
          location_name = as.character(loc$nome),
          period = periods,
          value = values
        )
      })
    })
    bind_rows(unlist(result_rows, recursive = FALSE))
  })
  bind_rows(rows)
}

#' IBGE API context for LLM use
#'
#' Prints package overview, auth info, and function signatures.
#' @return Invisible string with context info
#' @export
ibge_context <- function() {
  header <- c(
    "# servicodados.ibge.gov.br - IBGE Statistics API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: not documented",
    "#",
    "# Key aggregates: 4714 (population), 1712 (GDP), 1419 (IPCA inflation)",
    "# Geo levels: N1=Brazil, N2=Region, N3=State, N6=Municipality",
    "# Typical flow: ibge_aggregates() -> ibge_metadata(id) -> ibge_data(id, var, period, geo)"
  )
  .build_context("servicodados.ibge.gov.br", header_lines = header)
}

# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
