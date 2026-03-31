# servicodados-ibge-gov-br.R
# Self-contained IBGE (Instituto Brasileiro de Geografia e Estatistica) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be courteous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ibge_base <- "https://servicodados.ibge.gov.br/api/v3"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_aggregates <- tibble(
  research_id = character(), research_name = character(),
  aggregate_id = character(), aggregate_name = character()
)

.schema_metadata <- tibble(
  variable_id = integer(), variable_name = character(),
  unit = character()
)

.schema_data <- tibble(
  variable_id = character(), variable_name = character(),
  unit = character(), location_id = character(),
  location_level = character(), location_name = character(),
  period = character(), value = character()
)

# == Public functions ==========================================================

#' List IBGE aggregate tables
#'
#' Returns all available aggregate surveys/tables from the IBGE SIDRA system.
#'
#' @return tibble: research_id, research_name, aggregate_id, aggregate_name
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
#' @param geo Geographic level and filter. Format: "N1[all]" for Brazil,
#'   "N3[all]" for all states, "N6[3304557]" for a specific municipality.
#'   Common levels: N1=Brazil, N2=Region, N3=State, N6=Municipality
#' @return tibble: variable_id, variable_name, unit, location_id,
#'   location_level, location_name, period, value
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
