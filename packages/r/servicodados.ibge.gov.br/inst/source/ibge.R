


# servicodados-ibge-gov-br.R
# Self-contained IBGE (Instituto Brasileiro de Geografia e Estatistica) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ibge_base <- "https://servicodados.ibge.gov.br/api/v3"
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
# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

# == Context ===================================================================

#' Generate LLM-friendly context for servicodados.ibge.gov.br
#'
#' @return Character string with full function signatures and bodies
#' @export
ibge_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/servicodados.ibge.gov.br.R"
  if (!file.exists(src_file)) {
    cat("# servicodados.ibge.gov.br context - source not found\n")
    return(invisible("# servicodados.ibge.gov.br context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

