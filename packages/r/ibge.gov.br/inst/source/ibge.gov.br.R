# servicodados.ibge.gov.br.R - Self-contained servicodados.ibge.gov.br client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

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

#' List all IBGE aggregate survey tables
#'
#' Returns the complete catalog of aggregate tables from the IBGE SIDRA
#' statistical system, organized by research/survey. Useful for discovering
#' available datasets before fetching data with \code{ibge_data()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{research_id}{Character. Research/survey ID.}
#'     \item{research_name}{Character. Research/survey name (in Portuguese).}
#'     \item{aggregate_id}{Character. Aggregate table ID (use with \code{ibge_data()}).}
#'     \item{aggregate_name}{Character. Table name/description (in Portuguese).}
#'   }
#' @examples
#' ibge_aggregates()
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

#' Get variable metadata for an IBGE aggregate table
#'
#' Returns the variable definitions (names, IDs, and units) available in a
#' specific IBGE aggregate table. Use these variable IDs with \code{ibge_data()}.
#'
#' @param aggregate_id Character or integer. Aggregate table ID
#'   (e.g. \code{"4714"} for Population Census). Use \code{ibge_aggregates()}
#'   to discover table IDs.
#' @return A tibble with columns:
#'   \describe{
#'     \item{variable_id}{Integer. Variable identifier (use with \code{ibge_data()}).}
#'     \item{variable_name}{Character. Variable name (in Portuguese, e.g.
#'       \code{"Populacao residente"}).}
#'     \item{unit}{Character. Measurement unit (e.g. \code{"Pessoas"},
#'       \code{"Quilometros quadrados"}).}
#'   }
#' @examples
#' ibge_metadata("4714")
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

#' Fetch IBGE statistical data by aggregate, variable, period, and geography
#'
#' Retrieves actual data values from the IBGE SIDRA system for a specific
#' combination of aggregate table, variable, time period, and geographic level.
#'
#' @param aggregate_id Character. Aggregate table ID (e.g. \code{"4714"} for
#'   population data). Use \code{ibge_aggregates()} to discover IDs.
#' @param variable_id Character. Variable ID within the aggregate (e.g.
#'   \code{"93"} for resident population). Use \code{ibge_metadata()} to
#'   discover variable IDs.
#' @param period Character. Time period: \code{"-1"} for latest available
#'   (default), or a specific year like \code{"2022"}.
#' @param geo Character. Geographic level and filter in the format
#'   \code{"N<level>[<code>]"}:
#'   \itemize{
#'     \item \code{"N1[all]"} -- Brazil (national, default)
#'     \item \code{"N2[all]"} -- All macro-regions
#'     \item \code{"N3[all]"} -- All states
#'     \item \code{"N3[33]"} -- Rio de Janeiro state only
#'     \item \code{"N6[all]"} -- All municipalities
#'     \item \code{"N6[3304557]"} -- A specific municipality
#'   }
#' @return A tibble with columns:
#'   \describe{
#'     \item{variable_id}{Character. Variable identifier.}
#'     \item{variable_name}{Character. Variable name (in Portuguese).}
#'     \item{unit}{Character. Measurement unit.}
#'     \item{location_id}{Character. Geographic location code.}
#'     \item{location_level}{Character. Geographic level name (e.g. \code{"Brasil"}, \code{"Estado"}).}
#'     \item{location_name}{Character. Location name.}
#'     \item{period}{Character. Time period (typically a year).}
#'     \item{value}{Character. Data value (character to handle missing data codes).}
#'   }
#' @examples
#' ibge_data("4714", "93")
#' ibge_data("4714", "93", period = "2022", geo = "N3[all]")
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

# == Localities ================================================================

#' List all Brazilian states (Unidades Federativas)
#'
#' Returns all 27 Brazilian states and the Federal District from the IBGE
#' localities API, including their abbreviations and macro-region membership.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. State IBGE code (e.g. \code{33} for Rio de Janeiro).}
#'     \item{abbreviation}{Character. Two-letter state abbreviation (e.g. \code{"SP"}, \code{"RJ"}).}
#'     \item{name}{Character. Full state name (e.g. \code{"Sao Paulo"}).}
#'     \item{region_id}{Integer. Macro-region ID (1=Norte, 2=Nordeste, 3=Sudeste, 4=Sul, 5=Centro-Oeste).}
#'     \item{region_name}{Character. Macro-region name.}
#'   }
#' @examples
#' ibge_states()
#' @export
ibge_states <- function() {
  schema <- tibble(id = integer(), abbreviation = character(),
                   name = character(), region_id = integer(),
                   region_name = character())
  url <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados"
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)

  tibble(
    id = vapply(raw, function(x) as.integer(x$id %||% NA), integer(1)),
    abbreviation = vapply(raw, function(x) as.character(x$sigla %||% NA), character(1)),
    name = vapply(raw, function(x) as.character(x$nome %||% NA), character(1)),
    region_id = vapply(raw, function(x) as.integer(x$regiao$id %||% NA), integer(1)),
    region_name = vapply(raw, function(x) as.character(x$regiao$nome %||% NA), character(1))
  )
}

# == Context ===================================================================

#' Get ibge.gov.br client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ibge_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ibge_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ibge.gov.br.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ibge.gov.br")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ibge.gov.br context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ibge.gov.br", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
