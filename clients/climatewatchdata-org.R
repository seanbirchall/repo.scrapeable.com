# climatewatchdata-org.R
# Self-contained Climate Watch Data client (historical emissions).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: unknown / be polite

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cw_base <- "https://www.climatewatchdata.org/api/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_emissions <- tibble(
  iso_code3 = character(), country = character(), data_source = character(),
  sector = character(), gas = character(), unit = character(),
  year = integer(), value = numeric()
)

.schema_sources <- tibble(
  id = integer(), name = character()
)

# == Emissions =================================================================

#' Fetch historical emissions data from Climate Watch
#'
#' Returns yearly emissions data from the CAIT / PIK / other sources.
#' Data is pivoted from wide to long: one row per country-sector-gas-year.
#'
#' @param regions Character vector of ISO3 codes or region names
#'   (e.g. "USA", "WORLD", "CHN"). Default "WORLD".
#' @param source_ids Integer vector of data source IDs (default NULL = all).
#'   Use cw_sources() to see available sources.
#' @param gas_ids Integer vector of gas IDs (default NULL = all)
#' @param sector_ids Integer vector of sector IDs (default NULL = all)
#' @param per_page Max results per page (default 50)
#' @param page Page number (default 1)
#' @return tibble: iso_code3, country, data_source, sector, gas, unit, year, value
#' @export
cw_emissions <- function(regions = "WORLD", source_ids = NULL, gas_ids = NULL,
                         sector_ids = NULL, per_page = 50, page = 1) {
  url <- paste0(.cw_base, "/data/historical_emissions?per_page=", per_page, "&page=", page)
  for (r in regions) url <- paste0(url, "&regions[]=", utils::URLencode(r))
  if (!is.null(source_ids)) for (s in source_ids) url <- paste0(url, "&source_ids[]=", s)
  if (!is.null(gas_ids)) for (g in gas_ids) url <- paste0(url, "&gas_ids[]=", g)
  if (!is.null(sector_ids)) for (s in sector_ids) url <- paste0(url, "&sector_ids[]=", s)

  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_emissions)

  rows <- lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    em <- row$emissions[[1]]
    if (is.null(em) || length(em) == 0) return(NULL)
    tibble(
      iso_code3   = as.character(row$iso_code3),
      country     = as.character(row$country),
      data_source = as.character(row$data_source),
      sector      = as.character(row$sector),
      gas         = as.character(row$gas),
      unit        = as.character(row$unit),
      year        = as.integer(em$year),
      value       = as.numeric(em$value)
    )
  })
  bind_rows(rows)
}

#' List available data sources
#'
#' @return tibble: id, name
#' @export
cw_sources <- function() {
  url <- paste0(.cw_base, "/emissions/meta")
  raw <- .fetch_json(url)
  srcs <- raw$data_source
  if (is.null(srcs) || length(srcs) == 0) return(.schema_sources)
  tibble(
    id   = as.integer(srcs$id),
    name = as.character(srcs$name)
  )
}

#' Climate Watch context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
cw_context <- function() {
  .build_context("climatewatchdata.org", header_lines = c(
    "# climatewatchdata.org - Climate Watch Historical Emissions API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite",
    "#",
    "# Common regions: WORLD, USA, CHN, IND, EU, GBR, DEU, JPN, BRA",
    "# Use cw_sources() to discover available data source IDs"
  ))
}
