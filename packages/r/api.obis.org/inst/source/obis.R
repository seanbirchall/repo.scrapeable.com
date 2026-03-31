# api-obis-org.R
# Self-contained OBIS (Ocean Biodiversity Information System) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.obis_base <- "https://api.obis.org/v3"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_occurrence <- tibble(
  catalogNumber = character(), scientificName = character(),
  family = character(), genus = character(), species = character(),
  decimalLatitude = numeric(), decimalLongitude = numeric(),
  date_year = integer(), datasetName = character(),
  basisOfRecord = character()
)

.schema_taxon <- tibble(
  taxonID = integer(), scientificName = character(),
  taxonomicStatus = character(), taxonRank = character(),
  kingdom = character(), phylum = character(), class = character(),
  order = character(), family = character(), genus = character()
)

.schema_checklist <- tibble(
  taxonID = integer(), scientificName = character(),
  taxonRank = character(), family = character(),
  records = integer()
)

# == Occurrence ================================================================

#' Search OBIS occurrence records
#'
#' @param scientificname Scientific name to search (e.g. "Delphinidae")
#' @param taxonid OBIS taxon ID (alternative to name)
#' @param geometry WKT geometry for spatial filter
#' @param startdate Start date "YYYY-MM-DD"
#' @param enddate End date "YYYY-MM-DD"
#' @param size Number of records (default 100, max 10000)
#' @param offset Offset for pagination (default 0)
#' @return tibble: catalogNumber, scientificName, family, genus, species,
#'   decimalLatitude, decimalLongitude, date_year, datasetName, basisOfRecord
#' @export
obis_occurrence <- function(scientificname = NULL, taxonid = NULL,
                            geometry = NULL, startdate = NULL, enddate = NULL,
                            size = 100, offset = 0) {
  params <- list()
  if (!is.null(scientificname)) params$scientificname <- utils::URLencode(scientificname)
  if (!is.null(taxonid)) params$taxonid <- taxonid
  if (!is.null(geometry)) params$geometry <- utils::URLencode(geometry)
  if (!is.null(startdate)) params$startdate <- startdate
  if (!is.null(enddate)) params$enddate <- enddate
  params$size <- size
  params$offset <- offset

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.obis_base, "/occurrence?", query)
  raw <- .fetch_json(url)
  d <- raw$results
  if (is.null(d) || length(d) == 0) return(.schema_occurrence)

  as_tibble(d) |>
    transmute(
      catalogNumber    = as.character(catalogNumber %||% NA),
      scientificName   = as.character(scientificName %||% NA),
      family           = as.character(if ("family" %in% names(d)) family else NA),
      genus            = as.character(if ("genus" %in% names(d)) genus else NA),
      species          = as.character(if ("species" %in% names(d)) species else NA),
      decimalLatitude  = as.numeric(if ("decimalLatitude" %in% names(d)) decimalLatitude else NA),
      decimalLongitude = as.numeric(if ("decimalLongitude" %in% names(d)) decimalLongitude else NA),
      date_year        = as.integer(if ("date_year" %in% names(d)) date_year else NA),
      datasetName      = as.character(if ("datasetName" %in% names(d)) datasetName else NA),
      basisOfRecord    = as.character(if ("basisOfRecord" %in% names(d)) basisOfRecord else NA)
    )
}

#' Look up OBIS taxon by ID
#'
#' @param taxon_id OBIS taxon ID (integer)
#' @return tibble: taxonID, scientificName, taxonomicStatus, taxonRank,
#'   kingdom, phylum, class, order, family, genus
#' @export
obis_taxon <- function(taxon_id) {
  url <- paste0(.obis_base, "/taxon/", taxon_id)
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_taxon)
  r <- results[1, ]
  tibble(
    taxonID          = as.integer(r$taxonID %||% NA),
    scientificName   = as.character(r$scientificName %||% NA),
    taxonomicStatus  = as.character(r$taxonomicStatus %||% NA),
    taxonRank        = as.character(r$taxonRank %||% NA),
    kingdom          = as.character(r$kingdom %||% NA),
    phylum           = as.character(r$phylum %||% NA),
    class            = as.character(r$class %||% NA),
    order            = as.character(r$order %||% NA),
    family           = as.character(r$family %||% NA),
    genus            = as.character(if ("genus" %in% names(r)) r$genus else NA)
  )
}

#' Fetch OBIS species checklist
#'
#' @param scientificname Scientific name filter
#' @param taxonid Taxon ID filter
#' @param geometry WKT geometry filter
#' @param size Number of results (default 100)
#' @return tibble: taxonID, scientificName, taxonRank, family, records
#' @export
obis_checklist <- function(scientificname = NULL, taxonid = NULL,
                           geometry = NULL, size = 100) {
  params <- list()
  if (!is.null(scientificname)) params$scientificname <- utils::URLencode(scientificname)
  if (!is.null(taxonid)) params$taxonid <- taxonid
  if (!is.null(geometry)) params$geometry <- utils::URLencode(geometry)
  params$size <- size

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.obis_base, "/checklist?", query)
  raw <- .fetch_json(url)
  d <- raw$results
  if (is.null(d) || length(d) == 0) return(.schema_checklist)

  as_tibble(d) |>
    transmute(
      taxonID        = as.integer(if ("taxonID" %in% names(d)) taxonID else NA),
      scientificName = as.character(if ("scientificName" %in% names(d)) scientificName else NA),
      taxonRank      = as.character(if ("taxonRank" %in% names(d)) taxonRank else NA),
      family         = as.character(if ("family" %in% names(d)) family else NA),
      records        = as.integer(if ("records" %in% names(d)) records else NA)
    )
}

#' OBIS context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
obis_context <- function() {
  .build_context("api.obis.org", header_lines = c(
    "# api.obis.org - Ocean Biodiversity Information System Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite",
    "#",
    "# Common taxon IDs: 136980 (Delphinidae), 125925 (Balaenoptera),",
    "#   216 (Scleractinia/corals), 127160 (Cheloniidae/sea turtles)"
  ))
}
