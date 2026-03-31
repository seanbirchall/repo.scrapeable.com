
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
