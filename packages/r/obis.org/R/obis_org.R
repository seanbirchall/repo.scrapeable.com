# api.obis.org.R - Self-contained api.obis.org client



# api-obis-org.R
# Self-contained OBIS (Ocean Biodiversity Information System) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.obis_base <- "https://api.obis.org/v3"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
#' Queries the Ocean Biodiversity Information System for georeferenced
#' species-occurrence records. Supports taxonomic, spatial, and temporal
#' filters. Results are paginated; use \code{offset} to page through large
#' result sets.
#'
#' @param scientificname Character. Scientific name to search
#'   (e.g. \code{"Delphinidae"}, \code{"Tursiops truncatus"}).
#' @param taxonid Integer. OBIS taxon ID (alternative to name search).
#' @param geometry Character. WKT geometry string for spatial filtering
#'   (e.g. \code{"POLYGON((-10 50,10 50,10 60,-10 60,-10 50))"}).
#' @param startdate Character. Start date in \code{"YYYY-MM-DD"} format.
#' @param enddate Character. End date in \code{"YYYY-MM-DD"} format.
#' @param size Integer. Number of records to return (default 100, max 10000).
#' @param offset Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{catalogNumber}{Character. Record catalogue number.}
#'     \item{scientificName}{Character. Matched scientific name.}
#'     \item{family}{Character. Taxonomic family.}
#'     \item{genus}{Character. Taxonomic genus.}
#'     \item{species}{Character. Species binomial.}
#'     \item{decimalLatitude}{Numeric. Latitude in decimal degrees.}
#'     \item{decimalLongitude}{Numeric. Longitude in decimal degrees.}
#'     \item{date_year}{Integer. Year of observation.}
#'     \item{datasetName}{Character. Contributing dataset name.}
#'     \item{basisOfRecord}{Character. Record type (e.g. "HumanObservation").}
#'   }
#' @examples
#' obis_occurrence(scientificname = "Delphinidae", size = 5)
#' obis_occurrence(taxonid = 137111, startdate = "2020-01-01", size = 10)
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
#' Retrieves full taxonomic classification for a single OBIS taxon. Use
#' \code{\link{obis_checklist}} to discover taxon IDs for a given name.
#'
#' @param taxon_id Integer. OBIS taxon ID (e.g. \code{10194} for
#'   Actinopterygii, \code{137111} for Tursiops truncatus).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{taxonID}{Integer. OBIS taxon identifier.}
#'     \item{scientificName}{Character. Accepted scientific name.}
#'     \item{taxonomicStatus}{Character. Status (e.g. "accepted").}
#'     \item{taxonRank}{Character. Rank (e.g. "Species", "Family").}
#'     \item{kingdom}{Character. Kingdom (e.g. "Animalia").}
#'     \item{phylum}{Character. Phylum (e.g. "Chordata").}
#'     \item{class}{Character. Class.}
#'     \item{order}{Character. Order.}
#'     \item{family}{Character. Family.}
#'     \item{genus}{Character. Genus.}
#'   }
#' @examples
#' obis_taxon(10194)
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
#' Returns a species checklist from OBIS, optionally filtered by taxonomy or
#' geography. Each row represents a unique taxon with its occurrence count.
#' Useful for biodiversity inventories and discovering taxon IDs.
#'
#' @param scientificname Character. Scientific name filter
#'   (e.g. \code{"Delphinidae"}).
#' @param taxonid Integer. OBIS taxon ID filter.
#' @param geometry Character. WKT geometry string for spatial filtering.
#' @param size Integer. Number of results to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{taxonID}{Integer. OBIS taxon identifier.}
#'     \item{scientificName}{Character. Scientific name.}
#'     \item{taxonRank}{Character. Rank (e.g. "Species", "Family").}
#'     \item{family}{Character. Taxonomic family.}
#'     \item{records}{Integer. Number of occurrence records in OBIS.}
#'   }
#' @examples
#' obis_checklist(scientificname = "Delphinidae", size = 5)
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

# == Context ===================================================================

#' Get obis.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
obis_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(obis_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/obis.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "obis.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# obis.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# obis.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
