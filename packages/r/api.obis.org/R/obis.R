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


#' Search OBIS marine occurrence records
#'
#' Queries the Ocean Biodiversity Information System for georeferenced
#' species occurrence records from the global ocean. Filter by taxonomy,
#' geography (WKT), or date range. Each row is one observed occurrence
#' with coordinates, year, and taxonomic classification. Use
#' \code{obis_checklist()} for aggregated species lists or
#' \code{obis_taxon()} to look up taxonomy details.
#'
#' @param scientificname Character or NULL. Scientific name to search,
#'   at any taxonomic level: \code{"Delphinidae"} (family),
#'   \code{"Tursiops truncatus"} (species), \code{"Chordata"} (phylum).
#'   Default NULL (no name filter).
#' @param taxonid Integer or NULL. OBIS/WoRMS taxon ID as an alternative
#'   to name-based search, e.g. \code{137205} for Caretta caretta.
#'   Default NULL.
#' @param geometry Character or NULL. WKT geometry string for spatial
#'   filtering, e.g. \code{"POLYGON((-10 50, 0 50, 0 60, -10 60, -10 50))"}.
#'   Default NULL (global).
#' @param startdate Character or NULL. Start date in \code{"YYYY-MM-DD"}
#'   format, e.g. \code{"2020-01-01"}. Default NULL (no start filter).
#' @param enddate Character or NULL. End date in \code{"YYYY-MM-DD"}
#'   format, e.g. \code{"2023-12-31"}. Default NULL (no end filter).
#' @param size Integer. Number of records to return, default \code{100},
#'   maximum \code{10000}.
#' @param offset Integer. Offset for pagination, default \code{0}. Use
#'   with \code{size} to page through large result sets.
#' @return A tibble with one row per occurrence record:
#'   \describe{
#'     \item{catalogNumber}{\code{character} -- Unique record identifier within its dataset}
#'     \item{scientificName}{\code{character} -- Full scientific name (e.g. "Tursiops truncatus")}
#'     \item{family}{\code{character} -- Taxonomic family (e.g. "Delphinidae")}
#'     \item{genus}{\code{character} -- Genus name}
#'     \item{species}{\code{character} -- Binomial species name}
#'     \item{decimalLatitude}{\code{numeric} -- Latitude in decimal degrees}
#'     \item{decimalLongitude}{\code{numeric} -- Longitude in decimal degrees}
#'     \item{date_year}{\code{integer} -- Year of observation}
#'     \item{datasetName}{\code{character} -- Source dataset name}
#'     \item{basisOfRecord}{\code{character} -- Record type (e.g. "HumanObservation", "PreservedSpecimen")}
#'   }
#' @examples
#' \dontrun{
#' # Dolphin family occurrences
#' obis_occurrence(scientificname = "Delphinidae", size = 50)
#'
#' # Sea turtles in the Mediterranean
#' obis_occurrence(scientificname = "Caretta caretta",
#'                 geometry = "POLYGON((0 30, 35 30, 35 45, 0 45, 0 30))",
#'                 size = 200)
#'
#' # Recent whale sightings by taxon ID
#' obis_occurrence(taxonid = 137087, startdate = "2020-01-01", size = 100)
#' }
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

#' Look up OBIS taxon details by ID
#'
#' Returns taxonomic metadata for a single taxon identified by its
#' OBIS/WoRMS taxon ID, including full Linnaean classification and
#' taxonomic status. Use \code{obis_checklist()} or \code{obis_occurrence()}
#' to discover taxon IDs.
#'
#' @param taxon_id Integer. OBIS/WoRMS taxon ID, e.g. \code{137205}
#'   (Caretta caretta), \code{137111} (Tursiops truncatus). Obtain from
#'   \code{obis_checklist()$taxonID} or \code{obis_occurrence()} results.
#' @return A single-row tibble:
#'   \describe{
#'     \item{taxonID}{\code{integer} -- OBIS/WoRMS taxon identifier}
#'     \item{scientificName}{\code{character} -- Scientific name}
#'     \item{taxonomicStatus}{\code{character} -- Status: "accepted", "unaccepted", etc.}
#'     \item{taxonRank}{\code{character} -- Rank: "Species", "Genus", "Family", etc.}
#'     \item{kingdom}{\code{character} -- Kingdom (e.g. "Animalia")}
#'     \item{phylum}{\code{character} -- Phylum (e.g. "Chordata")}
#'     \item{class}{\code{character} -- Class (may be NA for some taxa)}
#'     \item{order}{\code{character} -- Order (e.g. "Testudines")}
#'     \item{family}{\code{character} -- Family (e.g. "Cheloniidae")}
#'     \item{genus}{\code{character} -- Genus name}
#'   }
#' @examples
#' \dontrun{
#' # Look up loggerhead sea turtle
#' obis_taxon(137205)
#'
#' # Look up bottlenose dolphin
#' obis_taxon(137111)
#' }
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
#' Returns an aggregated species list with occurrence counts, filtered
#' by taxonomy or geography. Unlike \code{obis_occurrence()} which returns
#' individual records, this returns one row per species with total record
#' counts -- useful for biodiversity assessments and species inventories.
#' Results are sorted by record count (most observed species first).
#'
#' @param scientificname Character or NULL. Scientific name to filter by,
#'   typically a higher taxon: \code{"Delphinidae"} (family),
#'   \code{"Cetacea"} (order). Default NULL.
#' @param taxonid Integer or NULL. OBIS/WoRMS taxon ID filter.
#'   Default NULL.
#' @param geometry Character or NULL. WKT geometry for spatial filter,
#'   e.g. \code{"POLYGON((-80 25, -60 25, -60 45, -80 45, -80 25))"}.
#'   Default NULL (global).
#' @param size Integer. Number of species to return, default \code{100}.
#' @return A tibble with one row per species:
#'   \describe{
#'     \item{taxonID}{\code{integer} -- OBIS/WoRMS taxon ID (use with \code{obis_taxon()})}
#'     \item{scientificName}{\code{character} -- Binomial species name (e.g. "Tursiops truncatus")}
#'     \item{taxonRank}{\code{character} -- Taxonomic rank ("Species", "Family", etc.)}
#'     \item{family}{\code{character} -- Family name}
#'     \item{records}{\code{integer} -- Total number of occurrence records in OBIS}
#'   }
#' @examples
#' \dontrun{
#' # Top 10 dolphin species by occurrence count
#' obis_checklist(scientificname = "Delphinidae", size = 10)
#'
#' # Species checklist for a Caribbean polygon
#' obis_checklist(geometry = "POLYGON((-90 15, -60 15, -60 30, -90 30, -90 15))",
#'                size = 50)
#'
#' # All whale species
#' obis_checklist(scientificname = "Cetacea", size = 200)
#' }
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

#' Get api.obis.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.obis.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.obis.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.obis.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.obis.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
