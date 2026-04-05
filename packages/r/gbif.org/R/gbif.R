# gbif.org.R - Self-contained GBIF (Global Biodiversity Information Facility) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gbif_base <- "https://api.gbif.org/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.build_url <- function(path, params = list()) {
  params <- params[!vapply(params, is.null, logical(1))]
  url <- paste0(.gbif_base, path)
  if (length(params) > 0) {
    pairs <- paste0(names(params), "=", vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), character(1)))
    url <- paste0(url, "?", paste(pairs, collapse = "&"))
  }
  url
}

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_occurrence <- tibble(
  key = numeric(), scientificName = character(), species = character(),
  family = character(), order = character(), class = character(),
  phylum = character(), kingdom = character(),
  decimalLatitude = numeric(), decimalLongitude = numeric(),
  country = character(), year = integer(), month = integer(),
  basisOfRecord = character(), datasetName = character(),
  institutionCode = character()
)

.schema_species <- tibble(
  key = integer(), scientificName = character(), canonicalName = character(),
  rank = character(), taxonomicStatus = character(),
  kingdom = character(), phylum = character(), class = character(),
  order = character(), family = character(), genus = character(),
  numOccurrences = integer()
)

.schema_dataset <- tibble(
  key = character(), title = character(), type = character(),
  description = character(), publishingCountry = character(),
  license = character(), recordCount = integer()
)

# == Occurrence ================================================================

#' Search GBIF occurrence records
#'
#' Searches the Global Biodiversity Information Facility for species
#' occurrence records. Supports filtering by taxonomy, geography, time,
#' and record type. Over 2 billion records are available.
#'
#' @param scientificName Character scientific name to search (e.g.
#'   "Puma concolor"). Matched against the GBIF backbone taxonomy.
#' @param taxonKey Numeric GBIF taxon key. More precise than name search.
#'   Get keys from \code{\link{gbif_species}}.
#' @param country Character two-letter ISO country code (e.g. "US", "BR").
#' @param geometry Character WKT geometry string for spatial filtering
#'   (e.g. "POLYGON((-10 50, 10 50, 10 40, -10 40, -10 50))").
#' @param year Integer year or character range (e.g. "2000,2020").
#' @param basisOfRecord Character record type filter: "HUMAN_OBSERVATION",
#'   "PRESERVED_SPECIMEN", "MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", etc.
#' @param limit Integer number of records (default 100, max 300).
#' @param offset Integer pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{GBIF occurrence key}
#'     \item{scientificName}{Full scientific name with author}
#'     \item{species}{Species binomial}
#'     \item{family, order, class, phylum, kingdom}{Taxonomic hierarchy}
#'     \item{decimalLatitude, decimalLongitude}{Coordinates (WGS84)}
#'     \item{country}{Country of occurrence}
#'     \item{year, month}{Observation date components}
#'     \item{basisOfRecord}{Record type}
#'     \item{datasetName}{Source dataset name}
#'     \item{institutionCode}{Contributing institution code}
#'   }
#' @examples
#' # Mountain lion sightings
#' gbif_occurrences(scientificName = "Puma concolor", limit = 10)
#'
#' # Bird observations in the US in 2023
#' gbif_occurrences(taxonKey = 212, country = "US", year = 2023, limit = 10)
#' @export
gbif_occurrences <- function(scientificName = NULL, taxonKey = NULL,
                             country = NULL, geometry = NULL,
                             year = NULL, basisOfRecord = NULL,
                             limit = 100, offset = 0) {
  params <- list(
    scientificName = scientificName, taxonKey = taxonKey,
    country = country, geometry = geometry,
    year = year, basisOfRecord = basisOfRecord,
    limit = min(limit, 300), offset = offset
  )
  url <- .build_url("/occurrence/search", params)
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || length(res$results) == 0) return(.schema_occurrence)

  d <- res$results
  tibble(
    key = as.numeric(.safe_col(d, "key", NA_real_)),
    scientificName = as.character(.safe_col(d, "scientificName")),
    species = as.character(.safe_col(d, "species")),
    family = as.character(.safe_col(d, "family")),
    order = as.character(.safe_col(d, "order")),
    class = as.character(.safe_col(d, "class")),
    phylum = as.character(.safe_col(d, "phylum")),
    kingdom = as.character(.safe_col(d, "kingdom")),
    decimalLatitude = as.numeric(.safe_col(d, "decimalLatitude", NA_real_)),
    decimalLongitude = as.numeric(.safe_col(d, "decimalLongitude", NA_real_)),
    country = as.character(.safe_col(d, "country")),
    year = as.integer(.safe_col(d, "year", NA_integer_)),
    month = as.integer(.safe_col(d, "month", NA_integer_)),
    basisOfRecord = as.character(.safe_col(d, "basisOfRecord")),
    datasetName = as.character(.safe_col(d, "datasetName")),
    institutionCode = as.character(.safe_col(d, "institutionCode"))
  )
}

# == Species / Taxonomy ========================================================

#' Search GBIF species/taxonomy
#'
#' Searches the GBIF backbone taxonomy for species and higher taxa.
#' Returns matching names with their full taxonomic classification and
#' occurrence counts.
#'
#' @param q Character search query (e.g. "Puma", "Quercus alba").
#' @param rank Character taxonomic rank filter: "KINGDOM", "PHYLUM",
#'   "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES".
#' @param kingdom Character kingdom filter (e.g. "Animalia", "Plantae").
#' @param limit Integer number of results (default 20, max 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{GBIF taxon key (use with \code{\link{gbif_occurrences}})}
#'     \item{scientificName}{Full scientific name with author}
#'     \item{canonicalName}{Name without author}
#'     \item{rank}{Taxonomic rank}
#'     \item{taxonomicStatus}{Status (ACCEPTED, SYNONYM, etc.)}
#'     \item{kingdom, phylum, class, order, family, genus}{Classification}
#'     \item{numOccurrences}{Total occurrence records in GBIF}
#'   }
#' @examples
#' # Search for cats
#' gbif_species("Felidae", rank = "FAMILY")
#'
#' # Search for oak species
#' gbif_species("Quercus", rank = "GENUS", kingdom = "Plantae")
#' @export
gbif_species <- function(q, rank = NULL, kingdom = NULL, limit = 20) {
  params <- list(q = q, rank = rank, kingdom = kingdom, limit = min(limit, 100))
  url <- .build_url("/species/search", params)
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || length(res$results) == 0) return(.schema_species)

  d <- res$results
  tibble(
    key = as.integer(.safe_col(d, "key", NA_integer_)),
    scientificName = as.character(.safe_col(d, "scientificName")),
    canonicalName = as.character(.safe_col(d, "canonicalName")),
    rank = as.character(.safe_col(d, "rank")),
    taxonomicStatus = as.character(.safe_col(d, "taxonomicStatus")),
    kingdom = as.character(.safe_col(d, "kingdom")),
    phylum = as.character(.safe_col(d, "phylum")),
    class = as.character(.safe_col(d, "class")),
    order = as.character(.safe_col(d, "order")),
    family = as.character(.safe_col(d, "family")),
    genus = as.character(.safe_col(d, "genus")),
    numOccurrences = as.integer(.safe_col(d, "numOccurrences", NA_integer_))
  )
}

#' Look up a single GBIF species by taxon key
#'
#' Retrieves the full taxonomic record for a single taxon from the GBIF
#' backbone taxonomy by its numeric key.
#'
#' @param taxonKey Numeric GBIF taxon key (e.g. 2435099 for Puma concolor).
#'   Get keys from \code{\link{gbif_species}} search results.
#' @return A one-row tibble with columns: key, scientificName, canonicalName,
#'   rank, taxonomicStatus, kingdom, phylum, class, order, family, genus,
#'   numOccurrences.
#' @examples
#' # Look up Puma concolor by key
#' gbif_species_lookup(2435099)
#' @export
gbif_species_lookup <- function(taxonKey) {
  url <- paste0(.gbif_base, "/species/", taxonKey)
  d <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(d)) return(.schema_species[0, ])

  tibble(
    key = as.integer(d$key %||% NA_integer_),
    scientificName = as.character(d$scientificName %||% NA_character_),
    canonicalName = as.character(d$canonicalName %||% NA_character_),
    rank = as.character(d$rank %||% NA_character_),
    taxonomicStatus = as.character(d$taxonomicStatus %||% NA_character_),
    kingdom = as.character(d$kingdom %||% NA_character_),
    phylum = as.character(d$phylum %||% NA_character_),
    class = as.character(d$class %||% NA_character_),
    order = as.character(d$order %||% NA_character_),
    family = as.character(d$family %||% NA_character_),
    genus = as.character(d$genus %||% NA_character_),
    numOccurrences = NA_integer_
  )
}

# == Datasets ==================================================================

#' Search GBIF datasets
#'
#' Searches the GBIF dataset registry. Datasets are collections of
#' occurrence records, checklists, or metadata published by institutions
#' worldwide.
#'
#' @param q Character search query (e.g. "birds", "marine mammals").
#' @param type Character dataset type filter: "OCCURRENCE", "CHECKLIST",
#'   "METADATA", or "SAMPLING_EVENT".
#' @param publishingCountry Character two-letter ISO country code of the
#'   publishing organization (e.g. "US", "GB").
#' @param limit Integer number of results (default 20, max 100).
#' @param offset Integer pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{Dataset UUID}
#'     \item{title}{Dataset title}
#'     \item{type}{Dataset type}
#'     \item{description}{Dataset description}
#'     \item{publishingCountry}{Country of the publishing organization}
#'     \item{license}{Data license}
#'     \item{recordCount}{Number of occurrence records}
#'   }
#' @examples
#' # Search for bird datasets
#' gbif_datasets(q = "birds", limit = 5)
#'
#' # US occurrence datasets
#' gbif_datasets(type = "OCCURRENCE", publishingCountry = "US", limit = 5)
#' @export
gbif_datasets <- function(q = NULL, type = NULL, publishingCountry = NULL,
                          limit = 20, offset = 0) {
  params <- list(q = q, type = type, publishingCountry = publishingCountry,
                 limit = min(limit, 100), offset = offset)
  url <- .build_url("/dataset/search", params)
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || length(res$results) == 0) return(.schema_dataset)

  d <- res$results
  tibble(
    key = as.character(.safe_col(d, "key")),
    title = as.character(.safe_col(d, "title")),
    type = as.character(.safe_col(d, "type")),
    description = as.character(.safe_col(d, "description")),
    publishingCountry = as.character(.safe_col(d, "publishingCountry")),
    license = as.character(.safe_col(d, "license")),
    recordCount = as.integer(.safe_col(d, "recordCount", NA_integer_))
  )
}

# == Context ===================================================================

#' Get gbif.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gbif_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gbif_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gbif.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gbif.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gbif.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gbif.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
