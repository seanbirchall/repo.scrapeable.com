# inaturalist.R
# Self-contained iNaturalist API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: ~100 requests/minute (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.inat_base <- "https://api.inaturalist.org/v1"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

.schema_observations <- tibble(
  id = integer(), species_guess = character(), taxon_name = character(),
  observed_on = as.Date(character()), place_guess = character(),
  latitude = numeric(), longitude = numeric(), quality_grade = character(),
  user_login = character()
)

.schema_taxa <- tibble(
  id = integer(), name = character(), common_name = character(),
  rank = character(), observations_count = integer(),
  iconic_taxon_name = character()
)

# == Public functions ==========================================================

#' Search iNaturalist observations
#'
#' Query the iNaturalist API for biodiversity observations. Observations
#' are citizen-science records of species sightings, each with taxonomic
#' identification, location, date, and quality grade. Filter by taxon,
#' place, or quality grade.
#'
#' @param taxon_name Taxon name to filter by (e.g. \code{"Panthera"},
#'   \code{"Aves"}, \code{"Quercus"}).
#' @param query Free-text search query across all fields.
#' @param place_id iNaturalist place ID to filter observations geographically.
#' @param quality_grade Quality grade filter: \code{"research"},
#'   \code{"needs_id"}, or \code{"casual"}.
#' @param per_page Results per page (default 30, max 200).
#' @param page Page number for pagination (default 1).
#' @return A tibble with one row per observation:
#'   \describe{
#'     \item{id}{\code{integer} -- iNaturalist observation ID.}
#'     \item{species_guess}{\code{character} -- Observer's species guess.}
#'     \item{taxon_name}{\code{character} -- Scientific name from community ID.}
#'     \item{observed_on}{\code{Date} -- Date of observation.}
#'     \item{place_guess}{\code{character} -- Free-text location description.}
#'     \item{latitude}{\code{numeric} -- Latitude in decimal degrees.}
#'     \item{longitude}{\code{numeric} -- Longitude in decimal degrees.}
#'     \item{quality_grade}{\code{character} -- Quality grade.}
#'     \item{user_login}{\code{character} -- Observer's username.}
#'   }
#' @examples
#' \dontrun{
#' inat_observations(taxon_name = "Panthera", per_page = 10)
#' inat_observations(query = "monarch butterfly", quality_grade = "research")
#' }
#' @export
inat_observations <- function(taxon_name = NULL, query = NULL, place_id = NULL,
                              quality_grade = NULL, per_page = 30, page = 1) {
  params <- list(
    taxon_name = taxon_name, q = query, place_id = place_id,
    quality_grade = quality_grade, per_page = per_page, page = page
  )
  params <- params[!vapply(params, is.null, logical(1))]
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.inat_base, "/observations?", qstr)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_observations)

  tibble(
    id = as.integer(results$id),
    species_guess = as.character(results$species_guess %||% NA_character_),
    taxon_name = as.character(
      if (!is.null(results$taxon) && !is.null(results$taxon$name)) results$taxon$name
      else NA_character_
    ),
    observed_on = as.Date(results$observed_on %||% NA_character_),
    place_guess = as.character(results$place_guess %||% NA_character_),
    latitude = as.numeric(vapply(results$geojson$coordinates, function(x) if (!is.null(x)) x[2] else NA_real_, numeric(1))),
    longitude = as.numeric(vapply(results$geojson$coordinates, function(x) if (!is.null(x)) x[1] else NA_real_, numeric(1))),
    quality_grade = as.character(results$quality_grade %||% NA_character_),
    user_login = as.character(results$user$login %||% NA_character_)
  )
}

#' Search iNaturalist taxa
#'
#' Search the iNaturalist taxonomy for species, genera, families, and other
#' taxonomic ranks. Returns scientific names, common names, and observation
#' counts.
#'
#' @param q Search query -- scientific or common name (e.g. \code{"wolf"},
#'   \code{"Canis lupus"}, \code{"oak"}).
#' @param per_page Results per page (default 30, max 200).
#' @return A tibble with one row per taxon:
#'   \describe{
#'     \item{id}{\code{integer} -- iNaturalist taxon ID.}
#'     \item{name}{\code{character} -- Scientific name.}
#'     \item{common_name}{\code{character} -- Preferred common name.}
#'     \item{rank}{\code{character} -- Taxonomic rank (e.g. \code{"species"}, \code{"genus"}, \code{"family"}).}
#'     \item{observations_count}{\code{integer} -- Total observations on iNaturalist.}
#'     \item{iconic_taxon_name}{\code{character} -- Iconic taxon group (e.g. \code{"Mammalia"}, \code{"Plantae"}).}
#'   }
#' @examples
#' \dontrun{
#' inat_taxa("wolf")
#' inat_taxa("Quercus", per_page = 50)
#' }
#' @export
inat_taxa <- function(q, per_page = 30) {
  url <- sprintf("%s/taxa?q=%s&per_page=%d", .inat_base,
                 utils::URLencode(q, reserved = TRUE), as.integer(per_page))
  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_taxa)

  tibble(
    id = as.integer(results$id),
    name = as.character(results$name),
    common_name = as.character(
      if (!is.null(results$preferred_common_name)) results$preferred_common_name
      else NA_character_
    ),
    rank = as.character(results$rank),
    observations_count = as.integer(results$observations_count),
    iconic_taxon_name = as.character(results$iconic_taxon_name %||% NA_character_)
  )
}

#' Get iNaturalist client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' inat_context()
#' }
#' @export
inat_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(inat_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/inaturalist.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "inaturalist")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# inaturalist context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# inaturalist", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
