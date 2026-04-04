# inaturalist.org.R - Self-contained inaturalist.org client



# inaturalist.R
# Self-contained iNaturalist API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: ~100 requests/minute (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.inat_base <- "https://api.inaturalist.org/v1"

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

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search iNaturalist observations
#'
#' Query the iNaturalist database for wildlife and plant observations.
#' Supports filtering by taxon, location, and data quality. Results are
#' paginated; use \code{page} to retrieve subsequent pages.
#'
#' @param taxon_name Character. Scientific or common taxon name to filter by
#'   (e.g., \code{"Panthera"}, \code{"Aves"}, \code{"Fungi"}). Default NULL.
#' @param query Character. Free-text search string (e.g., \code{"monarch butterfly"}).
#'   Default NULL.
#' @param place_id Integer. iNaturalist place ID to restrict results geographically
#'   (e.g., \code{1} for United States). Default NULL.
#' @param quality_grade Character. Filter by data quality: \code{"research"},
#'   \code{"needs_id"}, or \code{"casual"}. Default NULL (all grades).
#' @param per_page Integer. Results per page (default 30, max 200).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- unique observation identifier}
#'     \item{species_guess}{character -- observer's species guess}
#'     \item{taxon_name}{character -- community-agreed scientific name}
#'     \item{observed_on}{Date -- date the observation was made}
#'     \item{place_guess}{character -- human-readable location description}
#'     \item{latitude}{numeric -- observation latitude (WGS84)}
#'     \item{longitude}{numeric -- observation longitude (WGS84)}
#'     \item{quality_grade}{character -- \code{"research"}, \code{"needs_id"}, or \code{"casual"}}
#'     \item{user_login}{character -- observer's iNaturalist username}
#'   }
#' @export
#' @examples
#' \dontrun{
#' inat_observations(taxon_name = "Aves", per_page = 10)
#' inat_observations(query = "monarch butterfly", quality_grade = "research")
#' }
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
#' Look up taxonomic information in the iNaturalist database. Returns
#' scientific names, common names, taxonomic rank, and observation counts.
#'
#' @param q Character. Search query -- scientific or common name (e.g.,
#'   \code{"wolf"}, \code{"Canis lupus"}, \code{"sunflower"}).
#' @param per_page Integer. Maximum results to return (default 30, max 200).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- iNaturalist taxon identifier}
#'     \item{name}{character -- scientific name}
#'     \item{common_name}{character -- preferred common name (English)}
#'     \item{rank}{character -- taxonomic rank, e.g. \code{"species"}, \code{"genus"}, \code{"family"}}
#'     \item{observations_count}{integer -- total observations on iNaturalist}
#'     \item{iconic_taxon_name}{character -- broad group, e.g. \code{"Mammalia"}, \code{"Plantae"}, \code{"Arachnida"}}
#'   }
#' @export
#' @examples
#' \dontrun{
#' inat_taxa("wolf", per_page = 5)
#' inat_taxa("Canis lupus")
#' }
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

# == Context ===================================================================

#' Get inaturalist.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
inat_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(inat_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/inaturalist.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "inaturalist.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# inaturalist.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# inaturalist.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
