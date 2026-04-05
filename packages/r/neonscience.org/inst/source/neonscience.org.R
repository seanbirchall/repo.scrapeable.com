# data.neonscience.org.R - Self-contained data.neonscience.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# neon.R
# Self-contained NEON (National Ecological Observatory Network) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.neon_base <- "https://data.neonscience.org/api/v0"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_products <- tibble(
  productCode = character(), productName = character(),
  productDescription = character(), productScienceTeam = character(),
  productStatus = character()
)

.schema_product_detail <- tibble(
  productCode = character(), productName = character(),
  productDescription = character(), productScienceTeam = character(),
  productStatus = character(), siteCodes = character()
)

.schema_sites <- tibble(
  siteCode = character(), siteDescription = character(),
  siteType = character(), stateName = character(),
  domainCode = character(), siteLatitude = numeric(),
  siteLongitude = numeric()
)

# == Products ==================================================================

#' List all NEON data products
#'
#' Retrieve the complete catalog of data products available from the
#' National Ecological Observatory Network (NEON).
#'
#' @details
#' NEON operates 81 field sites across the United States and collects
#' ecological data spanning terrestrial and aquatic domains. Each data
#' product is identified by a code like \code{"DP1.10003.001"} and
#' belongs to a science team (e.g. Terrestrial Instruments, Biogeochemistry).
#' Use the returned \code{productCode} with \code{\link{neon_product}} for
#' detailed metadata and available site codes.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{productCode}{Character. Unique NEON product identifier.}
#'   \item{productName}{Character. Human-readable product name.}
#'   \item{productDescription}{Character. Full description of the product.}
#'   \item{productScienceTeam}{Character. Responsible science team.}
#'   \item{productStatus}{Character. \code{"ACTIVE"} or \code{"FUTURE"}.}
#' }
#' @export
#' @seealso \code{\link{neon_product}}, \code{\link{neon_sites}}
#' @examples
#' prods <- neon_products()
#' prods[grep("bird", prods$productName, ignore.case = TRUE), ]
neon_products <- function() {
  url <- paste0(.neon_base, "/products")
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_products)

  as_tibble(d) |>
    transmute(
      productCode = as.character(productCode),
      productName = as.character(productName),
      productDescription = as.character(if ("productDescription" %in% names(d)) productDescription else NA_character_),
      productScienceTeam = as.character(if ("productScienceTeam" %in% names(d)) productScienceTeam else NA_character_),
      productStatus = as.character(if ("productStatus" %in% names(d)) productStatus else NA_character_)
    )
}

# == Product detail ============================================================

#' Fetch details for a specific NEON data product
#'
#' Retrieve full metadata for a single NEON data product, including the list
#' of field sites where data are available.
#'
#' @details
#' The \code{siteCodes} column contains a semicolon-separated list of all NEON
#' site codes that have collected data for this product. Cross-reference with
#' \code{\link{neon_sites}} or \code{\link{neon_site}} for site metadata.
#'
#' @param code Character. NEON product code, e.g. \code{"DP1.10003.001"}
#'   (Breeding landbird point counts).
#' @return A single-row tibble with columns:
#' \describe{
#'   \item{productCode}{Character. Product identifier.}
#'   \item{productName}{Character. Product name.}
#'   \item{productDescription}{Character. Full description.}
#'   \item{productScienceTeam}{Character. Responsible science team.}
#'   \item{productStatus}{Character. \code{"ACTIVE"} or \code{"FUTURE"}.}
#'   \item{siteCodes}{Character. Semicolon-separated site codes.}
#' }
#' @export
#' @seealso \code{\link{neon_products}}, \code{\link{neon_site}}
#' @examples
#' neon_product("DP1.10003.001")
neon_product <- function(code) {
  url <- paste0(.neon_base, "/products/", code)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || is.null(d$productCode)) return(.schema_product_detail)

  site_str <- if (!is.null(d$siteCodes) && is.data.frame(d$siteCodes)) {
    paste(d$siteCodes$siteCode, collapse = "; ")
  } else if (!is.null(d$siteCodes) && is.character(d$siteCodes)) {
    paste(d$siteCodes, collapse = "; ")
  } else NA_character_

  tibble(
    productCode = as.character(d$productCode),
    productName = as.character(d$productName %||% NA_character_),
    productDescription = as.character(d$productDescription %||% NA_character_),
    productScienceTeam = as.character(d$productScienceTeam %||% NA_character_),
    productStatus = as.character(d$productStatus %||% NA_character_),
    siteCodes = site_str
  )
}

# == Sites =====================================================================

#' List all NEON field sites
#'
#' Retrieve the full list of NEON field sites with geographic coordinates
#' and domain information.
#'
#' @details
#' NEON sites fall into two categories: \code{"CORE"} sites are selected
#' to represent major ecosystems for 30-year monitoring, while
#' \code{"GRADIENT"} (previously "relocatable") sites capture local
#' ecological gradients and may move. The \code{domainCode} groups sites
#' into 20 eco-climatic domains (D01--D20).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{siteCode}{Character. Four-letter NEON site code (e.g. \code{"HARV"}).}
#'   \item{siteDescription}{Character. Full site name and description.}
#'   \item{siteType}{Character. \code{"CORE"} or \code{"GRADIENT"}.}
#'   \item{stateName}{Character. US state or territory.}
#'   \item{domainCode}{Character. Eco-climatic domain (e.g. \code{"D01"}).}
#'   \item{siteLatitude}{Numeric. Latitude in decimal degrees.}
#'   \item{siteLongitude}{Numeric. Longitude in decimal degrees.}
#' }
#' @export
#' @seealso \code{\link{neon_site}}, \code{\link{neon_products}}
#' @examples
#' sites <- neon_sites()
#' sites[sites$stateName == "Colorado", ]
neon_sites <- function() {
  url <- paste0(.neon_base, "/sites")
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_sites)

  as_tibble(d) |>
    transmute(
      siteCode = as.character(siteCode),
      siteDescription = as.character(if ("siteDescription" %in% names(d)) siteDescription else NA_character_),
      siteType = as.character(if ("siteType" %in% names(d)) siteType else NA_character_),
      stateName = as.character(if ("stateName" %in% names(d)) stateName else NA_character_),
      domainCode = as.character(if ("domainCode" %in% names(d)) domainCode else NA_character_),
      siteLatitude = as.numeric(if ("siteLatitude" %in% names(d)) siteLatitude else NA_real_),
      siteLongitude = as.numeric(if ("siteLongitude" %in% names(d)) siteLongitude else NA_real_)
    )
}

# == Site detail ================================================================

#' Get detailed information for a single NEON site
#'
#' Fetch full metadata for one NEON field site, including which data
#' products have been collected there.
#'
#' @details
#' The \code{data_products} column lists all NEON data product codes
#' available at this site, separated by semicolons. Use these codes
#' with \code{\link{neon_product}} for product details.
#'
#' @param site_code Character. Four-letter NEON site code, e.g.
#'   \code{"HARV"} (Harvard Forest), \code{"BART"} (Bartlett
#'   Experimental Forest).
#' @return A single-row tibble with columns:
#' \describe{
#'   \item{siteCode}{Character. Site code.}
#'   \item{siteDescription}{Character. Full site name.}
#'   \item{siteType}{Character. \code{"CORE"} or \code{"GRADIENT"}.}
#'   \item{stateName}{Character. US state.}
#'   \item{domainCode}{Character. Eco-climatic domain.}
#'   \item{siteLatitude}{Numeric. Latitude.}
#'   \item{siteLongitude}{Numeric. Longitude.}
#'   \item{data_products}{Character. Semicolon-separated product codes.}
#' }
#' @export
#' @seealso \code{\link{neon_sites}}, \code{\link{neon_products}}
#' @examples
#' neon_site("HARV")
neon_site <- function(site_code) {
  schema <- tibble(siteCode = character(), siteDescription = character(),
                   siteType = character(), stateName = character(),
                   domainCode = character(), siteLatitude = numeric(),
                   siteLongitude = numeric(), data_products = character())
  url <- paste0(.neon_base, "/sites/", site_code)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)
  d <- raw$data
  if (is.null(d)) return(schema)

  dp_str <- if (!is.null(d$dataProducts) && is.data.frame(d$dataProducts)) {
    paste(d$dataProducts$dataProductCode, collapse = "; ")
  } else NA_character_

  tibble(
    siteCode = as.character(d$siteCode %||% NA_character_),
    siteDescription = as.character(d$siteDescription %||% NA_character_),
    siteType = as.character(d$siteType %||% NA_character_),
    stateName = as.character(d$stateName %||% NA_character_),
    domainCode = as.character(d$domainCode %||% NA_character_),
    siteLatitude = as.numeric(d$siteLatitude %||% NA_real_),
    siteLongitude = as.numeric(d$siteLongitude %||% NA_real_),
    data_products = dp_str
  )
}

#' List NEON taxonomy entries
#'
#' Query the NEON taxonomy endpoint for species observed across the
#' observatory network, filtered by taxon type.
#'
#' @details
#' NEON maintains curated taxonomic lists for organisms sampled by its
#' protocols. Supported taxon type codes include \code{"BIRD"},
#' \code{"FISH"}, \code{"PLANT"}, \code{"BEETLE"}, \code{"MOSQUITO"},
#' \code{"SMALL_MAMMAL"}, \code{"MACROINVERTEBRATE"}, and \code{"TICK"}.
#' Results are paginated server-side; increase \code{limit} to retrieve
#' more entries.
#'
#' @param taxon_type_code Character. Taxon group code (default
#'   \code{"BIRD"}). See Details for supported values.
#' @param limit Integer. Maximum entries to return (default 20).
#' @return A tibble with columns:
#' \describe{
#'   \item{taxonTypeCode}{Character. Taxon group (e.g. \code{"BIRD"}).}
#'   \item{taxonID}{Character. NEON taxon identifier.}
#'   \item{scientificName}{Character. Binomial scientific name.}
#'   \item{commonName}{Character. Common English name.}
#'   \item{family}{Character. Taxonomic family.}
#' }
#' @export
#' @seealso \code{\link{neon_sites}}, \code{\link{neon_products}}
#' @examples
#' neon_taxonomy("BIRD", limit = 10)
#' neon_taxonomy("FISH", limit = 5)
neon_taxonomy <- function(taxon_type_code = "BIRD", limit = 20) {
  schema <- tibble(taxonTypeCode = character(), taxonID = character(),
                   scientificName = character(), commonName = character(),
                   family = character())
  url <- sprintf("%s/taxonomy?taxonTypeCode=%s&offset=0&limit=%d",
                 .neon_base, utils::URLencode(taxon_type_code), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(schema)
  if (!is.data.frame(d)) return(schema)

  as_tibble(d) |>
    transmute(
      taxonTypeCode = as.character(if ("taxonTypeCode" %in% names(d)) taxonTypeCode else NA_character_),
      taxonID = as.character(if ("taxonID" %in% names(d)) taxonID else NA_character_),
      scientificName = as.character(if ("scientificName" %in% names(d)) scientificName else NA_character_),
      commonName = as.character(if ("commonName" %in% names(d)) commonName else NA_character_),
      family = as.character(if ("family" %in% names(d)) family else NA_character_)
    )
}

# == Context ===================================================================

#' Get neonscience.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
neon_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(neon_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/neonscience.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "neonscience.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# neonscience.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# neonscience.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
