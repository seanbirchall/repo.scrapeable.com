# neon.R
# Self-contained NEON (National Ecological Observatory Network) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.neon_base <- "https://data.neonscience.org/api/v0"

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
    j <- fi - 1; rox_start <- fi
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
#' @return tibble: productCode, productName, productDescription,
#'   productScienceTeam, productStatus
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
#' @param code NEON product code (e.g. "DP1.10003.001")
#' @return tibble: one row with productCode, productName, productDescription,
#'   productScienceTeam, productStatus, siteCodes
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
#' @return tibble: siteCode, siteDescription, siteType, stateName,
#'   domainCode, siteLatitude, siteLongitude
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the NEON package
#'
#' @return Character string (invisibly), also printed
neon_context <- function() {
  .build_context("data.neonscience.org", header_lines = c(
    "# data.neonscience.org - NEON Ecological Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# NEON: National Ecological Observatory Network.",
    "# Products identified by codes like DP1.10003.001.",
    "# 81 field sites across the US."
  ))
}
