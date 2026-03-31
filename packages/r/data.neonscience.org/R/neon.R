# == Products ==================================================================

#' List all NEON data products
#'
#' @return tibble: productCode, productName, productDescription,
#'   productScienceTeam, productStatus
#' @export
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
#' @export
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
#' @export
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
#' @export
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
