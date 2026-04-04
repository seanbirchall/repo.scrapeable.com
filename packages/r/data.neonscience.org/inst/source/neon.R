


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

# == Context ===================================================================

#' Generate LLM-friendly context for data.neonscience.org
#'
#' @return Character string with full function signatures and bodies
#' @export
neon_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/data.neonscience.org.R"
  if (!file.exists(src_file)) {
    cat("# data.neonscience.org context - source not found\n")
    return(invisible("# data.neonscience.org context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

