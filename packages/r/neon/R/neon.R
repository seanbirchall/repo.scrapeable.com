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
#' Retrieve the complete catalog of data products available from the National
#' Ecological Observatory Network (NEON). Each row represents a distinct
#' ecological measurement type (e.g., air temperature, soil moisture,
#' bird counts). Use the \code{productCode} with \code{neon_product()} for
#' details and associated sites.
#'
#' @return A tibble with one row per data product:
#'   \describe{
#'     \item{productCode}{\code{character} -- NEON product code (e.g. \code{"DP1.10003.001"}).}
#'     \item{productName}{\code{character} -- Short product name.}
#'     \item{productDescription}{\code{character} -- Full description.}
#'     \item{productScienceTeam}{\code{character} -- Responsible science team.}
#'     \item{productStatus}{\code{character} -- Status (e.g. \code{"ACTIVE"}).}
#'   }
#' @examples
#' \dontrun{
#' neon_products()
#' }
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
#' Retrieve detailed metadata for a single NEON data product, including the
#' list of field sites where data has been collected.
#'
#' @param code NEON product code (e.g. \code{"DP1.10003.001"}). Obtain from
#'   the \code{productCode} column of \code{neon_products()}.
#' @return A single-row tibble:
#'   \describe{
#'     \item{productCode}{\code{character} -- NEON product code.}
#'     \item{productName}{\code{character} -- Short product name.}
#'     \item{productDescription}{\code{character} -- Full description.}
#'     \item{productScienceTeam}{\code{character} -- Responsible science team.}
#'     \item{productStatus}{\code{character} -- Status.}
#'     \item{siteCodes}{\code{character} -- Site codes where data exists, semicolon-separated.}
#'   }
#' @examples
#' \dontrun{
#' neon_product("DP1.10003.001")
#' }
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
#' Retrieve metadata for all NEON field sites across the United States,
#' including geographic coordinates, state, and domain classification. Sites
#' are classified as CORE (long-term) or GRADIENT (relocatable).
#'
#' @return A tibble with one row per field site:
#'   \describe{
#'     \item{siteCode}{\code{character} -- Four-letter site code (e.g. \code{"ABBY"}).}
#'     \item{siteDescription}{\code{character} -- Full site name.}
#'     \item{siteType}{\code{character} -- Site type (\code{"CORE"} or \code{"GRADIENT"}).}
#'     \item{stateName}{\code{character} -- U.S. state.}
#'     \item{domainCode}{\code{character} -- NEON domain code (e.g. \code{"D16"}).}
#'     \item{siteLatitude}{\code{numeric} -- Latitude in decimal degrees.}
#'     \item{siteLongitude}{\code{numeric} -- Longitude in decimal degrees.}
#'   }
#' @examples
#' \dontrun{
#' neon_sites()
#' }
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

#' Get NEON client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' neon_context()
#' }
#' @export
neon_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(neon_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/neon.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "neon")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# neon context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# neon", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
