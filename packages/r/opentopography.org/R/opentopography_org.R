# opentopography.org.R - Self-contained opentopography.org client



# opentopography.R
# Self-contained OpenTopography API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for catalog search
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.otopo_base <- "https://portal.opentopography.org/API"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_catalog <- tibble(
  name = character(), short_name = character(),
  dataset_type = character(), platform = character(),
  minx = numeric(), miny = numeric(), maxx = numeric(), maxy = numeric(),
  url = character()
)

# == Catalog search ============================================================

#' Search the OpenTopography dataset catalog by bounding box
#'
#' Queries the OpenTopography \code{otCatalog} endpoint for lidar point-cloud
#' or raster datasets whose spatial extent intersects the given bounding box.
#' Use this to discover high-resolution topographic data for a geographic area.
#'
#' @param minx Minimum longitude (western edge) in decimal degrees
#'   (e.g. \code{-118}).
#' @param miny Minimum latitude (southern edge) in decimal degrees
#'   (e.g. \code{33}).
#' @param maxx Maximum longitude (eastern edge) in decimal degrees
#'   (e.g. \code{-117}).
#' @param maxy Maximum latitude (northern edge) in decimal degrees
#'   (e.g. \code{34}).
#' @param product_format Product format filter: \code{"PointCloud"} (default)
#'   or \code{"Raster"}.
#' @param detail If \code{TRUE} (default), include full metadata for each
#'   dataset; if \code{FALSE}, return only summary records.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Full dataset name (character).}
#'     \item{short_name}{Short / alternate name (character).}
#'     \item{dataset_type}{File format, e.g. \code{"Point Cloud"} (character).}
#'     \item{platform}{Collection platform keywords (character).}
#'     \item{minx, miny, maxx, maxy}{Spatial extent coordinates (numeric).}
#'     \item{url}{URL to the dataset landing page (character).}
#'   }
#' @export
#' @seealso \code{\link{otopo_global_datasets}}, \code{\link{otopo_search}}
#' @examples
#' \dontrun{
#' # Lidar datasets in the Los Angeles area
#' otopo_catalog(-118.5, 33.5, -117.5, 34.5)
#'
#' # Raster datasets in the San Francisco Bay area
#' otopo_catalog(-122.5, 37.5, -122.0, 38.0, product_format = "Raster")
#' }
otopo_catalog <- function(minx, miny, maxx, maxy,
                          product_format = "PointCloud", detail = TRUE) {
  url <- paste0(.otopo_base, "/otCatalog?productFormat=", product_format,
                "&minx=", minx, "&miny=", miny,
                "&maxx=", maxx, "&maxy=", maxy,
                "&detail=", tolower(as.character(detail)),
                "&outputFormat=json")
  raw <- .fetch_json(url)
  d <- raw$Datasets %||% raw$datasets %||% raw
  if (is.null(d) || length(d) == 0) return(.schema_catalog)

  # Each item has a $Dataset sub-element with the actual data
  rows <- lapply(d, function(item) {
    ds <- item$Dataset %||% item
    if (is.null(ds)) return(NULL)

    # Extract spatial coverage
    sc <- ds$spatialCoverage %||% list()
    geo <- sc$geo %||% list()

    tibble(
      name = as.character(ds$name %||% NA_character_),
      short_name = as.character(ds$alternateName %||% NA_character_),
      dataset_type = as.character(ds$fileFormat %||% NA_character_),
      platform = as.character(ds$keywords %||% NA_character_),
      minx = as.numeric(geo$longitude %||% NA_real_),
      miny = as.numeric(geo$latitude %||% NA_real_),
      maxx = as.numeric(geo$eastLon %||% NA_real_),
      maxy = as.numeric(geo$northLat %||% NA_real_),
      url = as.character(ds$url %||% NA_character_)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(.schema_catalog)
  bind_rows(rows)
}

#' List all datasets available on OpenTopography
#'
#' Convenience wrapper around \code{\link{otopo_catalog}} that uses a
#' world-spanning bounding box (\code{-180, -90, 180, 90}) to return
#' every dataset in the catalog. Useful for browsing the full collection
#' without knowing a specific geographic area.
#'
#' @param product_format Product format filter: \code{"PointCloud"} (default)
#'   or \code{"Raster"}.
#' @return A tibble with the same columns as \code{\link{otopo_catalog}}.
#' @export
#' @seealso \code{\link{otopo_catalog}}, \code{\link{otopo_search}}
#' @examples
#' \dontrun{
#' otopo_global_datasets()
#' otopo_global_datasets(product_format = "Raster")
#' }
otopo_global_datasets <- function(product_format = "PointCloud") {
  otopo_catalog(minx = -180, miny = -90, maxx = 180, maxy = 90,
                product_format = product_format, detail = TRUE)
}

#' Search OpenTopography datasets by keyword
#'
#' Fetches the full global catalog via \code{\link{otopo_global_datasets}}
#' and filters locally for datasets whose \code{name} matches the given
#' keyword (case-insensitive regex). Handy for finding datasets by
#' project name, geographic feature, or collection campaign.
#'
#' @param keyword Search term to match against the dataset \code{name}
#'   column (case-insensitive regular expression).
#' @param product_format Product format filter: \code{"PointCloud"} (default)
#'   or \code{"Raster"}.
#' @return A tibble with the same columns as \code{\link{otopo_catalog}}.
#' @export
#' @seealso \code{\link{otopo_catalog}}, \code{\link{otopo_global_datasets}}
#' @examples
#' \dontrun{
#' otopo_search("San Gabriel")
#' otopo_search("volcano", product_format = "Raster")
#' }
otopo_search <- function(keyword, product_format = "PointCloud") {
  all_data <- otopo_global_datasets(product_format = product_format)
  if (nrow(all_data) == 0) return(.schema_catalog)
  all_data[grepl(keyword, all_data$name, ignore.case = TRUE), ]
}

# == Context ===================================================================

#' Get opentopography.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
otopo_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(otopo_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/opentopography.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "opentopography.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# opentopography.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# opentopography.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
