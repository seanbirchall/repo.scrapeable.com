# opentopography.R
# Self-contained OpenTopography API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for catalog search
# Rate limits: not documented, be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.otopo_base <- "https://portal.opentopography.org/API"

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

.schema_catalog <- tibble(
  name = character(), short_name = character(),
  dataset_type = character(), platform = character(),
  minx = numeric(), miny = numeric(), maxx = numeric(), maxy = numeric(),
  url = character()
)

# == Catalog search ============================================================

#' Search OpenTopography dataset catalog by bounding box
#'
#' Query the OpenTopography catalog for high-resolution topographic datasets
#' (LiDAR point clouds, DEMs, etc.) that intersect a given geographic bounding
#' box. Returns metadata about each dataset including spatial extent and a
#' download URL.
#'
#' @param minx Minimum longitude in decimal degrees (e.g. \code{-118}).
#' @param miny Minimum latitude in decimal degrees (e.g. \code{33}).
#' @param maxx Maximum longitude in decimal degrees (e.g. \code{-117}).
#' @param maxy Maximum latitude in decimal degrees (e.g. \code{34}).
#' @param product_format Product format filter: \code{"PointCloud"} or
#'   \code{"Raster"} (default \code{"PointCloud"}).
#' @param detail Whether to include detailed metadata (default \code{TRUE}).
#' @return A tibble with one row per dataset:
#'   \describe{
#'     \item{name}{\code{character} -- Full dataset name.}
#'     \item{short_name}{\code{character} -- Short identifier.}
#'     \item{dataset_type}{\code{character} -- Type (e.g. \code{"Lidar"}).}
#'     \item{platform}{\code{character} -- Collection platform (e.g. \code{"Airborne"}).}
#'     \item{minx}{\code{numeric} -- Western extent (longitude).}
#'     \item{miny}{\code{numeric} -- Southern extent (latitude).}
#'     \item{maxx}{\code{numeric} -- Eastern extent (longitude).}
#'     \item{maxy}{\code{numeric} -- Northern extent (latitude).}
#'     \item{url}{\code{character} -- Dataset URL on OpenTopography.}
#'   }
#' @examples
#' \dontrun{
#' # LiDAR datasets in the LA area
#' otopo_catalog(minx = -118.5, miny = 33.7, maxx = -117.5, maxy = 34.3)
#'
#' # Raster datasets
#' otopo_catalog(minx = -118, miny = 33, maxx = -117, maxy = 34,
#'               product_format = "Raster")
#' }
#' @export
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

  if (is.data.frame(d)) {
    as_tibble(d) |>
      transmute(
        name = as.character(if ("Dataset.Name" %in% names(d)) `Dataset.Name`
                            else if ("name" %in% names(d)) name else NA_character_),
        short_name = as.character(if ("Dataset.Short.Name" %in% names(d)) `Dataset.Short.Name`
                                  else if ("short_name" %in% names(d)) short_name else NA_character_),
        dataset_type = as.character(if ("Dataset.Type" %in% names(d)) `Dataset.Type`
                                    else if ("dataset_type" %in% names(d)) dataset_type else NA_character_),
        platform = as.character(if ("Platform" %in% names(d)) Platform
                                else if ("platform" %in% names(d)) platform else NA_character_),
        minx = as.numeric(if ("Dataset.Extent.W" %in% names(d)) `Dataset.Extent.W`
                          else if ("minx" %in% names(d)) minx else NA_real_),
        miny = as.numeric(if ("Dataset.Extent.S" %in% names(d)) `Dataset.Extent.S`
                          else if ("miny" %in% names(d)) miny else NA_real_),
        maxx = as.numeric(if ("Dataset.Extent.E" %in% names(d)) `Dataset.Extent.E`
                          else if ("maxx" %in% names(d)) maxx else NA_real_),
        maxy = as.numeric(if ("Dataset.Extent.N" %in% names(d)) `Dataset.Extent.N`
                          else if ("maxy" %in% names(d)) maxy else NA_real_),
        url = as.character(if ("Dataset.URL" %in% names(d)) `Dataset.URL`
                           else if ("url" %in% names(d)) url else NA_character_)
      )
  } else if (is.list(d)) {
    # Handle list-of-lists
    rows <- lapply(d, function(item) {
      tibble(
        name = as.character(item$`Dataset Name` %||% item$name %||% NA_character_),
        short_name = as.character(item$`Dataset Short Name` %||% item$short_name %||% NA_character_),
        dataset_type = as.character(item$`Dataset Type` %||% item$dataset_type %||% NA_character_),
        platform = as.character(item$Platform %||% item$platform %||% NA_character_),
        minx = as.numeric(item$`Dataset Extent W` %||% item$minx %||% NA_real_),
        miny = as.numeric(item$`Dataset Extent S` %||% item$miny %||% NA_real_),
        maxx = as.numeric(item$`Dataset Extent E` %||% item$maxx %||% NA_real_),
        maxy = as.numeric(item$`Dataset Extent N` %||% item$maxy %||% NA_real_),
        url = as.character(item$`Dataset URL` %||% item$url %||% NA_character_)
      )
    })
    bind_rows(rows)
  } else {
    .schema_catalog
  }
}

# == Context (LLM injection) ==================================================

#' Get OpenTopography client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' otopo_context()
#' }
#' @export
otopo_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(otopo_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/opentopography.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "opentopography")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# opentopography context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# opentopography", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
