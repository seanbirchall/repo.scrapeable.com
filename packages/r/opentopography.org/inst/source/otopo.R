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

.schema_catalog <- tibble(
  name = character(), short_name = character(),
  dataset_type = character(), platform = character(),
  minx = numeric(), miny = numeric(), maxx = numeric(), maxy = numeric(),
  url = character()
)

# == Catalog search ============================================================

#' Search OpenTopography dataset catalog by bounding box
#'
#' @param minx Minimum longitude (e.g. -118)
#' @param miny Minimum latitude (e.g. 33)
#' @param maxx Maximum longitude (e.g. -117)
#' @param maxy Maximum latitude (e.g. 34)
#' @param product_format Product format filter: "PointCloud" or "Raster"
#'   (default "PointCloud")
#' @param detail Include detailed metadata (default TRUE)
#' @return tibble: name, short_name, dataset_type, platform, minx, miny,
#'   maxx, maxy, url
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

#' Generate LLM-friendly context for the OpenTopography package
#'
#' @return Character string (invisibly), also printed
otopo_context <- function() {
  .build_context("opentopography.org", header_lines = c(
    "# opentopography.org - OpenTopography LiDAR Catalog API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for catalog search",
    "# All functions return tibbles with typed columns.",
    "#",
    "# OpenTopography provides access to high-resolution topographic data.",
    "# Search by bounding box (longitude/latitude).",
    "# Product formats: PointCloud, Raster"
  ))
}
