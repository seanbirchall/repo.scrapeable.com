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

#' Generate LLM-friendly context for the OpenTopography package
#'
#' @return Character string (invisibly), also printed
#' @export
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
