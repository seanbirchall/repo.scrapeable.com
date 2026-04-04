# imagery.oregonexplorer.info.R
# Self-contained Oregon Imagery Explorer client.
# Provides access to Oregon aerial imagery service metadata via ArcGIS REST.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.oimg_base <- "https://imagery.oregonexplorer.info/arcgis/rest/services"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_services <- tibble(
  folder = character(),
  service_name = character(),
  service_type = character(),
  url = character()
)

.schema_metadata <- tibble(
  name = character(),
  description = character(),
  band_count = integer(),
  pixel_type = character(),
  pixel_size_x = numeric(),
  pixel_size_y = numeric(),
  format = character(),
  extent_xmin = numeric(),
  extent_ymin = numeric(),
  extent_xmax = numeric(),
  extent_ymax = numeric(),
  spatial_ref_wkid = integer(),
  copyright = character()
)

# == Public functions ==========================================================

#' List all Oregon imagery service folders
#'
#' Returns the top-level folders (NAIP years, OSIP years, etc.)
#' available on the Oregon imagery ArcGIS server.
#'
#' @return tibble: folder (character)
oimg_folders <- function() {
  url <- sprintf("%s?f=json", .oimg_base)
  raw <- .fetch_json(url)
  folders <- raw$folders %||% character()
  tibble(folder = as.character(folders))
}

#' List all imagery services
#'
#' Enumerates all ImageServer services across all folders.
#'
#' @param folder Optional folder name to filter (e.g., "OSIP_2022").
#'   If NULL (default), lists services from all folders.
#' @return tibble: folder, service_name, service_type, url
oimg_services <- function(folder = NULL) {
  if (is.null(folder)) {
    folders <- oimg_folders()$folder
  } else {
    folders <- folder
  }

  all_rows <- lapply(folders, function(f) {
    url <- sprintf("%s/%s?f=json", .oimg_base, f)
    tryCatch({
      raw <- .fetch_json(url)
      svcs <- raw$services
      if (is.null(svcs) || nrow(svcs) == 0) return(NULL)
      tibble(
        folder = f,
        service_name = as.character(svcs$name),
        service_type = as.character(svcs$type),
        url = sprintf("%s/%s/ImageServer", .oimg_base,
                      gsub(" ", "%20", svcs$name))
      )
    }, error = function(e) NULL)
  })
  result <- bind_rows(all_rows)
  if (nrow(result) == 0) return(.schema_services)
  result
}

#' Get metadata for a specific imagery service
#'
#' Returns detailed metadata (band count, pixel type, extent, etc.)
#' for a named imagery service.
#'
#' @param service_name Full service name (e.g., "OSIP_2022/OSIP_2022_WM")
#' @return tibble (1 row): name, description, band_count, pixel_type,
#'   pixel_size_x, pixel_size_y, format, extent_xmin, extent_ymin,
#'   extent_xmax, extent_ymax, spatial_ref_wkid, copyright
oimg_metadata <- function(service_name) {
  url <- sprintf("%s/%s/ImageServer?f=json", .oimg_base, service_name)
  raw <- .fetch_json(url)

  ext <- raw$extent %||% list()
  sr <- ext$spatialReference %||% list()


  tibble(
    name = as.character(raw$name %||% NA),
    description = as.character(raw$description %||% NA),
    band_count = as.integer(raw$bandCount %||% NA),
    pixel_type = as.character(raw$pixelType %||% NA),
    pixel_size_x = as.numeric(raw$pixelSizeX %||% NA),
    pixel_size_y = as.numeric(raw$pixelSizeY %||% NA),
    format = as.character(raw$datasetFormat %||% NA),
    extent_xmin = as.numeric(ext$xmin %||% NA),
    extent_ymin = as.numeric(ext$ymin %||% NA),
    extent_xmax = as.numeric(ext$xmax %||% NA),
    extent_ymax = as.numeric(ext$ymax %||% NA),
    spatial_ref_wkid = as.integer(sr$wkid %||% NA),
    copyright = as.character(raw$copyrightText %||% NA)
  )
}

#' Search Oregon imagery services by keyword
#'
#' @param query Search term matched against folder and service names
#' @return tibble: folder, service_name, service_type, url
oimg_search <- function(query) {
  oimg_services() |>
    filter(grepl(query, service_name, ignore.case = TRUE) |
           grepl(query, folder, ignore.case = TRUE))
}

#' Show context for the imagery.oregonexplorer.info client
#'
#' @return Invisible string of context
oimg_context <- function() {
  src_dir <- system.file("source", package = "imagery.oregonexplorer.info")
  if (src_dir == "") {
    this_file <- sys.frame(1)$ofile %||%
      attr(body(oimg_folders), "srcfile")$filename %||%
      ""
    if (this_file != "" && file.exists(this_file)) {
      lines <- readLines(this_file, warn = FALSE)
    } else {
      cat("# imagery.oregonexplorer.info - Oregon Imagery Explorer client\n")
      cat("# Source not found. Use ?oimg_services for help.\n")
      return(invisible(""))
    }
  } else {
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) {
      cat("# No R source found.\n")
      return(invisible(""))
    }
    lines <- readLines(src_files[1], warn = FALSE)
  }

  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]
    k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) {
      k <- k + 1
      sig <- paste(sig, trimws(lines[k]))
    }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c(
    "# imagery.oregonexplorer.info - Oregon Imagery Explorer",
    "# ArcGIS ImageServer metadata for Oregon aerial imagery",
    "#",
    "# == Functions ==",
    "#",
    unlist(blocks)
  ), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
