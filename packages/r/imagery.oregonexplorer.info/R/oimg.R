# imagery.oregonexplorer.info.R
# Self-contained Oregon Imagery Explorer client.
# Provides access to Oregon aerial imagery service metadata via ArcGIS REST.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


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
#' available on the Oregon Imagery Explorer ArcGIS server. Each folder
#' typically represents an imagery vintage (e.g. `"NAIP_2020"`,
#' `"OSIP_2022"`) and contains one or more ImageServer services.
#'
#' @return A tibble with one column:
#'   \describe{
#'     \item{folder}{Character. Folder name on the ArcGIS server.}
#'   }
#'
#' @family oregonimagery discovery
#' @seealso [oimg_services()] to list ImageServer services within a folder,
#'   [oimg_search()] for keyword filtering
#'
#' @examples
#' \dontrun{
#' oimg_folders()
#' }
#' @export
oimg_folders <- function() {
  url <- sprintf("%s?f=json", .oimg_base)
  raw <- .fetch_json(url)
  folders <- raw$folders %||% character()
  tibble(folder = as.character(folders))
}

#' List all Oregon imagery services
#'
#' Enumerates all ImageServer services across all (or a specific) folder
#' on the Oregon Imagery Explorer. Each service provides a different
#' imagery product -- typically available in both State Lambert (`_SL`)
#' and Web Mercator (`_WM`) projections.
#'
#' @param folder Character or NULL. Optional folder name to restrict
#'   results (e.g. `"OSIP_2022"`, `"NAIP_2020"`). If `NULL` (default),
#'   services from all folders are returned.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{folder}{Character. Parent folder name.}
#'     \item{service_name}{Character. Full service path (folder/service).}
#'     \item{service_type}{Character. Always `"ImageServer"`.}
#'     \item{url}{Character. Full REST endpoint URL.}
#'   }
#'
#' @family oregonimagery discovery
#' @seealso [oimg_metadata()] to get detailed info for a specific service,
#'   [oimg_search()] for keyword filtering
#'
#' @examples
#' \dontrun{
#' oimg_services()
#' oimg_services("OSIP_2022")
#' }
#' @export
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
#' Fetches detailed technical metadata from an individual ImageServer
#' endpoint, including band count, pixel type and size, spatial extent
#' in the service's native coordinate system, and copyright information.
#'
#' @param service_name Character. Full service path as returned by
#'   [oimg_services()], e.g. `"OSIP_2022/OSIP_2022_WM"` or
#'   `"NAIP_2020/NAIP_2020_SL"`.
#'
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{name}{Character. Service name.}
#'     \item{description}{Character. Service description text.}
#'     \item{band_count}{Integer. Number of spectral bands.}
#'     \item{pixel_type}{Character. Pixel data type (e.g. `"U8"`).}
#'     \item{pixel_size_x}{Numeric. Pixel width in map units.}
#'     \item{pixel_size_y}{Numeric. Pixel height in map units.}
#'     \item{format}{Character. Dataset format (e.g. `"AMD"`).}
#'     \item{extent_xmin, extent_ymin, extent_xmax, extent_ymax}{Numeric.
#'       Bounding box in the service's spatial reference.}
#'     \item{spatial_ref_wkid}{Integer. Well-known ID of the coordinate system.}
#'     \item{copyright}{Character. Copyright text.}
#'   }
#'
#' @family oregonimagery data
#' @seealso [oimg_services()] to find valid service names
#'
#' @examples
#' \dontrun{
#' oimg_metadata("OSIP_2022/OSIP_2022_WM")
#' oimg_metadata("NAIP_2020/NAIP_2020_SL")
#' }
#' @export
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
#' Filters the full list of ImageServer services by a case-insensitive
#' keyword match against folder and service names. Useful for quickly
#' finding a specific imagery vintage or product type.
#'
#' @param query Character. Search term (e.g. `"NAIP"`, `"2022"`,
#'   `"OSIP"`). Matched against both folder and service_name columns.
#'
#' @return A tibble with the same columns as [oimg_services()]:
#'   folder, service_name, service_type, url. Only matching rows are
#'   returned.
#'
#' @family oregonimagery discovery
#' @seealso [oimg_services()] for the unfiltered list,
#'   [oimg_metadata()] to inspect a matched service
#'
#' @examples
#' \dontrun{
#' oimg_search("NAIP")
#' oimg_search("2022")
#' }
#' @export
oimg_search <- function(query) {
  oimg_services() |>
    filter(grepl(query, service_name, ignore.case = TRUE) |
           grepl(query, folder, ignore.case = TRUE))
}

#' Get imagery.oregonexplorer.info client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
oimg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(oimg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/imagery.oregonexplorer.info.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "imagery.oregonexplorer.info")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# imagery.oregonexplorer.info context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# imagery.oregonexplorer.info", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
