# gisservicemt.gov.R - Self-contained Montana GIS (MSDI) ArcGIS client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public ArcGIS services)
# Rate limits: be polite

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.mtgis_base <- "https://gisservicemt.gov/arcgis/rest/services"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_service <- tibble(
  name = character(), type = character()
)

.schema_layer <- tibble(
  id = integer(), name = character(), type = character(),
  geometryType = character(), minScale = numeric(), maxScale = numeric()
)

.schema_feature <- tibble(
  OBJECTID = integer()
)

# == Services ==================================================================

#' List available Montana GIS map services
#'
#' Browse the Montana State Library GIS (MSDI) ArcGIS REST services catalog.
#' Returns all map services and subfolders at the given path. Use subfolders
#' like \code{"MSDI_Framework"} to drill into thematic groups (boundaries,
#' cadastral, transportation, etc.).
#'
#' @param folder Character or \code{NULL}. Optional folder name to browse
#'   (e.g. \code{"MSDI_Framework"}, \code{"DLI"}, \code{"MSL"}). \code{NULL}
#'   (default) returns the root catalog.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Service or folder name.}
#'     \item{type}{Character. Either \code{"MapServer"}, \code{"FeatureServer"},
#'       or \code{"Folder"}.}
#'   }
#' @export
#' @family mtgis functions
#' @seealso [mtgis_layers()] to list layers within a service
#' @examples
#' \dontrun{
#' # Root catalog
#' mtgis_services()
#'
#' # Browse MSDI Framework folder
#' mtgis_services("MSDI_Framework")
#' }
mtgis_services <- function(folder = NULL) {
  path <- if (is.null(folder)) "" else paste0("/", folder)
  url <- paste0(.mtgis_base, path, "?f=json")
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res)) return(.schema_service)

  services <- res$services
  folders <- res$folders
  out <- .schema_service
  if (!is.null(services) && nrow(services) > 0) {
    out <- tibble(
      name = as.character(services$name),
      type = as.character(services$type)
    )
  }
  if (!is.null(folders) && length(folders) > 0) {
    folder_rows <- tibble(name = as.character(folders), type = rep("Folder", length(folders)))
    out <- bind_rows(out, folder_rows)
  }
  out
}

# == Layers ====================================================================

#' List layers in a Montana GIS map service
#'
#' Returns all feature layers in a given Montana GIS MapServer service.
#' Use the layer \code{id} with \code{\link{mtgis_query}} to fetch
#' actual feature data from a specific layer.
#'
#' @param service Character. Service path relative to the services root
#'   (e.g. \code{"MSDI_Framework/Boundaries"}, \code{"MSL/Cadastral"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Layer ID, used with \code{\link{mtgis_query}}.}
#'     \item{name}{Character. Layer display name.}
#'     \item{type}{Character. Layer type (e.g. "Feature Layer").}
#'     \item{geometryType}{Character. Geometry type (e.g. "esriGeometryPolygon").}
#'     \item{minScale}{Numeric. Minimum display scale (0 = no limit).}
#'     \item{maxScale}{Numeric. Maximum display scale (0 = no limit).}
#'   }
#' @export
#' @family mtgis functions
#' @seealso [mtgis_services()] to discover service paths,
#'   [mtgis_query()] to fetch layer features
#' @examples
#' \dontrun{
#' mtgis_layers("MSDI_Framework/Boundaries")
#' }
mtgis_layers <- function(service) {
  url <- paste0(.mtgis_base, "/", service, "/MapServer?f=json")
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$layers)) return(.schema_layer)

  d <- res$layers
  tibble(
    id = as.integer(.safe_col(d, "id", NA_integer_)),
    name = as.character(.safe_col(d, "name")),
    type = as.character(.safe_col(d, "type")),
    geometryType = as.character(.safe_col(d, "geometryType")),
    minScale = as.numeric(.safe_col(d, "minScale", NA_real_)),
    maxScale = as.numeric(.safe_col(d, "maxScale", NA_real_))
  )
}

# == Query Features ============================================================

#' Query features from a Montana GIS map layer
#'
#' Execute a SQL-style query against a specific layer in a Montana GIS
#' MapServer service. Returns feature attributes as a tibble (geometry
#' is not included). Use \code{\link{mtgis_layers}} to discover available
#' layer IDs and field names.
#'
#' @param service Character. Service path (e.g. \code{"MSDI_Framework/Boundaries"}).
#' @param layer_id Integer. Layer ID number from \code{\link{mtgis_layers}}.
#' @param where Character. SQL WHERE clause for filtering features.
#'   Default \code{"1=1"} returns all features.
#' @param out_fields Character. Comma-separated field names to return, or
#'   \code{"*"} (default) for all fields.
#' @param max_records Integer. Maximum number of features to return
#'   (default 100, server maximum 1000).
#' @return A tibble of feature attributes. Columns depend on the layer
#'   schema. Always includes \code{OBJECTID}.
#' @export
#' @family mtgis functions
#' @seealso [mtgis_layers()] to discover layer IDs,
#'   [mtgis_counties()] as a convenience wrapper
#' @examples
#' \dontrun{
#' # All features from layer 8 (counties)
#' mtgis_query("MSDI_Framework/Boundaries", layer_id = 8, max_records = 10)
#'
#' # Filter by county name
#' mtgis_query("MSDI_Framework/Boundaries", layer_id = 8,
#'             where = "NAME = 'YELLOWSTONE'")
#' }
mtgis_query <- function(service, layer_id, where = "1=1",
                        out_fields = "*", max_records = 100) {
  url <- sprintf(
    "%s/%s/MapServer/%d/query?where=%s&outFields=%s&f=json&resultRecordCount=%d&returnGeometry=false",
    .mtgis_base, service, layer_id,
    utils::URLencode(where, reserved = TRUE),
    utils::URLencode(out_fields, reserved = TRUE),
    min(max_records, 1000)
  )
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$features) || length(res$features) == 0)
    return(.schema_feature)

  attrs <- res$features$attributes
  if (is.data.frame(attrs)) as_tibble(attrs)
  else .schema_feature
}

# == Boundaries (convenience) ==================================================

#' Get Montana county boundaries (attributes only)
#'
#' Convenience wrapper around \code{\link{mtgis_query}} that fetches county
#' boundary attributes from the MSDI Boundaries service (layer 8). Returns
#' county names, FIPS codes, areas, and other administrative information
#' for Montana's 56 counties. Geometry is not included.
#'
#' @param max_records Integer. Maximum number of county records to return
#'   (default 100). Set to 56 or higher to get all Montana counties.
#' @return A tibble with county attributes including: OBJECTID, NAME, FIPS,
#'   Acres, SQMILES, CountyCode, and additional administrative fields.
#' @export
#' @family mtgis functions
#' @seealso [mtgis_query()] for general feature queries
#' @examples
#' \dontrun{
#' # All Montana counties
#' mtgis_counties(max_records = 60)
#' }
mtgis_counties <- function(max_records = 100) {
  # County boundaries are layer 8 in the Boundaries service
  mtgis_query("MSDI_Framework/Boundaries", layer_id = 8,
              max_records = max_records)
}

# == Context ===================================================================

#' Get gisservicemt.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mtgis_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mtgis_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gisservicemt.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gisservicemt.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gisservicemt.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gisservicemt.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
