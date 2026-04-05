# lakecountyil.gov.R - Lake County IL open data client (ArcGIS REST)
#
# Data source: maps.lakecountyil.gov (ArcGIS REST services)
# Also: data-lakecountyil.opendata.arcgis.com (ArcGIS Hub)
# Datasets: ~1217 (parcels, zoning, health, public works, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.lc_arcgis <- "https://maps.lakecountyil.gov/arcgis/rest/services"
.lc_hub <- "https://data-lakecountyil.opendata.arcgis.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.arcgis_query <- function(service_url, where = "1=1", out_fields = "*",
                          limit = 1000, offset = 0) {
  url <- sprintf("%s/query?where=%s&outFields=%s&f=json&resultRecordCount=%d&resultOffset=%d&returnGeometry=false",
                 service_url,
                 utils::URLencode(where),
                 utils::URLencode(out_fields),
                 limit, offset)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$features) || length(raw$features) == 0) return(tibble())
  attrs <- raw$features$attributes
  if (is.data.frame(attrs)) as_tibble(attrs) else tibble()
}

# == Known services ============================================================

.lc_services <- list(
  health    = "Health/HealthMapService/MapServer",
  dot       = "DOT/DOT/MapServer",
  sheriff   = "Sheriff/SheriffMapService/MapServer",
  utilities = "Utilities/UtilitiesMapService/MapServer",
  ccao      = "CCAO/CCAOMapService/MapServer",
  smc       = "SMC/SMCMapService/MapServer"
)

# == Data access ===============================================================

#' List available Lake County ArcGIS service folders
#'
#' Queries the top-level ArcGIS REST services directory for Lake County, IL
#' and returns the folder names (e.g. Health, DOT, Sheriff). Each folder
#' contains one or more MapServer services with queryable layers.
#'
#' @return A tibble with one column:
#'   \describe{
#'     \item{folder}{Character. Folder name on the ArcGIS server.}
#'   }
#'
#' @family lakecountyil discovery
#' @seealso [lc_layers()] to list layers within a folder,
#'   [lc_search()] to search the ArcGIS Hub portal
#'
#' @examples
#' \dontrun{
#' lc_folders()
#' }
#' @export
lc_folders <- function() {
  url <- sprintf("%s?f=json", .lc_arcgis)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$folders)) return(tibble(folder = character()))
  tibble(folder = as.character(raw$folders))
}

#' List layers in a Lake County ArcGIS service
#'
#' Enumerates all feature layers and tables within a specific ArcGIS
#' MapServer service. Use the returned layer IDs with [lc_query()] to
#' fetch attribute data.
#'
#' @param folder Character. Folder name on the ArcGIS server
#'   (e.g. `"Health"`, `"DOT"`, `"Sheriff"`). See [lc_folders()].
#' @param service Character. Service name within the folder. Defaults
#'   to the same value as `folder`.
#' @param type Character. ArcGIS service type. Default `"MapServer"`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Layer index within the service.}
#'     \item{name}{Character. Human-readable layer name.}
#'     \item{type}{Character. Layer type (e.g. `"Feature Layer"`, `"Table"`).}
#'   }
#'
#' @family lakecountyil discovery
#' @seealso [lc_query()] to query a specific layer
#'
#' @examples
#' \dontrun{
#' lc_layers("Health")
#' lc_layers("DOT")
#' }
#' @export
lc_layers <- function(folder, service = folder, type = "MapServer") {
  url <- sprintf("%s/%s/%s/%s?f=json", .lc_arcgis, folder, service, type)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(id = integer(), name = character(), type = character()))
  layers <- raw$layers
  if (is.null(layers) || !is.data.frame(layers)) return(tibble(id = integer(), name = character(), type = character()))
  tibble(
    id   = as.integer(layers$id),
    name = as.character(layers$name),
    type = as.character(layers$type %||% NA_character_)
  )
}

#' Query a Lake County ArcGIS layer
#'
#' Sends a SQL-like query to a specific feature layer on the Lake County
#' ArcGIS REST server and returns attribute data as a tibble. Geometry
#' is not returned by default for performance.
#'
#' @param folder Character. Folder name (e.g. `"Health"`, `"DOT"`).
#' @param service Character. Service name. Defaults to same as `folder`.
#' @param type Character. Service type. Default `"MapServer"`.
#' @param layer_id Integer. Layer index within the service. Use
#'   [lc_layers()] to find available layer IDs.
#' @param where Character. SQL WHERE clause to filter records.
#'   Default `"1=1"` returns all features.
#' @param out_fields Character. Comma-separated field names to return,
#'   or `"*"` (default) for all fields.
#' @param limit Integer. Maximum records to return. Default 1000.
#' @param offset Integer. Number of records to skip for pagination.
#'   Default 0.
#'
#' @return A tibble with columns matching the layer's attribute fields.
#'   Returns an empty tibble if the query returns no results.
#'
#' @family lakecountyil data
#' @seealso [lc_layers()] to discover layer IDs,
#'   [lc_folders()] to discover available services
#'
#' @examples
#' \dontrun{
#' # Query Health service, layer 0
#' lc_query("Health", layer_id = 0, limit = 10)
#'
#' # Query DOT service with a filter
#' lc_query("DOT", layer_id = 0, where = "1=1", limit = 50)
#' }
#' @export
lc_query <- function(folder, service = folder, type = "MapServer",
                     layer_id = 0, where = "1=1", out_fields = "*",
                     limit = 1000, offset = 0) {
  svc_url <- sprintf("%s/%s/%s/%s/%d", .lc_arcgis, folder, service, type, layer_id)
  .arcgis_query(svc_url, where = where, out_fields = out_fields,
                limit = limit, offset = offset)
}

#' Search Lake County Hub datasets
#'
#' Searches the Lake County ArcGIS Hub open data portal for datasets
#' matching a keyword query. Returns metadata including download URLs
#' for each matching dataset.
#'
#' @param q Character. Search query string (e.g. `"parcel"`, `"zoning"`,
#'   `"roads"`). Default `""` returns recent datasets.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Hub dataset identifier.}
#'     \item{type}{Character. Resource type (e.g. `"dataset"`).}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Short description.}
#'     \item{url}{Character. URL to the dataset page.}
#'   }
#'
#' @family lakecountyil discovery
#' @seealso [lc_folders()] for ArcGIS REST service discovery,
#'   [lc_query()] to query a specific layer
#'
#' @examples
#' \dontrun{
#' lc_search("parcel")
#' lc_search("health")
#' }
#' @export
lc_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .lc_hub, utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(tibble())
  d <- raw$data
  tibble(
    id          = as.character(d$id),
    type        = as.character(d$type %||% NA_character_),
    title       = as.character(d$attributes$name %||% NA_character_),
    description = as.character(d$attributes$searchDescription %||% NA_character_),
    url         = as.character(d$attributes$url %||% NA_character_)
  )
}

# == Context ===================================================================

#' Get lakecountyil.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
lc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(lc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/lakecountyil.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "lakecountyil.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# lakecountyil.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# lakecountyil.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
