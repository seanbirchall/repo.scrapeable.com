# arkansas.gov.R - State of Arkansas GIS open data client (ArcGIS FeatureServer)
#
# Data source: gis.arkansas.gov (ArcGIS REST FeatureServer)
# Datasets: ~72 (boundaries, environment, transportation, health, farming, water)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ark_base <- "https://gis.arkansas.gov/arcgis/rest/services/FEATURESERVICES"

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

# == Known feature services ====================================================

.ark_services <- list(
  boundaries     = "Boundaries/FeatureServer",
  environment    = "Environment/FeatureServer",
  transportation = "Transportation/FeatureServer",
  health         = "Health/FeatureServer",
  farming        = "Farming/FeatureServer",
  water          = "Water/FeatureServer"
)

# Known layer IDs within services
.ark_layers <- list(
  senate_districts     = list(svc = "boundaries", layer = 34, name = "Senate Districts"),
  house_districts      = list(svc = "boundaries", layer = 14, name = "House Districts"),
  fire_districts       = list(svc = "boundaries", layer = 12, name = "Fire Districts"),
  hospitals            = list(svc = "health",     layer = 2,  name = "Hospital Related Services"),
  trout_streams        = list(svc = "environment", layer = 5, name = "Trout Streams"),
  erw_segments         = list(svc = "environment", layer = 2, name = "Extraordinary Resource Waters"),
  posted_bridges       = list(svc = "transportation", layer = 3, name = "Posted Highway Bridges"),
  intermodal_terminals = list(svc = "transportation", layer = 1, name = "Intermodal Terminals"),
  hydrologic_units     = list(svc = "water",      layer = 24, name = "8 Digit Hydrologic Units")
)

# == Data access ===============================================================

#' List available Arkansas GIS feature services
#'
#' Returns the top-level ArcGIS FeatureServer service categories available from
#' the Arkansas GIS portal (gis.arkansas.gov). Use the \code{key} values with
#' \code{\link{ark_layers}} to discover layers within each service category.
#'
#' @return A tibble with one row per service and the following columns:
#' \describe{
#'   \item{key}{Character. Service key for use with \code{ark_layers()}.
#'     Values: \code{"boundaries"}, \code{"environment"},
#'     \code{"transportation"}, \code{"health"}, \code{"farming"},
#'     \code{"water"}.}
#'   \item{service_path}{Character. ArcGIS REST API path for the service.}
#' }
#'
#' @examples
#' \dontrun{
#' # List all available services
#' ark_services()
#' }
ark_services <- function() {
  tibble(
    key          = names(.ark_services),
    service_path = as.character(unlist(.ark_services))
  )
}

#' List layers in an Arkansas GIS feature service
#'
#' Returns all GIS layers available within a specific Arkansas FeatureServer
#' service. Each layer can be queried with \code{\link{ark_query}} using its
#' layer ID. Use \code{\link{ark_services}} to see available service keys.
#'
#' @param service Character. Service key from \code{\link{ark_services}}
#'   (e.g. \code{"boundaries"}, \code{"health"}, \code{"transportation"}) or
#'   a full ArcGIS REST service path.
#'
#' @return A tibble with one row per layer and the following columns:
#' \describe{
#'   \item{id}{Integer. Layer ID for building query URLs.}
#'   \item{name}{Character. Layer name (e.g. \code{"Senate Districts"},
#'     \code{"Hospital Related Services"}).}
#'   \item{type}{Character. Layer geometry type (e.g. \code{"Feature Layer"}).}
#' }
#'
#' @examples
#' \dontrun{
#' # List layers in the health service
#' ark_layers("health")
#'
#' # List boundary layers
#' ark_layers("boundaries")
#' }
ark_layers <- function(service) {
  path <- if (service %in% names(.ark_services)) .ark_services[[service]] else service
  url <- sprintf("%s/%s?f=json", .ark_base, path)
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

#' List known Arkansas GIS datasets with friendly names
#'
#' Returns a catalog of pre-configured datasets with friendly keys for use
#' with \code{\link{ark_query}}. These are curated layer references that save
#' you from manually browsing services and layers. Includes datasets for
#' senate/house districts, hospitals, trout streams, bridges, and more.
#'
#' @return A tibble with one row per known dataset and the following columns:
#' \describe{
#'   \item{key}{Character. Friendly dataset key for use with \code{ark_query()}.
#'     Values include: \code{"senate_districts"}, \code{"house_districts"},
#'     \code{"fire_districts"}, \code{"hospitals"}, \code{"trout_streams"},
#'     \code{"erw_segments"}, \code{"posted_bridges"},
#'     \code{"intermodal_terminals"}, \code{"hydrologic_units"}.}
#'   \item{service}{Character. Parent service category.}
#'   \item{layer_id}{Integer. ArcGIS layer ID within the service.}
#'   \item{name}{Character. Human-readable dataset name.}
#' }
#'
#' @examples
#' \dontrun{
#' # See all pre-configured datasets
#' ark_datasets()
#' }
ark_datasets <- function() {
  tibble(
    key      = names(.ark_layers),
    service  = vapply(.ark_layers, function(x) x$svc, character(1)),
    layer_id = vapply(.ark_layers, function(x) as.integer(x$layer), integer(1)),
    name     = vapply(.ark_layers, function(x) x$name, character(1))
  )
}

#' Query an Arkansas GIS layer
#'
#' Fetches feature records from an ArcGIS FeatureServer layer. Accepts either
#' a friendly dataset key from \code{\link{ark_datasets}} or a raw ArcGIS
#' service URL. Supports SQL WHERE filtering and pagination. Geometry is
#' excluded from results (attribute data only).
#'
#' @param dataset Character. Either a dataset key from
#'   \code{\link{ark_datasets}} (e.g. \code{"hospitals"},
#'   \code{"senate_districts"}, \code{"posted_bridges"}) or a full ArcGIS
#'   FeatureServer layer URL.
#' @param where Character. SQL WHERE clause to filter records (default
#'   \code{"1=1"} for all records). Example: \code{"county = 'PULASKI'"},
#'   \code{"name LIKE '\%River\%'"}.
#' @param out_fields Character. Comma-separated field names to return (default
#'   \code{"*"} for all fields). Example: \code{"name,city,county"}.
#' @param limit Integer. Maximum number of records to return (default 1000).
#' @param offset Integer. Number of records to skip for pagination (default 0).
#'
#' @return A tibble with columns depending on the dataset queried. Common
#'   columns across datasets include \code{objectid} and dataset-specific
#'   attributes. For example, hospitals returns: \code{objectid}, \code{type},
#'   \code{address}, \code{name}, \code{city}, \code{zip}, \code{county},
#'   \code{licenseinfo}, \code{state}, \code{phone}, \code{latitude},
#'   \code{longitude}, etc.
#'
#' @examples
#' \dontrun{
#' # Query all hospitals
#' ark_query("hospitals", limit = 50)
#'
#' # Query senate districts
#' ark_query("senate_districts")
#'
#' # Filter posted bridges by county
#' ark_query("posted_bridges", where = "county = 'PULASKI'", limit = 100)
#'
#' # Paginate through results
#' page1 <- ark_query("hospitals", limit = 100, offset = 0)
#' page2 <- ark_query("hospitals", limit = 100, offset = 100)
#' }
ark_query <- function(dataset, where = "1=1", out_fields = "*",
                      limit = 1000, offset = 0) {
  if (dataset %in% names(.ark_layers)) {
    info <- .ark_layers[[dataset]]
    svc_path <- .ark_services[[info$svc]]
    svc_url <- sprintf("%s/%s/%d", .ark_base, svc_path, info$layer)
  } else {
    svc_url <- dataset
  }
  .arcgis_query(svc_url, where = where, out_fields = out_fields,
                limit = limit, offset = offset)
}

#' Get Arkansas Senate district boundaries
#'
#' Convenience wrapper around \code{\link{ark_query}} that fetches Arkansas
#' Senate district data. Returns district boundaries and representative info.
#'
#' @param limit Integer. Maximum number of records to return (default 100).
#'   Arkansas has 35 Senate districts.
#'
#' @return A tibble with one row per district and columns including:
#'   \code{objectid}, \code{ndistrict} (district number), \code{name}
#'   (senator name), \code{hyperlink}, \code{party}, \code{acres},
#'   \code{Shape__Area}, \code{Shape__Length}.
#'
#' @examples
#' \dontrun{
#' # Get all Senate districts
#' ark_senate_districts()
#' }
ark_senate_districts <- function(limit = 100) {
  ark_query("senate_districts", limit = limit)
}

#' Get Arkansas hospital/health facility locations
#'
#' Convenience wrapper around \code{\link{ark_query}} that fetches Arkansas
#' hospital and health-related facility locations with addresses and
#' licensing information.
#'
#' @param limit Integer. Maximum number of records to return (default 1000).
#'
#' @return A tibble with one row per facility and columns including:
#'   \code{objectid}, \code{type}, \code{address}, \code{name}, \code{city},
#'   \code{zip}, \code{county}, \code{licenseinfo}, \code{otherinfo},
#'   \code{state}, \code{phone}, \code{begin_date}, \code{edit_date},
#'   \code{longitude}, \code{latitude}.
#'
#' @examples
#' \dontrun{
#' # Get all hospitals
#' hospitals <- ark_hospitals()
#'
#' # Filter to a specific city
#' library(dplyr)
#' hospitals |> filter(city == "LITTLE ROCK")
#' }
ark_hospitals <- function(limit = 1000) {
  ark_query("hospitals", limit = limit)
}

# == Context ===================================================================

#' Get arkansas.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ark_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ark_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/arkansas.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "arkansas.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# arkansas.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# arkansas.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
