# osmre.gov.R - Self-contained OSMRE (Office of Surface Mining) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: ArcGIS REST Services at geoservices.osmre.gov

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.osmre_base <- "https://geoservices.osmre.gov/arcgis/rest/services/GeoMine"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_options(ssl_verifypeer = 0L) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.query_layer <- function(service, layer = 0, where = "1=1",
                          out_fields = "*", max_records = 1000) {
  base_url <- paste0(.osmre_base, "/", service, "/MapServer/", layer, "/query")
  tmp <- tempfile(fileext = ".json")
  httr2::request(base_url) |>
    httr2::req_url_query(where = where, outFields = out_fields,
                          f = "json", resultRecordCount = max_records) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_options(ssl_verifypeer = 0L) |>
    httr2::req_perform(path = tmp)
  data <- jsonlite::fromJSON(tmp, simplifyVector = TRUE)
  if (is.null(data$features) || length(data$features) == 0) return(tibble())
  attrs <- data$features$attributes
  if (is.null(attrs)) return(tibble())
  as_tibble(attrs)
}

# == Layer definitions =========================================================

.osmre_layers <- tibble(
  id = c("offices", "mine_map_repo", "coalmine_operations",
         "aml_awards", "coal_prep_plants", "coal_refuse",
         "coal_fields", "env_monitoring", "bond_status",
         "post_mining_land"),
  name = c("OSMRE Office Locations", "National Mine Map Repository",
           "Surface Coal Mine Permit Boundaries",
           "Abandoned Mine Land Awards", "Coal Preparation Plants",
           "Coal Refuse Disposal Areas", "Coal Fields of the US",
           "Environmental Resource Monitoring Locations",
           "Reclamation Bond Status Areas", "Post-Mining Land Use"),
  service = c("OSMRE_Offices", "NationalMineMapRepository_OSMRE",
              "AllCoalmineOperations",
              "OSMRE_AML_Awards", "CoalPrepPlants",
              "CoalRefuse", "BackgroundFeatures",
              "EnvironmentalResourceMonitoringLocations",
              "ReclamationBondStatus", "PostMiningLandUse")
)

# == Public functions ==========================================================

#' List available OSMRE GeoMine data layers
#'
#' Returns a catalog of geospatial data layers available through the
#' Office of Surface Mining Reclamation and Enforcement (OSMRE) GeoMine
#' ArcGIS REST services. Each layer can be queried with
#' \code{\link{osmre_query}} using its \code{id}.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{id}{Character. Layer identifier for use with \code{\link{osmre_query}}.}
#'   \item{name}{Character. Human-readable layer name.}
#'   \item{service}{Character. ArcGIS MapServer service name.}
#' }
#'
#' @examples
#' \dontrun{
#' osmre_list()
#' }
#'
#' @seealso \code{\link{osmre_query}} for querying any layer by ID.
#' @export
osmre_list <- function() {
  .osmre_layers
}

#' Fetch OSMRE office locations
#'
#' Returns the locations and contact information for OSMRE regional and
#' field offices across the United States.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{office_name}{Character. Name of the OSMRE office.}
#'   \item{address}{Character. Street address.}
#'   \item{city}{Character. City.}
#'   \item{state}{Character. Two-letter state abbreviation.}
#'   \item{zip}{Character. ZIP code.}
#'   \item{osm_region}{Character. OSMRE administrative region
#'     (e.g. "Appalachian", "Mid-Continent", "Western").}
#'   \item{web_site}{Character. Office website URL.}
#'   \item{lat}{Numeric. Latitude in decimal degrees.}
#'   \item{lon}{Numeric. Longitude in decimal degrees.}
#' }
#'
#' @examples
#' \dontrun{
#' osmre_offices()
#' }
#'
#' @export
osmre_offices <- function() {
  raw <- .query_layer("OSMRE_Offices")
  if (nrow(raw) == 0) return(tibble())
  out <- tibble(
    office_name = as.character(raw$office_name %||% raw$OFFICE_NAME),
    address = as.character(raw$address %||% raw$ADDRESS),
    city = as.character(raw$city %||% raw$CITY),
    state = as.character(raw$state %||% raw$STATE),
    zip = as.character(raw$zip %||% raw$ZIP),
    osm_region = as.character(raw$osm_region %||% raw$OSM_REGION),
    web_site = as.character(raw$web_site %||% raw$WEB_SITE),
    lat = suppressWarnings(as.numeric(raw$point_y %||% raw$POINT_Y)),
    lon = suppressWarnings(as.numeric(raw$point_x %||% raw$POINT_X))
  )
  out
}

#' Fetch mine records from the National Mine Map Repository
#'
#' Queries the National Mine Map Repository, the largest collection of
#' mine maps in the US with over 180,000 maps covering coal, metal, and
#' non-metal mines. Records include mine names, companies, commodities,
#' mining types, and geographic coordinates.
#'
#' @param state Character. Optional state name filter. Note: the mine map
#'   repository does not have a direct state field, so state-level filtering
#'   would require post-hoc coordinate-based subsetting.
#' @param commodity Character. Optional commodity filter using SQL LIKE
#'   matching (e.g. \code{"Coal"}, \code{"Iron"}).
#' @param max_records Integer. Maximum records to return (default 1000).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{mine_id}{Numeric. Mine identifier.}
#'   \item{mine_name}{Character. Mine name.}
#'   \item{document_mine_name}{Character. Mine name as recorded on the document.}
#'   \item{company}{Character. Mining company name.}
#'   \item{commodity}{Character. Commodity mined (e.g. "COAL ANTHRACITE").}
#'   \item{mining_type}{Character. Type of mining (e.g. "UNDERGROUND").}
#'   \item{latitude}{Numeric. Latitude in decimal degrees.}
#'   \item{longitude}{Numeric. Longitude in decimal degrees.}
#'   \item{map_year}{Character. Year the map was produced.}
#'   \item{document_number}{Character. Document reference number.}
#' }
#'
#' @examples
#' \dontrun{
#' osmre_mine_maps(commodity = "Coal", max_records = 50)
#' }
#'
#' @seealso \code{\link{osmre_search}} for name-based mine searching.
#' @export
osmre_mine_maps <- function(state = NULL, commodity = NULL, max_records = 1000) {
  where_parts <- "1=1"
  if (!is.null(state)) {
    # Mine map repo doesn't have a state field directly - skip state filter
    # The data is point-based, filter by coordinate or post-hoc
  }
  if (!is.null(commodity)) {
    where_parts <- paste0("commodity LIKE '%", commodity, "%'")
  }
  raw <- .query_layer("NationalMineMapRepository_OSMRE", where = where_parts,
                       max_records = max_records)
  if (nrow(raw) == 0) return(tibble())
  tibble(
    mine_id = suppressWarnings(as.numeric(raw$mineid)),
    mine_name = as.character(raw$minename),
    document_mine_name = as.character(raw$documentminename),
    company = as.character(raw$minecompany),
    commodity = as.character(raw$commodity),
    mining_type = as.character(raw$miningtype),
    latitude = suppressWarnings(as.numeric(raw$latitude)),
    longitude = suppressWarnings(as.numeric(raw$longitude)),
    map_year = as.character(raw$mapyear),
    document_number = as.character(raw$documentnumber)
  )
}

#' Fetch coal mine operation permit boundaries
#'
#' Returns surface coal mine permit boundary records from OSMRE, including
#' mine names, permittees, MSHA IDs, coal bed names, and calculated
#' permit areas in acres.
#'
#' @param state Character. Optional state filter (abbreviation or name as
#'   stored in data).
#' @param status Integer or character. Optional coalmine operation status code
#'   to filter by.
#' @param max_records Integer. Maximum records to return (default 500).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{mine_name}{Character. Mine name.}
#'   \item{permittee}{Character. Permit holder name.}
#'   \item{company}{Character. Operating company.}
#'   \item{permit_id}{Character. Permit identifier.}
#'   \item{msha_id}{Character. Mine Safety and Health Administration ID.}
#'   \item{status}{Integer. Operation status code.}
#'   \item{coal_bed_names}{Character. Names of coal beds being mined.}
#'   \item{calculated_area_acres}{Numeric. Permit area in acres.}
#' }
#'
#' @examples
#' \dontrun{
#' osmre_coalmine_operations(max_records = 50)
#' }
#'
#' @export
osmre_coalmine_operations <- function(state = NULL, status = NULL, max_records = 500) {
  where <- "1=1"
  if (!is.null(status)) {
    where <- paste0("coalmine_op_status=", status)
  }
  raw <- .query_layer("AllCoalmineOperations", where = where,
                       max_records = max_records)
  if (nrow(raw) == 0) return(tibble())
  tibble(
    mine_name = as.character(raw$mine_name),
    permittee = as.character(raw$permittee),
    company = as.character(raw$company),
    permit_id = as.character(raw$permit_id),
    msha_id = as.character(raw$msha_id),
    status = suppressWarnings(as.integer(raw$coalmine_op_status)),
    coal_bed_names = as.character(raw$coal_bed_names),
    calculated_area_acres = suppressWarnings(as.numeric(raw$calculated_area))
  )
}

#' Search OSMRE mine maps by name
#'
#' Searches the National Mine Map Repository by mine name using a
#' case-insensitive partial match (SQL LIKE). Useful for finding specific
#' mines or mining operations.
#'
#' @param query Character. Search string matched against mine names
#'   (converted to uppercase for case-insensitive matching).
#' @param max_records Integer. Maximum records to return (default 100).
#'
#' @return A tibble with the same columns as \code{\link{osmre_mine_maps}}.
#'
#' @examples
#' \dontrun{
#' osmre_search("COAL")
#' osmre_search("BLACK DIAMOND")
#' }
#'
#' @seealso \code{\link{osmre_mine_maps}} for browsing by commodity.
#' @export
osmre_search <- function(query, max_records = 100) {
  where <- paste0("minename LIKE '%", toupper(query), "%'")
  raw <- .query_layer("NationalMineMapRepository_OSMRE", where = where,
                       max_records = max_records)
  if (nrow(raw) == 0) return(tibble())
  tibble(
    mine_id = suppressWarnings(as.numeric(raw$mineid)),
    mine_name = as.character(raw$minename),
    document_mine_name = as.character(raw$documentminename),
    company = as.character(raw$minecompany),
    commodity = as.character(raw$commodity),
    mining_type = as.character(raw$miningtype),
    latitude = suppressWarnings(as.numeric(raw$latitude)),
    longitude = suppressWarnings(as.numeric(raw$longitude)),
    map_year = as.character(raw$mapyear),
    document_number = as.character(raw$documentnumber)
  )
}

#' Query any OSMRE GeoMine layer
#'
#' Generic accessor for any OSMRE GeoMine ArcGIS MapServer layer. Provide
#' a \code{layer_id} from \code{\link{osmre_list}} and an optional SQL
#' WHERE clause for server-side filtering.
#'
#' @param layer_id Character. Layer identifier as returned by
#'   \code{\link{osmre_list}} (e.g. \code{"coal_fields"},
#'   \code{"aml_awards"}).
#' @param where Character. SQL WHERE clause for filtering attributes
#'   (default \code{"1=1"} returns all records).
#' @param max_records Integer. Maximum records to return (default 500).
#'
#' @return A tibble of raw attribute columns from the ArcGIS layer.
#'   Column names and types depend on the specific layer.
#'
#' @examples
#' \dontrun{
#' osmre_query("coal_fields", max_records = 20)
#' osmre_query("aml_awards", where = "STATE='PA'", max_records = 50)
#' }
#'
#' @seealso \code{\link{osmre_list}} for available layer IDs.
#' @export
osmre_query <- function(layer_id, where = "1=1", max_records = 500) {
  match <- .osmre_layers |> filter(.data$id == !!layer_id)
  if (nrow(match) == 0) {
    stop("Unknown layer: ", layer_id,
         ". Use osmre_list() to see available layers.", call. = FALSE)
  }
  .query_layer(match$service[1], where = where, max_records = max_records)
}

#' Get osmre.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
osmre_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(osmre_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/osmre.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "osmre.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# osmre.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# osmre.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
