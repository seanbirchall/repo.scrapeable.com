#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# osmre.gov.R - Self-contained OSMRE (Office of Surface Mining) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: ArcGIS REST Services at geoservices.osmre.gov


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

