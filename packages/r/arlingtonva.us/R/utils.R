#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# arlingtonva.us.R - Arlington County VA open data client (custom JSON API)
#
# Data source: datahub-v2.arlingtonva.us (OData-style JSON API)
# Datasets: ~182 (property, business licenses, permits, service requests, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.arl_base <- "https://datahub-v2.arlingtonva.us/api"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Known endpoint catalog ====================================================

.arl_endpoints <- list(
  trade_names       = "DemographicEconomic/TradeName",
  business_licenses = "DemographicEconomic/CurrentBusinessLicenseList",
  property          = "RealEstate/Property",
  sales_history     = "RealEstate/SalesHistory",
  permits           = "Permit/PermitEventLog",
  service_requests  = "Environmental/ServiceRequest",
  developments      = "HousingBuilding/DevelopmentProject",
  parks_reservations = "Recreation/ParkFacilityReservations",
  rezoning_cases    = "RealEstate/RezoningCase",
  streets           = "Location/StandardArlingtonStreet",
  property_class    = "RealEstate/PropertyClassType",
  dwelling_interiors = "RealEstate/ImprovementInterior"
)

# == Data access ===============================================================

