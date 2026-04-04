# arlingtonva.us.R - Arlington County VA open data client (custom JSON API)
#
# Data source: datahub-v2.arlingtonva.us (OData-style JSON API)
# Datasets: ~182 (property, business licenses, permits, service requests, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

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

#' List available Arlington data endpoints
#'
#' @return tibble: endpoint_key, api_path
arl_endpoints <- function() {
  tibble(
    endpoint_key = names(.arl_endpoints),
    api_path     = as.character(unlist(.arl_endpoints))
  )
}

#' Query an Arlington County dataset
#'
#' Fetches records from one of the Arlington datahub JSON endpoints.
#'
#' @param endpoint Either a key from arl_endpoints() or a full API path
#' @param top Number of records to return (default 1000, max 10000)
#' @param skip Number of records to skip for pagination (default 0)
#' @param filter Optional OData filter expression
#' @return tibble with dataset fields
arl_query <- function(endpoint, top = 1000, skip = 0, filter = NULL) {
  path <- if (endpoint %in% names(.arl_endpoints)) .arl_endpoints[[endpoint]] else endpoint
  url <- sprintf("%s/%s?$top=%d&$skip=%d", .arl_base, path, top, skip)
  if (!is.null(filter)) url <- paste0(url, "&$filter=", utils::URLencode(filter))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  if (is.data.frame(raw)) return(as_tibble(raw))
  if (is.list(raw) && "value" %in% names(raw)) return(as_tibble(raw$value))
  tibble()
}

#' Get Arlington County business licenses
#'
#' @param top Number of records (default 1000)
#' @param skip Pagination offset (default 0)
#' @return tibble with active business license data
arl_businesses <- function(top = 1000, skip = 0) {
  arl_query("business_licenses", top = top, skip = skip)
}

#' Get Arlington County property sales history
#'
#' @param top Number of records (default 1000)
#' @param skip Pagination offset (default 0)
#' @return tibble with property sale records
arl_sales <- function(top = 1000, skip = 0) {
  arl_query("sales_history", top = top, skip = skip)
}

#' Get Arlington County service requests
#'
#' @param top Number of records (default 1000)
#' @param skip Pagination offset (default 0)
#' @return tibble with 311 service request data
arl_service_requests <- function(top = 1000, skip = 0) {
  arl_query("service_requests", top = top, skip = skip)
}

#' Get Arlington County trade name registrations
#'
#' @param top Number of records (default 1000)
#' @param skip Pagination offset (default 0)
#' @return tibble with trade name records
arl_trade_names <- function(top = 1000, skip = 0) {
  arl_query("trade_names", top = top, skip = skip)
}

# == Context ===================================================================

#' Generate LLM-friendly context for arlingtonva.us
#'
#' @return Character string with full function signatures and bodies
arl_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/arlingtonva.us.R"
  if (!file.exists(src_file)) { cat("# arlingtonva.us context - source not found\n"); return(invisible(NULL)) }
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
