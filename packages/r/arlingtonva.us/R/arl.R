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

#' List available Arlington County data endpoints
#'
#' Returns the catalog of known Arlington County, Virginia open data endpoints
#' from the datahub-v2 API. Use the \code{endpoint_key} values with
#' \code{\link{arl_query}} to fetch data from any endpoint, or use the
#' convenience functions (\code{\link{arl_businesses}},
#' \code{\link{arl_sales}}, \code{\link{arl_service_requests}},
#' \code{\link{arl_trade_names}}) for common datasets.
#'
#' @return A tibble with one row per endpoint and the following columns:
#' \describe{
#'   \item{endpoint_key}{Character. Friendly key for use with \code{arl_query()}.
#'     Values: \code{"trade_names"}, \code{"business_licenses"},
#'     \code{"property"}, \code{"sales_history"}, \code{"permits"},
#'     \code{"service_requests"}, \code{"developments"},
#'     \code{"parks_reservations"}, \code{"rezoning_cases"},
#'     \code{"streets"}, \code{"property_class"},
#'     \code{"dwelling_interiors"}.}
#'   \item{api_path}{Character. REST API path segment.}
#' }
#'
#' @examples
#' \dontrun{
#' # See all available endpoints
#' arl_endpoints()
#' }
arl_endpoints <- function() {
  tibble(
    endpoint_key = names(.arl_endpoints),
    api_path     = as.character(unlist(.arl_endpoints))
  )
}

#' Query an Arlington County dataset
#'
#' Fetches records from any Arlington County datahub endpoint. This is the
#' general-purpose query function that powers the convenience wrappers
#' (\code{\link{arl_businesses}}, \code{\link{arl_sales}}, etc.). Supports
#' OData-style pagination and filtering. Use \code{\link{arl_endpoints}}
#' to see all available endpoint keys.
#'
#' @param endpoint Character. Either a key from \code{\link{arl_endpoints}}
#'   (e.g. \code{"property"}, \code{"permits"}, \code{"developments"}) or a
#'   full API path (e.g. \code{"RealEstate/Property"}).
#' @param top Integer. Number of records to return (default 1000, max 10000).
#' @param skip Integer. Number of records to skip for pagination (default 0).
#' @param filter Character or \code{NULL}. Optional OData filter expression.
#'   Example: \code{"cityName eq 'ARLINGTON'"}.
#'
#' @return A tibble with columns depending on the endpoint queried. The
#'   structure varies by dataset.
#'
#' @examples
#' \dontrun{
#' # Query property data
#' arl_query("property", top = 10)
#'
#' # Query permits with pagination
#' page1 <- arl_query("permits", top = 1000, skip = 0)
#' page2 <- arl_query("permits", top = 1000, skip = 1000)
#'
#' # Query development projects
#' arl_query("developments", top = 50)
#' }
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
#' Convenience wrapper around \code{\link{arl_query}} that fetches current
#' business license records from Arlington County, Virginia. Returns
#' registered business names, addresses, and license categories.
#'
#' @param top Integer. Number of records to return (default 1000).
#' @param skip Integer. Pagination offset (default 0).
#'
#' @return A tibble with one row per business license and columns including:
#'   \code{businessLegalName}, \code{doingBusinessAsName},
#'   \code{businessAddressText}, \code{streetAddressText}, \code{cityName},
#'   \code{stateCode}, \code{zipCode}, \code{sectionCodeList}.
#'
#' @examples
#' \dontrun{
#' # Get first 100 business licenses
#' arl_businesses(top = 100)
#'
#' # Paginate through all records
#' page1 <- arl_businesses(top = 1000, skip = 0)
#' page2 <- arl_businesses(top = 1000, skip = 1000)
#' }
arl_businesses <- function(top = 1000, skip = 0) {
  arl_query("business_licenses", top = top, skip = skip)
}

#' Get Arlington County property sales history
#'
#' Convenience wrapper around \code{\link{arl_query}} that fetches real estate
#' sales transaction history for Arlington County, Virginia. Includes sale
#' amounts, dates, deed references, and buyer names.
#'
#' @param top Integer. Number of records to return (default 1000).
#' @param skip Integer. Pagination offset (default 0).
#'
#' @return A tibble with one row per sale transaction and columns including:
#'   \code{salesHistoryKey}, \code{propertyKey},
#'   \code{realEstatePropertyCode}, \code{neighborhoodNbr},
#'   \code{propertyStreetNbrNameText}, \code{salesTypeCode},
#'   \code{salesTypeDsc}, \code{deedBookNbr}, \code{deedPageNbr},
#'   \code{granteeName}, \code{saleAmt} (integer, dollars),
#'   \code{saleDate} (character, ISO 8601).
#'
#' @examples
#' \dontrun{
#' # Get recent sales
#' arl_sales(top = 50)
#' }
arl_sales <- function(top = 1000, skip = 0) {
  arl_query("sales_history", top = top, skip = skip)
}

#' Get Arlington County service requests
#'
#' Convenience wrapper around \code{\link{arl_query}} that fetches 311-style
#' service requests from Arlington County, Virginia. Includes pothole reports,
#' cart deliveries, metadata requests, and other citizen service interactions.
#'
#' @param top Integer. Number of records to return (default 1000).
#' @param skip Integer. Pagination offset (default 0).
#'
#' @return A tibble with one row per service request and columns including:
#'   \code{serviceRequestId}, \code{name} (request type),
#'   \code{description}, \code{status} (e.g. \code{"closed"}),
#'   \code{longitude}, \code{latitude}, \code{address},
#'   \code{createdAt} (character, ISO 8601),
#'   \code{updatedAt} (character, ISO 8601),
#'   \code{permission}, \code{sourceSystem}.
#'
#' @examples
#' \dontrun{
#' # Get recent service requests
#' arl_service_requests(top = 100)
#' }
arl_service_requests <- function(top = 1000, skip = 0) {
  arl_query("service_requests", top = top, skip = skip)
}

#' Get Arlington County trade name registrations
#'
#' Convenience wrapper around \code{\link{arl_query}} that fetches trade name
#' (DBA -- "doing business as") registrations from Arlington County, Virginia.
#' Records date back to 1941.
#'
#' @param top Integer. Number of records to return (default 1000).
#' @param skip Integer. Pagination offset (default 0).
#'
#' @return A tibble with one row per registration and columns including:
#'   \code{formId} (integer), \code{tradeName},
#'   \code{registeredBusinessName}, \code{recordedDate} (character, ISO 8601),
#'   \code{bookNbr}, \code{pageNbr},
#'   \code{businessOwnerRegisteredAgentList}.
#'
#' @examples
#' \dontrun{
#' # Get trade name registrations
#' arl_trade_names(top = 50)
#' }
arl_trade_names <- function(top = 1000, skip = 0) {
  arl_query("trade_names", top = top, skip = skip)
}

# == Context ===================================================================

#' Get arlingtonva.us client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
arl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(arl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/arlingtonva.us.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "arlingtonva.us")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# arlingtonva.us context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# arlingtonva.us", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
