# waterqualitydata.us.R - Self-contained waterqualitydata.us client



# waterqualitydata-us.R
# Self-contained Water Quality Portal client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: be polite - large queries can be slow


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wqp_base <- "https://www.waterqualitydata.us"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_results <- tibble(
  organization = character(), activity_id = character(),
  activity_type = character(), activity_date = as.Date(character()),
  monitoring_location = character(), characteristic = character(),
  result_value = numeric(), result_unit = character(),
  result_status = character(), method_id = character()
)

.schema_stations <- tibble(
  organization = character(), monitoring_location_id = character(),
  monitoring_location_name = character(), location_type = character(),
  latitude = numeric(), longitude = numeric(),
  state_code = character(), county_code = character(),
  huc_code = character()
)

# == Results ===================================================================


#' Fetch water quality results from the Water Quality Portal
#'
#' Queries the national Water Quality Portal (WQP) for laboratory and
#' field measurement results. The WQP aggregates data from USGS, EPA
#' STORET, and USDA ARS. Filter by location (state, county, site) and
#' characteristic name.
#'
#' @param countycode Optional county FIPS code in WQP format
#'   (e.g., \code{"US:06:037"} for Los Angeles County, CA).
#' @param statecode Optional state FIPS code in WQP format
#'   (e.g., \code{"US:06"} for California, \code{"US:24"} for Maryland).
#' @param siteid Optional monitoring location ID
#'   (e.g., \code{"USGS-01646500"}).
#' @param characteristicName Optional parameter name filter
#'   (e.g., \code{"Temperature, water"}, \code{"pH"}, \code{"Dissolved oxygen (DO)"}).
#' @param startDateLo Optional start date in \code{"MM-DD-YYYY"} format.
#' @param startDateHi Optional end date in \code{"MM-DD-YYYY"} format.
#' @param dataProfile Character data profile: \code{"resultPhysChem"} (default),
#'   \code{"biological"}, or \code{"narrowResult"}.
#' @param top Integer maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{organization}{Reporting organization identifier}
#'     \item{activity_id}{Unique activity/sample identifier}
#'     \item{activity_type}{Type of sampling activity}
#'     \item{activity_date}{Date of the activity}
#'     \item{monitoring_location}{Monitoring site identifier}
#'     \item{characteristic}{Measured parameter name}
#'     \item{result_value}{Numeric measurement value}
#'     \item{result_unit}{Unit of measurement}
#'     \item{result_status}{Quality status (e.g., "Final", "Preliminary")}
#'     \item{method_id}{Analytical method identifier}
#'   }
#' @export
#' @family WQP functions
#' @seealso \code{\link{wqp_stations}} to find monitoring locations,
#'   \code{\link{wqp_characteristics}} to list available parameters at a site
#' @examples
#' \dontrun{
#' # Water quality results in Maryland
#' wqp_results(statecode = "US:24", top = 50)
#'
#' # Temperature data at a specific USGS site
#' wqp_results(siteid = "USGS-01646500", characteristicName = "Temperature, water")
#' }
wqp_results <- function(countycode = NULL, statecode = NULL, siteid = NULL,
                        characteristicName = NULL, startDateLo = NULL,
                        startDateHi = NULL, dataProfile = "resultPhysChem",
                        top = 100) {
  params <- list(mimeType = "csv", zip = "no", sorted = "no")
  if (!is.null(countycode)) params$countycode <- countycode
  if (!is.null(statecode)) params$statecode <- statecode
  if (!is.null(siteid)) params$siteid <- siteid
  if (!is.null(characteristicName)) params$characteristicName <- characteristicName
  if (!is.null(startDateLo)) params$startDateLo <- startDateLo
  if (!is.null(startDateHi)) params$startDateHi <- startDateHi
  if (!is.null(dataProfile)) params$dataProfile <- dataProfile
  params$top <- as.integer(top)

  query <- paste(names(params), sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)), sep = "=", collapse = "&")
  url <- paste0(.wqp_base, "/data/Result/search?", query)
  f <- .fetch_csv(url)
  raw <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) data.frame())
  if (nrow(raw) == 0) return(.schema_results)

  as_tibble(raw) |>
    transmute(
      organization        = as.character(if ("OrganizationIdentifier" %in% names(raw)) OrganizationIdentifier else NA),
      activity_id         = as.character(if ("ActivityIdentifier" %in% names(raw)) ActivityIdentifier else NA),
      activity_type       = as.character(if ("ActivityTypeCode" %in% names(raw)) ActivityTypeCode else NA),
      activity_date       = tryCatch(as.Date(if ("ActivityStartDate" %in% names(raw)) ActivityStartDate else NA), error = function(e) NA),
      monitoring_location = as.character(if ("MonitoringLocationIdentifier" %in% names(raw)) MonitoringLocationIdentifier else NA),
      characteristic      = as.character(if ("CharacteristicName" %in% names(raw)) CharacteristicName else NA),
      result_value        = suppressWarnings(as.numeric(if ("ResultMeasureValue" %in% names(raw)) ResultMeasureValue else NA)),
      result_unit         = as.character(if ("ResultMeasure/MeasureUnitCode" %in% names(raw)) `ResultMeasure/MeasureUnitCode` else NA),
      result_status       = as.character(if ("ResultStatusIdentifier" %in% names(raw)) ResultStatusIdentifier else NA),
      method_id           = as.character(if ("ResultAnalyticalMethod/MethodIdentifier" %in% names(raw)) `ResultAnalyticalMethod/MethodIdentifier` else NA)
    )
}

#' Fetch monitoring stations from the Water Quality Portal
#'
#' Returns metadata for water quality monitoring stations. Stations
#' span USGS, EPA, and USDA networks. Use returned station IDs with
#' \code{\link{wqp_results}} to fetch measurement data.
#'
#' @param countycode Optional county FIPS code in WQP format
#'   (e.g., \code{"US:06:037"}).
#' @param statecode Optional state FIPS code in WQP format
#'   (e.g., \code{"US:06"} for California).
#' @param top Integer maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{organization}{Reporting organization}
#'     \item{monitoring_location_id}{Station identifier (e.g., "USGS-01646500")}
#'     \item{monitoring_location_name}{Human-readable station name}
#'     \item{location_type}{Type of monitoring location}
#'     \item{latitude}{Numeric latitude}
#'     \item{longitude}{Numeric longitude}
#'     \item{state_code}{FIPS state code}
#'     \item{county_code}{FIPS county code}
#'     \item{huc_code}{Eight-digit Hydrologic Unit Code}
#'   }
#' @export
#' @family WQP functions
#' @seealso \code{\link{wqp_results}} to fetch data from a station,
#'   \code{\link{wqp_station_detail}} for a single station
#' @examples
#' \dontrun{
#' # Stations in Maryland
#' wqp_stations(statecode = "US:24", top = 50)
#' }
wqp_stations <- function(countycode = NULL, statecode = NULL, top = 100) {
  params <- list(mimeType = "csv", zip = "no", sorted = "no")
  if (!is.null(countycode)) params$countycode <- countycode
  if (!is.null(statecode)) params$statecode <- statecode
  params$top <- as.integer(top)

  query <- paste(names(params), sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)), sep = "=", collapse = "&")
  url <- paste0(.wqp_base, "/data/Station/search?", query)
  f <- .fetch_csv(url)
  raw <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) data.frame())
  if (nrow(raw) == 0) return(.schema_stations)

  as_tibble(raw) |>
    transmute(
      organization            = as.character(if ("OrganizationIdentifier" %in% names(raw)) OrganizationIdentifier else NA),
      monitoring_location_id  = as.character(if ("MonitoringLocationIdentifier" %in% names(raw)) MonitoringLocationIdentifier else NA),
      monitoring_location_name = as.character(if ("MonitoringLocationName" %in% names(raw)) MonitoringLocationName else NA),
      location_type           = as.character(if ("MonitoringLocationTypeName" %in% names(raw)) MonitoringLocationTypeName else NA),
      latitude                = as.numeric(if ("LatitudeMeasure" %in% names(raw)) LatitudeMeasure else NA),
      longitude               = as.numeric(if ("LongitudeMeasure" %in% names(raw)) LongitudeMeasure else NA),
      state_code              = as.character(if ("StateCode" %in% names(raw)) StateCode else NA),
      county_code             = as.character(if ("CountyCode" %in% names(raw)) CountyCode else NA),
      huc_code                = as.character(if ("HUCEightDigitCode" %in% names(raw)) HUCEightDigitCode else NA)
    )
}

# == Characteristics lookup =====================================================

#' List available characteristics (parameters) for a monitoring location
#'
#' Returns a summary of which water quality parameters have been measured
#' at a given site, along with the count of results for each. Useful
#' for discovering what data is available before calling \code{\link{wqp_results}}.
#'
#' @param siteid Character monitoring location ID
#'   (e.g., \code{"USGS-01646500"}).
#' @param top Integer maximum records to consider (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{characteristic}{Parameter name (e.g., "Temperature, water", "pH")}
#'     \item{result_count}{Integer count of results for this parameter}
#'   }
#' @export
#' @family WQP functions
#' @seealso \code{\link{wqp_results}} to fetch actual measurement values
#' @examples
#' \dontrun{
#' wqp_characteristics("USGS-01646500")
#' }
wqp_characteristics <- function(siteid, top = 100) {
  schema <- tibble(characteristic = character(), result_count = integer())
  params <- list(mimeType = "csv", zip = "no", sorted = "no",
                 siteid = siteid, top = as.integer(top),
                 dataProfile = "resultPhysChem")
  query <- paste(names(params), sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)), sep = "=", collapse = "&")
  url <- paste0(.wqp_base, "/data/Result/search?", query)
  f <- tryCatch(.fetch_csv(url), error = function(e) NULL)
  if (is.null(f)) return(schema)
  raw <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) data.frame())
  if (nrow(raw) == 0 || !("CharacteristicName" %in% names(raw))) return(schema)

  as_tibble(raw) |>
    group_by(characteristic = as.character(CharacteristicName)) |>
    summarise(result_count = n(), .groups = "drop") |>
    arrange(desc(result_count))
}

#' Fetch detailed metadata for a single monitoring station
#'
#' Returns the same columns as \code{\link{wqp_stations}} but for a
#' single site identified by its monitoring location ID.
#'
#' @param siteid Character monitoring location ID
#'   (e.g., \code{"USGS-01646500"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{organization}{Reporting organization}
#'     \item{monitoring_location_id}{Station identifier}
#'     \item{monitoring_location_name}{Human-readable name}
#'     \item{location_type}{Type of monitoring location}
#'     \item{latitude}{Numeric latitude}
#'     \item{longitude}{Numeric longitude}
#'     \item{state_code}{FIPS state code}
#'     \item{county_code}{FIPS county code}
#'     \item{huc_code}{Eight-digit Hydrologic Unit Code}
#'   }
#' @export
#' @family WQP functions
#' @seealso \code{\link{wqp_stations}} to search by area,
#'   \code{\link{wqp_characteristics}} to see what is measured at a site
#' @examples
#' \dontrun{
#' wqp_station_detail("USGS-01646500")
#' }
wqp_station_detail <- function(siteid) {
  params <- list(mimeType = "csv", zip = "no", sorted = "no",
                 siteid = siteid, top = 1L)
  query <- paste(names(params), sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)), sep = "=", collapse = "&")
  url <- paste0(.wqp_base, "/data/Station/search?", query)
  f <- tryCatch(.fetch_csv(url), error = function(e) NULL)
  if (is.null(f)) return(.schema_stations)
  raw <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) data.frame())
  if (nrow(raw) == 0) return(.schema_stations)

  as_tibble(raw) |>
    transmute(
      organization            = as.character(if ("OrganizationIdentifier" %in% names(raw)) OrganizationIdentifier else NA),
      monitoring_location_id  = as.character(if ("MonitoringLocationIdentifier" %in% names(raw)) MonitoringLocationIdentifier else NA),
      monitoring_location_name = as.character(if ("MonitoringLocationName" %in% names(raw)) MonitoringLocationName else NA),
      location_type           = as.character(if ("MonitoringLocationTypeName" %in% names(raw)) MonitoringLocationTypeName else NA),
      latitude                = as.numeric(if ("LatitudeMeasure" %in% names(raw)) LatitudeMeasure else NA),
      longitude               = as.numeric(if ("LongitudeMeasure" %in% names(raw)) LongitudeMeasure else NA),
      state_code              = as.character(if ("StateCode" %in% names(raw)) StateCode else NA),
      county_code             = as.character(if ("CountyCode" %in% names(raw)) CountyCode else NA),
      huc_code                = as.character(if ("HUCEightDigitCode" %in% names(raw)) HUCEightDigitCode else NA)
    )
}

# == Context ===================================================================

#' Get waterqualitydata.us client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wqp_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wqp_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/waterqualitydata.us.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "waterqualitydata.us")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# waterqualitydata.us context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# waterqualitydata.us", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
