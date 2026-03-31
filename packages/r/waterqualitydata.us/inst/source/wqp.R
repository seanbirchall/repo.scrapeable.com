# waterqualitydata-us.R
# Self-contained Water Quality Portal client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: be polite - large queries can be slow

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wqp_base <- "https://www.waterqualitydata.us"

`%||%` <- function(a, b) if (is.null(a)) b else a

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
#' @param countycode County FIPS code (e.g. "US:06:037" for LA County)
#' @param statecode State FIPS code (e.g. "US:06" for California)
#' @param siteid Monitoring location ID
#' @param characteristicName Filter by characteristic (e.g. "Temperature, water")
#' @param startDateLo Start date "MM-DD-YYYY"
#' @param startDateHi End date "MM-DD-YYYY"
#' @param dataProfile Data profile: "resultPhysChem" (default), "biological", "narrowResult"
#' @param top Max number of records (default 100)
#' @return tibble: organization, activity_id, activity_type, activity_date,
#'   monitoring_location, characteristic, result_value, result_unit,
#'   result_status, method_id
#' @export
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
#' @param countycode County FIPS code (e.g. "US:06:037")
#' @param statecode State FIPS code (e.g. "US:06")
#' @param top Max number of records (default 100)
#' @return tibble: organization, monitoring_location_id, monitoring_location_name,
#'   location_type, latitude, longitude, state_code, county_code, huc_code
#' @export
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

#' Water Quality Portal context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
wqp_context <- function() {
  .build_context("waterqualitydata.us", header_lines = c(
    "# waterqualitydata.us - Water Quality Portal Client",
    "# Deps: httr2, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite, large queries can be slow",
    "#",
    "# County codes: US:SS:CCC (e.g. US:06:037 = LA County, CA)",
    "# State codes: US:SS (e.g. US:06 = California)",
    "# Data profiles: resultPhysChem, biological, narrowResult"
  ))
}
