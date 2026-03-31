# == Schemas ===================================================================

.schema_pv_monthly <- tibble(
  month = integer(), energy_daily_kwh = numeric(), energy_monthly_kwh = numeric(),
  irradiation_daily = numeric(), irradiation_monthly = numeric(), sd_monthly = numeric()
)

.schema_radiation <- tibble(
  month = integer(), global_horiz = numeric(), direct_normal = numeric(),
  diffuse_horiz = numeric(), global_optimum = numeric()
)

# == PV calculation ============================================================

#' Calculate photovoltaic energy output
#'
#' Estimates monthly and annual energy production from a grid-connected PV system
#' at a given location using PVGIS satellite-derived solar radiation data.
#'
#' @param lat Latitude in decimal degrees (-90 to 90)
#' @param lon Longitude in decimal degrees (-180 to 180)
#' @param peakpower Installed peak PV power in kWp (default 1)
#' @param loss System losses in percent (default 14)
#' @param angle PV module tilt angle in degrees (default: optimal calculated by PVGIS)
#' @param aspect PV module azimuth: 0=south, 90=west, -90=east (default: 0)
#' @return tibble: month, energy_daily_kwh, energy_monthly_kwh,
#'   irradiation_daily, irradiation_monthly, sd_monthly
#' @export
pvgis_pv <- function(lat, lon, peakpower = 1, loss = 14, angle = NULL, aspect = NULL) {
  params <- sprintf("lat=%s&lon=%s&peakpower=%s&loss=%s&outputformat=json",
                    lat, lon, peakpower, loss)
  if (!is.null(angle)) params <- paste0(params, "&angle=", angle)
  if (!is.null(aspect)) params <- paste0(params, "&aspect=", aspect)
  url <- paste0(.pvgis_base, "/PVcalc?", params)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("PVGIS PV calc failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_pv_monthly)

  monthly <- raw$outputs$monthly$fixed
  if (is.null(monthly) || length(monthly) == 0) return(.schema_pv_monthly)

  as_tibble(monthly) |>
    transmute(
      month               = as.integer(month),
      energy_daily_kwh    = as.numeric(E_d),
      energy_monthly_kwh  = as.numeric(E_m),
      irradiation_daily   = as.numeric(`H(i)_d`),
      irradiation_monthly = as.numeric(`H(i)_m`),
      sd_monthly          = as.numeric(SD_m)
    )
}

# == Solar radiation ===========================================================

#' Get monthly solar radiation data
#'
#' Returns monthly average solar radiation components for a location,
#' including global horizontal, direct normal, and diffuse radiation.
#'
#' @param lat Latitude in decimal degrees
#' @param lon Longitude in decimal degrees
#' @param angle Tilt angle in degrees (default: 0 = horizontal)
#' @return tibble: month, global_horiz, direct_normal, diffuse_horiz,
#'   global_optimum (all in kWh/m2/day)
#' @export
pvgis_radiation <- function(lat, lon, angle = 0) {
  url <- sprintf("%s/MRcalc?lat=%s&lon=%s&horirrad=1&optrad=1&mr_dni=1&d2glob=1&angle=%s&outputformat=json",
                 .pvgis_base, lat, lon, angle)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("PVGIS radiation calc failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_radiation)

  monthly <- raw$outputs$monthly
  if (is.null(monthly) || length(monthly) == 0) return(.schema_radiation)

  cols <- names(monthly)
  as_tibble(monthly) |>
    transmute(
      month          = as.integer(month),
      global_horiz   = as.numeric(if ("H(h)_m" %in% cols) `H(h)_m` else NA_real_),
      direct_normal  = as.numeric(if ("Hb(n)_m" %in% cols) `Hb(n)_m` else NA_real_),
      diffuse_horiz  = as.numeric(if ("Hd(h)_m" %in% cols) `Hd(h)_m` else NA_real_),
      global_optimum = as.numeric(if ("H(i_opt)_m" %in% cols) `H(i_opt)_m` else NA_real_)
    )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the re.jrc.ec.europa.eu package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
#' @export
pvgis_context <- function() {
  .build_context("re.jrc.ec.europa.eu", header_lines = c(
    "# re.jrc.ec.europa.eu - PVGIS Solar Energy API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Coverage: Europe, Africa, large parts of Asia and Americas",
    "# Data: satellite-derived solar radiation (PVGIS-ERA5)",
    "# All functions return tibbles with typed columns."
  ))
}
