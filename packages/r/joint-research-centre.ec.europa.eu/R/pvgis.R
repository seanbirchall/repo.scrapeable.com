# joint-research-centre.ec.europa.eu.R - Self-contained joint-research-centre.ec.europa.eu client


`%||%` <- function(a, b) if (is.null(a)) b else a

.ua <- "support@scrapeable.com"
.pvgis_base <- "https://re.jrc.ec.europa.eu/api/v5_3"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

#' Get monthly PV energy output estimates
#'
#' Calculates monthly photovoltaic energy output estimates for a given
#' location using the EU Joint Research Centre PVGIS (Photovoltaic
#' Geographical Information System) API v5.3. Based on satellite-derived
#' solar radiation data and PV system modeling.
#'
#' @param lat Latitude in decimal degrees (e.g. 52.5 for Berlin, 40.7 for NYC).
#' @param lon Longitude in decimal degrees (e.g. 13.4 for Berlin, -74.0 for NYC).
#' @param peakpower Peak power of the PV system in kW (default 1). Scales
#'   output proportionally.
#' @return A tibble with 12 rows (one per month) and columns:
#'   \describe{
#'     \item{month}{Month number (1-12)}
#'     \item{E_m}{Monthly energy output in kWh}
#'     \item{H_m}{Monthly in-plane irradiation in kWh/m2 (may be NA)}
#'     \item{SD_m}{Standard deviation of monthly energy output}
#'   }
#' @examples
#' pvgis_monthly(52.5, 13.4)
#' pvgis_monthly(40.7, -74.0, peakpower = 5)
#' @seealso [pvgis_radiation()], [pvgis_optimal()], [pvgis_context()]
#' @source <https://re.jrc.ec.europa.eu/pvg_tools/en/>
pvgis_monthly <- function(lat, lon, peakpower = 1) {
  url <- sprintf("%s/PVcalc?lat=%f&lon=%f&peakpower=%f&loss=14&outputformat=json", .pvgis_base, lat, lon, peakpower)
  raw <- .fetch_json(url)
  months <- raw$outputs$monthly$fixed
  if (length(months) == 0) return(tibble::tibble(month = integer(), E_m = numeric()))
  tibble::tibble(
    month = vapply(months, function(x) x$month %||% NA_integer_, integer(1)),
    E_m = vapply(months, function(x) as.numeric(x$E_m %||% NA), numeric(1)),
    H_m = vapply(months, function(x) as.numeric(x$H_m %||% NA), numeric(1)),
    SD_m = vapply(months, function(x) as.numeric(x$SD_m %||% NA), numeric(1))
  )
}

#' Get solar radiation data
#'
#' Retrieves monthly average solar radiation (horizontal global irradiation)
#' for a given location from the PVGIS API. Useful for assessing solar
#' energy potential without specifying a PV system configuration.
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @return A tibble with 12 rows (one per month) and columns:
#'   \describe{
#'     \item{month}{Month number (1-12)}
#'     \item{H_sun}{Monthly horizontal global irradiation in kWh/m2}
#'   }
#' @examples
#' pvgis_radiation(48.9, 2.3)
#' @seealso [pvgis_monthly()], [pvgis_optimal()], [pvgis_context()]
#' @source <https://re.jrc.ec.europa.eu/pvg_tools/en/>
pvgis_radiation <- function(lat, lon) {
  url <- sprintf("%s/MRcalc?lat=%f&lon=%f&outputformat=json", .pvgis_base, lat, lon)
  raw <- .fetch_json(url)
  months <- raw$outputs$monthly
  if (length(months) == 0) return(tibble::tibble(month = integer(), H_sun = numeric()))
  tibble::tibble(
    month = vapply(months, function(x) x$month %||% NA_integer_, integer(1)),
    H_sun = vapply(months, function(x) as.numeric(x$H_sun %||% NA), numeric(1))
  )
}

#' Get optimal PV system angle and yearly output
#'
#' Calculates the optimal tilt angle and azimuth for a fixed-mount PV system
#' at a given location, along with the resulting yearly energy output and
#' total irradiation. Uses the PVGIS optimal angle optimization feature.
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param peakpower Peak power of the PV system in kW (default 1).
#' @param loss System losses as a percentage (default 14). Accounts for
#'   inverter losses, cable losses, dust, temperature effects, etc.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{optimal_angle}{Optimal tilt angle in degrees from horizontal}
#'     \item{optimal_azimuth}{Optimal azimuth in degrees (0 = south in Northern Hemisphere)}
#'     \item{yearly_energy_kwh}{Estimated total annual energy output in kWh}
#'     \item{yearly_irradiation_kwh_m2}{Total annual in-plane irradiation in kWh/m2}
#'   }
#' @examples
#' pvgis_optimal(52.5, 13.4)
#' pvgis_optimal(35.7, 139.7, peakpower = 10, loss = 10)
#' @seealso [pvgis_monthly()], [pvgis_radiation()], [pvgis_context()]
#' @source <https://re.jrc.ec.europa.eu/pvg_tools/en/>
pvgis_optimal <- function(lat, lon, peakpower = 1, loss = 14) {
  schema <- tibble(optimal_angle = numeric(), optimal_azimuth = numeric(),
                   yearly_energy_kwh = numeric(), yearly_irradiation_kwh_m2 = numeric())
  url <- sprintf("%s/PVcalc?lat=%f&lon=%f&peakpower=%f&loss=%d&outputformat=json&optimalangles=1",
                 .pvgis_base, lat, lon, peakpower, loss)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)
  inp <- raw$inputs$mounting_system$fixed$slope
  azm <- raw$inputs$mounting_system$fixed$azimuth
  totals <- raw$outputs$totals$fixed
  tibble(
    optimal_angle = as.numeric(inp$value %||% NA),
    optimal_azimuth = as.numeric(azm$value %||% NA),
    yearly_energy_kwh = as.numeric(totals$E_y %||% NA),
    yearly_irradiation_kwh_m2 = as.numeric(totals$`H(i)_y` %||% NA)
  )
}

# == Context ===================================================================

#' Get joint-research-centre.ec.europa.eu client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the PVGIS solar energy client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' pvgis_context()
#' @export
pvgis_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pvgis_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/joint-research-centre.ec.europa.eu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "joint-research-centre.ec.europa.eu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# joint-research-centre.ec.europa.eu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# joint-research-centre.ec.europa.eu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
