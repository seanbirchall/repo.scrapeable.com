# re-jrc-ec-europa-eu.R
# Self-contained PVGIS (Photovoltaic Geographical Information System) client.
# European Commission Joint Research Centre solar radiation API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pvgis_base <- "https://re.jrc.ec.europa.eu/api/v5_3"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

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
#' @return A tibble with columns:
#'   \describe{
#'     \item{month}{Month number 1-12 (integer)}
#'     \item{energy_daily_kwh}{Average daily energy output in kWh (numeric)}
#'     \item{energy_monthly_kwh}{Total monthly energy output in kWh (numeric)}
#'     \item{irradiation_daily}{Daily solar irradiation on plane in kWh/m2 (numeric)}
#'     \item{irradiation_monthly}{Monthly solar irradiation in kWh/m2 (numeric)}
#'     \item{sd_monthly}{Standard deviation of monthly production (numeric)}
#'   }
#' @examples
#' pvgis_pv(48.2, 16.4)
#' pvgis_pv(40.7, -74.0, peakpower = 5, loss = 10)
#' @seealso [pvgis_radiation()], [pvgis_context()]
#' @source <https://re.jrc.ec.europa.eu/api/v5_3>
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
#' @return A tibble with columns:
#'   \describe{
#'     \item{month}{Month number 1-12 (integer)}
#'     \item{global_horiz}{Global horizontal irradiation in kWh/m2/month (numeric)}
#'     \item{direct_normal}{Direct normal irradiation in kWh/m2/month (numeric)}
#'     \item{diffuse_horiz}{Diffuse horizontal irradiation in kWh/m2/month (numeric)}
#'     \item{global_optimum}{Global irradiation at optimal angle in kWh/m2/month (numeric)}
#'   }
#' @examples
#' pvgis_radiation(48.2, 16.4)
#' pvgis_radiation(35.7, 139.7, angle = 30)
#' @seealso [pvgis_pv()], [pvgis_context()]
#' @source <https://re.jrc.ec.europa.eu/api/v5_3>
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

#' Get re-jrc-ec-europa-eu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
pvgis_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(pvgis_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/re-jrc-ec-europa-eu.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "re-jrc-ec-europa-eu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# re-jrc-ec-europa-eu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# re-jrc-ec-europa-eu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
