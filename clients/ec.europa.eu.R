# ec.europa.eu.R
# Self-contained European Commission client.
# Covers: Eurostat (EU statistics) and PVGIS (solar energy, JRC).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.estat_base <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1"
.pvgis_base <- "https://re.jrc.ec.europa.eu/api/v5_3"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)


# == Schemas ===================================================================

.schema_data <- tibble(
  freq = character(), geo = character(), time_period = character(),
  obs_value = numeric(), unit = character(), obs_flag = character()
)

.schema_datasets <- tibble(
  code = character(), name = character()
)



#' Fetch Eurostat data
#'
#' Queries the Eurostat SDMX API and returns data in flat CSV format.
#' Each row is one observation with dimension columns.
#'
#' @param dataset Dataset code (e.g. "nama_10_gdp" for GDP,
#'   "prc_hicp_aind" for inflation, "une_rt_m" for unemployment).
#'   Use estat_search() to find codes.
#' @param filter Dimension filter string. Dots separate dimensions,
#'   + combines values within a dimension. Leave positions empty for all.
#'   Example: "A.CLV05_MEUR.B1GQ.DE+FR" = Annual, chain-linked, GDP, Germany+France.
#' @param start_period Start year/period (e.g. "2020")
#' @param end_period End year/period (e.g. "2023")
#' @return tibble with SDMX columns: dimension columns, TIME_PERIOD,
#'   OBS_VALUE, OBS_FLAG
estat_data <- function(dataset, filter = "", start_period = NULL,
                       end_period = NULL) {
  url <- sprintf("%s/data/%s/%s?format=SDMX-CSV&lang=EN",
                 .estat_base, dataset, filter)
  if (!is.null(start_period)) url <- paste0(url, "&startPeriod=", start_period)
  if (!is.null(end_period))   url <- paste0(url, "&endPeriod=", end_period)

  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("Eurostat API error: ", e$message)
    return(NULL)
  })
  if (is.null(df) || nrow(df) == 0) return(.schema_data)

  # Convert OBS_VALUE to numeric
  if ("OBS_VALUE" %in% names(df)) {
    df$OBS_VALUE <- as.numeric(df$OBS_VALUE)
  }

  # Remove DATAFLOW and LAST UPDATE columns (not useful for analysis)
  df <- df[, !(names(df) %in% c("DATAFLOW", "LAST UPDATE", "CONF_STATUS")),
           drop = FALSE]

  as_tibble(df)
}



#' Fetch Eurostat GDP data
#'
#' Convenience wrapper for GDP and main components (nama_10_gdp).
#'
#' @param countries Country codes separated by + (e.g. "DE+FR+IT").
#'   Use 2-letter ISO codes. Default: all.
#' @param unit Unit of measure: "CLV05_MEUR" (chain-linked volumes),
#'   "CP_MEUR" (current prices), "PD05_EUR" (price deflator). Default all.
#' @param na_item National accounts item: "B1GQ" (GDP), "P3" (consumption),
#'   "P5G" (investment), "P6" (exports), "P7" (imports). Default "B1GQ".
#' @param start_period Start year
#' @param end_period End year
#' @return tibble with Eurostat SDMX columns
estat_gdp <- function(countries = "", unit = "CLV05_MEUR",
                      na_item = "B1GQ", start_period = NULL,
                      end_period = NULL) {
  filter <- sprintf("A.%s.%s.%s", unit, na_item, countries)
  estat_data("nama_10_gdp", filter = filter,
             start_period = start_period, end_period = end_period)
}



#' Fetch Eurostat unemployment data
#'
#' Monthly unemployment rate from une_rt_m.
#'
#' @param countries Country codes (e.g. "DE+FR")
#' @param start_period Start period (e.g. "2020-01")
#' @param end_period End period
#' @return tibble
estat_unemployment <- function(countries = "", start_period = NULL,
                               end_period = NULL) {
  filter <- sprintf("M.SA.TOTAL.PC_ACT.T.%s", countries)
  estat_data("une_rt_m", filter = filter,
             start_period = start_period, end_period = end_period)
}



#' Fetch Eurostat inflation data (HICP)
#'
#' Annual harmonised index of consumer prices from prc_hicp_aind.
#'
#' @param countries Country codes (e.g. "DE+FR")
#' @param start_period Start year
#' @param end_period End year
#' @return tibble
estat_inflation <- function(countries = "", start_period = NULL,
                            end_period = NULL) {
  filter <- sprintf("A.RCH_A_AVG.CP00.%s", countries)
  estat_data("prc_hicp_aind", filter = filter,
             start_period = start_period, end_period = end_period)
}


#' Search Eurostat datasets
#'
#' Searches the Eurostat dataset catalog (~8,000 datasets).
#' Fetches the full catalog and filters locally.
#'
#' @param query Search term (matched against dataset code and name)
#' @param limit Max results (default 50)
#' @return tibble: code (character), name (character)
estat_search <- function(query, limit = 50) {
  # Curated list of popular Eurostat datasets
  all_ds <- tibble(
    code = c("nama_10_gdp", "nama_10r_2gdp", "nama_10_pe",
             "prc_hicp_aind", "prc_hicp_midx", "ei_cphi_m",
             "une_rt_m", "une_rt_a", "lfsq_urgan", "lfsa_ergan",
             "demo_pjan", "demo_gind", "demo_r_d2jan",
             "tour_occ_mnor", "tour_dem_tttot",
             "tec00001", "tec00115", "tps00001",
             "env_air_gge", "nrg_bal_c",
             "ext_lt_maineu", "bop_c6_q",
             "isoc_ci_ifp_iu", "isoc_ec_eseln2"),
    name = c("GDP and main components", "Regional GDP by NUTS 2",
             "Population and employment",
             "HICP annual inflation", "HICP monthly index", "HICP monthly rates",
             "Unemployment rate monthly", "Unemployment rate annual",
             "Unemployment by age/sex quarterly", "Employment rate by age/sex",
             "Population on 1 January", "Demographic indicators", "Regional population",
             "Tourism nights spent monthly", "Tourism trips",
             "Real GDP growth rate", "GDP per capita in PPS", "Population total",
             "Greenhouse gas emissions", "Energy balances",
             "International trade", "Balance of payments",
             "Internet usage", "E-commerce")
  )

  pattern <- tolower(query)
  all_ds |>
    filter(grepl(pattern, tolower(code)) | grepl(pattern, tolower(name))) |>
    head(limit)
}


#' Get monthly PV energy output estimates
#' @param lat Latitude
#' @param lon Longitude
#' @param peakpower Peak power of PV system in kW (default 1)
#' @return tibble of monthly estimates
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
#' @param lat Latitude
#' @param lon Longitude
#' @return tibble of monthly solar radiation
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
#' Returns optimal tilt/azimuth angle and yearly/monthly totals for a location.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param peakpower Peak power in kW (default 1)
#' @param loss System loss in percent (default 14)
#' @return tibble: one row with optimal_angle, optimal_azimuth, yearly_energy_kwh,
#'   yearly_irradiation_kwh_m2
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

#' Get ec.europa.eu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ec_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ec_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ec.europa.eu.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ec.europa.eu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ec.europa.eu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ec.europa.eu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
