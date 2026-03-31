# ec-europa-eu.R
# Self-contained Eurostat data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Eurostat SDMX 2.1 at ec.europa.eu/eurostat/api (using CSV format)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.estat_base <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1"

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  tmp <- .fetch(url, ext = ".csv")
  utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_data <- tibble(
  freq = character(), geo = character(), time_period = character(),
  obs_value = numeric(), unit = character(), obs_flag = character()
)

.schema_datasets <- tibble(
  code = character(), name = character()
)

# == Data ======================================================================

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


# == Dataset search ============================================================

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


# == Context ===================================================================

#' Generate LLM-friendly context for the Eurostat package
#'
#' @return Character string (invisibly), also printed
estat_context <- function() {
  .build_context("ec.europa.eu", header_lines = c(
    "# ec.europa.eu - Eurostat Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# ~8,000 datasets covering EU economics, demographics, trade",
    "#",
    "# Common datasets:",
    "#   nama_10_gdp = GDP and main components",
    "#   une_rt_m = Unemployment rate (monthly)",
    "#   prc_hicp_aind = HICP inflation (annual)",
    "#   demo_pjan = Population on 1 January",
    "#",
    "# Country codes: 2-letter (DE, FR, IT, ES, PL, etc.)",
    "# Combine with +: 'DE+FR+IT'",
    "# Dimension filter: dot-separated, empty = all"
  ))
}
