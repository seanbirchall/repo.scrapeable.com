# gml.noaa.gov.R - Self-contained gml.noaa.gov client

library(httr2)
library(tibble)
library(dplyr)


# gml-noaa-gov.R
# Self-contained NOAA GML CO2/CH4 trends client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: none - static CSV files


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gml_base <- "https://gml.noaa.gov/webdata/ccgg/trends"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_trend <- tibble(
  date = as.Date(character()), smoothed = numeric(), trend = numeric()
)

.schema_annual <- tibble(
  year = integer(), mean = numeric(), unc = numeric()
)

.schema_monthly <- tibble(
  year = integer(), month = integer(), decimal = numeric(),
  average = numeric(), average_unc = numeric(),
  trend = numeric(), trend_unc = numeric()
)

# == CO2 data ==================================================================


#' Fetch daily global CO2 trend data
#'
#' Download the daily globally-averaged marine surface CO2 trend from NOAA's
#' Global Monitoring Laboratory (GML). Contains smoothed and trend values in
#' parts per million (ppm). Updated daily with a few days' lag.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{smoothed}{Numeric. Smoothed daily CO2 value in ppm.}
#'     \item{trend}{Numeric. Long-term trend CO2 value in ppm.}
#'   }
#' @export
#' @family co2 functions
#' @seealso [co2_annual()] for yearly averages, [co2_monthly()] for monthly data
#' @examples
#' \dontrun{
#' co2_global_trend()
#' }
co2_global_trend <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_trend_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_trend)
  raw |>
    as_tibble() |>
    transmute(
      date     = as.Date(sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))),
      smoothed = as.numeric(smoothed),
      trend    = as.numeric(trend)
    )
}

#' Fetch annual global mean CO2
#'
#' Download the annual globally-averaged marine surface CO2 concentration
#' from NOAA GML. Data begins in 1979 and is updated once per year.
#' Includes uncertainty estimates.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year.}
#'     \item{mean}{Numeric. Annual mean CO2 concentration in ppm.}
#'     \item{unc}{Numeric. Uncertainty of the annual mean in ppm.}
#'   }
#' @export
#' @family co2 functions
#' @seealso [co2_monthly()] for finer temporal resolution
#' @examples
#' \dontrun{
#' co2_annual()
#' }
co2_annual <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_annmean_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_annual)
  raw |>
    as_tibble() |>
    transmute(
      year = as.integer(year),
      mean = as.numeric(mean),
      unc  = as.numeric(unc)
    )
}

#' Fetch monthly global CO2 data
#'
#' Download the monthly globally-averaged marine surface CO2 concentration
#' from NOAA GML. Includes both the seasonal average and the de-seasonalized
#' trend, each with uncertainty estimates. Data begins in 1979.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year.}
#'     \item{month}{Integer. Month (1-12).}
#'     \item{decimal}{Numeric. Decimal date (e.g. 2024.042).}
#'     \item{average}{Numeric. Monthly mean CO2 in ppm.}
#'     \item{average_unc}{Numeric. Uncertainty of the monthly mean.}
#'     \item{trend}{Numeric. De-seasonalized trend value in ppm.}
#'     \item{trend_unc}{Numeric. Uncertainty of the trend value.}
#'   }
#' @export
#' @family co2 functions
#' @seealso [co2_annual()] for yearly averages, [co2_ch4_monthly()] for methane
#' @examples
#' \dontrun{
#' co2_monthly()
#' }
co2_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}

#' Fetch monthly global CH4 (methane) data
#'
#' Download the monthly globally-averaged marine surface methane (CH4)
#' concentration from NOAA GML. Methane is the second most important
#' greenhouse gas after CO2. Values are in parts per billion (ppb).
#' Data begins in 1983.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year.}
#'     \item{month}{Integer. Month (1-12).}
#'     \item{decimal}{Numeric. Decimal date.}
#'     \item{average}{Numeric. Monthly mean CH4 in ppb.}
#'     \item{average_unc}{Numeric. Uncertainty of the monthly mean.}
#'     \item{trend}{Numeric. De-seasonalized trend value in ppb.}
#'     \item{trend_unc}{Numeric. Uncertainty of the trend value.}
#'   }
#' @export
#' @family co2 functions
#' @seealso [co2_monthly()] for CO2 data
#' @examples
#' \dontrun{
#' co2_ch4_monthly()
#' }
co2_ch4_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/ch4/ch4_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  # ch4 csv may have blank rows
  raw <- raw[!is.na(raw$year) & raw$year != "", ]
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}

# == Context ===================================================================

#' Get gml.noaa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
co2_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(co2_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gml.noaa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gml.noaa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gml.noaa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gml.noaa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
