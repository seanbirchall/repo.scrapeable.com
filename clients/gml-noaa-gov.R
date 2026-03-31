# gml-noaa-gov.R
# Self-contained NOAA GML CO2/CH4 trends client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: none - static CSV files

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gml_base <- "https://gml.noaa.gov/webdata/ccgg/trends"

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
#' Smoothed and trend values from NOAA GML. Updated daily.
#'
#' @return tibble: date (Date), smoothed (numeric ppm), trend (numeric ppm)
#' @export
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
#' @return tibble: year (integer), mean (numeric ppm), unc (numeric)
#' @export
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
#' @return tibble: year, month, decimal, average, average_unc, trend, trend_unc
#' @export
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
#' @return tibble: year, month, decimal, average, average_unc, trend, trend_unc
#' @export
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

#' NOAA GML context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
co2_context <- function() {
  .build_context("gml.noaa.gov", header_lines = c(
    "# gml.noaa.gov - NOAA Global Monitoring Lab CO2/CH4 Client",
    "# Deps: httr2, dplyr, tibble",
    "# Auth: none",
    "# Data: static CSV files updated daily/monthly",
    "#",
    "# CO2 measured in ppm, CH4 in ppb"
  ))
}
