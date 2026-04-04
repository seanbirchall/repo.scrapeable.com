# usaspending.R
# USA Spending federal award data client.
# All functions return data.frames. No database dependencies.
#
# Data source: https://files.usaspending.gov/award_data_archive/

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

.usas_base <- "https://files.usaspending.gov/award_data_archive"

.usas_download_zip <- function(url) {
  tmp <- http_get(url, ext = ".zip")
  files <- utils::unzip(tmp, list = TRUE)
  csv_file <- files$Name[grepl("\\.csv$", files$Name)][1]
  if (is.na(csv_file)) return(data.frame())
  read.csv(unz(tmp, csv_file), stringsAsFactors = FALSE)
}


# == Assistance awards =========================================================

#' Fetch USA Spending assistance award data for a fiscal year
#'
#' Downloads federal assistance (grants, subsidies, loans) award data
#' for the given fiscal year.
#'
#' @param year Fiscal year (e.g. 2023)
#' @param date_stamp Month/day stamp for the archive file (default: "0108" for
#'   January 8th). The archive files are dated; this may need adjustment.
#' @return data.frame of assistance award records
usas_assistance <- function(year, date_stamp = "0108") {
  url <- paste0(.usas_base, "/FY", year, "_All_Assistance_Full_", year, date_stamp, ".zip")
  .usas_download_zip(url)
}


# == Contract awards ===========================================================

#' Fetch USA Spending contract award data for a fiscal year
#'
#' Downloads federal procurement contract award data for the given fiscal year.
#'
#' @param year Fiscal year (e.g. 2023)
#' @param date_stamp Month/day stamp for the archive file
#' @return data.frame of contract award records
usas_contracts <- function(year, date_stamp = "0108") {
  url <- paste0(.usas_base, "/FY", year, "_All_Contracts_Full_", year, date_stamp, ".zip")
  .usas_download_zip(url)
}


# == Multi-year convenience ====================================================

#' Fetch assistance data for a range of years
#'
#' @param years Integer vector of fiscal years
#' @param date_stamp Month/day stamp
#' @return data.frame (all years combined)
usas_assistance_bulk <- function(years, date_stamp = "0108") {
  results <- lapply(years, function(y) {
    tryCatch({
      message(sprintf("Downloading FY%d assistance...", y))
      usas_assistance(y, date_stamp)
    }, error = function(e) {
      message(sprintf("Failed FY%d: %s", y, e$message))
      NULL
    })
  })
  do.call(rbind, Filter(Negate(is.null), results))
}

#' Fetch contract data for a range of years
#'
#' @param years Integer vector of fiscal years
#' @param date_stamp Month/day stamp
#' @return data.frame (all years combined)
usas_contracts_bulk <- function(years, date_stamp = "0108") {
  results <- lapply(years, function(y) {
    tryCatch({
      message(sprintf("Downloading FY%d contracts...", y))
      usas_contracts(y, date_stamp)
    }, error = function(e) {
      message(sprintf("Failed FY%d: %s", y, e$message))
      NULL
    })
  })
  do.call(rbind, Filter(Negate(is.null), results))
}
