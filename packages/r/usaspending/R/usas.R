# usaspending.R
# USA Spending federal award data client.
# All functions return data.frames. No database dependencies.
#
# Data source: https://files.usaspending.gov/award_data_archive/

# == Setup =====================================================================



# -- HTTP helpers (inlined) ----------------------------------------------------
http_get <- function(url, ext = ".html", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

http_post <- function(url, body, ext = ".zip", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  tmp
}

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
#' Downloads the full federal assistance (grants, subsidies, loans, direct
#' payments) award data archive from USAspending.gov for a given fiscal year.
#' Files are large (hundreds of MB compressed). Returns a data.frame with
#' hundreds of columns following the DAIMS schema.
#'
#' @param year Fiscal year as an integer (e.g. \code{2023}, \code{2024}).
#' @param date_stamp Month/day stamp matching the archive file on the server
#'   (default \code{"0108"} for January 8th). USAspending publishes monthly
#'   snapshots; adjust this to match the available file.
#' @return A data.frame of assistance award records. Columns follow the DAIMS
#'   (DATA Act Information Model Schema) and include fields like
#'   \code{award_id_fain}, \code{recipient_name}, \code{total_obligated_amount},
#'   \code{awarding_agency_name}, \code{primary_place_of_performance_state_name},
#'   and many more.
#' @examples
#' \dontrun{
#' usas_assistance(2023)
#' }
#' @export
usas_assistance <- function(year, date_stamp = "0108") {
  url <- paste0(.usas_base, "/FY", year, "_All_Assistance_Full_", year, date_stamp, ".zip")
  .usas_download_zip(url)
}


# == Contract awards ===========================================================

#' Fetch USA Spending contract award data for a fiscal year
#'
#' Downloads the full federal procurement contract award data archive from
#' USAspending.gov for a given fiscal year. Files are large (hundreds of MB
#' compressed). Returns a data.frame with hundreds of columns following the
#' DAIMS schema.
#'
#' @param year Fiscal year as an integer (e.g. \code{2023}, \code{2024}).
#' @param date_stamp Month/day stamp matching the archive file on the server
#'   (default \code{"0108"}).
#' @return A data.frame of contract award records. Columns follow the DAIMS
#'   schema and include fields like \code{contract_award_unique_key},
#'   \code{recipient_name}, \code{total_dollars_obligated},
#'   \code{awarding_agency_name}, \code{product_or_service_description}, etc.
#' @examples
#' \dontrun{
#' usas_contracts(2023)
#' }
#' @export
usas_contracts <- function(year, date_stamp = "0108") {
  url <- paste0(.usas_base, "/FY", year, "_All_Contracts_Full_", year, date_stamp, ".zip")
  .usas_download_zip(url)
}


# == Multi-year convenience ====================================================

#' Fetch assistance data for a range of fiscal years
#'
#' Convenience wrapper that downloads and row-binds federal assistance award
#' data for multiple fiscal years. Prints progress messages. Skips years
#' that fail to download.
#'
#' @param years Integer vector of fiscal years (e.g. \code{2020:2023}).
#' @param date_stamp Month/day stamp for the archive files (default
#'   \code{"0108"}).
#' @return A combined data.frame of all successfully downloaded years.
#' @examples
#' \dontrun{
#' usas_assistance_bulk(2021:2023)
#' }
#' @export
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

#' Fetch contract data for a range of fiscal years
#'
#' Convenience wrapper that downloads and row-binds federal contract award
#' data for multiple fiscal years. Prints progress messages. Skips years
#' that fail to download.
#'
#' @param years Integer vector of fiscal years (e.g. \code{2020:2023}).
#' @param date_stamp Month/day stamp for the archive files (default
#'   \code{"0108"}).
#' @return A combined data.frame of all successfully downloaded years.
#' @examples
#' \dontrun{
#' usas_contracts_bulk(2021:2023)
#' }
#' @export
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
