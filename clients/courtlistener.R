# courtlistener.R
# CourtListener / FreeLaw bulk data client.
# All functions return data.frames. No database dependencies.
#
# Data source: https://storage.courtlistener.com/bulk-data/

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

.cl_base <- "https://storage.courtlistener.com/bulk-data"

.cl_month_end <- function(months_back = 1) {
  d <- Sys.Date() - (months_back * 30)
  as.Date(format(d, "%Y-%m-01")) - 1
}


# == FJC integrated database ===================================================

#' Fetch FJC (Federal Judicial Center) integrated database
#'
#' Downloads the monthly bulk CSV of federal case records from CourtListener.
#'
#' @param date End-of-month date string (YYYY-MM-DD). Defaults to last month.
#' @return data.frame of FJC case records
cl_fjc <- function(date = NULL) {
  if (is.null(date)) date <- .cl_month_end()
  url <- paste0(.cl_base, "/fjc-integrated-database-", date, ".csv.bz2")
  df <- read_bz2_csv(url)
  prefix_cols(df, "fjc_")
}


# == Dockets ===================================================================

#' Fetch CourtListener docket data
#'
#' Downloads the monthly bulk CSV of docket case records.
#'
#' @param date End-of-month date string. Defaults to last month.
#' @param cols Columns to keep (NULL = all). Default keeps key identifiers.
#' @return data.frame of docket records
cl_dockets <- function(date = NULL, cols = c("idb_data_id", "case_name", "case_name_full", "slug")) {
  if (is.null(date)) date <- .cl_month_end()
  url <- paste0(.cl_base, "/dockets-", date, ".csv.bz2")
  df <- read_bz2_csv(url)
  if (!is.null(cols)) {
    keep <- intersect(cols, names(df))
    df <- df[, keep, drop = FALSE]
  }
  prefix_cols(df, "docket_")
}


# == Filings (FJC + Docket join) ===============================================

#' Fetch combined FJC + docket filing data
#'
#' Downloads both FJC and docket datasets and joins them on
#' fjc_id = docket_idb_data_id.
#'
#' @param date End-of-month date string. Defaults to last month.
#' @return data.frame of joined filing records
cl_filings <- function(date = NULL) {
  if (is.null(date)) date <- .cl_month_end()

  fjc <- cl_fjc(date)
  dockets <- cl_dockets(date)

  if (nrow(fjc) == 0 || nrow(dockets) == 0) return(fjc)

  # Join on fjc_id = docket_idb_data_id
  if ("fjc_id" %in% names(fjc) && "docket_idb_data_id" %in% names(dockets)) {
    result <- merge(fjc, dockets, by.x = "fjc_id", by.y = "docket_idb_data_id", all.x = TRUE)
  } else {
    result <- fjc
  }
  result
}
