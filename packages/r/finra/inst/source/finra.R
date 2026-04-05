# finra.R
# FINRA short interest and short volume client.
# All functions return data.frames. No database dependencies.

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

.finra_get <- function(url, ext = ".txt") http_get(url, ext, ua = "Mozilla/5.0")


# == Short interest ============================================================

#' Fetch FINRA consolidated short interest data
#'
#' Downloads consolidated short interest data from FINRA's OTC market reporting
#' API for a given settlement date range. The data covers short positions
#' reported by broker-dealers across all OTC markets.
#'
#' @param start Start date for the settlement date range. Accepts a \code{Date}
#'   object or string coercible to Date (default: 60 days ago).
#' @param end End date for the settlement date range (default: today).
#' @return A data.frame with columns including:
#'   \describe{
#'     \item{settlement_date}{\code{Date} -- Settlement date for the report.}
#'     \item{symbolcode}{\code{character} -- Ticker symbol.}
#'     \item{issuename}{\code{character} -- Security issue name.}
#'     \item{marketclasscode}{\code{character} -- Market class code.}
#'     \item{currentshortpositionquantity}{\code{numeric} -- Current short position.}
#'     \item{previousshortpositionquantity}{\code{numeric} -- Previous short position.}
#'     \item{changepreviousnumber}{\code{numeric} -- Change from previous report.}
#'     \item{changepercent}{\code{numeric} -- Percent change from previous report.}
#'     \item{averagedailyvolumequantity}{\code{numeric} -- Average daily volume.}
#'     \item{daystocoverquantity}{\code{numeric} -- Days to cover.}
#'   }
#' @examples
#' \dontrun{
#' # Last 30 days of short interest
#' finra_short_interest(start = Sys.Date() - 30)
#' }
#' @export
finra_short_interest <- function(start = Sys.Date() - 60, end = Sys.Date()) {
  url <- "https://services-dynarep.ddwa.finra.org/public/reporting/v2/data/export/group/OTCMarket/name/ConsolidatedShortInterest"

  body <- list(
    dateRangeFilters = list(
      list(
        fieldName = "settlementDate",
        startDate = format(as.Date(start), "%Y-%m-%d"),
        endDate = format(as.Date(end), "%Y-%m-%d")
      )
    ),
    delimiter = ",",
    fields = list(
      "settlementDate", "symbolCode", "issueName", "marketClassCode",
      "currentShortPositionQuantity", "previousShortPositionQuantity",
      "changePreviousNumber", "changePercent",
      "averageDailyVolumeQuantity", "daysToCoverQuantity",
      "groupExchangeCode", "revisionFlag", "splitFlag", "stockSplitFlag"
    ),
    reportName = "ConsolidatedShortInterest",
    limit = 500000
  )

  tmp <- http_post(url, body, ext = ".zip", ua = "Mozilla/5.0")

  files <- utils::unzip(tmp, list = TRUE)
  csv_file <- files$Name[grepl("\\.csv$", files$Name)][1]
  if (is.na(csv_file)) return(data.frame())

  df <- read.csv(unz(tmp, csv_file), stringsAsFactors = FALSE)
  df <- fix_names(df)

  if ("settlementdate" %in% names(df)) {
    df$settlement_date <- as.Date(df$settlementdate)
    df$settlementdate <- NULL
  }
  df
}


# == Short volume ==============================================================

#' Fetch FINRA daily short volume data
#'
#' Downloads daily RegSHO short sale volume from the FINRA CDN. By default
#' fetches data from all six FINRA market centers (CNMS, FNQC, FNRA, FNSQ,
#' FNYX, FORF). Data is published with a one-day lag.
#'
#' @param date Date to fetch. Accepts a \code{Date} object or string coercible
#'   to Date (default: today). Must be a trading day.
#' @param markets Character vector of FINRA market center IDs. Default:
#'   \code{c("CNMS", "FNQC", "FNRA", "FNSQ", "FNYX", "FORF")} (all six).
#' @return A data.frame with columns:
#'   \describe{
#'     \item{date}{\code{Date} -- Trade date.}
#'     \item{symbol}{\code{character} -- Ticker symbol.}
#'     \item{short_volume}{\code{numeric} -- Short sale volume.}
#'     \item{short_exempt_volume}{\code{numeric} -- Short exempt volume.}
#'     \item{total_volume}{\code{numeric} -- Total volume.}
#'     \item{market}{\code{character} -- FINRA market center code.}
#'   }
#' @examples
#' \dontrun{
#' # Yesterday's short volume across all markets
#' finra_short_volume(date = Sys.Date() - 1)
#'
#' # Specific market only
#' finra_short_volume(date = Sys.Date() - 1, markets = "FNRA")
#' }
#' @export
finra_short_volume <- function(date = Sys.Date(), markets = NULL) {
  if (is.null(markets)) {
    markets <- c("CNMS", "FNQC", "FNRA", "FNSQ", "FNYX", "FORF")
  }

  date_str <- format(as.Date(date), "%Y%m%d")

  results <- lapply(markets, function(mkt) {
    url <- paste0("https://cdn.finra.org/equity/regsho/daily/", mkt, "shvol", date_str, ".txt")
    tryCatch({
      path <- .finra_get(url, ".txt")
      df <- read.csv(path, sep = "|", header = TRUE, stringsAsFactors = FALSE)
      df <- fix_names(df)
      df$market <- mkt
      df
    }, error = function(e) NULL)
  })

  result <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(result) || nrow(result) == 0) return(data.frame())

  # Standardize date column
  if ("date" %in% names(result)) {
    result$date <- as.Date(as.character(result$date), format = "%Y%m%d")
  }

  # Convert volume columns to numeric
  vol_cols <- grep("volume", names(result), value = TRUE)
  for (col in vol_cols) {
    result[[col]] <- suppressWarnings(as.numeric(result[[col]]))
  }

  result
}
