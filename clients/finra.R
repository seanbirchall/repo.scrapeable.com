# finra.R
# FINRA short interest and short volume client.
# All functions return data.frames. No database dependencies.

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

.finra_get <- function(url, ext = ".txt") http_get(url, ext, ua = "Mozilla/5.0")


# == Short interest ============================================================

#' Fetch FINRA consolidated short interest data
#'
#' Downloads short interest data from FINRA's OTC market reporting API
#' for a given date range.
#'
#' @param start Start date (default: 60 days ago)
#' @param end End date (default: today)
#' @return data.frame with columns: settlement_date, symbol, issue_name,
#'   market, current_short_position, previous_short_position, etc.
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
#' Downloads daily RegSHO short volume from FINRA CDN for all 6 market centers.
#'
#' @param date Date to fetch (default: today)
#' @param markets Character vector of FINRA market IDs.
#'   Default: all 6 market centers.
#' @return data.frame with columns: date, symbol, short_volume,
#'   short_exempt_volume, total_volume, market
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
