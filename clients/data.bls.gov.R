# data.bls.gov.R
# Self-contained data.bls.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# bls-gov.R
# Self-contained Bureau of Labor Statistics API v2 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional API key (registrationKey). Without: 25 series/query, 10yr range,
#   25 req/day. With: 50 series/query, 20yr range, 500 req/day.
#   Register at https://data.bls.gov/registrationEngine/
# Docs: https://www.bls.gov/developers/api_signature_v2.htm


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.bls_base <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
# -- Fetch helper --------------------------------------------------------------

.bls_post <- function(body, key = NULL) {
  if (!is.null(key)) body$registrationKey <- key
  tmp <- tempfile(fileext = ".json")
  httr2::request(.bls_base) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  if (raw$status != "REQUEST_SUCCEEDED")
    warning("BLS API: ", paste(raw$message, collapse = "; "), call. = FALSE)
  raw
}


# == Schemas ===================================================================

.schema_series <- tibble(
  series_id = character(), year = integer(), period = character(),
  period_name = character(), value = numeric(), date = as.Date(character())
)

.schema_catalog <- tibble(
  series_id = character(), series_title = character(),
  survey_name = character(), survey_abbreviation = character(),
  seasonal = character(), area_code = character(), area_name = character(),
  item_code = character(), item_name = character()
)

.schema_search <- tibble(
  series_id = character(), series_title = character(),
  survey_name = character()
)



# == Core data fetching ========================================================

#' Fetch BLS time series data
#'
#' Retrieves observations for one or more Bureau of Labor Statistics series.
#' Supports CPI, unemployment, payrolls, PPI, JOLTS, and all other BLS
#' time series. Periods are automatically converted to proper dates.
#'
#' Without an API key: max 25 series per request, 10-year range, 25 queries/day.
#' With a key (register at \url{https://data.bls.gov/registrationEngine/}):
#' max 50 series, 20-year range, 500 queries/day.
#'
#' @param series Character vector of BLS series IDs. Common examples:
#'   \describe{
#'     \item{\code{"CUUR0000SA0"}}{CPI-U, All Items, Not Seasonally Adjusted}
#'     \item{\code{"CUSR0000SA0"}}{CPI-U, All Items, Seasonally Adjusted}
#'     \item{\code{"LNS14000000"}}{Unemployment Rate}
#'     \item{\code{"CES0000000001"}}{Total Nonfarm Payrolls}
#'     \item{\code{"CES0500000003"}}{Avg Hourly Earnings, Private}
#'   }
#' @param start_year Integer. Start year (default: current year minus 9).
#' @param end_year Integer. End year (default: current year).
#' @param key Character or \code{NULL}. BLS API v2 registration key.
#' @param catalog Logical. If \code{TRUE} and \code{key} is provided, include
#'   series metadata in the API response (default \code{FALSE}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{series_id}{\code{character} -- BLS series identifier}
#'     \item{year}{\code{integer} -- Calendar year}
#'     \item{period}{\code{character} -- Period code (e.g., "M01"--"M12" monthly,
#'       "Q01"--"Q04" quarterly, "A01" annual)}
#'     \item{period_name}{\code{character} -- Human-readable period (e.g., "January")}
#'     \item{value}{\code{numeric} -- Observation value (index, rate, or count)}
#'     \item{date}{\code{Date} -- First day of the period (e.g., 2024-01-01)}
#'   }
#' @examples
#' bls_series("CUSR0000SA0", start_year = 2023, end_year = 2024)
#' bls_series(c("LNS14000000", "CES0000000001"), start_year = 2020)
#' @export
bls_series <- function(series, start_year = NULL, end_year = NULL,
                       key = NULL, catalog = FALSE) {
  if (is.null(end_year)) end_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(start_year)) start_year <- end_year - 9L

  body <- list(
    seriesid = as.list(series),
    startyear = as.character(start_year),
    endyear = as.character(end_year)
  )
  if (catalog && !is.null(key)) body$catalog <- TRUE

  raw <- .bls_post(body, key)
  results <- raw$Results$series
  if (is.null(results) || length(results) == 0) return(.schema_series)

  bind_rows(lapply(results, function(s) {
    data <- s$data
    if (is.null(data) || length(data) == 0) return(NULL)

    bind_rows(lapply(data, function(d) {
      tibble(
        series_id   = s$seriesID,
        year        = as.integer(d$year),
        period      = d$period,
        period_name = d$periodName,
        value       = as.numeric(d$value)
      )
    })) |>
      mutate(
        # Convert period to date: M01-M12 = monthly, Q01-Q05 = quarterly, A01 = annual
        date = case_when(
          grepl("^M\\d{2}$", period) ~
            as.Date(sprintf("%d-%s-01", year, sub("^M", "", period))),
          grepl("^Q01$", period) ~ as.Date(sprintf("%d-01-01", year)),
          grepl("^Q02$", period) ~ as.Date(sprintf("%d-04-01", year)),
          grepl("^Q03$", period) ~ as.Date(sprintf("%d-07-01", year)),
          grepl("^Q04$", period) ~ as.Date(sprintf("%d-10-01", year)),
          grepl("^Q05$", period) ~ as.Date(sprintf("%d-01-01", year)),
          grepl("^A01$", period) ~ as.Date(sprintf("%d-01-01", year)),
          TRUE ~ as.Date(NA)
        )
      )
  })) |> arrange(series_id, date)
}

#' Fetch multiple series with automatic chunking
#'
#' Handles the BLS limit of 25 (no key) or 50 (with key) series per request
#' by automatically splitting into chunks with rate-limit-friendly pauses.
#'
#' @param series Character vector of BLS series IDs (any length).
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key. Determines
#'   chunk size: 25 without key, 50 with key.
#' @param sleep Numeric. Seconds to pause between chunk requests (default 1).
#' @return A tibble with the same columns as \code{bls_series()}:
#'   series_id, year, period, period_name, value, date.
#' @examples
#' # Fetch 30 series without a key (auto-splits into 2 requests)
#' ids <- paste0("CUUR0000SA0", sprintf("L1%02d", 1:30))
#' bls_series_bulk(ids, start_year = 2023, end_year = 2024)
#' @export
bls_series_bulk <- function(series, start_year = NULL, end_year = NULL,
                            key = NULL, sleep = 1) {
  chunk_size <- if (!is.null(key)) 50L else 25L
  chunks <- split(series, ceiling(seq_along(series) / chunk_size))
  n <- length(chunks)

  results <- lapply(seq_along(chunks), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] Fetching %d series...", i, n, length(chunks[[i]])))
    tryCatch(
      bls_series(chunks[[i]], start_year, end_year, key),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
  })
  bind_rows(results)
}


# == Series catalog/metadata ===================================================

#' Fetch series catalog metadata
#'
#' Returns descriptive information about BLS series including survey name,
#' geographic area, and item details. Requires a BLS API registration key.
#'
#' @param series Character vector of BLS series IDs.
#' @param key Character. BLS API v2 registration key (required).
#' @return A tibble with columns:
#'   \describe{
#'     \item{series_id}{\code{character} -- Series identifier}
#'     \item{series_title}{\code{character} -- Human-readable series description}
#'     \item{survey_name}{\code{character} -- Survey program name}
#'     \item{survey_abbreviation}{\code{character} -- Survey abbreviation (e.g., "CU", "LN")}
#'     \item{seasonal}{\code{character} -- Seasonal adjustment code}
#'     \item{area_code}{\code{character} -- Geographic area code}
#'     \item{area_name}{\code{character} -- Geographic area name}
#'     \item{item_code}{\code{character} -- Item code}
#'     \item{item_name}{\code{character} -- Item name}
#'   }
#' @examples
#' \dontrun{
#' bls_catalog("CUSR0000SA0", key = Sys.getenv("BLS_API_KEY"))
#' }
#' @export
bls_catalog <- function(series, key) {
  body <- list(
    seriesid = as.list(series),
    startyear = format(Sys.Date(), "%Y"),
    endyear = format(Sys.Date(), "%Y"),
    catalog = TRUE,
    registrationKey = key
  )
  raw <- .bls_post(body, key)
  results <- raw$Results$series
  if (is.null(results) || length(results) == 0) return(.schema_catalog)

  bind_rows(lapply(results, function(s) {
    cat <- s$catalog
    if (is.null(cat)) return(NULL)
    tibble(
      series_id            = s$seriesID,
      series_title         = cat$series_title %||% NA_character_,
      survey_name          = cat$survey_name %||% NA_character_,
      survey_abbreviation  = cat$survey_abbreviation %||% NA_character_,
      seasonal             = cat$seasonal %||% NA_character_,
      area_code            = cat$area_code %||% NA_character_,
      area_name            = cat$area_name %||% NA_character_,
      item_code            = cat$item_code %||% NA_character_,
      item_name            = cat$item_name %||% NA_character_
    )
  }))
}


# == Convenience wrappers (popular series) =====================================

#' Consumer Price Index (CPI-U, All Items, US City Average)
#'
#' Convenience wrapper around \code{bls_series()} for the Consumer Price Index
#' for All Urban Consumers, All Items, US City Average. The base period is
#' 1982-84 = 100.
#'
#' @param start_year Integer. Start year (default: current year minus 9).
#' @param end_year Integer. End year (default: current year).
#' @param key Character or \code{NULL}. BLS API registration key.
#' @param seasonal Character. \code{"S"} for seasonally adjusted (default,
#'   series CUSR0000SA0) or \code{"U"} for unadjusted (series CUUR0000SA0).
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (index level), date. See \code{bls_series()} for details.
#' @examples
#' bls_cpi(start_year = 2023, end_year = 2024)
#' @export
bls_cpi <- function(start_year = NULL, end_year = NULL, key = NULL,
                    seasonal = "S") {
  sid <- if (seasonal == "S") "CUSR0000SA0" else "CUUR0000SA0"
  bls_series(sid, start_year, end_year, key)
}

#' Unemployment Rate (seasonally adjusted)
#'
#' Convenience wrapper for the civilian unemployment rate (series LNS14000000),
#' seasonally adjusted. Value is a percentage (e.g., 3.7 = 3.7%).
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key.
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (percentage), date. See \code{bls_series()}.
#' @examples
#' bls_unemployment(start_year = 2023, end_year = 2024)
#' @export
bls_unemployment <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("LNS14000000", start_year, end_year, key)
}

#' Total Nonfarm Payrolls (seasonally adjusted)
#'
#' Convenience wrapper for total nonfarm employment (series CES0000000001),
#' seasonally adjusted. Value is in thousands of persons.
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key.
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (thousands of persons), date. See \code{bls_series()}.
#' @examples
#' bls_payrolls(start_year = 2023, end_year = 2024)
#' @export
bls_payrolls <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("CES0000000001", start_year, end_year, key)
}

#' Average Hourly Earnings (private sector, seasonally adjusted)
#'
#' Convenience wrapper for average hourly earnings of all employees on
#' private nonfarm payrolls (series CES0500000003). Value is in dollars.
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key.
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (dollars per hour), date. See \code{bls_series()}.
#' @examples
#' bls_earnings(start_year = 2023, end_year = 2024)
#' @export
bls_earnings <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("CES0500000003", start_year, end_year, key)
}

#' Producer Price Index (all commodities)
#'
#' Convenience wrapper for the PPI for all commodities (series WPUFD4),
#' not seasonally adjusted. Base period 1982 = 100.
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key.
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (index level), date. See \code{bls_series()}.
#' @examples
#' bls_ppi(start_year = 2023, end_year = 2024)
#' @export
bls_ppi <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("WPUFD4", start_year, end_year, key)
}

#' Employment Cost Index (total compensation, private)
#'
#' Convenience wrapper for the Employment Cost Index, total compensation,
#' private industry workers (series CIS1010000000000A). Quarterly data.
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key.
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (index level), date. See \code{bls_series()}.
#' @examples
#' bls_eci(start_year = 2023, end_year = 2024)
#' @export
bls_eci <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("CIS1010000000000A", start_year, end_year, key)
}

#' Job Openings (JOLTS, total nonfarm)
#'
#' Convenience wrapper for JOLTS job openings, total nonfarm
#' (series JTS000000000000000JOL). Value is in thousands.
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key.
#' @return A tibble with columns: series_id, year, period, period_name,
#'   value (thousands), date. See \code{bls_series()}.
#' @examples
#' bls_jolts(start_year = 2023, end_year = 2024)
#' @export
bls_jolts <- function(start_year = NULL, end_year = NULL, key = NULL) {
  bls_series("JTS000000000000000JOL", start_year, end_year, key)
}


# == Key economic indicators bundle ============================================

#' Fetch all major economic indicators in one call
#'
#' Returns CPI, unemployment rate, nonfarm payrolls, average hourly earnings,
#' PPI, and JOLTS job openings in a single tibble. Fetches all six series
#' in one BLS API request.
#'
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#' @param key Character or \code{NULL}. BLS API registration key (recommended
#'   to avoid hitting the 25 queries/day limit).
#' @return A tibble with columns:
#'   \describe{
#'     \item{series_id}{\code{character} -- BLS series identifier}
#'     \item{year}{\code{integer} -- Calendar year}
#'     \item{period}{\code{character} -- Period code}
#'     \item{period_name}{\code{character} -- Human-readable period name}
#'     \item{value}{\code{numeric} -- Observation value}
#'     \item{date}{\code{Date} -- First day of the period}
#'     \item{indicator}{\code{character} -- Human-readable label (e.g.,
#'       "CPI", "Unemployment Rate", "Nonfarm Payrolls")}
#'   }
#' @examples
#' bls_indicators(start_year = 2024, end_year = 2024)
#' @export
bls_indicators <- function(start_year = NULL, end_year = NULL, key = NULL) {
  ids <- c(
    "CUSR0000SA0",              # CPI-U (seasonally adjusted)
    "LNS14000000",              # Unemployment rate
    "CES0000000001",            # Total nonfarm payrolls
    "CES0500000003",            # Average hourly earnings
    "WPUFD4",                   # PPI all commodities
    "JTS000000000000000JOL"     # JOLTS job openings
  )
  labels <- c("CPI", "Unemployment Rate", "Nonfarm Payrolls",
              "Avg Hourly Earnings", "PPI", "Job Openings")

  df <- bls_series(ids, start_year, end_year, key)
  if (nrow(df) == 0) return(df)

  lookup <- tibble(series_id = ids, indicator = labels)
  df |> left_join(lookup, by = "series_id")
}


# == Context ===================================================================

#' Get data.bls.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bls_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bls_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.bls.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.bls.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.bls.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.bls.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
