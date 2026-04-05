# federalreserve.gov.R - Self-contained federalreserve.gov client




# federalreserve-gov.R
# Self-contained Federal Reserve Board data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tidyr, tibble
# Auth: none required (public CSV downloads from Fed data program)
# Data source: https://www.federalreserve.gov/datadownload/
#
# Note: The Fed Board publishes data directly via CSV. This client
# uses hardcoded series hashes for major releases. For broader discovery,
# use the fred.stlouisfed.org package which indexes all Fed data.


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
.fed_base <- "https://www.federalreserve.gov/datadownload"
# -- Known series hashes (from the Fed data download program) ------------------

.fed_series <- list(
  # H.15 Selected Interest Rates — Treasury yields
  h15_treasury = "bf17364827e38702b42a58cf8eaa3f78",
  # H.15 — Federal funds rate
  h15_fedfunds = "1345e87c69e0ca0d3ff12584d4b3d656",
  # H.15 — Commercial paper rates
  h15_cp       = "dedd2a0e7e88a0f86f1b7b52315e43ff",
  # H.15 — Bank prime rate
  h15_prime    = "22ae479f5d28fbc8b83eaca6b8b4e152"
)

# -- CSV fetch + parse engine --------------------------------------------------

.fed_fetch_csv <- function(release, series_hash, lastobs = NULL,
                           from = NULL, to = NULL) {
  url <- sprintf("%s/Output.aspx?rel=%s&series=%s&filetype=csv&label=include&layout=seriescolumn",
                 .fed_base, release, series_hash)
  if (!is.null(lastobs)) url <- paste0(url, "&lastobs=", lastobs)
  if (!is.null(from))    url <- paste0(url, "&from=", format(as.Date(from), "%m/%d/%Y"))
  if (!is.null(to))      url <- paste0(url, "&to=", format(as.Date(to), "%m/%d/%Y"))

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  lines <- readLines(tmp, warn = FALSE)
  if (length(lines) < 7) return(tibble())

  # Fed CSV format:
  # Row 1: Series Description (long names)
  # Row 2: Unit
  # Row 3: Multiplier
  # Row 4: Currency
  # Row 5: Unique Identifier (series codes)
  # Row 6: Time Period header + series codes
  # Row 7+: Data

  # Parse descriptions (row 1)
  desc_line <- read.csv(text = lines[1], header = FALSE, stringsAsFactors = FALSE)
  descriptions <- as.character(desc_line[1, -1])

  # Parse series codes (row 6)
  header_line <- read.csv(text = lines[6], header = FALSE, stringsAsFactors = FALSE)
  series_codes <- as.character(header_line[1, -1])

  # Parse data (row 7+)
  data_lines <- lines[7:length(lines)]
  data_lines <- data_lines[nchar(data_lines) > 0]
  if (length(data_lines) == 0) return(tibble())

  data_text <- paste(c(lines[6], data_lines), collapse = "\n")
  df <- read.csv(text = data_text, stringsAsFactors = FALSE, check.names = FALSE)

  # Rename first column
  names(df)[1] <- "date"
  df$date <- as.Date(df$date)

  # Pivot to long format
  df <- as_tibble(df)
  value_cols <- setdiff(names(df), "date")

  result <- df |>
    pivot_longer(cols = all_of(value_cols), names_to = "series_code",
                 values_to = "value") |>
    mutate(value = suppressWarnings(as.numeric(value)))

  # Add descriptions
  desc_lookup <- tibble(series_code = series_codes, description = descriptions)
  result <- result |> left_join(desc_lookup, by = "series_code")

  result |> filter(!is.na(value)) |> arrange(date, series_code)
}


# == Schemas ===================================================================

.schema_rates <- tibble(
  date = as.Date(character()), series_code = character(),
  value = numeric(), description = character()
)



# == H.15 Selected Interest Rates =============================================

#' Treasury yield curve rates (daily)
#'
#' Constant maturity Treasury yields from 1-month to 30-year. Data comes
#' directly from the Federal Reserve H.15 statistical release. Returns
#' long-format data with one row per date per maturity.
#'
#' @param lastobs Integer. Number of most recent business-day observations
#'   to return (default 250, approximately 1 year).
#' @param from Character or Date. Start date filter (e.g. \code{"2024-01-01"}).
#' @param to Character or Date. End date filter.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{series_code}{Character. Fed series identifier (e.g. "RIFLGFCM01_N.B").}
#'     \item{value}{Numeric. Yield in percent (e.g. 4.31 = 4.31\%).}
#'     \item{description}{Character. Human-readable series label (e.g.
#'       "Market yield on U.S. Treasury securities at 10-year constant maturity").}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_treasury_yields(lastobs = 30)
#' fed_treasury_yields(from = "2024-01-01", to = "2024-12-31")
#' }
fed_treasury_yields <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_treasury, lastobs, from, to)
}

#' Federal funds rate (daily)
#'
#' Effective federal funds rate and target range from the Federal Reserve
#' H.15 release. The fed funds rate is the interest rate at which depository
#' institutions lend reserve balances to other institutions overnight.
#'
#' @param lastobs Integer. Number of most recent observations (default 250).
#' @param from Character or Date. Start date filter.
#' @param to Character or Date. End date filter.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{series_code}{Character. Fed series identifier.}
#'     \item{value}{Numeric. Rate in percent.}
#'     \item{description}{Character. Human-readable series label.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_funds_rate(lastobs = 60)
#' }
fed_funds_rate <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_fedfunds, lastobs, from, to)
}

#' Commercial paper rates (daily)
#'
#' Daily commercial paper interest rates from the Federal Reserve H.15
#' release. Commercial paper is short-term unsecured debt issued by
#' corporations.
#'
#' @param lastobs Integer. Number of most recent observations (default 250).
#' @param from Character or Date. Start date filter.
#' @param to Character or Date. End date filter.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{series_code}{Character. Fed series identifier.}
#'     \item{value}{Numeric. Rate in percent.}
#'     \item{description}{Character. Human-readable series label.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_commercial_paper(lastobs = 60)
#' }
fed_commercial_paper <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_cp, lastobs, from, to)
}

#' Bank prime loan rate
#'
#' The bank prime loan rate from the Federal Reserve H.15 release.
#' This is the rate posted by a majority of the top 25 US banks,
#' typically 3 percentage points above the federal funds rate.
#'
#' @param lastobs Integer. Number of most recent observations (default 250).
#' @param from Character or Date. Start date filter.
#' @param to Character or Date. End date filter.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{series_code}{Character. Fed series identifier.}
#'     \item{value}{Numeric. Rate in percent.}
#'     \item{description}{Character. Human-readable series label.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_prime_rate(lastobs = 60)
#' }
fed_prime_rate <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_prime, lastobs, from, to)
}

#' All H.15 interest rates (Treasury + Fed funds + CP + prime)
#'
#' Convenience function that fetches all major H.15 series in one call:
#' Treasury yields, federal funds rate, commercial paper rates, and bank
#' prime rate. Makes 4 HTTP requests internally.
#'
#' @param lastobs Integer. Number of recent observations per series (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{series_code}{Character. Fed series identifier.}
#'     \item{value}{Numeric. Rate in percent.}
#'     \item{description}{Character. Human-readable series label.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_h15(lastobs = 10)
#' }
fed_h15 <- function(lastobs = 60) {
  bind_rows(
    tryCatch(fed_treasury_yields(lastobs), error = function(e) NULL),
    tryCatch(fed_funds_rate(lastobs), error = function(e) NULL),
    tryCatch(fed_commercial_paper(lastobs), error = function(e) NULL),
    tryCatch(fed_prime_rate(lastobs), error = function(e) NULL)
  )
}


# == Yield curve snapshot ======================================================

#' Current yield curve (wide format)
#'
#' Returns the most recent Treasury yields in wide format with one column
#' per maturity. Useful for plotting the yield curve or computing term spreads.
#'
#' @param n Integer. Number of trading days to include (default 1 = latest only).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{1mo}{Numeric. 1-month yield (\%).}
#'     \item{3mo}{Numeric. 3-month yield (\%).}
#'     \item{6mo}{Numeric. 6-month yield (\%).}
#'     \item{1yr}{Numeric. 1-year yield (\%).}
#'     \item{2yr}{Numeric. 2-year yield (\%).}
#'     \item{3yr}{Numeric. 3-year yield (\%).}
#'     \item{5yr}{Numeric. 5-year yield (\%).}
#'     \item{7yr}{Numeric. 7-year yield (\%).}
#'     \item{10yr}{Numeric. 10-year yield (\%).}
#'     \item{20yr}{Numeric. 20-year yield (\%).}
#'     \item{30yr}{Numeric. 30-year yield (\%).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_yield_curve()
#' fed_yield_curve(n = 5)
#' }
fed_yield_curve <- function(n = 1) {
  df <- fed_treasury_yields(lastobs = n * 2)  # extra buffer for missing days
  if (nrow(df) == 0) return(tibble())

  # Extract maturity from description
  df <- df |>
    mutate(
      maturity = case_when(
        grepl("1-month", description)  ~ "1mo",
        grepl("3-month", description)  ~ "3mo",
        grepl("6-month", description)  ~ "6mo",
        grepl("1-year", description)   ~ "1yr",
        grepl("2-year", description)   ~ "2yr",
        grepl("3-year", description)   ~ "3yr",
        grepl("5-year", description)   ~ "5yr",
        grepl("7-year", description)   ~ "7yr",
        grepl("10-year", description)  ~ "10yr",
        grepl("20-year", description)  ~ "20yr",
        grepl("30-year", description)  ~ "30yr",
        TRUE ~ series_code
      )
    ) |>
    select(date, maturity, value) |>
    pivot_wider(names_from = maturity, values_from = value)

  # Keep only the most recent n dates
  df |> arrange(desc(date)) |> head(n)
}


# == Available releases ========================================================

#' List Federal Reserve statistical releases
#'
#' Returns a reference table of major Federal Reserve Board statistical
#' releases and their descriptions. These are the primary data publications
#' from the Fed Board of Governors.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{release_code}{Character. Fed release code (e.g. "H15", "G17").}
#'     \item{name}{Character. Official release name (e.g. "H.15 Selected Interest Rates").}
#'     \item{description}{Character. Brief description of contents.}
#'     \item{frequency}{Character. Publication frequency: "daily", "weekly",
#'       "biweekly", "monthly", or "quarterly".}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fed_releases()
#' }
fed_releases <- function() {
  tibble(
    release_code = c("H15", "H41", "H8", "H3", "H6", "G17", "G19", "G5", "Z1"),
    name = c(
      "H.15 Selected Interest Rates",
      "H.4.1 Factors Affecting Reserve Balances",
      "H.8 Assets and Liabilities of Commercial Banks",
      "H.3 Aggregate Reserves",
      "H.6 Money Stock Measures",
      "G.17 Industrial Production and Capacity Utilization",
      "G.19 Consumer Credit",
      "G.5 Foreign Exchange Rates",
      "Z.1 Financial Accounts of the United States"
    ),
    description = c(
      "Treasury yields, fed funds, commercial paper, prime rate",
      "Federal Reserve balance sheet, reserve factors",
      "Bank assets, liabilities, loans, deposits",
      "Reserve balances, monetary base",
      "M1, M2 money supply aggregates",
      "Industrial production indexes, capacity utilization",
      "Consumer credit outstanding (revolving, nonrevolving)",
      "Foreign exchange rates against US dollar",
      "Flow of funds, financial accounts, wealth"
    ),
    frequency = c("daily", "weekly", "weekly", "biweekly", "weekly",
                  "monthly", "monthly", "daily", "quarterly")
  )
}


# == Context ===================================================================

#' Get federalreserve.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fed_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fed_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/federalreserve.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "federalreserve.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# federalreserve.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# federalreserve.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
