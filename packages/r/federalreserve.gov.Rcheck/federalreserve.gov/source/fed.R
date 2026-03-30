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

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
.fed_base <- "https://www.federalreserve.gov/datadownload"

# -- Context generator ---------------------------------------------------------

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
#' Constant maturity Treasury yields from 1-month to 30-year.
#' Primary source: Federal Reserve H.15 release.
#'
#' @param lastobs Number of most recent observations (default 250 = ~1yr)
#' @param from Start date (optional)
#' @param to End date (optional)
#' @return tibble: date, series_code, value, description
fed_treasury_yields <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_treasury, lastobs, from, to)
}

#' Federal funds rate (daily)
#'
#' Effective federal funds rate and target range.
#' Primary source: Federal Reserve H.15 release.
#'
#' @param lastobs Number of recent observations (default 250)
#' @param from Start date
#' @param to End date
#' @return tibble: date, series_code, value, description
fed_funds_rate <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_fedfunds, lastobs, from, to)
}

#' Commercial paper rates (daily)
#'
#' @param lastobs Number of recent observations (default 250)
#' @param from Start date
#' @param to End date
#' @return tibble: date, series_code, value, description
fed_commercial_paper <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_cp, lastobs, from, to)
}

#' Bank prime loan rate
#'
#' @param lastobs Number of recent observations (default 250)
#' @param from Start date
#' @param to End date
#' @return tibble: date, series_code, value, description
fed_prime_rate <- function(lastobs = 250, from = NULL, to = NULL) {
  .fed_fetch_csv("H15", .fed_series$h15_prime, lastobs, from, to)
}

#' All H.15 interest rates (Treasury + Fed funds + CP + prime)
#'
#' Convenience function that fetches all major H.15 series in one call.
#' Makes 4 HTTP requests.
#'
#' @param lastobs Number of recent observations (default 60)
#' @return tibble: date, series_code, value, description
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
#' Returns the most recent Treasury yields in wide format — one column per
#' maturity (1mo, 3mo, 6mo, 1yr, 2yr, 3yr, 5yr, 7yr, 10yr, 20yr, 30yr).
#'
#' @param n Number of days to include (default 1 = latest only)
#' @return tibble: date + one column per maturity
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
#' Returns the major Fed Board statistical releases with descriptions.
#'
#' @return tibble: release_code, name, description, frequency
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

#' Generate LLM-friendly context for the federalreserve.gov package
#'
#' @return Character string (invisibly), also printed
fed_context <- function() {
  .build_context("federalreserve.gov", header_lines = c(
    "# federalreserve.gov - Federal Reserve Board Data Client for R",
    "# Dependencies: httr2, dplyr, tidyr, tibble",
    "# Auth: none (public CSV downloads)",
    "# Primary source for: Treasury yields, fed funds rate, bank rates",
    "# All functions return tibbles.",
    "#",
    "# Key releases:",
    "#   H.15 = Interest rates (Treasury yields, fed funds, CP, prime)",
    "#   H.4.1 = Fed balance sheet",
    "#   H.8 = Commercial bank assets/liabilities",
    "#   G.17 = Industrial production",
    "#   G.19 = Consumer credit",
    "#",
    "# For broader Fed data access, also see: fred.stlouisfed.org package"
  ))
}
