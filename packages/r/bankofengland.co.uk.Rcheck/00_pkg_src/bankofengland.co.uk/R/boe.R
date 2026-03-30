# == Yield curves ==============================================================

#' Latest UK government yield curve data (daily)
#'
#' Downloads the BoE's latest yield curve data zip containing:
#' - Nominal yield curves
#' - Real yield curves (inflation-linked)
#' - Inflation curves
#' - OIS curves
#'
#' Requires the readxl package.
#'
#' @param curve Which curve: "nominal" (default), "real", "inflation", "ois"
#' @return tibble: date + maturity columns with yield values
#' @export
boe_yield_curve <- function(curve = "nominal") {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' required for boe_yield_curve", call. = FALSE)

  tmp <- .boe_download("/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip", ".zip")
  files <- utils::unzip(tmp, list = TRUE)

  # Map curve type to file
  pattern <- switch(curve,
    nominal   = "Nominal daily",
    real      = "Real daily",
    inflation = "Inflation daily",
    ois       = "OIS daily",
    stop("curve must be 'nominal', 'real', 'inflation', or 'ois'", call. = FALSE)
  )
  target <- files$Name[grepl(pattern, files$Name, ignore.case = TRUE)]
  if (length(target) == 0) return(tibble())

  xlsx_path <- utils::unzip(tmp, target[1], exdir = tempdir())
  sheets <- readxl::excel_sheets(xlsx_path)

  # Use "4. spot curve" for spot rates (most useful)
  # Sheets: info, 1. fwds short end, 2. fwd curve, 3. spot short end, 4. spot curve
  sheet_idx <- switch(curve,
    nominal   = "4. spot curve",
    real      = "4. spot curve",
    inflation = "4. spot curve",
    ois       = "4. spot curve"
  )
  if (!sheet_idx %in% sheets) sheet_idx <- sheets[length(sheets)]

  # Read raw — this is a transposed layout: row 1 = blank, row 2 = dates (columns),
  # subsequent rows = maturity (col 1) + values
  raw <- suppressMessages(
    as.data.frame(readxl::read_excel(xlsx_path, sheet = sheet_idx,
                                      col_names = FALSE, .name_repair = "minimal"))
  )
  if (nrow(raw) < 3 || ncol(raw) < 3) return(tibble())

  # Row 1 has dates in columns 2+
  # The dates are in Excel serial number format or character dates
  date_row <- raw[1, ]
  dates <- as.character(date_row[-1])
  # Try parsing: could be Excel serial numbers or date strings
  parsed_dates <- tryCatch(as.Date(as.numeric(dates), origin = "1899-12-30"),
                           error = function(e) tryCatch(as.Date(dates), error = function(e2) NULL))
  if (is.null(parsed_dates)) return(tibble())

  # Rows 2+ have maturity in col 1, values in cols 2+
  data_rows <- raw[-1, ]
  maturities <- as.character(data_rows[[1]])

  # Build long-format tibble
  results <- list()
  for (i in seq_along(parsed_dates)) {
    if (is.na(parsed_dates[i])) next
    vals <- suppressWarnings(as.numeric(as.character(data_rows[[i + 1]])))
    results[[length(results) + 1]] <- tibble(
      date     = parsed_dates[i],
      maturity = maturities,
      value    = vals
    )
  }
  if (length(results) == 0) return(tibble())

  bind_rows(results) |> filter(!is.na(value), !is.na(maturity), maturity != "")
}


# == Bank Rate =================================================================

#' Current Bank Rate
#'
#' Scrapes the BoE website for the current Bank Rate. Returns a single-row
#' tibble with the current rate and effective date.
#'
#' @return tibble: rate, effective_date, description
#' @export
boe_bank_rate <- function() {
  tmp <- tempfile(fileext = ".html")
  httr2::request(paste0(.boe_base, "/monetary-policy/the-interest-rate-bank-rate")) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  doc <- xml2::read_html(tmp)

  # The bank rate is typically displayed prominently on the page
  # Look for the rate value in the hero section
  text <- xml2::xml_text(xml2::xml_find_all(doc, ".//main"), trim = TRUE)
  text <- paste(text, collapse = " ")

  # Extract rate: look for pattern like "4.5%" or "Bank Rate is 4.5%"
  rate_match <- regmatches(text, regexpr("\\d+\\.?\\d*%", text))
  rate <- if (length(rate_match) > 0) as.numeric(gsub("%", "", rate_match[1])) else NA_real_

  tibble(
    rate = rate,
    description = "Bank of England Bank Rate"
  )
}


# == Research datasets =========================================================

#' Download BoE Millennium macroeconomic dataset
#'
#' A comprehensive dataset of UK macroeconomic data spanning centuries.
#' Very large Excel file (~15MB).
#'
#' @param sheet Sheet name or number to read. NULL lists all sheets.
#' @return tibble from the specified sheet, or tibble of sheet names if NULL
#' @export
boe_millennium <- function(sheet = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' required", call. = FALSE)

  message("Downloading Millennium dataset (~15MB)...")
  tmp <- .boe_download("/-/media/boe/files/statistics/research-datasets/a-millennium-of-macroeconomic-data-for-the-uk.xlsx", ".xlsx")

  sheets <- readxl::excel_sheets(tmp)
  if (is.null(sheet)) {
    return(tibble(sheet_number = seq_along(sheets), sheet_name = sheets))
  }

  df <- as_tibble(readxl::read_excel(tmp, sheet = sheet))
  df
}


# == Context ===================================================================

#' Generate LLM-friendly context for the bankofengland.co.uk package
#'
#' @return Character string (invisibly), also printed
#' @export
boe_context <- function() {
  .build_context("bankofengland.co.uk", header_lines = c(
    "# bankofengland.co.uk - Bank of England Data Client for R",
    "# Dependencies: httr2, readxl, xml2, dplyr, tibble",
    "# Auth: none (direct file downloads + HTML scraping)",
    "# All functions return tibbles.",
    "#",
    "# Note: BoE's old CSV API (SID) is deprecated. This client uses",
    "# direct Excel/ZIP downloads and HTML scraping.",
    "#",
    "# Yield curves: nominal, real, inflation, OIS (daily, current month)",
    "# Bank Rate: current rate from the monetary policy page",
    "# Millennium: centuries of UK macroeconomic data"
  ))
}
