# bankofengland.co.uk.R - Self-contained bankofengland.co.uk client




# bankofengland-co-uk.R
# Self-contained Bank of England data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, readxl, dplyr, tibble
# Auth: none required
# Note: BoE's old CSV API (SID) is deprecated. This client uses:
#   1. Direct media file downloads (yield curves, research datasets)
#   2. HTML table scraping for current rates
#   3. readxl for Excel file parsing


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.boe_base <- "https://www.bankofengland.co.uk"
# -- Fetch helpers -------------------------------------------------------------

.boe_download <- function(path, ext = ".xlsx") {
  url <- paste0(.boe_base, path)
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}



# == Yield curves ==============================================================

#' Latest UK government yield curve data (daily)
#'
#' Downloads and parses the Bank of England's latest yield curve data from
#' a zip archive of Excel files. Contains spot rates for nominal, real
#' (inflation-linked), inflation, and OIS curves at various maturities.
#' The Excel file is ~2MB. Requires the \pkg{readxl} package.
#'
#' @param curve Character. Which yield curve to extract. One of:
#'   \describe{
#'     \item{\code{"nominal"}}{Nominal gilt spot rates (default).}
#'     \item{\code{"real"}}{Real (inflation-linked) gilt spot rates.}
#'     \item{\code{"inflation"}}{Implied inflation curve.}
#'     \item{\code{"ois"}}{Overnight Index Swap rates.}
#'   }
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{date}{Date. Trading date.}
#'     \item{maturity}{Character. Maturity tenor (e.g. "0.5", "1", "5", "25").}
#'     \item{value}{Numeric. Spot rate as a percentage.}
#'   }
#'   Returns empty tibble if parsing fails.
#' @export
#' @examples
#' \dontrun{
#' boe_yield_curve("nominal")
#' boe_yield_curve("ois")
#' }
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
#' Scrapes the Bank of England website to extract the current Bank Rate
#' (the UK's base interest rate set by the Monetary Policy Committee).
#' Returns a single-row tibble.
#'
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{rate}{Numeric. Current Bank Rate as a percentage (e.g. 4.5).}
#'     \item{description}{Character. Always \code{"Bank of England Bank Rate"}.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' boe_bank_rate()
#' }
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
#' Downloads and reads the Bank of England's "A Millennium of Macroeconomic
#' Data for the UK" Excel workbook. This comprehensive research dataset spans
#' centuries of UK economic history. The file is ~15MB and contains multiple
#' sheets covering GDP, inflation, interest rates, population, and more.
#' Requires the \pkg{readxl} package.
#'
#' @param sheet Character or integer or NULL. Sheet name or index to read.
#'   When \code{NULL} (default), returns a tibble listing all available sheet
#'   names. Example sheet names: \code{"A1. Headline series"},
#'   \code{"BEG Table of contents"}.
#' @return When \code{sheet = NULL}: a tibble with columns:
#'   \describe{
#'     \item{sheet_number}{Integer. 1-based sheet index.}
#'     \item{sheet_name}{Character. Sheet name (e.g. "A1. Headline series").}
#'   }
#'   When \code{sheet} is specified: a tibble with columns from that sheet
#'   (varies by sheet).
#' @export
#' @examples
#' \dontrun{
#' # List available sheets
#' boe_millennium()
#'
#' # Read headline series
#' boe_millennium(sheet = "A1. Headline series")
#' }
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

#' Get bankofengland.co.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
boe_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(boe_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bankofengland.co.uk.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bankofengland.co.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bankofengland.co.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bankofengland.co.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
