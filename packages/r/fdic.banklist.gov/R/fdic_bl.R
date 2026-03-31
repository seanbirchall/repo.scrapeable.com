
# == Failed banks ==============================================================

#' Fetch the FDIC failed bank list
#'
#' Downloads the complete list of failed banks from the FDIC.
#' Includes bank name, location, closing date, and acquiring institution.
#'
#' @return tibble: bank_name (character), city (character), state (character),
#'   cert (integer), acquiring_institution (character), closing_date (Date),
#'   fund (integer)
#' @export
fdic_failed_banks <- function() {
  f <- .fetch(.fdic_bl_url, ext = ".csv")
  # Read lines, strip non-ASCII (BOM, \xa0 chars in headers)
  lines <- readLines(f, warn = FALSE, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "ASCII", sub = "")
  tc <- textConnection(lines)
  df <- tryCatch(utils::read.csv(tc, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  close(tc)
  if (is.null(df) || nrow(df) == 0) return(.schema_failed_banks)
  df <- as_tibble(df)
  # Normalize column names — strip BOM and special chars
  nms <- names(df)
  nms <- gsub("\uFEFF", "", nms)  # strip BOM
  nms <- tolower(gsub("[^a-zA-Z0-9]", "_", nms))
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  names(df) <- nms

  # Map to standardized names
  bank_col <- intersect(c("bank_name", "bank", "institution_name", "name"), names(df))
  city_col <- intersect(c("city", "city_name"), names(df))
  state_col <- intersect(c("state", "st"), names(df))
  cert_col <- intersect(c("cert", "cert_number", "certificate_number", "cert_"), names(df))
  acq_col <- intersect(c("acquiring_institution", "acquirer", "acquiring_inst"), names(df))
  close_col <- intersect(c("closing_date", "close_date", "date_closed", "closing"), names(df))
  fund_col <- intersect(c("fund", "fund_number"), names(df))

  tibble(
    bank_name = if (length(bank_col) > 0) as.character(df[[bank_col[1]]]) else NA_character_,
    city = if (length(city_col) > 0) as.character(df[[city_col[1]]]) else NA_character_,
    state = if (length(state_col) > 0) as.character(df[[state_col[1]]]) else NA_character_,
    cert = if (length(cert_col) > 0) suppressWarnings(as.integer(df[[cert_col[1]]])) else NA_integer_,
    acquiring_institution = if (length(acq_col) > 0) as.character(df[[acq_col[1]]]) else NA_character_,
    closing_date = if (length(close_col) > 0) {
      tryCatch(as.Date(df[[close_col[1]]], format = "%d-%b-%y"), error = function(e) {
        tryCatch(as.Date(df[[close_col[1]]], format = "%B %d, %Y"), error = function(e2) {
          tryCatch(as.Date(df[[close_col[1]]]), error = function(e3) {
            as.Date(rep(NA, nrow(df)))
          })
        })
      })
    } else as.Date(rep(NA, nrow(df))),
    fund = if (length(fund_col) > 0) suppressWarnings(as.integer(df[[fund_col[1]]])) else NA_integer_
  )
}

# == Context ===================================================================

#' Show FDIC Failed Banks context for LLMs
#'
#' Displays package overview and function signatures.
#' @return Invisibly returns the context string
#' @export
fdic_bl_context <- function() {
  .build_context(
    "fdic.banklist.gov",
    header_lines = c(
      "# fdic.banklist.gov",
      "# FDIC Failed Bank List client",
      "# Auth: none required",
      "# Data: complete list of all FDIC-insured banks that have failed",
      "#   since October 1, 2000",
      "#",
      "# Note: This is different from banks.data.fdic.gov which has",
      "#   active bank financial data via BankFind Suite API"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
