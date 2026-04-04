# house-disclosures.R
# U.S. House of Representatives financial disclosure client.
# All functions return data.frames. No database dependencies.
#
# Data source: https://disclosures-clerk.house.gov

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

.hd_base <- "https://disclosures-clerk.house.gov"
.hd_get <- function(url, ext = ".html") http_get(url, ext)


# == Filing index ==============================================================

#' Fetch House financial disclosure filing index for a year
#'
#' Downloads the annual XML filing index and returns a data.frame of all
#' financial disclosure filings.
#'
#' @param year Calendar year (default: current year)
#' @return data.frame with columns: doc_id, name, last, first, state, district,
#'   filing_type, filing_date, year
hd_filings <- function(year = format(Sys.Date(), "%Y")) {
  url <- paste0(.hd_base, "/public_disc/financial-pdfs/", year, "FD.ZIP")
  path <- .hd_get(url, ".zip")

  files <- utils::unzip(path, list = TRUE)
  xml_file <- files$Name[grepl("\\.xml$", files$Name, ignore.case = TRUE)][1]
  if (is.na(xml_file)) return(data.frame())

  xml_path <- utils::unzip(path, xml_file, exdir = tempdir())
  doc <- xml2::read_xml(xml_path)

  members <- xml2::xml_find_all(doc, ".//Member")
  if (length(members) == 0) return(data.frame())

  extract <- function(node, tag) {
    val <- xml2::xml_text(xml2::xml_find_first(node, paste0(".//", tag)))
    if (is.na(val)) "" else trimws(val)
  }

  rows <- lapply(members, function(m) {
    data.frame(
      doc_id = extract(m, "DocID"),
      name = paste(clean_text(extract(m, "Last")), clean_text(extract(m, "First"))),
      last = clean_text(extract(m, "Last")),
      first = clean_text(extract(m, "First")),
      state = extract(m, "StateDst"),
      filing_type = extract(m, "FilingType"),
      filing_date = extract(m, "FilingDate"),
      year = as.integer(year),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


# == PDF text extraction =======================================================

#' Download and extract text from a disclosure PDF
#'
#' @param doc_id Document ID from filing index
#' @param year Calendar year
#' @return Character string of extracted PDF text
hd_pdf_text <- function(doc_id, year) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' required for PDF text extraction", call. = FALSE)
  }
  url <- paste0(.hd_base, "/public_disc/ptr-pdfs/", year, "/", doc_id, ".pdf")
  path <- .hd_get(url, ".pdf")
  pdftools::pdf_text(path)
}


# == Parse disclosure transactions =============================================

#' Parse financial transactions from a disclosure PDF
#'
#' Extracts stock transactions from a periodic transaction report PDF.
#' Looks for transaction markers (SP, JT, DC ownership types; S, P, E
#' transaction types) and associated dates, amounts, and tickers.
#'
#' @param doc_id Document ID
#' @param year Calendar year
#' @return data.frame with columns: line, owner_type, tx_type, date, amount,
#'   ticker, asset_type, description
hd_parse_transactions <- function(doc_id, year) {
  text <- hd_pdf_text(doc_id, year)
  lines <- unlist(strsplit(paste(text, collapse = "\n"), "\n"))
  lines <- clean_text(lines)
  lines <- lines[nchar(lines) > 0]

  if (length(lines) == 0) return(data.frame())

  # Transaction markers
  owner_pattern <- "\\b(SP|JT|DC)\\b"
  tx_pattern <- "\\b([SPE])\\b"
  date_pattern <- "\\d{2}/\\d{2}/\\d{4}"
  amount_pattern <- "\\$[0-9,]+"
  ticker_pattern <- "\\(([A-Z]{1,5})\\)"

  results <- lapply(seq_along(lines), function(i) {
    line <- lines[i]

    # Only process lines with transaction markers
    has_owner <- grepl(owner_pattern, line)
    has_date <- grepl(date_pattern, line)
    if (!has_owner && !has_date) return(NULL)

    owner <- regmatches(line, regexpr(owner_pattern, line))
    tx <- regmatches(line, regexpr(tx_pattern, line))
    dates <- regmatches(line, gregexpr(date_pattern, line))[[1]]
    amounts <- regmatches(line, gregexpr(amount_pattern, line))[[1]]
    tickers <- regmatches(line, gregexpr(ticker_pattern, line))[[1]]
    tickers <- gsub("[()]", "", tickers)

    data.frame(
      line = i,
      owner_type = if (length(owner) > 0) owner[1] else NA_character_,
      tx_type = if (length(tx) > 0) tx[1] else NA_character_,
      date = if (length(dates) > 0) dates[1] else NA_character_,
      amount = if (length(amounts) > 0) amounts[1] else NA_character_,
      ticker = if (length(tickers) > 0) tickers[1] else NA_character_,
      description = line,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(result)) return(data.frame())

  result$date <- as.Date(result$date, format = "%m/%d/%Y")
  result
}
