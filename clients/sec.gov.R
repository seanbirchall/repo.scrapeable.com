# sec.gov.R
# Self-contained SEC EDGAR client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, xml2, jsonlite, dplyr, tidyr, tibble
# Optional: readxl (sec_filing_xlsx only)
# Auth: User-Agent header (set to support@scrapeable.com)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# sec-gov.R
# Self-contained SEC EDGAR client.
# All public functions return tibbles.
#
# Dependencies: httr2, xml2, jsonlite, dplyr, tidyr, tibble
# Optional: readxl (sec_filing_xlsx only)


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.sec_cache <- new.env(parent = emptyenv())

.pad_cik <- function(cik) sprintf("%010d", as.integer(cik))
.clean_text <- function(x) gsub("\\s+", " ", trimws(x))

# -- Return schemas (typed empty tibbles) --------------------------------------

.schema_entities <- tibble(cik = integer(), ticker = character(), name = character())

.schema_search <- tibble(cik = character(), name = character())

.schema_filings <- tibble(
  accessionnumber = character(), filingdate = as.Date(character()),
  reportdate = as.Date(character()), acceptancedatetime = character(),
  form = character(), primarydocument = character(), size = integer(),
  isxbrl = integer(), isinlinexbrl = integer(),
  cik = integer(), entity = character()
)

.schema_facts <- tibble(
  end = as.Date(character()), val = numeric(), accn = character(),
  fy = integer(), fp = character(), form = character(),
  filed = as.Date(character()), start = as.Date(character()),
  frame = character(), taxonomy = character(), tag = character(),
  label = character(), description = character(), unit = character(),
  cik = integer(), entity = character()
)

.schema_concepts <- tibble(
  end = as.Date(character()), val = numeric(), accn = character(),
  fy = integer(), fp = character(), form = character(),
  filed = as.Date(character()), unit = character(),
  cik = integer(), entity = character(), taxonomy = character(),
  tag = character(), label = character(), description = character()
)

.schema_frames <- tibble(
  cik = integer(), entityName = character(), loc = character(),
  end = as.Date(character()), val = numeric(), accn = character(),
  taxonomy = character(), tag = character(), unit = character(),
  period = character(), label = character(), description = character()
)

.schema_tags <- tibble(
  taxonomy = character(), tag = character(),
  label = character(), description = character()
)

.schema_filing_docs <- tibble(
  name = character(), `last-modified` = character(), size = character(),
  url = character(), category = character(), description = character(),
  report_name = character(), report_short = character(),
  report_role = character(), accession = character()
)

.schema_xbrl <- tibble(
  tag = character(), value = character(), context_ref = character(),
  unit_ref = character(), decimals = character(), accession = character()
)

.schema_xbrl_segment <- tibble(
  context_id = character(), dimension = character(), segment = character(),
  start_date = as.Date(character()), end_date = as.Date(character()),
  xbrl_tag = character(), value = character(), accession = character()
)

.schema_xbrl_ownership <- tibble(
  type = character(), security_title = character(), tx_date = as.Date(character()),
  tx_code = character(), shares = numeric(), price_per_share = numeric(),
  acquired_disposed = character(), shares_after = numeric(), ownership = character(),
  owner_cik = character(), owner_name = character(),
  issuer_cik = character(), issuer_ticker = character(), accession = character()
)

.schema_xbrl_holding <- tibble(
  name_of_issuer = character(), title_of_class = character(), cusip = character(),
  value = numeric(), shares = numeric(), shares_type = character(),
  put_call = character(), investment_discretion = character(),
  voting_sole = integer(), voting_shared = integer(), voting_none = integer(),
  accession = character()
)

.schema_table <- tibble(
  table_index = integer(), row_index = integer(), col_index = integer(),
  name = character(), value = character(), accession = character()
)

.schema_text <- tibble(
  line = integer(), text = character(), accession = character()
)

.schema_xlsx <- tibble(
  sheet_name = character(), sheet_position = integer(),
  row_index = integer(), col_index = integer(),
  name = character(), value = character(), accession = character()
)

.schema_html <- tibble(
  accession = character(), document = character(),
  path = character(), bytes = numeric()
)
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_html <- function(url) xml2::read_html(.fetch(url, ".html"))
.fetch_xml  <- function(url) xml2::read_xml(.fetch(url, ".xml"))

# -- Filing URL ----------------------------------------------------------------

.filing_url <- function(accession) {
  cik <- as.integer(sub("-.*", "", accession))
  paste0("https://www.sec.gov/Archives/edgar/data/",
         cik, "/", gsub("-", "", accession), "/")
}

# -- File classification -------------------------------------------------------

.classify_file <- function(name, size, primary_name = "") {
  n <- tolower(name)
  case_when(
    n == tolower(primary_name)                                          ~ "primary",
    grepl("^r\\d+\\.htm", n)                                           ~ "xbrl_report",
    grepl("filingsummary\\.xml$", n)                                   ~ "xbrl_summary",
    grepl("_htm\\.xml$", n)                                            ~ "xbrl_instance",
    grepl("_cal\\.xml$|_def\\.xml$|_lab\\.xml$|_pre\\.xml$|_ref\\.xml$|\\.xsd$", n) ~ "xbrl_schema",
    grepl("financial_report\\.xlsx?$|report\\.xlsx?$", n)              ~ "excel",
    grepl("^ex|exhibit", n)                                            ~ "exhibit",
    grepl("\\.jpg$|\\.gif$|\\.png$|\\.bmp$", n)                        ~ "graphic",
    grepl("index", n)                                                  ~ "index",
    grepl("\\.xml$", n)                                                ~ "xml",
    grepl("\\.(htm|html)$", n)                                         ~ "html",
    TRUE                                                               ~ "other"
  )
}

# -- Full filing inventory -----------------------------------------------------

.filing_docs <- function(accession) {
  base <- .filing_url(accession)

  raw <- .fetch_json(paste0(base, "index.json"))
  items <- raw$directory$item
  if (is.null(items)) return(tibble(name = character(), size = character(),
                                     url = character(), category = character()))

  dir_df <- as_tibble(items) |>
    mutate(url = paste0(base, name))

  # -index.htm for official document descriptions
  desc_df <- tryCatch({
    doc <- .fetch_html(paste0(base, accession, "-index.htm"))
    tbls <- .html_tables(doc)
    found <- NULL
    for (tbl in tbls)
      if (any(grepl("document", names(tbl), ignore.case = TRUE))) { found <- tbl; break }
    if (!is.null(found)) as_tibble(found) |> rename_with(tolower) else NULL
  }, error = function(e) NULL)

  primary_name <- ""
  if (!is.null(desc_df) && "document" %in% names(desc_df) && nrow(desc_df) > 0)
    primary_name <- desc_df$document[1]
  if (primary_name == "") {
    htm <- dir_df |> filter(grepl("\\.(htm|html)$", name, ignore.case = TRUE),
                            !grepl("^R\\d|index|FilingSummary", name, ignore.case = TRUE))
    if (nrow(htm) > 0) primary_name <- htm$name[which.max(as.numeric(htm$size))]
  }

  dir_df <- dir_df |>
    mutate(category = .classify_file(name, size, primary_name))

  # Merge descriptions
  if (!is.null(desc_df) && "document" %in% names(desc_df)) {
    desc_col <- if ("description" %in% names(desc_df)) "description"
                else if ("type" %in% names(desc_df)) "type" else NULL
    if (!is.null(desc_col)) {
      lookup <- desc_df |> select(document, description = all_of(desc_col)) |>
        mutate(document = tolower(document))
      dir_df <- dir_df |>
        left_join(lookup, by = c("name" = "document"), relationship = "many-to-one") |>
        distinct()
    } else {
      dir_df$description <- NA_character_
    }
  } else {
    dir_df$description <- NA_character_
  }

  # Enrich XBRL reports from MetaLinks.json or FilingSummary.xml
  dir_df <- dir_df |>
    mutate(report_name = NA_character_, report_short = NA_character_, report_role = NA_character_)

  ml_row <- dir_df |> filter(name == "MetaLinks.json")
  if (nrow(ml_row) > 0) {
    rpt_map <- tryCatch({
      ml <- .fetch_json(ml_row$url[1])
      reports <- ml$instance[[1]]$report
      if (is.null(reports)) NULL
      else {
        tibble(
          html_file  = paste0("R", seq_along(reports), ".htm"),
          long_name  = vapply(reports, function(r) r$longName %||% NA_character_, character(1)),
          short_name = vapply(reports, function(r) r$shortName %||% NA_character_, character(1)),
          role       = vapply(reports, function(r) r$role %||% NA_character_, character(1))
        )
      }
    }, error = function(e) NULL)

    if (!is.null(rpt_map)) {
      dir_df <- dir_df |>
        left_join(rpt_map, by = c("name" = "html_file")) |>
        mutate(
          report_name  = coalesce(long_name, report_name),
          report_short = coalesce(short_name, report_short),
          report_role  = coalesce(role, report_role)
        ) |>
        select(-any_of(c("long_name", "short_name", "role")))
    }
  } else {
    summary_row <- dir_df |> filter(category == "xbrl_summary")
    if (nrow(summary_row) > 0) {
      rpt_map <- tryCatch({
        fs <- xml2::as_list(.fetch_xml(summary_row$url[1]))
        reports <- fs[["FilingSummary"]][["MyReports"]]
        if (is.null(reports)) NULL
        else {
          tibble(
            html_file  = vapply(reports, function(r) r[["HtmlFileName"]][[1]] %||% NA_character_, character(1)),
            long_name  = vapply(reports, function(r) r[["LongName"]][[1]] %||% NA_character_, character(1)),
            short_name = vapply(reports, function(r) r[["ShortName"]][[1]] %||% NA_character_, character(1))
          )
        }
      }, error = function(e) NULL)

      if (!is.null(rpt_map)) {
        dir_df <- dir_df |>
          left_join(rpt_map, by = c("name" = "html_file")) |>
          mutate(
            report_name  = coalesce(long_name, report_name),
            report_short = coalesce(short_name, report_short)
          ) |>
          select(-any_of(c("long_name", "short_name")))
      }
    }
  }

  dir_df
}

# Convenience filters
.find_primary <- function(docs) {
  hit <- docs |> filter(category == "primary")
  if (nrow(hit) > 0) return(hit[1, ])
  htm <- docs |> filter(category %in% c("html", "primary"))
  if (nrow(htm) > 0) return(htm[which.max(as.numeric(htm$size)), ])
  NULL
}

.find_xbrl <- function(docs) {
  hit <- docs |> filter(category == "xbrl_instance")
  if (nrow(hit) > 0) return(hit[1, ])
  xmls <- docs |> filter(category == "xml")
  if (nrow(xmls) > 0) return(xmls[which.max(as.numeric(xmls$size)), ])
  NULL
}

.find_xlsx <- function(docs) {
  hit <- docs |> filter(category == "excel")
  if (nrow(hit) > 0) return(hit[1, ])
  NULL
}

# -- HTML table parser ---------------------------------------------------------

.html_tables <- function(doc, xpath = ".//table") {
  nodes <- xml2::xml_find_all(doc, xpath)
  lapply(nodes, function(tbl) {
    rows <- xml2::xml_find_all(tbl, ".//tr")
    if (length(rows) == 0) return(tibble())
    cells <- lapply(rows, function(r)
      xml2::xml_text(xml2::xml_find_all(r, ".//td|.//th"), trim = TRUE))
    max_cols <- max(lengths(cells), 0L)
    if (max_cols == 0) return(tibble())
    mat <- do.call(rbind, lapply(cells, function(r) { length(r) <- max_cols; r }))
    df <- as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
    if (nrow(df) <= 1) return(df)
    hdr <- as.character(df[1, ])
    hdr[is.na(hdr) | hdr == ""] <- paste0("X", which(is.na(hdr) | hdr == ""))
    names(df) <- make.unique(hdr)
    df[-1, , drop = FALSE]
  })
}

# -- TSV / zip helpers ---------------------------------------------------------

.read_tsv_zip <- function(zip_path, entry) {
  as_tibble(read.delim(unz(zip_path, entry), sep = "\t", header = TRUE,
                        stringsAsFactors = FALSE, na.strings = c("", "NA"), quote = "")) |>
    rename_with(~ gsub("[. ]+", "_", tolower(.x)))
}

.scrape_zip_links <- function(page_url) {
  doc <- .fetch_html(page_url)
  links <- xml2::xml_find_all(doc, ".//a")
  hrefs <- xml2::xml_attr(links, "href")
  texts <- xml2::xml_text(links, trim = TRUE)
  downloads <- xml2::xml_attr(links, "download")

  keep <- (!is.na(hrefs) & grepl("\\.zip$", hrefs, ignore.case = TRUE)) | !is.na(downloads)
  keep <- keep & !is.na(hrefs)
  if (!any(keep)) return(tibble(text = character(), href = character(), url = character()))

  hrefs <- hrefs[keep]
  tibble(
    text = texts[keep],
    href = hrefs,
    url  = ifelse(grepl("^https?://", hrefs), hrefs,
                  paste0("https://www.sec.gov", hrefs))
  )
}

# -- Bulk zip parsers ----------------------------------------------------------

.parse_insiders_zip <- function(path) {
  tsv_files <- utils::unzip(path, list = TRUE)$Name
  tsv_files <- tsv_files[grepl("\\.tsv$", tsv_files)]
  sheets <- lapply(tsv_files, function(f) .read_tsv_zip(path, f))
  names(sheets) <- tsv_files

  submission     <- sheets[["SUBMISSION.tsv"]]
  reportingowner <- sheets[["REPORTINGOWNER.tsv"]]

  owner_single <- NULL
  if (!is.null(reportingowner) && "accession_number" %in% names(reportingowner)) {
    owner_single <- reportingowner |>
      count(accession_number) |>
      filter(n == 1) |>
      select(accession_number) |>
      left_join(reportingowner, by = "accession_number")
  }

  join <- function(df, tag) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- df |> mutate(source_tsv = tag)
    if (!is.null(submission) && "accession_number" %in% names(df))
      df <- df |> left_join(submission, by = "accession_number", suffix = c("", ".sub"))
    if (!is.null(owner_single) && "accession_number" %in% names(df))
      df <- df |> left_join(owner_single, by = "accession_number", suffix = c("", ".owner"))
    df
  }

  bind_rows(
    join(sheets[["DERIV_TRANS.tsv"]], "DERIV_TRANS"),
    join(sheets[["NONDERIV_TRANS.tsv"]], "NONDERIV_TRANS")
  )
}

.parse_13f_zip <- function(path) {
  tsv_files <- utils::unzip(path, list = TRUE)$Name
  tsv_files <- tsv_files[grepl("\\.tsv$", tsv_files)]
  if (length(tsv_files) == 0) return(NULL)
  sheets <- lapply(tsv_files, function(f) .read_tsv_zip(path, f))
  names(sheets) <- tsv_files

  result <- sheets[["INFOTABLE.tsv"]]
  if (is.null(result)) return(NULL)

  om <- sheets[["OTHERMANAGER2.tsv"]]
  if (!is.null(om) && "accession_number" %in% names(om)) {
    if ("sequencenumber" %in% names(om)) om <- om |> mutate(othermanager = as.character(sequencenumber))
    om <- om |>
      select(any_of(c("accession_number", "othermanager", "cik", "name"))) |>
      rename(any_of(c(othermanager_cik = "cik", othermanager_name = "name"))) |>
      distinct()
    if ("othermanager" %in% names(result) && "othermanager" %in% names(om))
      result <- result |> left_join(om, by = c("accession_number", "othermanager"))
  }
  sub <- sheets[["SUBMISSION.tsv"]]
  if (!is.null(sub) && "accession_number" %in% names(sub))
    result <- result |> left_join(sub, by = "accession_number")
  cp <- sheets[["COVERPAGE.tsv"]]
  if (!is.null(cp) && "accession_number" %in% names(cp)) {
    cp <- cp |> select(any_of(c("accession_number", "filingmanager_name"))) |> distinct()
    result <- result |> left_join(cp, by = "accession_number")
  }
  result
}

.parse_ftd_zip <- function(path) {
  entries <- utils::unzip(path, list = TRUE)$Name
  results <- lapply(entries, function(f) {
    df <- read.delim(unz(path, f), sep = "|", header = TRUE,
                     stringsAsFactors = FALSE, na.strings = c("", "NA"), quote = "")
    if (ncol(df) >= 6)
      names(df) <- c("settlement_date", "cusip", "ticker",
                      "quantity_fails", "description", "price")[seq_len(ncol(df))]
    df |>
      as_tibble() |>
      mutate(
        settlement_date = as.Date(as.character(settlement_date), format = "%Y%m%d"),
        quantity_fails  = suppressWarnings(as.numeric(quantity_fails)),
        price           = suppressWarnings(as.numeric(price)),
        price           = replace_na(price, 0),
        across(any_of(c("cusip", "ticker", "description")),
               ~ iconv(.x, "UTF-8", "UTF-8", sub = ""))
      )
  })
  bind_rows(results)
}



# == Entity resolution =========================================================

#' @title List all SEC-registered entities
#' @description Returns a tibble of every company and fund registered with the SEC,
#'   including their CIK number, ticker symbol, and official name. This is the
#'   master lookup table for resolving tickers to CIKs before calling other
#'   functions like sec_filings() or sec_facts(). Results are cached in-session
#'   so subsequent calls are instant.
#' @return A tibble with columns:
#'   \describe{
#'     \item{cik}{integer. Central Index Key — the SEC's unique numeric identifier for each entity.}
#'     \item{ticker}{character. Exchange ticker symbol (e.g., "AAPL", "MSFT"). May be NA for non-traded entities.}
#'     \item{name}{character. Official entity name as registered with the SEC (e.g., "Apple Inc.").}
#'   }
#' @examples
#' \dontrun{
#' # Get the full entity list
#' entities <- sec_entities()
#'
#' # Filter to find a company
#' entities |> dplyr::filter(grepl("Tesla", name, ignore.case = TRUE))
#'
#' # Look up a CIK by ticker
#' entities |> dplyr::filter(ticker == "NVDA")
#' }
sec_entities <- function() {
  if (!is.null(.sec_cache$entities)) return(.sec_cache$entities)
  raw <- .fetch_json("https://www.sec.gov/files/company_tickers.json")
  df <- bind_rows(lapply(raw, as_tibble)) |>
    setNames(c("cik", "ticker", "name")) |>
    mutate(cik = as.integer(cik), ticker = as.character(ticker), name = as.character(name))
  .sec_cache$entities <- df
  df
}


#' @title Resolve a company by CIK, ticker, or name
#' @description Filters the SEC entity master list to find companies matching the
#'   given CIK, ticker, or name pattern. Use this to look up a company's CIK
#'   before calling sec_filings() or sec_facts(). Supports partial name matching
#'   via regex. Multiple filters can be combined (all must match).
#' @param cik Integer or character. Central Index Key to match exactly.
#'   Example: 320193 (Apple). NULL to skip this filter.
#' @param ticker Character. Ticker symbol to match (case-insensitive).
#'   Example: "AAPL", "MSFT", "GOOGL". NULL to skip this filter.
#' @param name Character. Regex pattern matched against entity names (case-insensitive).
#'   Example: "Tesla", "Berkshire", "^Meta". NULL to skip this filter.
#' @return A tibble with columns: cik (integer), ticker (character), name (character).
#'   Returns all matching entities; may be zero rows if no match found.
#' @examples
#' \dontrun{
#' # Resolve by ticker
#' sec_resolve(ticker = "AAPL")
#'
#' # Fuzzy search by name
#' sec_resolve(name = "Berkshire")
#'
#' # Resolve by CIK
#' sec_resolve(cik = 320193)
#' }
sec_resolve <- function(cik = NULL, ticker = NULL, name = NULL) {
  df <- sec_entities()
  if (!is.null(cik))    df <- df |> filter(.data$cik == as.integer(!!cik))
  if (!is.null(ticker)) df <- df |> filter(toupper(.data$ticker) == toupper(!!ticker))
  if (!is.null(name))   df <- df |> filter(grepl(!!name, .data$name, ignore.case = TRUE))
  df
}

#' @title Search SEC entities by name (typeahead)
#' @description Searches the SEC EDGAR entity index using a typeahead-style query.
#'   Returns matching companies with their CIK and display name. This is a
#'   lightweight entity search — for full-text filing search, use secft_search().
#'   Use this when you have a partial company name and need to find the CIK.
#' @param query Character. Search string to match against entity names.
#'   Examples: "apple", "tesla motors", "berkshire hathaway".
#' @return A tibble with columns:
#'   \describe{
#'     \item{cik}{character. Central Index Key for the matched entity.}
#'     \item{name}{character. Entity display name with ticker, e.g., "Apple Inc. (AAPL)".}
#'   }
#' @examples
#' \dontrun{
#' # Search for companies by name
#' sec_search("artificial intelligence")
#'
#' # Find all entities matching "pharma"
#' sec_search("pharma")
#' }
sec_search <- function(query) {
  empty <- .schema_search
  url <- paste0("https://efts.sec.gov/LATEST/search-index?keysTyped=",
                utils::URLencode(query))
  raw <- .fetch_json(url)

  if (is.data.frame(raw)) return(as_tibble(raw) |> rename_with(tolower))
  hits <- raw$hits
  if (is.null(hits)) return(empty)
  if (is.data.frame(hits)) return(as_tibble(hits) |> rename_with(tolower))
  inner <- hits$hits
  if (!is.data.frame(inner)) return(empty)

  tibble(
    cik  = inner$`_id`,
    name = if (!is.null(inner$`_source`)) inner$`_source`$entity else NA_character_
  )
}


# == Company data ==============================================================

#' @title Get SEC filings for a company
#' @description Returns a tibble of all SEC filings submitted by a company, identified
#'   by CIK number. Includes filing metadata such as form type, dates, accession
#'   numbers, and document references. Use the accession number with sec_filing()
#'   to drill into any specific filing. By default returns recent filings only;
#'   set all = TRUE to include the full historical archive.
#' @param cik Integer or character. Central Index Key for the company.
#'   Example: 320193 (Apple), 789019 (Microsoft), 1318605 (Tesla).
#'   Use sec_resolve(ticker = "AAPL") to find a CIK.
#' @param form Character or NULL. Filter to a specific form type (case-insensitive).
#'   Common values: "10-K" (annual report), "10-Q" (quarterly), "8-K" (current event),
#'   "4" (insider transaction), "S-1" (IPO registration), "DEF 14A" (proxy).
#'   NULL returns all form types.
#' @param all Logical. If TRUE, fetches the complete filing history by following
#'   pagination files. If FALSE (default), returns only the most recent filings
#'   (typically ~40-100 filings).
#' @return A tibble with columns:
#'   \describe{
#'     \item{accessionnumber}{character. Unique filing identifier, e.g., "0000320193-24-000123". Pass to sec_filing().}
#'     \item{filingdate}{Date. Date the filing was submitted to EDGAR.}
#'     \item{reportdate}{Date. Period end date covered by the report.}
#'     \item{acceptancedatetime}{character. Timestamp when EDGAR accepted the filing.}
#'     \item{form}{character. Form type (e.g., "10-K", "10-Q", "8-K").}
#'     \item{primarydocument}{character. Filename of the primary document in the filing.}
#'     \item{size}{integer. Size of the primary document in bytes.}
#'     \item{isxbrl}{integer. 1 if the filing contains XBRL data, 0 otherwise.}
#'     \item{isinlinexbrl}{integer. 1 if the filing uses Inline XBRL, 0 otherwise.}
#'     \item{cik}{integer. The CIK used in the query.}
#'     \item{entity}{character. Company name as registered with the SEC.}
#'   }
#' @examples
#' \dontrun{
#' # Get Apple's recent 10-K filings
#' sec_filings(320193, form = "10-K")
#'
#' # Get all Tesla 8-K filings (full history)
#' sec_filings(1318605, form = "8-K", all = TRUE)
#'
#' # Get all recent filings for Microsoft (any form type)
#' sec_filings(789019)
#' }
sec_filings <- function(cik, form = NULL, all = FALSE) {
  raw <- .fetch_json(paste0("https://data.sec.gov/submissions/CIK",
                            .pad_cik(cik), ".json"))
  df <- as_tibble(raw$filings$recent)

  if (all && is.data.frame(raw$filings$files) && nrow(raw$filings$files) > 0) {
    older <- lapply(raw$filings$files$name, function(fname) {
      tryCatch(as_tibble(.fetch_json(paste0("https://data.sec.gov/submissions/", fname))),
               error = function(e) NULL)
    })
    df <- bind_rows(c(list(df), Filter(Negate(is.null), older)))
  }

  if (!is.null(form)) df <- df |> filter(toupper(.data$form) == toupper(.env$form))
  df |>
    rename_with(~ gsub("[. ]+", "_", tolower(.x))) |>
    mutate(
      cik = as.integer(!!cik), entity = !!raw$name,
      across(any_of(c("filingdate", "reportdate")), as.Date),
      across(any_of(c("size", "isxbrl", "isinlinexbrl")), as.integer)
    )
}

#' @title Get all XBRL facts for a company
#' @description Returns every structured financial fact reported by a company in XBRL
#'   format across all filings. This is the richest source of standardized
#'   financial data — revenue, assets, liabilities, shares outstanding, etc.
#'   Each row is one reported value from one filing, tagged with its XBRL
#'   taxonomy, concept tag, time period, and units. Use sec_concepts() instead
#'   if you want a single specific metric.
#' @param cik Integer or character. Central Index Key for the company.
#'   Example: 320193 (Apple), 789019 (Microsoft).
#' @return A tibble with columns:
#'   \describe{
#'     \item{end}{Date. Period end date for the reported value.}
#'     \item{val}{numeric. The reported value (e.g., revenue in USD, shares count).}
#'     \item{accn}{character. Accession number of the filing that reported this fact.}
#'     \item{fy}{integer. Fiscal year.}
#'     \item{fp}{character. Fiscal period ("FY", "Q1", "Q2", "Q3", "Q4").}
#'     \item{form}{character. Form type that reported this fact (e.g., "10-K", "10-Q").}
#'     \item{filed}{Date. Date the filing was submitted.}
#'     \item{start}{Date. Period start date (NA for instant/point-in-time values).}
#'     \item{frame}{character. XBRL frame identifier (e.g., "CY2023Q4I").}
#'     \item{taxonomy}{character. XBRL taxonomy namespace (e.g., "us-gaap", "dei", "invest").}
#'     \item{tag}{character. XBRL concept tag name (e.g., "Revenues", "Assets", "NetIncomeLoss").}
#'     \item{label}{character. Human-readable label for the tag.}
#'     \item{description}{character. Full description of the concept.}
#'     \item{unit}{character. Unit of measurement (e.g., "USD", "shares", "pure").}
#'     \item{cik}{integer. Company CIK.}
#'     \item{entity}{character. Company name.}
#'   }
#' @examples
#' \dontrun{
#' # Get all Apple financial facts
#' facts <- sec_facts(320193)
#'
#' # Filter to revenue only
#' facts |> dplyr::filter(tag == "Revenues", form == "10-K")
#'
#' # Find all available tags for a company
#' facts |> dplyr::distinct(taxonomy, tag, label)
#' }
sec_facts <- function(cik) {
  raw <- .fetch_json(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",
                            .pad_cik(cik), ".json"))
  rows <- list()
  for (taxonomy in names(raw$facts)) {
    for (tag_name in names(raw$facts[[taxonomy]])) {
      tag_data <- raw$facts[[taxonomy]][[tag_name]]
      for (unit_name in names(tag_data$units)) {
        obs <- tag_data$units[[unit_name]]
        if (is.null(obs) || length(obs) == 0) next
        rows[[length(rows) + 1]] <- as_tibble(obs) |>
          mutate(taxonomy = taxonomy, tag = tag_name,
                 label = tag_data$label %||% NA_character_,
                 description = tag_data$description %||% NA_character_,
                 unit = unit_name)
      }
    }
  }
  df <- bind_rows(rows)
  if (nrow(df) == 0) return(.schema_facts)
  df |> mutate(
    cik = as.integer(raw$cik), entity = raw$entityName,
    val = suppressWarnings(as.numeric(val)),
    fy = suppressWarnings(as.integer(fy)),
    across(any_of(c("end", "filed", "start")), as.Date)
  )
}

#' @title Get a single XBRL concept time series for a company
#' @description Returns the time series of a specific XBRL concept (financial metric)
#'   for a company. Unlike sec_facts() which returns everything, this focuses on
#'   one concept — making it ideal for tracking a specific metric over time
#'   (e.g., Revenue, NetIncomeLoss, Assets). Much faster than sec_facts() when
#'   you know the exact tag you want.
#' @param cik Integer or character. Central Index Key for the company.
#'   Example: 320193 (Apple).
#' @param taxonomy Character. XBRL taxonomy namespace. Common values:
#'   "us-gaap" (US GAAP financial concepts), "dei" (document/entity info),
#'   "invest" (investment company concepts), "srt" (SEC reporting taxonomy).
#' @param tag Character. XBRL concept tag name. Examples:
#'   "Revenues", "NetIncomeLoss", "Assets", "StockholdersEquity",
#'   "EarningsPerShareBasic", "CommonStockSharesOutstanding".
#'   Use sec_tags() to discover available tags.
#' @return A tibble with columns:
#'   \describe{
#'     \item{end}{Date. Period end date for the reported value.}
#'     \item{val}{numeric. The reported value.}
#'     \item{accn}{character. Accession number of the source filing.}
#'     \item{fy}{integer. Fiscal year.}
#'     \item{fp}{character. Fiscal period ("FY", "Q1", "Q2", "Q3", "Q4").}
#'     \item{form}{character. Form type (e.g., "10-K", "10-Q").}
#'     \item{filed}{Date. Filing submission date.}
#'     \item{unit}{character. Unit of measurement (e.g., "USD", "shares").}
#'     \item{cik}{integer. Company CIK.}
#'     \item{entity}{character. Company name.}
#'     \item{taxonomy}{character. XBRL taxonomy.}
#'     \item{tag}{character. XBRL concept tag.}
#'     \item{label}{character. Human-readable label.}
#'     \item{description}{character. Full concept description.}
#'   }
#' @examples
#' \dontrun{
#' # Get Apple's revenue history
#' sec_concepts(320193, "us-gaap", "Revenues")
#'
#' # Get Tesla's net income over time
#' sec_concepts(1318605, "us-gaap", "NetIncomeLoss")
#'
#' # Filter to annual reports only
#' sec_concepts(320193, "us-gaap", "Assets") |> dplyr::filter(form == "10-K")
#' }
sec_concepts <- function(cik, taxonomy, tag) {
  raw <- .fetch_json(paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK",
                            .pad_cik(cik), "/", taxonomy, "/", tag, ".json"))
  df <- bind_rows(lapply(names(raw$units), function(u) {
    as_tibble(raw$units[[u]]) |> mutate(unit = u)
  }))
  if (nrow(df) == 0) return(.schema_concepts)
  df |> mutate(
    cik = as.integer(raw$cik), entity = raw$entityName,
    taxonomy = raw$taxonomy, tag = raw$tag,
    label = raw$label %||% NA_character_,
    description = raw$description %||% NA_character_,
    val = suppressWarnings(as.numeric(val)),
    fy = suppressWarnings(as.integer(fy)),
    across(any_of(c("end", "filed", "start")), as.Date)
  )
}

#' @title List available XBRL tags (concept names)
#' @description Returns a tibble of distinct XBRL tags with human-readable labels and
#'   descriptions, derived from Apple's company facts as a representative sample.
#'   Use this to discover valid tag names for sec_concepts() or sec_frames().
#'   Results are cached in-session. Note: different companies may report
#'   additional tags not present in this list.
#' @param taxonomy Character or NULL. Filter to a specific taxonomy namespace.
#'   Common values: "us-gaap" (default, ~500 tags), "dei", "invest".
#'   NULL returns tags from all taxonomies.
#' @return A tibble with columns:
#'   \describe{
#'     \item{taxonomy}{character. Taxonomy namespace (e.g., "us-gaap", "dei").}
#'     \item{tag}{character. XBRL tag name to use in sec_concepts() or sec_frames().}
#'     \item{label}{character. Human-readable label (e.g., "Revenues", "Net Income (Loss)").}
#'     \item{description}{character. Full description of what the concept measures.}
#'   }
#' @examples
#' \dontrun{
#' # Browse all US-GAAP tags
#' sec_tags()
#'
#' # Search for revenue-related tags
#' sec_tags() |> dplyr::filter(grepl("revenue", label, ignore.case = TRUE))
#'
#' # Get tags from all taxonomies
#' sec_tags(taxonomy = NULL)
#' }
sec_tags <- function(taxonomy = "us-gaap") {
  key <- paste0("tags_", if (is.null(taxonomy)) "all" else taxonomy)
  if (!is.null(.sec_cache[[key]])) return(.sec_cache[[key]])

  facts <- sec_facts(320193)
  if (nrow(facts) == 0) return(.schema_tags)

  if (!is.null(taxonomy)) facts <- facts |> filter(.data$taxonomy == !!taxonomy)
  df <- facts |> distinct(taxonomy, tag, label, description)
  .sec_cache[[key]] <- df
  df
}


# == Cross-company =============================================================

#' @title Get a single XBRL concept across ALL companies for one period
#' @description Returns a cross-sectional snapshot of a single financial metric across
#'   all companies that reported it in a given period. This is the most powerful
#'   function for comparative analysis — e.g., "show me every company's revenue
#'   for Q4 2023." Returns thousands of rows (one per reporting company).
#' @param taxonomy Character. XBRL taxonomy namespace.
#'   Common values: "us-gaap", "dei", "invest".
#' @param tag Character. XBRL concept tag name.
#'   Examples: "Revenues", "Assets", "NetIncomeLoss", "AccountsPayableCurrent".
#'   Use sec_tags() to discover valid tags.
#' @param unit Character. Unit of measurement for the values.
#'   Common values: "USD" (US dollars), "shares" (share counts), "pure" (ratios/percentages).
#' @param period Character. XBRL frame period identifier. Format: "CY{year}Q{quarter}I"
#'   for quarterly instant values, "CY{year}Q{quarter}" for quarterly durations,
#'   "CY{year}" for annual durations.
#'   Examples: "CY2023Q4I" (Q4 2023 instant), "CY2023" (full year 2023).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cik}{integer. Company CIK.}
#'     \item{entityName}{character. Company name.}
#'     \item{loc}{character. State/country code of incorporation.}
#'     \item{end}{Date. Period end date.}
#'     \item{val}{numeric. Reported value.}
#'     \item{accn}{character. Accession number of the source filing.}
#'     \item{taxonomy}{character. XBRL taxonomy.}
#'     \item{tag}{character. XBRL concept tag.}
#'     \item{unit}{character. Unit of measurement.}
#'     \item{period}{character. Frame period identifier.}
#'     \item{label}{character. Human-readable label.}
#'     \item{description}{character. Full concept description.}
#'   }
#' @examples
#' \dontrun{
#' # All companies' revenues for 2023
#' sec_frames("us-gaap", "Revenues", "USD", "CY2023")
#'
#' # All companies' total assets at Q4 2023
#' sec_frames("us-gaap", "Assets", "USD", "CY2023Q4I")
#'
#' # Sort by value to find the largest
#' sec_frames("us-gaap", "Revenues", "USD", "CY2023") |>
#'   dplyr::arrange(dplyr::desc(val))
#' }
sec_frames <- function(taxonomy, tag, unit, period) {
  raw <- .fetch_json(paste0("https://data.sec.gov/api/xbrl/frames/",
                            taxonomy, "/", tag, "/", unit, "/", period, ".json"))
  df <- as_tibble(raw$data)
  if (nrow(df) == 0) return(.schema_frames)
  df |> mutate(
    taxonomy = raw$taxonomy, tag = raw$tag, unit = raw$uom,
    period = raw$ccp, label = raw$label %||% NA_character_,
    description = raw$description %||% NA_character_,
    val = suppressWarnings(as.numeric(val)),
    cik = as.integer(cik),
    across(any_of("end"), as.Date)
  )
}


# == Bulk datasets =============================================================

#' @title Download the full SEC submissions bulk dataset
#' @description Downloads and parses the complete SEC EDGAR bulk submissions archive
#'   (~3GB compressed). Contains recent filing metadata for every SEC registrant.
#'   This is a heavy operation — use sec_filings() for individual companies instead.
#'   Intended for building comprehensive filing databases or offline analysis.
#' @return A tibble with filing metadata for all companies, including columns like
#'   accessionnumber, filingdate, form, primarydocument, cik, entity_name,
#'   sic (industry code), sic_description. Expect millions of rows.
#' @examples
#' \dontrun{
#' # Download the full submissions archive (slow, large download)
#' all_filings <- sec_filings_bulk()
#'
#' # Filter to recent 10-Ks
#' all_filings |> dplyr::filter(form == "10-K", filingdate >= "2024-01-01")
#' }
sec_filings_bulk <- function() {
  zip <- .fetch("https://www.sec.gov/Archives/edgar/daily-index/bulkdata/submissions.zip", ".zip")
  dir <- file.path(tempdir(), "sec_submissions")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  utils::unzip(zip, exdir = dir, overwrite = TRUE)

  main <- list.files(dir, pattern = "^CIK\\d+\\.json$", full.names = TRUE)
  message(sprintf("Parsing %d company submissions...", length(main)))

  results <- lapply(main, function(f) {
    tryCatch({
      raw <- jsonlite::fromJSON(f)
      recent <- raw$filings$recent
      if (is.null(recent) || length(recent$accessionNumber) == 0) return(NULL)
      as_tibble(recent) |>
        mutate(cik = as.integer(raw$cik), entity_name = raw$name,
               sic = raw$sic, sic_description = raw$sicDescription)
    }, error = function(e) NULL)
  })
  bind_rows(results) |> rename_with(~ gsub("[. ]+", "_", tolower(.x)))
}

#' @title Download the full SEC company facts bulk dataset
#' @description Downloads and indexes the complete SEC EDGAR company facts archive
#'   (~2GB compressed). Returns a summary tibble listing each company's CIK,
#'   name, available taxonomies, and number of XBRL tags, plus a local file
#'   path to the full JSON for deeper parsing. Use sec_facts() for individual
#'   companies instead — this is for building comprehensive datasets.
#' @return A tibble with columns:
#'   \describe{
#'     \item{cik}{integer. Company CIK.}
#'     \item{entity}{character. Company name.}
#'     \item{taxonomies}{character. Comma-separated list of XBRL taxonomies reported.}
#'     \item{n_tags}{integer. Number of distinct XBRL tags reported.}
#'     \item{path}{character. Local file path to the extracted JSON for further parsing.}
#'   }
#' @examples
#' \dontrun{
#' # Download and index all company facts (slow, large download)
#' index <- sec_facts_bulk()
#'
#' # Find companies with the most XBRL data
#' index |> dplyr::arrange(dplyr::desc(n_tags))
#' }
sec_facts_bulk <- function() {
  zip <- .fetch("https://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip", ".zip")
  dir <- file.path(tempdir(), "sec_companyfacts")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  utils::unzip(zip, exdir = dir, overwrite = TRUE)

  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
  message(sprintf("Indexing %d company facts files...", length(files)))

  results <- lapply(files, function(f) {
    tryCatch({
      raw <- jsonlite::fromJSON(f, simplifyVector = FALSE)
      taxonomies <- names(raw$facts)
      n_tags <- sum(vapply(taxonomies, function(t) length(raw$facts[[t]]), integer(1)))
      tibble(cik = as.integer(raw$cik), entity = raw$entityName,
             taxonomies = paste(taxonomies, collapse = ","),
             n_tags = n_tags, path = f)
    }, error = function(e) NULL)
  })
  bind_rows(results)
}

#' @title Download SEC insider transaction bulk datasets
#' @description Downloads and parses insider transaction data sets from SEC.gov.
#'   These contain Form 3, 4, and 5 filings in structured TSV format, including
#'   derivative and non-derivative transactions, reporting owner details, and
#'   submission metadata. Filter by year and quarter to download specific periods.
#' @param year Integer or NULL. Filter to a specific year (e.g., 2024). NULL downloads all available.
#' @param quarter Integer (1-4) or NULL. Filter to a specific quarter. NULL downloads all quarters.
#' @return A tibble of insider transactions with columns from DERIV_TRANS and
#'   NONDERIV_TRANS datasets, joined with SUBMISSION and REPORTINGOWNER metadata.
#'   Key columns include accession_number, security_title, transaction_date,
#'   transaction_shares, transaction_price, owner CIK/name, and source_tsv.
#' @examples
#' \dontrun{
#' # Download insider transactions for Q1 2024
#' insiders <- sec_insiders_bulk(year = 2024, quarter = 1)
#'
#' # Download all 2023 insider transactions
#' insiders_2023 <- sec_insiders_bulk(year = 2023)
#' }
sec_insiders_bulk <- function(year = NULL, quarter = NULL) {
  links <- .scrape_zip_links(
    "https://www.sec.gov/data-research/sec-markets-data/insider-transactions-data-sets")
  if (nrow(links) == 0) return(tibble())

  if (!is.null(year))    links <- links |> filter(grepl(as.character(year), href))
  if (!is.null(quarter)) links <- links |> filter(grepl(paste0("q", quarter), href, ignore.case = TRUE))
  if (nrow(links) == 0) return(tibble())
  message(sprintf("Downloading %d insider transaction file(s)...", nrow(links)))

  n_total <- nrow(links)
  results <- lapply(seq_along(links$url), function(i) {
    message(sprintf("[%d/%d] %s", i, n_total, links$href[i]))
    tryCatch(.parse_insiders_zip(.fetch(links$url[i], ".zip")),
             error = function(e) { message("  Failed: ", e$message); NULL })
  })
  bind_rows(results)
}

#' @title Download SEC Form 13F bulk datasets
#' @description Downloads and parses 13F-HR institutional holdings data sets from SEC.gov.
#'   These contain quarterly holdings reports from institutional investment managers
#'   with >$100M in qualifying assets. Each row represents one security holding.
#'   Includes INFOTABLE (holdings), SUBMISSION, COVERPAGE, and OTHERMANAGER data.
#' @param n Integer or NULL. Maximum number of zip files to download (most recent first).
#'   NULL downloads all available files.
#' @param year Integer or NULL. Filter to a specific year (e.g., 2024).
#'   NULL includes all available years.
#' @return A tibble of institutional holdings with columns from the INFOTABLE dataset,
#'   joined with submission and cover page metadata. Key columns include
#'   accession_number, nameofissuer, titleofclass, cusip, value (in thousands),
#'   sshprnamt (shares), sshprnamttype, putcall, investmentdiscretion,
#'   votingauthority_sole/shared/none, and filingmanager_name.
#' @examples
#' \dontrun{
#' # Download the most recent 13F dataset
#' holdings <- sec_13fs_bulk(n = 1)
#'
#' # Download all 2024 13F data
#' holdings_2024 <- sec_13fs_bulk(year = 2024)
#' }
sec_13fs_bulk <- function(n = NULL, year = NULL) {
  links <- .scrape_zip_links(
    "https://www.sec.gov/data-research/sec-markets-data/form-13f-data-sets")
  if (nrow(links) == 0) return(tibble())
  if (!is.null(year)) links <- links |> filter(grepl(as.character(year), href))
  if (!is.null(n)) links <- head(links, n)
  if (nrow(links) == 0) return(tibble())
  message(sprintf("Downloading %d 13F file(s)...", nrow(links)))

  n_total <- nrow(links)
  results <- lapply(seq_along(links$url), function(i) {
    message(sprintf("[%d/%d] %s", i, n_total, links$href[i]))
    tryCatch(.parse_13f_zip(.fetch(links$url[i], ".zip")),
             error = function(e) { message("  Failed: ", e$message); NULL })
  })
  bind_rows(results)
}

#' @title Download SEC Fails-to-Deliver bulk datasets
#' @description Downloads and parses Fails-to-Deliver (FTD) data from SEC.gov. FTDs
#'   occur when a broker-dealer fails to deliver securities by the settlement date.
#'   Data is published twice monthly. Each row represents one security with
#'   outstanding delivery failures on a given settlement date. Useful for
#'   identifying heavily shorted or hard-to-borrow securities.
#' @param n Integer or NULL. Maximum number of zip files to download (most recent first).
#'   NULL downloads all available files.
#' @param year Integer or NULL. Filter to a specific year (e.g., 2024).
#' @param month Integer (1-12) or NULL. Filter to a specific month (requires year to also be set).
#' @return A tibble with columns:
#'   \describe{
#'     \item{settlement_date}{Date. Settlement date of the failure.}
#'     \item{cusip}{character. CUSIP identifier for the security.}
#'     \item{ticker}{character. Ticker symbol.}
#'     \item{quantity_fails}{numeric. Number of shares that failed to deliver.}
#'     \item{description}{character. Security description.}
#'     \item{price}{numeric. Closing price of the security on that date.}
#'   }
#' @examples
#' \dontrun{
#' # Download the most recent FTD file
#' ftds <- sec_ftds_bulk(n = 1)
#'
#' # Download FTD data for January 2024
#' ftds_jan <- sec_ftds_bulk(year = 2024, month = 1)
#'
#' # Find securities with the most fails
#' ftds |> dplyr::group_by(ticker) |>
#'   dplyr::summarise(total_fails = sum(quantity_fails, na.rm = TRUE)) |>
#'   dplyr::arrange(dplyr::desc(total_fails))
#' }
sec_ftds_bulk <- function(n = NULL, year = NULL, month = NULL) {
  links <- .scrape_zip_links(
    "https://www.sec.gov/data-research/sec-markets-data/fails-deliver-data")
  if (nrow(links) == 0) return(tibble())
  if (!is.null(year) && !is.null(month))
    links <- links |> filter(grepl(paste0(year, sprintf("%02d", as.integer(month))), href))
  else if (!is.null(year))
    links <- links |> filter(grepl(as.character(year), href))
  if (!is.null(n)) links <- head(links, n)
  if (nrow(links) == 0) return(tibble())
  message(sprintf("Downloading %d FTD file(s)...", nrow(links)))

  n_total <- nrow(links)
  results <- lapply(seq_along(links$url), function(i) {
    message(sprintf("[%d/%d] %s", i, n_total, links$href[i]))
    tryCatch(.parse_ftd_zip(.fetch(links$url[i], ".zip")),
             error = function(e) { message("  Failed: ", e$message); NULL })
  })
  bind_rows(results)
}


# == sec_filing: S3 object (the foundation) ====================================

#' @title Load a specific SEC filing as an S3 object
#' @description Creates a sec_filing S3 object from an accession number. This is the
#'   entry point for drilling into individual filings. The returned object is a
#'   tibble of all documents in the filing (HTML, XML, XBRL, exhibits, etc.)
#'   with rich metadata attached as attributes. Print it to see a summary.
#'   Pass the result to parser functions like sec_filing_xbrl(), sec_filing_text(),
#'   sec_filing_table(), etc. to extract structured data.
#' @param accession Character. The filing's accession number in SEC format.
#'   Example: "0000320193-24-000123" (Apple 10-K). Get accession numbers from
#'   sec_filings() via the accessionnumber column. Can also pass an existing
#'   sec_filing object (returned as-is).
#' @return An S3 object of class "sec_filing" (extends tibble). The tibble lists all
#'   documents in the filing with columns: name, last-modified, size, url,
#'   category (e.g., "primary", "xbrl_instance", "exhibit", "excel"),
#'   description, report_name, report_short, report_role, accession.
#'   Attributes: accession, cik, entity, form, filed, period, accepted, source_url.
#'   Print the object to see available parsers for the filing's form type.
#' @examples
#' \dontrun{
#' # Load an Apple 10-K filing
#' filing <- sec_filing("0000320193-24-000123")
#' print(filing)  # Shows summary with available parsers
#'
#' # Chain with parsers
#' sec_filing("0000320193-24-000123") |> sec_filing_xbrl()
#' sec_filing("0000320193-24-000123") |> sec_filing_text()
#'
#' # Get a filing from sec_filings() results
#' filings <- sec_filings(320193, form = "10-K")
#' filing <- sec_filing(filings$accessionnumber[1])
#' }
sec_filing <- function(accession) {
  if (inherits(accession, "sec_filing")) return(accession)
  docs <- .filing_docs(accession) |> mutate(accession = accession)

  # Extract metadata from -index.htm
  meta <- tryCatch({
    base <- .filing_url(accession)
    doc <- .fetch_html(paste0(base, accession, "-index.htm"))

    company_node <- xml2::xml_find_first(doc,
      ".//span[@class='companyName']|.//div[@class='companyName']")
    company_raw <- if (!is.na(company_node)) .clean_text(xml2::xml_text(company_node)) else NA
    company_name <- sub("\\s*\\(?(Filer|Issuer|Reporting).*$", "", company_raw)

    form_node <- xml2::xml_find_first(doc, ".//*[@id='formName']")
    form_raw <- if (!is.na(form_node)) .clean_text(xml2::xml_text(form_node)) else ""
    form_type <- sub("^Form\\s+", "", form_raw)
    form_type <- sub("\\s+-\\s+.*", "", form_type)

    content_node <- xml2::xml_find_first(doc, ".//div[@class='formContent']")
    content_text <- if (!is.na(content_node)) .clean_text(xml2::xml_text(content_node)) else ""
    extract <- function(label) {
      m <- regmatches(content_text, regexpr(paste0(label, "\\s+([0-9-]+)"), content_text))
      if (length(m) > 0) sub(paste0(label, "\\s+"), "", m) else NA_character_
    }

    list(entity = company_name,
         form   = if (nchar(form_type) > 0) form_type else NA,
         filed  = extract("Filing Date"),
         period = extract("Period of Report"),
         accepted = extract("Accepted"))
  }, error = function(e) list(entity = NA, form = NA, filed = NA, period = NA, accepted = NA))

  if (is.na(meta$form)) {
    primary_row <- docs |> filter(category == "primary")
    if (nrow(primary_row) > 0 && !is.na(primary_row$description[1]))
      meta$form <- .clean_text(primary_row$description[1])
  }

  attr(docs, "accession")  <- accession
  attr(docs, "cik")        <- as.integer(sub("-.*", "", accession))
  attr(docs, "entity")     <- meta$entity
  attr(docs, "form")       <- meta$form
  attr(docs, "filed")      <- meta$filed
  attr(docs, "period")     <- meta$period
  attr(docs, "accepted")   <- meta$accepted
  attr(docs, "source_url") <- .filing_url(accession)
  class(docs) <- c("sec_filing", class(docs))
  docs
}

#' @title Print a sec_filing summary
#' @description Prints a human-readable summary of a sec_filing object showing the
#'   company name, CIK, form type, filing date, period, document counts by
#'   category, and available parser functions for the filing type.
#' @param x A sec_filing object returned by sec_filing().
#' @param ... Additional arguments passed to the underlying print method.
#' @return Invisible sec_filing object (called for side effects).
print.sec_filing <- function(x, ...) {
  accn   <- attr(x, "accession")
  cik    <- attr(x, "cik")
  entity <- attr(x, "entity")
  form   <- attr(x, "form")
  filed  <- attr(x, "filed")
  period <- attr(x, "period")
  cats   <- table(x$category)

  cat(strrep("\u2500", 70), "\n")
  if (!is.na(entity)) cat(entity, "\n")
  cat(sprintf("CIK: %d | Form: %s | Filed: %s | Period: %s\n",
              cik, if (is.na(form)) "?" else form,
              if (is.na(filed)) "?" else filed,
              if (is.na(period)) "?" else period))
  cat(sprintf("Accession: %s\n", accn))
  cat(strrep("\u2500", 70), "\n")

  cat(sprintf("\n%d documents:\n", nrow(x)))
  cat_order <- c("primary", names(sort(cats[names(cats) != "primary"], decreasing = TRUE)))
  cat_order <- intersect(cat_order, names(cats))

  for (cat_name in cat_order) {
    rows <- x |> filter(category == cat_name)
    n <- nrow(rows)
    detail <- ""
    if (cat_name == "primary") {
      detail <- paste0("  ", rows$name[1])
    } else if (cat_name == "exhibit") {
      descs <- rows$description[!is.na(rows$description)]
      if (length(descs) > 0) detail <- paste0("  ", paste(descs, collapse = ", "))
    } else if (cat_name == "xbrl_report") {
      stmts <- rows$report_short[!is.na(rows$report_short)]
      if (length(stmts) > 0) detail <- paste0("  ", paste(head(stmts, 5), collapse = ", "))
      if (length(stmts) > 5) detail <- paste0(detail, ", ...")
    } else if (cat_name == "excel") {
      detail <- paste0("  ", rows$name[1])
    }
    cat(sprintf("  %-18s %3d%s\n", cat_name, n, detail))
  }

  if (!is.na(form)) {
    parsers <- sec_parsers(form)
    if (nrow(parsers) > 0)
      cat(sprintf("\nParsers: %s\n", paste(parsers$fn, collapse = ", ")))
  }

  # Show the tibble data
  cat("\n")
  NextMethod()
  invisible(x)
}

.as_filing <- function(x) {
  if (inherits(x, "sec_filing")) x else sec_filing(x)
}

.tag_result <- function(df, filing, parser) {
  df <- as_tibble(df)
  attr(df, "accession")  <- attr(filing, "accession")
  attr(df, "cik")        <- attr(filing, "cik")
  attr(df, "entity")     <- attr(filing, "entity")
  attr(df, "form")       <- attr(filing, "form")
  attr(df, "filed")      <- attr(filing, "filed")
  attr(df, "period")     <- attr(filing, "period")
  attr(df, "parser")     <- parser
  attr(df, "source_url") <- attr(filing, "source_url")
  df
}


# == Filing parsers: XBRL =====================================================

#' @title Parse raw XBRL facts from a filing
#' @description Extracts all XBRL-tagged facts from a filing's instance document.
#'   Returns every tagged value with its context reference, unit, and decimals.
#'   Works on 10-K, 10-Q, 20-F, 40-F, and other XBRL-tagged filings. For
#'   dimensional/segment breakdowns, use sec_filing_xbrl_segment() instead.
#'   For insider transactions (Form 3/4), use sec_filing_xbrl_ownership().
#' @param x A sec_filing object (from sec_filing()) or an accession number string.
#' @return A tibble with columns:
#'   \describe{
#'     \item{tag}{character. XBRL tag name (e.g., "Revenues", "Assets").}
#'     \item{value}{character. The reported value as text.}
#'     \item{context_ref}{character. XBRL context reference ID.}
#'     \item{unit_ref}{character. XBRL unit reference (e.g., "usd", "shares").}
#'     \item{decimals}{character. Precision indicator (e.g., "-6" for millions).}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Parse XBRL from an Apple 10-K
#' xbrl <- sec_filing("0000320193-24-000123") |> sec_filing_xbrl()
#'
#' # Filter to financial statement items
#' xbrl |> dplyr::filter(grepl("Revenue|Assets|Liabilities", tag))
#' }
sec_filing_xbrl <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  xbrl_doc <- .find_xbrl(filing)
  if (is.null(xbrl_doc))
    stop("No XBRL document found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(xbrl_doc$url)
  facts <- xml2::xml_find_all(doc, "//*[@contextRef]")
  if (length(facts) == 0)
    return(.schema_xbrl)

  df <- tibble(
    tag         = xml2::xml_name(facts),
    value       = xml2::xml_text(facts, trim = TRUE),
    context_ref = xml2::xml_attr(facts, "contextRef"),
    unit_ref    = xml2::xml_attr(facts, "unitRef"),
    decimals    = xml2::xml_attr(facts, "decimals"),
    accession   = accession
  )
  .tag_result(df, filing, "xbrl")
}

#' @title Parse XBRL dimensional/segment breakdowns from a filing
#' @description Extracts segment-level (dimensional) data from XBRL contexts. This
#'   reveals breakdowns that aggregate XBRL facts hide — e.g., revenue by
#'   geographic region, assets by business segment, or expenses by category.
#'   Joins segment dimensions with their associated fact values. Works on
#'   10-K and 10-Q filings that contain segment reporting in XBRL.
#' @param x A sec_filing object (from sec_filing()) or an accession number string.
#' @return A tibble with columns:
#'   \describe{
#'     \item{context_id}{character. XBRL context ID linking the segment to its values.}
#'     \item{dimension}{character. Dimension axis name (e.g., "StatementBusinessSegments", "GeographicArea").}
#'     \item{segment}{character. Segment member value (e.g., "AmericasSegment", "EuropeSegment").}
#'     \item{start_date}{Date. Period start date for the context.}
#'     \item{end_date}{Date. Period end date or instant date.}
#'     \item{xbrl_tag}{character. XBRL fact tag associated with this segment context.}
#'     \item{value}{character. Reported value for this segment/tag combination.}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Get segment breakdowns from Apple's 10-K
#' segments <- sec_filing("0000320193-24-000123") |> sec_filing_xbrl_segment()
#'
#' # Filter to geographic segments
#' segments |> dplyr::filter(grepl("Geographic", dimension))
#'
#' # See what dimensions are available
#' segments |> dplyr::distinct(dimension)
#' }
sec_filing_xbrl_segment <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  xbrl_doc <- .find_xbrl(filing)
  if (is.null(xbrl_doc))
    stop("No XBRL document found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(xbrl_doc$url)

  # Parse contexts with segments
  contexts <- xml2::xml_find_all(doc, "//*[local-name()='context']")
  ctx_list <- lapply(contexts, function(ctx) {
    id <- xml2::xml_attr(ctx, "id")
    instant <- xml2::xml_text(xml2::xml_find_first(ctx, ".//*[local-name()='instant']"))
    start   <- xml2::xml_text(xml2::xml_find_first(ctx, ".//*[local-name()='startDate']"))
    end     <- xml2::xml_text(xml2::xml_find_first(ctx, ".//*[local-name()='endDate']"))
    members <- xml2::xml_find_all(ctx, ".//*[local-name()='explicitMember']")
    if (length(members) == 0) return(NULL)
    tibble(
      context_id = id,
      dimension  = gsub("Axis$", "", gsub(".*:", "", xml2::xml_attr(members, "dimension"))),
      segment    = gsub("Member$", "", gsub(".*:", "", xml2::xml_text(members))),
      start_date = if (is.na(start)) instant else start,
      end_date   = if (is.na(end)) instant else end
    )
  })
  seg_df <- bind_rows(ctx_list)
  if (nrow(seg_df) == 0)
    return(.tag_result(.schema_xbrl_segment, filing, "xbrl_segment"))

  # Join with facts
  facts <- xml2::xml_find_all(doc, "//*[@contextRef]")
  if (length(facts) > 0) {
    fact_df <- tibble(
      context_id = xml2::xml_attr(facts, "contextRef"),
      xbrl_tag   = xml2::xml_name(facts),
      value      = xml2::xml_text(facts, trim = TRUE)
    )
    seg_df <- seg_df |> left_join(fact_df, by = "context_id", relationship = "many-to-many")
  } else {
    seg_df <- seg_df |> mutate(xbrl_tag = NA_character_, value = NA_character_)
  }

  seg_df |> mutate(accession = accession) |> .tag_result(filing, "xbrl_segment")
}

#' @title Parse insider ownership transactions from Form 3/4/5 filings
#' @description Parses the XML of SEC Forms 3, 4, and 5 to extract insider stock
#'   transactions. Returns both derivative and non-derivative transactions
#'   with share counts, prices, and post-transaction holdings. Includes
#'   reporting owner and issuer details. Use with accession numbers from
#'   sec_filings(cik, form = "4").
#' @param x A sec_filing object (from sec_filing()) or an accession number string
#'   for a Form 3, 4, or 5 filing.
#' @return A tibble with columns:
#'   \describe{
#'     \item{type}{character. "non_derivative" or "derivative".}
#'     \item{security_title}{character. Name of the security (e.g., "Common Stock", "Stock Option").}
#'     \item{tx_date}{Date. Transaction date.}
#'     \item{tx_code}{character. Transaction code: "P" (purchase), "S" (sale), "A" (grant), "M" (exercise), etc.}
#'     \item{shares}{numeric. Number of shares transacted.}
#'     \item{price_per_share}{numeric. Price per share (0 for grants/exercises).}
#'     \item{acquired_disposed}{character. "A" (acquired) or "D" (disposed).}
#'     \item{shares_after}{numeric. Total shares held after the transaction.}
#'     \item{ownership}{character. "D" (direct) or "I" (indirect) ownership.}
#'     \item{owner_cik}{character. CIK of the reporting owner (insider).}
#'     \item{owner_name}{character. Name of the insider.}
#'     \item{issuer_cik}{character. CIK of the issuing company.}
#'     \item{issuer_ticker}{character. Ticker of the issuing company.}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Get insider trades from a Form 4 filing
#' trades <- sec_filing("0001610717-24-000042") |> sec_filing_xbrl_ownership()
#'
#' # Find Form 4 filings for Apple, then parse the most recent one
#' f4s <- sec_filings(320193, form = "4")
#' sec_filing(f4s$accessionnumber[1]) |> sec_filing_xbrl_ownership()
#' }
sec_filing_xbrl_ownership <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")

  xml_doc <- filing |> filter(category %in% c("xbrl_instance", "xml"),
                              grepl("\\.xml$", name, ignore.case = TRUE))
  if (nrow(xml_doc) == 0)
    stop("No XML document found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(xml_doc$url[1])
  tree <- xml2::as_list(doc)
  root <- tree[[1]]

  owner <- root[["reportingOwner"]]
  owner_cik  <- unlist(owner[["reportingOwnerId"]][["rptOwnerCik"]])
  owner_name <- unlist(owner[["reportingOwnerId"]][["rptOwnerName"]])
  issuer_cik    <- unlist(root[["issuer"]][["issuerCik"]])
  issuer_ticker <- unlist(root[["issuer"]][["issuerTradingSymbol"]])

  parse_tx <- function(table_node, type) {
    if (is.null(table_node)) return(NULL)
    txs <- table_node[grepl("Transaction$", names(table_node))]
    lapply(txs, function(tx) {
      get_val <- function(...) {
        path <- list(...)
        node <- tx
        for (p in path) { node <- node[[p]]; if (is.null(node)) return(NA_character_) }
        val <- unlist(node)
        if (is.null(val)) NA_character_ else val[1]
      }
      tibble(
        type            = type,
        security_title  = get_val("securityTitle", "value"),
        tx_date         = get_val("transactionDate", "value"),
        tx_code         = get_val("transactionCoding", "transactionCode"),
        shares          = get_val("transactionAmounts", "transactionShares", "value"),
        price_per_share = get_val("transactionAmounts", "transactionPricePerShare", "value"),
        acquired_disposed = get_val("transactionAmounts", "transactionAcquiredDisposedCode", "value"),
        shares_after    = get_val("postTransactionAmounts", "sharesOwnedFollowingTransaction", "value"),
        ownership       = get_val("ownershipNature", "directOrIndirectOwnership", "value")
      )
    })
  }

  df <- bind_rows(
    bind_rows(parse_tx(root[["nonDerivativeTable"]], "non_derivative")),
    bind_rows(parse_tx(root[["derivativeTable"]], "derivative"))
  )
  if (nrow(df) == 0)
    return(.tag_result(.schema_xbrl_ownership, filing, "xbrl_ownership"))

  df |>
    mutate(
      owner_cik = owner_cik, owner_name = owner_name,
      issuer_cik = issuer_cik, issuer_ticker = issuer_ticker,
      across(any_of(c("shares", "price_per_share", "shares_after")),
             ~ suppressWarnings(as.numeric(.x))),
      across(any_of("tx_date"), as.Date),
      accession = accession
    ) |>
    .tag_result(filing, "xbrl_ownership")
}

#' @title Parse 13F-HR institutional holdings from a filing
#' @description Parses the information table XML from a Form 13F-HR filing to extract
#'   institutional investment holdings. Each row represents one security position
#'   held by the filing manager. Includes CUSIP identifiers, share counts, values,
#'   and voting authority breakdowns. Use with accession numbers from
#'   sec_filings(cik, form = "13F-HR").
#' @param x A sec_filing object (from sec_filing()) or an accession number string
#'   for a 13F-HR filing.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name_of_issuer}{character. Name of the security issuer (e.g., "APPLE INC").}
#'     \item{title_of_class}{character. Class of security (e.g., "COM", "CL A").}
#'     \item{cusip}{character. 9-character CUSIP identifier.}
#'     \item{value}{numeric. Market value of the holding in thousands of USD.}
#'     \item{shares}{numeric. Number of shares or principal amount held.}
#'     \item{shares_type}{character. "SH" (shares) or "PRN" (principal amount).}
#'     \item{put_call}{character. "PUT", "CALL", or NA for equity positions.}
#'     \item{investment_discretion}{character. "SOLE", "SHARED", or "DEFINED".}
#'     \item{voting_sole}{integer. Shares with sole voting authority.}
#'     \item{voting_shared}{integer. Shares with shared voting authority.}
#'     \item{voting_none}{integer. Shares with no voting authority.}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Parse Berkshire Hathaway's 13F holdings
#' filings <- sec_filings(1067983, form = "13F-HR")
#' holdings <- sec_filing(filings$accessionnumber[1]) |> sec_filing_xbrl_holding()
#'
#' # Find top holdings by value
#' holdings |> dplyr::arrange(dplyr::desc(value))
#' }
sec_filing_xbrl_holding <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")

  xml_docs <- filing |> filter(category %in% c("xml", "xbrl_instance"))
  info_doc <- xml_docs |> filter(grepl("infotable|information", name, ignore.case = TRUE))
  if (nrow(info_doc) == 0) info_doc <- xml_docs |> filter(grepl("\\.xml$", name, ignore.case = TRUE))
  if (nrow(info_doc) == 0)
    stop("No information table XML found in filing ", accession, call. = FALSE)

  doc <- .fetch_xml(info_doc$url[1])
  entries <- xml2::xml_find_all(doc, "//*[local-name()='infoTable']")
  if (length(entries) == 0)
    return(.tag_result(.schema_xbrl_holding, filing, "xbrl_holding"))

  tibble(
    name_of_issuer = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='nameOfIssuer']")),
    title_of_class = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='titleOfClass']")),
    cusip          = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='cusip']")),
    value          = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='value']")))),
    shares         = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='sshPrnamt']")))),
    shares_type    = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='sshPrnamtType']")),
    put_call       = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='putCall']")),
    investment_discretion = xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='investmentDiscretion']")),
    voting_sole    = suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='Sole']")))),
    voting_shared  = suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='Shared']")))),
    voting_none    = suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_first(entries, ".//*[local-name()='None']")))),
    accession      = accession
  ) |> .tag_result(filing, "xbrl_holding")
}


# == Filing parsers: table =====================================================

#' @title Extract all HTML tables from a filing's primary document
#' @description Parses and extracts every HTML table from the primary document of a
#'   filing. Returns tables in long (melted) format with table/row/column indices.
#'   Works on any filing type that has HTML tables — 10-K, 10-Q, 8-K, proxy
#'   statements, etc. Useful for extracting financial statements, compensation
#'   tables, or any other tabular data embedded in the filing.
#' @param x A sec_filing object (from sec_filing()) or an accession number string.
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{table_index}{integer. Which table in the document (1-based).}
#'     \item{row_index}{integer. Row number within the table (1-based, excludes header).}
#'     \item{col_index}{integer. Column number within the table (1-based).}
#'     \item{name}{character. Column header name for this cell.}
#'     \item{value}{character. Cell value as text.}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Extract all tables from an Apple 10-K
#' tables <- sec_filing("0000320193-24-000123") |> sec_filing_table()
#'
#' # See how many tables are in the filing
#' max(tables$table_index)
#'
#' # Pivot a specific table back to wide format
#' tables |> dplyr::filter(table_index == 5) |>
#'   tidyr::pivot_wider(names_from = name, values_from = value)
#' }
sec_filing_table <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  primary <- .find_primary(filing)
  if (is.null(primary))
    stop("No HTML document found in filing ", accession, call. = FALSE)

  doc <- .fetch_html(primary$url)
  tbls <- tryCatch(.html_tables(doc), error = function(e) list())

  results <- lapply(seq_along(tbls), function(i) {
    df <- tbls[[i]]
    if (nrow(df) == 0) return(NULL)
    nr <- nrow(df); nc <- ncol(df)
    tibble(
      table_index = i,
      row_index   = rep(seq_len(nr), each = nc),
      col_index   = rep(seq_len(nc), nr),
      name        = rep(names(df), nr),
      value       = .clean_text(c(t(as.matrix(df)))),
      accession   = accession
    )
  })
  bind_rows(results) |> .tag_result(filing, "table")
}

#' @title Extract subsidiary list from Exhibit 21
#' @description Parses Exhibit 21 (List of Subsidiaries) from a 10-K, 20-F, or 40-F
#'   filing. Returns the subsidiary names and jurisdictions of incorporation.
#'   First attempts to parse HTML tables; if no tables are found, falls back
#'   to extracting text lines. Exhibit 21 is required in annual reports and
#'   lists all significant subsidiaries of the registrant.
#' @param x A sec_filing object (from sec_filing()) or an accession number string
#'   for a 10-K, 20-F, or 40-F filing that contains Exhibit 21.
#' @return A tibble. If a table is found, columns match the exhibit's table headers
#'   (typically subsidiary name and jurisdiction), plus accession and source columns.
#'   If parsed as text, returns columns: value (character, one line per row),
#'   accession (character), source (character, "table" or "text").
#' @examples
#' \dontrun{
#' # Get Apple's subsidiary list from its 10-K
#' filings <- sec_filings(320193, form = "10-K")
#' subs <- sec_filing(filings$accessionnumber[1]) |> sec_filing_table_subsidiary()
#'
#' # View the subsidiaries
#' print(subs, n = 50)
#' }
sec_filing_table_subsidiary <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")

  ex21 <- filing |> filter(grepl("EX-21|exhibit.*21", description, ignore.case = TRUE))
  if (nrow(ex21) == 0)
    ex21 <- filing |> filter(grepl("ex.?21|exhibit.?21", name, ignore.case = TRUE))
  if (nrow(ex21) == 0)
    stop("No Exhibit 21 found in filing ", accession, call. = FALSE)

  doc <- .fetch_html(ex21$url[1])
  tbls <- tryCatch(.html_tables(doc), error = function(e) list())

  if (length(tbls) > 0) {
    sizes <- vapply(tbls, nrow, integer(1))
    best <- as_tibble(tbls[[which.max(sizes)]], .name_repair = "unique")
    best <- best |> filter(rowSums(is.na(pick(everything())) | pick(everything()) == "") < ncol(best))
    return(best |> mutate(accession = accession, source = "table") |>
             .tag_result(filing, "table_subsidiary"))
  }

  nodes <- xml2::xml_find_all(doc, ".//p|.//font|.//span|.//div")
  lines <- .clean_text(xml2::xml_text(nodes))
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0) {
    body <- xml2::xml_find_first(doc, ".//body")
    if (!is.na(body)) lines <- .clean_text(unlist(strsplit(xml2::xml_text(body), "\n")))
    lines <- lines[nchar(lines) > 0]
  }
  if (length(lines) == 0)
    return(.tag_result(tibble(value = character(), accession = character(),
                              source = character()), filing, "table_subsidiary"))


  tibble(value = lines, accession = accession, source = "text") |>
    .tag_result(filing, "table_subsidiary")
}


# == Filing parsers: text ======================================================

#' @title Extract plain text from a filing's primary document
#' @description Extracts all readable text from the primary HTML document of a filing,
#'   returning one row per text block (paragraph, heading, list item, or table cell).
#'   Whitespace is normalized. Useful for full-text analysis, keyword searching,
#'   or building text corpora from SEC filings. Works on any filing type.
#' @param x A sec_filing object (from sec_filing()) or an accession number string.
#' @return A tibble with columns:
#'   \describe{
#'     \item{line}{integer. Sequential line number (1-based).}
#'     \item{text}{character. Extracted text content, whitespace-normalized.}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Extract text from an Apple 10-K
#' txt <- sec_filing("0000320193-24-000123") |> sec_filing_text()
#'
#' # Search for risk factors
#' txt |> dplyr::filter(grepl("risk factor", text, ignore.case = TRUE))
#'
#' # Get word count
#' sum(lengths(strsplit(txt$text, "\\s+")))
#' }
sec_filing_text <- function(x) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  primary <- .find_primary(filing)
  if (is.null(primary))
    stop("No HTML document found in filing ", accession, call. = FALSE)

  doc <- .fetch_html(primary$url)
  nodes <- xml2::xml_find_all(doc, ".//p|.//h1|.//h2|.//h3|.//h4|.//h5|.//h6|.//li|.//td")

  lines <- if (length(nodes) > 0) {
    xml2::xml_text(nodes)
  } else {
    body <- xml2::xml_find_first(doc, ".//body")
    if (is.na(body)) return(.schema_text)
    unlist(strsplit(xml2::xml_text(body), "\n"))
  }
  lines <- .clean_text(lines)
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0)
    return(.schema_text)

  tibble(line = seq_along(lines), text = lines, accession = accession) |>
    .tag_result(filing, "text")
}


# == Filing parsers: xlsx ======================================================

#' @title Parse Excel report from a filing
#' @description Extracts data from the Financial_Report.xlsx (or similar Excel file)
#'   included in XBRL filings. Returns cell-level data in long format across
#'   all sheets. Requires the readxl package. Not all filings include Excel
#'   reports — this is most common in 10-K and 10-Q filings with Inline XBRL.
#' @param x A sec_filing object (from sec_filing()) or an accession number string.
#' @return A tibble with columns:
#'   \describe{
#'     \item{sheet_name}{character. Excel worksheet name (e.g., "Consolidated Balance Sheet").}
#'     \item{sheet_position}{integer. Sheet position in the workbook (1-based).}
#'     \item{row_index}{integer. Row number in the sheet.}
#'     \item{col_index}{integer. Column number in the sheet.}
#'     \item{name}{character. Row label from the first column (e.g., "Total Assets").}
#'     \item{value}{character. Cell value as text.}
#'     \item{accession}{character. Filing accession number.}
#'   }
#' @examples
#' \dontrun{
#' # Parse Excel from an Apple 10-K
#' xlsx <- sec_filing("0000320193-24-000123") |> sec_filing_xlsx()
#'
#' # See available sheets
#' xlsx |> dplyr::distinct(sheet_name, sheet_position)
#'
#' # Get data from a specific sheet
#' xlsx |> dplyr::filter(sheet_name == "Consolidated Balance Sheets")
#' }
sec_filing_xlsx <- function(x) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' required for sec_filing_xlsx", call. = FALSE)

  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  xlsx_doc <- .find_xlsx(filing)
  if (is.null(xlsx_doc))
    stop("No Excel report found in filing ", accession, call. = FALSE)

  ext <- if (grepl("\\.xls$", xlsx_doc$name)) ".xls" else ".xlsx"
  path <- .fetch(xlsx_doc$url, ext)
  sheet_names <- readxl::excel_sheets(path)

  results <- lapply(seq_along(sheet_names), function(i) {
    raw <- as_tibble(readxl::read_excel(path, sheet = sheet_names[i],
                                         col_names = FALSE, .name_repair = "unique"))
    if (nrow(raw) == 0) return(NULL)
    mat <- as.matrix(raw)
    mat <- ifelse(is.na(mat) | trimws(mat) == "", NA_character_, as.character(mat))
    idx <- which(!is.na(mat), arr.ind = TRUE)
    if (nrow(idx) == 0) return(NULL)
    row_labels <- as.character(raw[[1]])
    tibble(
      sheet_name = sheet_names[i], sheet_position = i,
      row_index = idx[, "row"], col_index = idx[, "col"],
      name = ifelse(!is.na(row_labels[idx[, "row"]]), row_labels[idx[, "row"]], ""),
      value = mat[idx], accession = accession
    )
  })
  bind_rows(results) |> .tag_result(filing, "xlsx")
}


# == Filing parsers: html ======================================================

#' @title Download a filing's primary HTML document to disk
#' @description Downloads the primary HTML document from a filing to a local file.
#'   Useful when you need the raw HTML for custom parsing, rendering in a browser,
#'   or archiving. Returns a tibble with the local file path and file size.
#' @param x A sec_filing object (from sec_filing()) or an accession number string.
#' @param dest Character or NULL. Destination file path. If NULL (default),
#'   saves to a temporary file with .html extension.
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{character. Filing accession number.}
#'     \item{document}{character. Filename of the downloaded document.}
#'     \item{path}{character. Local file path where the HTML was saved.}
#'     \item{bytes}{numeric. File size in bytes.}
#'   }
#' @examples
#' \dontrun{
#' # Download to a temp file
#' result <- sec_filing("0000320193-24-000123") |> sec_filing_html()
#' browseURL(result$path)
#'
#' # Download to a specific location
#' sec_filing("0000320193-24-000123") |>
#'   sec_filing_html(dest = "apple_10k.html")
#' }
sec_filing_html <- function(x, dest = NULL) {
  filing <- .as_filing(x)
  accession <- attr(filing, "accession")
  primary <- .find_primary(filing)
  if (is.null(primary))
    stop("No HTML document found in filing ", accession, call. = FALSE)

  if (is.null(dest)) dest <- tempfile(fileext = ".html")
  httr2::request(primary$url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = dest)

  tibble(accession = accession, document = primary$name,
         path = dest, bytes = file.size(dest)) |>
    .tag_result(filing, "html")
}


# == Parser discovery ==========================================================

#' @title List available filing parsers
#' @description Returns a tibble of all available sec_filing_* parser functions with
#'   descriptions of what they extract and which form types they support. Use this
#'   to discover which parser to call for a given filing type. Optionally filter
#'   to parsers compatible with a specific form type.
#' @param form Character or NULL. Filter to parsers compatible with this form type.
#'   Examples: "10-K", "10-Q", "8-K", "4", "13F-HR", "DEF-14A".
#'   NULL returns all parsers.
#' @return A tibble with columns:
#'   \describe{
#'     \item{parser}{character. Parser name (e.g., "xbrl", "table", "text").}
#'     \item{forms}{character. Space-separated list of compatible form types.}
#'     \item{description}{character. What the parser extracts.}
#'     \item{fn}{character. Function name to call (e.g., "sec_filing_xbrl").}
#'   }
#' @examples
#' \dontrun{
#' # See all available parsers
#' sec_parsers()
#'
#' # Which parsers work on 10-K filings?
#' sec_parsers("10-K")
#'
#' # Which parsers work on Form 4 (insider transactions)?
#' sec_parsers("4")
#' }
sec_parsers <- function(form = NULL) {
  all_parsers <- tibble(
    parser = c("xbrl", "xbrl_segment", "xbrl_ownership", "xbrl_holding",
               "table", "table_subsidiary", "text", "xlsx", "html"),
    forms  = c("10-K 10-Q 20-F 40-F 4 3 13F-HR N-PORT",
               "10-K 10-Q 20-F 40-F",
               "4 3",
               "13F-HR",
               "10-K 10-Q 20-F 40-F 8-K 4 13F-HR DEF-14A S-1",
               "10-K 20-F 40-F",
               "10-K 10-Q 20-F 40-F 8-K 4 13F-HR DEF-14A S-1",
               "10-K 10-Q 20-F 40-F",
               "10-K 10-Q 20-F 40-F 8-K 4 13F-HR DEF-14A S-1"),
    description = c(
      "Raw XBRL tagged facts from instance document",
      "Dimensional/segment breakdowns from XBRL contexts",
      "Form 3/4/5 insider ownership transactions",
      "13F-HR institutional holdings from information table",
      "All HTML tables from primary document",
      "Exhibit 21 subsidiary list (10-K/20-F/40-F)",
      "Plain text extraction from primary document",
      "Excel report sheets (Financial_Report.xlsx)",
      "Download primary HTML document to disk"),
    fn = c("sec_filing_xbrl", "sec_filing_xbrl_segment",
           "sec_filing_xbrl_ownership", "sec_filing_xbrl_holding",
           "sec_filing_table", "sec_filing_table_subsidiary",
           "sec_filing_text", "sec_filing_xlsx", "sec_filing_html")
  )
  if (is.null(form)) return(all_parsers)

  pattern <- paste0("\\b", toupper(form), "\\b")
  all_parsers |> filter(grepl(pattern, toupper(forms)))
}


# == EFTS (Full-Text Search) ==================================================

.efts_base <- "https://efts.sec.gov/LATEST/search-index"

.schema_search <- tibble(
  file_date = as.Date(character()), form = character(),
  display_names = character(), file_number = character(),
  cik = character(), period_ending = character(),
  file_description = character(), adsh = character()
)

#' @title Full-text search across all SEC EDGAR filings
#' @description Searches the EDGAR Full-Text Search System (EFTS) for filings containing
#'   specific words or phrases. Unlike sec_search() which finds entities by name,
#'   this searches the actual content of filed documents. Supports date range
#'   filtering and form type restrictions. Use secft_company() as a convenience
#'   wrapper for company-name searches, or secft_recent() for recent filings.
#' @param query Character. Search query string. Wrap in quotes for exact phrases.
#'   Examples: "artificial intelligence", "climate risk", "cybersecurity breach".
#'   Use "*" as a wildcard to match all filings (useful with date/form filters).
#' @param forms Character or NULL. Filter to specific form types.
#'   Examples: "10-K", "10-Q", "8-K", "4". Comma-separated for multiple: "10-K,10-Q".
#'   NULL searches all form types.
#' @param start_date Date or character ("YYYY-MM-DD") or NULL. Start of date range filter.
#'   Must be used together with end_date.
#' @param end_date Date or character ("YYYY-MM-DD") or NULL. End of date range filter.
#'   Must be used together with start_date.
#' @param limit Integer. Maximum number of results to return. Default 40, max 100.
#' @return A tibble with columns:
#'   \describe{
#'     \item{file_date}{Date. Filing date.}
#'     \item{form}{character. Form type (e.g., "10-K", "8-K").}
#'     \item{display_names}{character. Filer name(s), semicolon-separated if multiple.}
#'     \item{file_number}{character. SEC file number.}
#'     \item{cik}{character. CIK(s) of the filer, semicolon-separated if multiple.}
#'     \item{period_ending}{character. Period of report ending date.}
#'     \item{file_description}{character. Filing description text.}
#'     \item{adsh}{character. Accession number (use with sec_filing() to drill in).}
#'   }
#' @examples
#' \dontrun{
#' # Search for filings mentioning AI
#' secft_search("artificial intelligence", forms = "10-K")
#'
#' # Search for cybersecurity incidents in 8-Ks this year
#' secft_search("cybersecurity incident", forms = "8-K",
#'              start_date = "2024-01-01", end_date = "2024-12-31")
#'
#' # Get recent filings of any type
#' secft_search("*", forms = "10-K", start_date = "2024-06-01",
#'              end_date = "2024-06-30", limit = 100)
#' }
secft_search <- function(query, forms = NULL, start_date = NULL,
                         end_date = NULL, limit = 40) {
  params <- list(q = paste0('"', query, '"'))
  if (!is.null(forms)) params$forms <- forms
  if (!is.null(start_date) && !is.null(end_date)) {
    params$dateRange <- "custom"
    params$startdt <- as.character(start_date)
    params$enddt <- as.character(end_date)
  }

  query_str <- paste(names(params), vapply(params, utils::URLencode, character(1), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- paste0(.efts_base, "?", query_str)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("EFTS search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_search)

  hits <- raw$hits$hits
  if (is.null(hits) || length(hits) == 0) return(.schema_search)

  src <- hits$`_source`
  if (is.null(src)) return(.schema_search)

  result <- as_tibble(src)
  n_rows <- min(nrow(result), limit)
  result <- result[seq_len(n_rows), ]

  dnames <- if ("display_names" %in% names(result)) {
    vapply(result$display_names, function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
    }, character(1))
  } else rep(NA_character_, n_rows)

  tibble(
    file_date        = tryCatch(as.Date(result$file_date %||% NA_character_), error = function(e) as.Date(rep(NA, n_rows))),
    form             = as.character(result$form %||% NA_character_),
    display_names    = dnames,
    file_number      = as.character(result$file_num %||% NA_character_),
    cik              = vapply(result$ciks, function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
    }, character(1)),
    period_ending    = as.character(result$period_ending %||% NA_character_),
    file_description = as.character(result$file_description %||% NA_character_),
    adsh             = as.character(result$adsh %||% NA_character_)
  )
}

#' @title Search SEC EDGAR filings by company name
#' @description Convenience wrapper around secft_search() that searches for filings
#'   by company name. Returns recent filings filed by or mentioning the company.
#'   For entity lookup (CIK/ticker resolution), use sec_resolve() instead.
#' @param company Character. Company name to search for.
#'   Examples: "Apple", "Tesla", "Berkshire Hathaway", "Goldman Sachs".
#' @param forms Character or NULL. Filter to specific form types (e.g., "10-K", "8-K").
#'   NULL searches all form types.
#' @param limit Integer. Maximum number of results to return. Default 20.
#' @return A tibble with columns: file_date (Date), form (character),
#'   display_names (character), file_number (character), cik (character),
#'   period_ending (character), file_description (character), adsh (character).
#' @examples
#' \dontrun{
#' # Find recent Apple filings
#' secft_company("Apple", forms = "10-K")
#'
#' # Find Tesla 8-K filings
#' secft_company("Tesla", forms = "8-K", limit = 50)
#' }
secft_company <- function(company, forms = NULL, limit = 20) {
  secft_search(company, forms = forms, limit = limit)
}

#' @title Get the most recent SEC filings by form type
#' @description Returns the most recent filings of a specific form type within a
#'   lookback window. Useful for monitoring new filings across all companies.
#'   Wraps secft_search() with a date range from (today - days) to today.
#' @param forms Character. Form type to filter by. Default "10-K".
#'   Common values: "10-K" (annual reports), "10-Q" (quarterly), "8-K" (current events),
#'   "4" (insider trades), "S-1" (IPO registrations), "DEF 14A" (proxy statements).
#' @param days Integer. Number of days to look back from today. Default 30.
#' @param limit Integer. Maximum number of results to return. Default 20.
#' @return A tibble with columns: file_date (Date), form (character),
#'   display_names (character), file_number (character), cik (character),
#'   period_ending (character), file_description (character), adsh (character).
#' @examples
#' \dontrun{
#' # Get 10-K filings from the last 30 days
#' secft_recent()
#'
#' # Get recent 8-K filings from the last week
#' secft_recent(forms = "8-K", days = 7, limit = 50)
#'
#' # Get recent S-1 filings (IPO registrations) from the last 90 days
#' secft_recent(forms = "S-1", days = 90)
#' }
secft_recent <- function(forms = "10-K", days = 30, limit = 20) {
  end_date <- Sys.Date()
  start_date <- end_date - days
  secft_search("*", forms = forms, start_date = start_date,
               end_date = end_date, limit = limit)
}


# == Context ===================================================================

#' @title Get sec.gov client context for LLM consumption
#' @description Returns the complete roxygen documentation and function signatures for
#'   every public function in this client, without implementation details. Designed
#'   to be read by an LLM to understand what functions are available, what
#'   parameters they accept, and what data they return. Call this first to
#'   discover the API surface before calling specific functions.
#' @return Character string containing formatted documentation for all public
#'   functions (printed to console and returned invisibly).
#' @examples
#' \dontrun{
#' # Print the full client documentation
#' sec_context()
#' }
#' @export
sec_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sec_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/sec.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "sec.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# sec.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# sec.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
