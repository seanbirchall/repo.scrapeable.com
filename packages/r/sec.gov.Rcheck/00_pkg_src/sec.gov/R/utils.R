library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tibble)

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
  case <- function(pattern) grepl(pattern, n)

  ifelse(n == tolower(primary_name),                           "primary",
  ifelse(case("^r\\d+\\.htm"),                                 "xbrl_report",
  ifelse(case("filingsummary\\.xml$"),                         "xbrl_summary",
  ifelse(case("_htm\\.xml$"),                                  "xbrl_instance",
  ifelse(case("_cal\\.xml$|_def\\.xml$|_lab\\.xml$|_pre\\.xml$|_ref\\.xml$|\\.xsd$"),
                                                               "xbrl_schema",
  ifelse(case("financial_report\\.xlsx?$|report\\.xlsx?$"),    "excel",
  ifelse(case("^ex|exhibit"),                                  "exhibit",
  ifelse(case("\\.jpg$|\\.gif$|\\.png$|\\.bmp$"),              "graphic",
  ifelse(case("index"),                                        "index",
  ifelse(case("\\.xml$") & !case("filingsummary|_cal|_def|_lab|_pre|_ref"),
                                                               "xml",
  ifelse(case("\\.(htm|html)$"),                               "html",
         "other")))))))))))
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
        left_join(lookup, by = c("name" = "document"), relationship = "many-to-many") |>
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


