# ncua.gov.R - National Credit Union Administration data client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ncua_base <- "https://www.ncua.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".zip") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_zip_csv <- function(url, max_rows = 10000) {
  zip_path <- .fetch(url, ext = ".zip")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(zip_path, exdir = tmp_dir)
  csvs <- list.files(tmp_dir, pattern = "\\.(csv|txt)$", full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  if (length(csvs) == 0) {
    warning("No CSV/TXT files found in ZIP")
    return(tibble())
  }
  target <- csvs[which.max(file.size(csvs))]
  df <- tryCatch(
    utils::read.csv(target, stringsAsFactors = FALSE, check.names = FALSE,
                    nrows = max_rows),
    error = function(e) {
      tryCatch(
        utils::read.delim(target, stringsAsFactors = FALSE, check.names = FALSE,
                          nrows = max_rows),
        error = function(e2) data.frame()
      )
    }
  )
  tibble::as_tibble(df)
}

# == Public functions ==========================================================

#' List available NCUA quarterly call report data files
#'
#' Scrapes the NCUA quarterly data page for available ZIP download links.
#' @return tibble with year, quarter, zip_url
ncua_list <- function() {
  tmp <- tempfile(fileext = ".html")
  httr2::request("https://ncua.gov/analysis/credit-union-corporate-call-report-data/quarterly-data") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  html <- readLines(tmp, warn = FALSE)
  html_text <- paste(html, collapse = "\n")

  # Extract ZIP URLs
  m <- gregexpr('href="(/files/publications/analysis/call-report-data-[^"]+\\.zip)"', html_text)
  matches <- regmatches(html_text, m)[[1]]
  if (length(matches) == 0) return(tibble(year = integer(), quarter = integer(), zip_url = character()))

  paths <- gsub('href="([^"]+)"', '\\1', matches)

  # Parse year/quarter from filenames
  years <- as.integer(gsub(".*call-report-data-(\\d{4})-.*", "\\1", paths))
  months <- as.integer(gsub(".*call-report-data-\\d{4}-(\\d{2})\\.zip", "\\1", paths))
  quarters <- ceiling(months / 3)

  tibble(
    year    = years,
    quarter = quarters,
    zip_url = paste0(.ncua_base, paths)
  ) |> dplyr::arrange(dplyr::desc(year), dplyr::desc(quarter))
}

#' Download NCUA call report data for a specific period
#'
#' @param year Year (e.g. 2024).
#' @param quarter Quarter (1-4, corresponds to month 03/06/09/12).
#' @param max_rows Maximum rows to return (default 10000).
#' @return tibble of credit union financial data
ncua_data <- function(year, quarter, max_rows = 10000) {
  month <- sprintf("%02d", quarter * 3)
  url <- sprintf("%s/files/publications/analysis/call-report-data-%d-%s.zip",
                 .ncua_base, year, month)
  .fetch_zip_csv(url, max_rows = max_rows)
}

#' Search credit unions by name in the latest call report
#'
#' Downloads the most recent quarterly data and filters by name.
#' @param name Credit union name pattern (case-insensitive grep).
#' @param max_rows Max rows to scan (default 50000).
#' @return tibble of matching credit unions
ncua_search <- function(name, max_rows = 50000) {
  avail <- ncua_list()
  if (nrow(avail) == 0) {
    warning("No call report data found")
    return(tibble())
  }
  latest <- avail[1, ]
  df <- ncua_data(latest$year, latest$quarter, max_rows = max_rows)
  if (nrow(df) == 0) return(tibble())

  # Find name column (often "CU_NAME" or similar)
  name_cols <- grep("name|cu_name", names(df), ignore.case = TRUE, value = TRUE)
  if (length(name_cols) == 0) {
    warning("No name column found in data")
    return(df)
  }
  name_col <- name_cols[1]
  df |> dplyr::filter(grepl(name, .data[[name_col]], ignore.case = TRUE))
}

#' Get summary statistics from the latest call report
#'
#' @return tibble with column names, types, and sample values from latest data
ncua_summary <- function() {
  avail <- ncua_list()
  if (nrow(avail) == 0) return(tibble())
  latest <- avail[1, ]
  df <- ncua_data(latest$year, latest$quarter, max_rows = 100)
  if (nrow(df) == 0) return(tibble())

  tibble(
    column = names(df),
    type   = vapply(df, function(x) class(x)[1], character(1)),
    sample = vapply(df, function(x) {
      vals <- x[!is.na(x)]
      if (length(vals) == 0) return(NA_character_)
      as.character(vals[1])
    }, character(1))
  )
}

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
ncua_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(ncua_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "ncua.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# ncua.gov R client",
      "# Functions: ncua_list, ncua_data, ncua_search, ncua_summary, ncua_context",
      "# NCUA credit union call report data (quarterly ZIP files)",
      sep = "\n"
    )
    cat(msg, "\n"); return(invisible(msg))
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
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c("# ncua.gov R client", "# National Credit Union Administration", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
