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
#' The National Credit Union Administration publishes quarterly financial
#' data for all federally insured credit unions in the United States.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Reporting year.}
#'     \item{quarter}{Integer. Reporting quarter (1--4).}
#'     \item{zip_url}{Character. URL to download the ZIP archive.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ncua_list()
#' }
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
#' Downloads and parses the quarterly call report ZIP file for the
#' specified year and quarter. The data contains detailed financial
#' information for all federally insured credit unions.
#'
#' @param year Integer. Reporting year (e.g. \code{2024}).
#' @param quarter Integer. Reporting quarter (1--4, maps to months 03/06/09/12).
#' @param max_rows Integer. Maximum rows to return (default 10000).
#' @return A tibble of credit union financial data. Columns include
#'   charter number, credit union name, state, assets, loans, shares,
#'   and other financial metrics.
#' @export
#' @examples
#' \dontrun{
#' ncua_data(2024, 4, max_rows = 100)
#' }
ncua_data <- function(year, quarter, max_rows = 10000) {
  month <- sprintf("%02d", quarter * 3)
  url <- sprintf("%s/files/publications/analysis/call-report-data-%d-%s.zip",
                 .ncua_base, year, month)
  .fetch_zip_csv(url, max_rows = max_rows)
}

#' Search credit unions by name in the latest call report
#'
#' Downloads the most recent quarterly call report data and filters
#' by credit union name. Note: this downloads a large file on first call.
#'
#' @param name Character. Credit union name pattern (case-insensitive
#'   regular expression).
#' @param max_rows Integer. Max rows to scan from the full dataset
#'   (default 50000).
#' @return A tibble of matching credit unions with full financial data.
#' @export
#' @examples
#' \dontrun{
#' ncua_search("Navy Federal")
#' ncua_search("teachers", max_rows = 20000)
#' }
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
#' Downloads a small sample (100 rows) from the latest quarterly call
#' report and returns column metadata including names, types, and
#' sample values. Useful for understanding the data schema before
#' querying with \code{\link{ncua_data}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{column}{Character. Column name from the call report.}
#'     \item{type}{Character. R class of the column.}
#'     \item{sample}{Character. First non-NA value as a string.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ncua_summary()
#' }
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

#' Get ncua.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ncua_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ncua_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ncua.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ncua.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ncua.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ncua.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
