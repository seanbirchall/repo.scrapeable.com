# philasd.org.R - Self-contained Philadelphia School District client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required
# Note: Data is in ZIP archives containing Excel files with complex headers

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(readxl)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.psd_base <- "https://cdn.philasd.org/offices/performance/Open_Data"

.fetch_file <- function(url, ext = ".zip") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_zip_excel <- function(url, sheet = 1, skip = 0) {
  f <- .fetch_file(url, ext = ".zip")
  d <- tempfile()
  dir.create(d)
  utils::unzip(f, exdir = d)
  xlsx_files <- list.files(d, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  if (length(xlsx_files) == 0) {
    xls_files <- list.files(d, pattern = "\\.xls$", full.names = TRUE, recursive = TRUE)
    csv_files <- list.files(d, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
    if (length(csv_files) > 0) {
      return(lapply(csv_files, function(cf) {
        tryCatch(utils::read.csv(cf, stringsAsFactors = FALSE) |> as_tibble(),
                 error = function(e) tibble())
      }))
    }
    if (length(xls_files) > 0) xlsx_files <- xls_files
  }
  if (length(xlsx_files) == 0) return(list(tibble()))
  lapply(xlsx_files, function(xf) {
    tryCatch(readxl::read_excel(xf, sheet = sheet, skip = skip) |> as_tibble(),
             error = function(e) tibble())
  })
}

# == Dataset definitions =======================================================

.psd_datasets <- tibble(
  id = c("pssa_2016_2017", "pssa_2015_2016", "pssa_2014_2015",
         "pssa_2013_2014", "pssa_2012_2013", "pssa_2011_2012",
         "pssa_2010_2011", "pssa_2009_2010", "pssa_all_years"),
  name = c("PSSA & Keystone 2016-17", "PSSA & Keystone 2015-16",
           "PSSA & Keystone 2014-15", "PSSA & Keystone 2013-14",
           "PSSA & Keystone 2012-13", "PSSA 2011-12",
           "PSSA 2010-11", "PSSA 2009-10", "PSSA & Keystone All Years"),
  url = c(
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2016_2017_PSSA_Keystone_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2015_2016_PSSA_Keystone_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2014_2015_PSSA_Keystone_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2013_2014_PSSA_Keystone_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2012_2013_PSSA_Keystone_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2011_2012_PSSA_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2010_2011_PSSA_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/2009_2010_PSSA_All_Data.zip"),
    paste0(.psd_base, "/School_Performance/PSSA_Keystone/PSSA_Keystone_All_Years.zip")
  ),
  category = rep("pssa_keystone", 9)
)

# == Public functions ==========================================================

#' List available Philadelphia School District datasets
#'
#' Returns a catalog of PSSA and Keystone standardized test datasets
#' published by the School District of Philadelphia. Each dataset
#' corresponds to one academic year (or "all years") and is distributed
#' as a ZIP archive containing Excel workbooks.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Dataset identifier (e.g. \code{"pssa_2016_2017"}).}
#'     \item{name}{Character. Human-readable name.}
#'     \item{url}{Character. Download URL for the ZIP archive.}
#'     \item{category}{Character. Dataset category (\code{"pssa_keystone"}).}
#'   }
#'
#' @examples
#' psd_list()
#'
#' @seealso \code{\link{psd_data}}, \code{\link{psd_sheets}},
#'   \code{\link{psd_zip_contents}}
#' @export
psd_list <- function() {
  .psd_datasets
}

#' List files inside a PSSA/Keystone ZIP archive
#'
#' Downloads the ZIP archive for a given dataset and lists its file entries
#' without extracting the data. Useful for inspecting what Excel or CSV
#' files are bundled in the archive before calling \code{psd_data()}.
#'
#' @param dataset_id Character. Dataset identifier from \code{psd_list()}
#'   (e.g. \code{"pssa_2016_2017"}).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{Name}{Character. File path within the archive.}
#'     \item{Length}{Integer. Uncompressed file size in bytes.}
#'     \item{Date}{Date. Modification date of the archived file.}
#'   }
#'
#' @examples
#' psd_zip_contents("pssa_2016_2017")
#'
#' @seealso \code{\link{psd_list}}, \code{\link{psd_data}},
#'   \code{\link{psd_sheets}}
#' @export
psd_zip_contents <- function(dataset_id) {
  match <- .psd_datasets |> filter(.data$id == !!dataset_id)
  if (nrow(match) == 0) {
    stop("Unknown dataset: ", dataset_id, call. = FALSE)
  }
  f <- .fetch_file(match$url[1], ext = ".zip")
  utils::unzip(f, list = TRUE) |> as_tibble()
}

#' Download and extract a PSSA/Keystone dataset
#'
#' Downloads a ZIP archive from the School District of Philadelphia CDN,
#' extracts it to a temporary directory, and reads every Excel workbook
#' (or CSV) found inside. Returns a list of tibbles, one per file.
#'
#' @details These Excel files often have complex multi-row headers with
#'   merged cells. Use \code{psd_sheets()} first to discover sheet names,
#'   then adjust \code{sheet} and \code{skip} to target the correct data
#'   range. A common pattern is \code{skip = 3} or \code{skip = 4} to
#'   bypass header rows.
#'
#' @param dataset_id Character. Dataset identifier from \code{psd_list()}
#'   (e.g. \code{"pssa_2016_2017"}, \code{"pssa_all_years"}).
#' @param sheet Integer or character. Sheet number or name to read from
#'   each Excel file (default 1).
#' @param skip Integer. Number of leading rows to skip before reading
#'   data (default 0). Increase to bypass multi-row headers.
#'
#' @return A list of tibbles, one per Excel (or CSV) file in the archive.
#'   Column names and types depend on the specific dataset.
#'
#' @examples
#' # Read the most recent PSSA data
#' data <- psd_data("pssa_2016_2017", skip = 3)
#' str(data[[1]])
#'
#' @seealso \code{\link{psd_list}}, \code{\link{psd_sheets}},
#'   \code{\link{psd_zip_contents}}
#' @export
psd_data <- function(dataset_id, sheet = 1, skip = 0) {
  match <- .psd_datasets |> filter(.data$id == !!dataset_id)
  if (nrow(match) == 0) {
    stop("Unknown dataset: ", dataset_id, call. = FALSE)
  }
  .fetch_zip_excel(match$url[1], sheet = sheet, skip = skip)
}

#' List sheets in the Excel files of a PSSA dataset
#'
#' Downloads the ZIP archive and inspects each Excel workbook to report
#' available sheet names. Use this to determine which \code{sheet}
#' argument to pass to \code{psd_data()}.
#'
#' @param dataset_id Character. Dataset identifier from \code{psd_list()}
#'   (e.g. \code{"pssa_2016_2017"}).
#'
#' @return A named list where each element name is a filename (basename)
#'   and each value is a character vector of sheet names in that workbook.
#'
#' @examples
#' psd_sheets("pssa_2016_2017")
#'
#' @seealso \code{\link{psd_list}}, \code{\link{psd_data}},
#'   \code{\link{psd_zip_contents}}
#' @export
psd_sheets <- function(dataset_id) {
  match <- .psd_datasets |> filter(.data$id == !!dataset_id)
  if (nrow(match) == 0) stop("Unknown dataset: ", dataset_id, call. = FALSE)
  f <- .fetch_file(match$url[1], ext = ".zip")
  d <- tempfile(); dir.create(d)
  utils::unzip(f, exdir = d)
  xlsx_files <- list.files(d, pattern = "\\.xlsx?$", full.names = TRUE, recursive = TRUE)
  result <- lapply(xlsx_files, function(xf) {
    tryCatch(readxl::excel_sheets(xf), error = function(e) character(0))
  })
  names(result) <- basename(xlsx_files)
  result
}

#' Get philasd.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
psd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(psd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/philasd.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "philasd.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# philasd.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# philasd.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
