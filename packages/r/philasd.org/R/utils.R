#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# philasd.org.R - Self-contained Philadelphia School District client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required
# Note: Data is in ZIP archives containing Excel files with complex headers


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

