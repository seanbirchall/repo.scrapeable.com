#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_timeout
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# nrc.gov - Self-contained NRC (Nuclear Regulatory Commission) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required (public data)
# Data sources: TSV/pipe-delimited files from nrc.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nrc_base <- "https://www.nrc.gov"
.nrc_cdn  <- "https://www.nrc.gov/cdn/data/rango"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".txt") {
  tmp <- tempfile(fileext = ext)
  for (attempt in 1:4) {
    ok <- tryCatch({
      httr2::request(url) |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_timeout(seconds = 60) |>
        httr2::req_perform(path = tmp)
      TRUE
    }, error = function(e) {
      if (attempt < 4) { Sys.sleep(attempt); FALSE }
      else stop(e)
    })
    if (ok) break
  }
  tmp
}

.fetch_tsv <- function(url) {
  f <- .fetch(url, ext = ".tsv")
  utils::read.delim(f, sep = "\t", stringsAsFactors = FALSE,
                    check.names = FALSE, na.strings = c("", "NA"))
}

.fetch_pipe <- function(url) {
  f <- .fetch(url, ext = ".txt")
  utils::read.delim(f, sep = "|", stringsAsFactors = FALSE,
                    check.names = FALSE, na.strings = c("", "NA"))
}

.safe_date <- function(x, fmt = "%m/%d/%Y") {
  suppressWarnings(as.Date(x, format = fmt))
}

# == Schemas ===================================================================

.schema_reactor_status <- tibble(
  report_date = as.Date(character()), unit = character(), power_pct = integer()
)

.schema_findings <- tibble(
  procedure = character(), issue_date = as.Date(character()),
  report_number = character(), type = character(),
  cornerstone_code = character(), cornerstone = character(),
  title = character(), docket_number = character(),
  site_name = character(), site_code = character(),
  cross_cutting_aspect = character(), identified_by = character(),
  significance = character(), region = integer(),
  accession_number = character(), link = character(),
  is_traditional_enforcement = logical(),
  severity_type_code = character(),
  cornerstone_attribute_type = character(),
  year = integer(), quarter = integer(),
  cross_cutting_area = character()
)

.schema_pi <- tibble(
  date = character(), docket = character()
)
