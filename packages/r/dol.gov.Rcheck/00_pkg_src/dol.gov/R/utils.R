#' @importFrom httr2 request req_headers req_perform
#' @importFrom tibble tibble as_tibble
#' @importFrom xml2 read_html xml_find_all xml_attr
#' @import dplyr
#' @keywords internal
NULL

# dol.R
# Self-contained Department of Labor foreign labor disclosure client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2, readxl
# Auth: none
# Data source: https://www.dol.gov/agencies/eta/foreign-labor/performance
# Updated quarterly. XLSX files for PERM, H-1B, H-2A, H-2B, CW-1, PW programs.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dol_page <- "https://www.dol.gov/agencies/eta/foreign-labor/performance"

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_html <- function(url) xml2::read_html(.fetch(url, ".html"))

# -- Program classifier -------------------------------------------------------

.classify_program <- function(filename) {
  f <- toupper(filename)
  case_when(
    grepl("PERM", f)        ~ "PERM",
    grepl("H.?2A|_2A", f)   ~ "H-2A",
    grepl("H.?2B|_2B", f)   ~ "H-2B",
    grepl("LCA|H.?1B", f)   ~ "H-1B",
    grepl("PW", f)           ~ "PW",
    grepl("CW", f)           ~ "CW-1",
    TRUE                     ~ "OTHER"
  )
}

# == Schemas ===================================================================

.schema_files <- tibble(
  url = character(), filename = character(), program = character()
)


