#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# fvap.gov.R - Self-contained FVAP (Federal Voting Assistance Program) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required


`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.fvap_base <- "https://www.fvap.gov"

.fetch_excel <- function(url, ext = ".xls") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Dataset metadata ==========================================================

.fvap_datasets <- tibble(
  id = c("military", "overseas_citizens", "vao_military",
         "leo", "federal_civilians", "dos_vao"),
  name = c("Uniformed Services Absentee Voting",
           "Overseas Citizens Absentee Voting",
           "Military Voting Assistance Officers",
           "Local Election Official Data",
           "Federal Employees Overseas Absentee Voting",
           "Department of State Voting Assistance Officers"),
  description = c(
    "2008 post election survey of active duty military on absentee voting",
    "2008 post election survey of American citizens living overseas on absentee voting",
    "2008 post election survey of military voting assistance officers (VAO)",
    "2008 post election survey of Local Election Officials on military/overseas absentee voting",
    "2008 post election survey of Federal employees overseas on absentee voting",
    "2008 post election survey of Dept of State voting assistance officers"
  ),
  url = c(
    paste0(.fvap_base, "/uploads/FVAP/Surveys/military_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/overseas_citizens_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/uvao_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/leo_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/federal_civilians_data.xls"),
    paste0(.fvap_base, "/uploads/FVAP/Surveys/dosvao_data.xls")
  ),
  year = rep(2008L, 6)
)

