#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# pbgc.gov.R - Self-contained PBGC (Pension Benefit Guaranty Corporation) client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required


`%||%` <- function(a, b) if (is.null(a)) b else a

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.pbgc_base <- "https://www.pbgc.gov"

.fetch_excel <- function(url, ext = ".xlsx") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_trusteed <- tibble(
  case_number = character(), sponsor_name = character(), plan_name = character(),
  ein = character(), plan_number = numeric(), city = character(), state = character(),
  termination_date = as.Date(character()), trusteeship_date = as.Date(character()),
  participants = numeric()
)

.schema_multiemployer <- tibble(
  plan_name = character(), admin_city = character(), admin_state = character(),
  sponsor_name = character(), phone = character(), effective_date = character(),
  plan_id = character(), ein = character(), pn = character(),
  participant_count = numeric()
)

