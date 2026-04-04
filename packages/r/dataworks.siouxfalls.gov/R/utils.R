#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# siouxfalls.gov.R - City of Sioux Falls SD open data client (ArcGIS Hub)
#
# Data source: dataworks.siouxfalls.gov (ArcGIS Hub)
# Datasets: ~233 (crime, taxes, housing, library, fire, annexations, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sf_base <- "https://dataworks.siouxfalls.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.fetch_csv <- function(url) {
  tmp <- tryCatch(.fetch(url, ext = ".csv"), error = function(e) NULL)
  if (is.null(tmp)) return(tibble())
  tryCatch(as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE)),
           error = function(e) tibble())
}

# == Known datasets ============================================================

.sf_datasets <- list(
  entertainment_tax = list(id = "eb2ae9c0c34044f2968499bc7c39dd9e", layer = 6,  name = "Entertainment Tax Growth"),
  fire_travel       = list(id = "da04cf7da54147068575467512cd1140", layer = 2,  name = "Fire Travel Time"),
  violent_crimes    = list(id = "cd28866b6c56472cb9c1ee660ff535b2", layer = 12, name = "Violent Crimes by Year"),
  annexations       = list(id = "6b2dc2947c3749009feb92b819bcb002", layer = 17, name = "Annexations by Year"),
  library_stats     = list(id = "89bbb70a6afc4f018face386bb26f589", layer = 22, name = "Library Statistics"),
  sales_tax         = list(id = "c8f7902737b94322acc8c8d621127533", layer = 7,  name = "Sales Tax Growth")
)

