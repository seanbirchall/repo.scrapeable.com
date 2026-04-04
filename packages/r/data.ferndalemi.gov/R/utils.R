#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# ferndalemi.gov.R - City of Ferndale MI open data client (ArcGIS Hub)
#
# Data source: data.ferndalemi.gov (ArcGIS Hub)
# Datasets: ~273 (bus routes, drug incidents, electoral districts, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fern_base <- "https://data.ferndalemi.gov"

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

.fern_datasets <- list(
  bus_445        = list(id = "7079716c96b14dcab1c5d73d7fdc1274", layer = 0, name = "SMART Bus Route 445"),
  bus_475        = list(id = "7079716c96b14dcab1c5d73d7fdc1274", layer = 2, name = "SMART Bus Route 475"),
  bus_710        = list(id = "7079716c96b14dcab1c5d73d7fdc1274", layer = 3, name = "SMART Bus Route 710"),
  drug_incidents = list(id = "0511440139bd4437b5ffa10a41f05a68", layer = 0, name = "Drug Incidents & NARCAN 2007-2017"),
  congress_dist  = list(id = "c0d77fa257eb410c8aaa0be2d4917d51", layer = 0, name = "MI Congressional Districts 2010"),
  house_dist     = list(id = "2bb8d3e1f78c441abb8e03a16be6acd0", layer = 0, name = "MI House Districts 2010"),
  senate_dist    = list(id = "14890ed705184b6cacd5fde2647026f7", layer = 0, name = "MI Senate Districts 2010"),
  land_water     = list(id = "4f9202a507244b10bb12942d5f9f46c7", layer = 0, name = "Land & Water Fund Detroit 2016")
)

