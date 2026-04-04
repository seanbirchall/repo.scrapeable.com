#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom xml2  read_xml
#' @importFrom xml2  read_html
#' @keywords internal
NULL

# Inlined from _helpers.R
http_get <- function(url  ext = ".html"  ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_perform(path = tmp
  tmp
}

http_post <- function(url  body  ext = ".zip"  ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp
  tmp
}

