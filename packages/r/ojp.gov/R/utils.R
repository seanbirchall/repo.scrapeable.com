#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_url_query
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# ojp.gov.R
# Self-contained Office of Justice Programs (OJP) API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Docs: https://bjs.ojp.gov/national-crime-victimization-survey-ncvs-api

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ojp_api_base <- "https://api.ojp.gov"

# -- Core fetch engine (Socrata-style JSON API) --------------------------------

.ojp_fetch_json <- function(path, params = list(), limit = 1000, offset = 0) {
  params[["$limit"]] <- min(limit, 50000)
  if (offset > 0) params[["$offset"]] <- offset

  url <- paste0(.ojp_api_base, "/", path)
  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("OJP API returned HTTP ", status, " for ", path)
    return(list())
  }
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

.ojp_fetch_all <- function(path, params = list(), max_results = 1000) {
  all_rows <- list()
  offset <- 0
  page_size <- min(max_results, 50000)

  repeat {
    raw <- .ojp_fetch_json(path, params, limit = page_size, offset = offset)
    if (!is.data.frame(raw) && is.list(raw) && length(raw) == 0) break
    if (is.data.frame(raw)) {
      chunk <- tibble::as_tibble(raw)
    } else if (is.list(raw) && length(raw) > 0) {
      chunk <- tryCatch(dplyr::bind_rows(lapply(raw, tibble::as_tibble)),
                         error = function(e) tibble::tibble())
    } else {
      break
    }
    if (nrow(chunk) == 0) break
    all_rows[[length(all_rows) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (offset >= max_results) break
    if (nrow(chunk) < page_size) break
  }

  if (length(all_rows) == 0) return(tibble::tibble())
  result <- dplyr::bind_rows(all_rows)
  if (nrow(result) > max_results) result <- result[seq_len(max_results), ]
  result
}

.ojp_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("OJP CSV returned HTTP ", status)
    return(tibble::tibble())
  }
  tibble::as_tibble(read.csv(tmp, stringsAsFactors = FALSE))
}
