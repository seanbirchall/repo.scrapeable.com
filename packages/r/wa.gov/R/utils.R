#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.wa_domain <- "data.wa.gov"
.wa_disco  <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_   else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_      else as.double(x)

.wa_fetch <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

.wa_fetch_list <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE, flatten = FALSE)
}

._soda_query <- function(view_id, select = NULL, where = NULL,
                         order = NULL, group = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  url <- sprintf("https://%s/resource/%s.json", .wa_domain, view_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(order))  params[["$order"]]   <- order
  if (!is.null(group))  params[["$group"]]   <- group
  if (!is.null(q))      params[["$q"]]       <- q

  raw <- .wa_fetch(url, params)
  if (length(raw) == 0) return(tibble::tibble())
  df <- tibble::as_tibble(raw)
  .type_cols(df)
}

.type_cols <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) == 0) next
      if (all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
      if (grepl("date|Date|updated|created|At$", col) &&
          sum(grepl("^\\d{4}-\\d{2}-\\d{2}", vals)) > length(vals) * 0.5) {
        df[[col]] <- as.Date(substr(df[[col]], 1, 10))
      }
    }
  }
  df
}

._soda_all <- function(view_id, select = NULL, where = NULL,
                       order = NULL, max_rows = 50000, page_size = 5000) {
  all_data <- list()
  offset <- 0
  repeat {
    remaining <- max_rows - offset
    if (remaining <= 0) break
    lim <- min(page_size, remaining)
    chunk <- ._soda_query(view_id, select = select, where = where,
                          order = order, limit = lim, offset = offset)
    if (nrow(chunk) == 0) break
    all_data[[length(all_data) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(all_data) == 0) return(tibble::tibble())
  dplyr::bind_rows(all_data)
}
