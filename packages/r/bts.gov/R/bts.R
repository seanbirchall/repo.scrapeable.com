# == Functions =================================================================

#' List BTS datasets
#'
#' @param limit Number of datasets (default 50)
#' @param query Optional name filter
#' @return tibble: id, name, category, description
#' @export
bts_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .bts_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BTS API error: ", e$message); NULL
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id          = as.character(raw$id),
    name        = as.character(raw$name),
    category    = as.character(raw$category %||% NA_character_),
    description = ifelse(nchar(raw$description %||% "") > 200,
                         paste0(substr(raw$description, 1, 200), "..."),
                         as.character(raw$description %||% ""))
  )
  if (!is.null(query)) result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  result
}

#' Query a BTS dataset
#'
#' @param dataset_id Socrata dataset ID
#' @param where SoQL where clause
#' @param select SoQL select clause
#' @param group SoQL group by
#' @param order SoQL order
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble
#' @export
bts_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL,
                      limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .bts_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BTS query error: ", e$message); NULL
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

#' Fetch all records with auto-pagination
#'
#' @param dataset_id Socrata dataset ID
#' @param where Optional SoQL where clause
#' @param max_rows Maximum rows (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble
#' @export
bts_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                          page_size = 1000) {
  results <- list(); offset <- 0
  while (offset < max_rows) {
    batch <- bts_query(dataset_id, where = where, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}

#' Generate context
#' @return Character string (invisibly)
#' @export
bts_context <- function() {
  .build_context("bts.gov", header_lines = c(
    "# bts.gov - Bureau of Transportation Statistics Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Socrata SODA API at datahub.transportation.gov"
  ))
}
