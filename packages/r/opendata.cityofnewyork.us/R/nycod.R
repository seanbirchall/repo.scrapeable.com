# == Datasets ==================================================================

#' List NYC Open Data datasets
#'
#' Returns metadata for datasets available on the NYC Open Data portal.
#'
#' @param limit Number of datasets to return (default 50)
#' @param query Optional search filter on dataset name
#' @return tibble: id, name, category, description, rows_updated_at
#' @export
nycod_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .nyc_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NYC Open Data API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id               = as.character(raw$id),
    name             = as.character(raw$name),
    category         = as.character(raw$category %||% NA_character_),
    description      = ifelse(nchar(raw$description %||% "") > 200,
                              paste0(substr(raw$description, 1, 200), "..."),
                              as.character(raw$description %||% "")),
    rows_updated_at  = as.POSIXct(as.numeric(raw$rowsUpdatedAt), origin = "1970-01-01")
  )

  if (!is.null(query)) {
    result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  }
  result
}


#' Fetch records from an NYC Open Data dataset
#'
#' Queries a specific dataset using the Socrata SODA API.
#' Supports SoQL filtering, ordering, and pagination.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "erm2-nwe9" for 311).
#'   Find IDs via nycod_datasets().
#' @param where SoQL where clause (e.g. "borough='MANHATTAN'")
#' @param select SoQL select clause (e.g. "borough,count(*)")
#' @param group SoQL group by clause
#' @param order SoQL order clause (e.g. "created_date DESC")
#' @param limit Max rows to return (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
nycod_query <- function(dataset_id, where = NULL, select = NULL,
                        group = NULL, order = NULL,
                        limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .nyc_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NYC Open Data query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)

  as_tibble(raw)
}


#' Fetch all records from an NYC dataset with auto-pagination
#'
#' @param dataset_id Socrata dataset identifier
#' @param where Optional SoQL where clause
#' @param max_rows Maximum total rows to fetch (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble of all matching records
#' @export
nycod_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                            page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- nycod_query(dataset_id, where = where,
                         limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the NYC Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
nycod_context <- function() {
  .build_context("opendata.cityofnewyork.us", header_lines = c(
    "# opendata.cityofnewyork.us - NYC Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Popular datasets:",
    "#   erm2-nwe9 = 311 Service Requests",
    "#   s3l6-3lzj = Parking Violations",
    "#   h9gi-nx95 = Motor Vehicle Collisions",
    "#   rc75-m7u3 = Building Permits",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering"
  ))
}
