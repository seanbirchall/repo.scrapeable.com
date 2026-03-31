# == Public functions ==========================================================

#' Browse SSB statistical tables
#'
#' Navigates the hierarchical table tree. Items with type "l" are folders;
#' items ending in digits are usually data tables.
#'
#' @param path Path in the table tree. Empty string for root categories.
#'   Use table IDs directly for specific tables (e.g. "07459").
#' @return tibble: id, type ("l" = folder), text
#' @export
ssb_tables <- function(path = "") {
  url <- if (nchar(path) > 0) {
    sprintf("%s/%s", .ssb_base, path)
  } else {
    .ssb_base
  }
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_tables)

  # If result is a list with 'title' and 'variables', it's table metadata not a listing
  if (!is.null(raw$title)) {
    return(tibble(
      id = as.character(path),
      type = "t",
      text = as.character(raw$title)
    ))
  }

  as_tibble(raw) |>
    transmute(
      id = as.character(id),
      type = as.character(type),
      text = as.character(text)
    )
}

#' Get metadata for an SSB table
#'
#' Returns variable definitions for a specific table, useful for building queries.
#'
#' @param table_id Table ID (e.g. "07459" for population)
#' @return tibble: code, text, n_values, values_sample (first 5 values as comma-separated string)
#' @export
ssb_metadata <- function(table_id) {
  url <- sprintf("%s/%s", .ssb_base, table_id)
  raw <- jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
  vars <- raw$variables
  if (is.null(vars) || length(vars) == 0) return(.schema_metadata)

  rows <- lapply(vars, function(v) {
    vals <- unlist(v$values)
    tibble(
      code = as.character(v$code),
      text = as.character(v$text),
      n_values = as.integer(length(vals)),
      values_sample = paste(head(vals, 5), collapse = ", ")
    )
  })
  bind_rows(rows)
}

#' Query SSB table data
#'
#' Sends a POST query to fetch data from a specific table.
#' Returns JSON-stat2 format parsed into a tibble.
#'
#' @param table_id Table ID (e.g. "07459")
#' @param query List of selection filters. Each element should be a list with
#'   "code", "selection" (list with "filter" and "values").
#'   Use ssb_metadata() to discover valid codes and values.
#'   Example: list(list(code="Region", selection=list(filter="item", values=list("0"))))
#' @param format Response format: "json-stat2" (default) or "csv"
#' @return tibble with dimensions and values from the query result
#' @export
ssb_query <- function(table_id, query = list(), format = "json-stat2") {
  url <- sprintf("%s/%s", .ssb_base, table_id)
  body <- list(query = query, response = list(format = format))

  raw <- .post_json(url, body)
  if (is.null(raw)) return(.schema_data)

  # Parse JSON-stat2 response
  dims <- raw$id
  sizes <- unlist(raw$size)
  if (is.null(dims) || is.null(sizes)) return(.schema_data)

  # Build dimension labels
  dim_labels <- list()
  for (d in dims) {
    dim_info <- raw$dimension[[d]]
    cat_index <- dim_info$category$index
    cat_label <- dim_info$category$label
    if (is.null(cat_label)) {
      # Use index keys as labels
      dim_labels[[d]] <- names(cat_index)
    } else {
      # Order by index
      ordered_keys <- names(cat_index)[order(unlist(cat_index))]
      dim_labels[[d]] <- vapply(ordered_keys, function(k) as.character(cat_label[[k]]), character(1))
    }
  }

  # Build cartesian product of dimension values
  grid <- expand.grid(dim_labels, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  names(grid) <- vapply(dims, function(d) as.character(raw$dimension[[d]]$label), character(1))

  # Extract values
  vals <- unlist(raw$value)
  if (length(vals) == nrow(grid)) {
    grid$value <- as.character(vals)
  }

  as_tibble(grid)
}

#' SSB API context for LLM use
#'
#' Prints package overview, auth info, and function signatures.
#' @return Invisible string with context info
#' @export
ssb_context <- function() {
  header <- c(
    "# data.ssb.no - Statistics Norway (SSB) PxWeb API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: not documented",
    "#",
    "# Top categories: al=Labour, bf=Banking, be=Population, ei=Energy",
    "# Key tables: 07459=Population, 09786=CPI, 09842=GDP",
    "# Typical flow: ssb_tables() -> ssb_metadata(id) -> ssb_query(id, query)"
  )
  .build_context("data.ssb.no", header_lines = header)
}
