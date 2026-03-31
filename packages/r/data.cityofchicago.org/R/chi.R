

# == Dataset discovery =========================================================

#' List available datasets on Chicago Open Data
#'
#' Returns a catalog of datasets available on data.cityofchicago.org.
#' Filter by category to narrow results.
#'
#' @param limit Number of datasets to return (default 50, max 200)
#' @param category Optional category filter (e.g. "Public Safety")
#' @return tibble: id, name, description, category, type, updated_at, view_count
#' @export
chi_datasets <- function(limit = 50, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .chi_base, limit)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)
  if (!is.data.frame(raw)) return(.schema_datasets)

  df <- as_tibble(raw)
  result <- df |>
    transmute(
      id          = as.character(id),
      name        = as.character(name),
      description = as.character(description %||% NA),
      category    = as.character(category %||% NA),
      type        = as.character(displayType %||% NA),
      updated_at  = as.POSIXct(as.numeric(viewLastModified %||% NA),
                                origin = "1970-01-01", tz = "UTC"),
      view_count  = as.integer(viewCount %||% NA)
    )

  if (!is.null(category)) {
    result <- result |> filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  result
}


# == Query a dataset ===========================================================

#' Query a Socrata dataset on Chicago Open Data
#'
#' Runs a SoQL query against a specific dataset. Uses the SODA 2.0 API.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "ijzp-q8t2")
#' @param where Optional SoQL WHERE clause (e.g. "primary_type='THEFT'")
#' @param select Optional SoQL SELECT clause (e.g. "date, primary_type")
#' @param order Optional SoQL ORDER BY clause (e.g. "date DESC")
#' @param limit Number of rows to return (default 1000, max 50000)
#' @param offset Offset for pagination (default 0)
#' @return tibble with columns from the dataset
#' @export
chi_query <- function(dataset_id, where = NULL, select = NULL,
                      order = NULL, limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order

  query_str <- paste(names(params), utils::URLencode(as.character(params), reserved = TRUE),
                     sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .chi_base, dataset_id, query_str)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}


# == Context ===================================================================

#' Show Chicago Open Data package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
#' @export
chi_context <- function() {
  header <- c(
    "# data.cityofchicago.org - Chicago Open Data Client (Socrata SODA)",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (app tokens optional for higher rate limits)",
    "# Rate limits: 1000 requests/hour without app token",
    "#",
    "# Popular dataset IDs:",
    "#   ijzp-q8t2 - Crimes - 2001 to Present",
    "#   85ca-t3if - Crimes - 2001 to Present (Map)",
    "#   ydr8-5enu - Strategic Subject List (SSL)",
    "#   6zsd-86xi - Chicago Traffic Crashes",
    "#   97t2-u7y7 - Red Light Camera Violations",
    "#",
    "# SoQL reference: https://dev.socrata.com/docs/queries/"
  )
  .build_context("data.cityofchicago.org", header_lines = header)
}
