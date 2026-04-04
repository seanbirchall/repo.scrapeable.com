# wprdc.org.R - Western PA Regional Data Center (CKAN API)
# Self-contained client for data.wprdc.org
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Base: https://data.wprdc.org/api/3

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wprdc_base <- "https://data.wprdc.org/api/3"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_packages <- tibble(
  name = character(), title = character(), notes = character(),
  organization = character(), num_resources = integer(),
  metadata_modified = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character()
)

# == Dataset discovery =========================================================

#' Search WPRDC datasets
#'
#' Search the Western PA Regional Data Center catalog by keyword.
#'
#' @param q Search query string (optional; returns all if NULL)
#' @param rows Max results (default 20, max 1000)
#' @param start Pagination offset (default 0)
#' @return tibble: name, title, notes, organization, num_resources, metadata_modified
wprdc_search <- function(q = NULL, rows = 20, start = 0) {
  url <- sprintf("%s/action/package_search?rows=%d&start=%d",
                 .wprdc_base, as.integer(rows), as.integer(start))
  if (!is.null(q)) url <- paste0(url, "&q=", utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(.schema_packages)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0) return(.schema_packages)
  tibble(
    name              = as.character(results$name %||% NA),
    title             = as.character(results$title %||% NA),
    notes             = substr(as.character(results$notes %||% ""), 1, 300),
    organization      = as.character(results$organization$title %||% NA),
    num_resources     = as.integer(results$num_resources %||% NA),
    metadata_modified = as.character(results$metadata_modified %||% NA)
  )
}

#' List all WPRDC dataset names
#'
#' @param limit Max results (default 500)
#' @return character vector of dataset names/slugs
wprdc_list <- function(limit = 500) {
  url <- sprintf("%s/action/package_list?limit=%d", .wprdc_base, as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(character())
  as.character(raw$result)
}

#' Get metadata for a WPRDC dataset
#'
#' @param name Dataset slug (e.g. "police-incident-blotter")
#' @return tibble with dataset metadata and resource list
wprdc_package <- function(name) {
  url <- sprintf("%s/action/package_show?id=%s", .wprdc_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  r <- raw$result
  resources <- r$resources
  if (is.null(resources) || !is.data.frame(resources)) return(tibble())
  tibble(
    id          = as.character(resources$id %||% NA),
    name        = as.character(resources$name %||% NA),
    format      = as.character(resources$format %||% NA),
    url         = as.character(resources$url %||% NA),
    description = as.character(resources$description %||% NA)
  )
}

# == Data access ===============================================================

#' Query WPRDC datastore records
#'
#' Fetch actual data rows from a WPRDC resource using the CKAN datastore API.
#'
#' @param resource_id Resource UUID
#' @param q Full-text search query (optional)
#' @param filters Named list of field:value filters (optional)
#' @param limit Max records (default 100, max 32000)
#' @param offset Pagination offset (default 0)
#' @param sort Sort string (e.g. "INCIDENTTIME desc")
#' @return tibble of data records
wprdc_data <- function(resource_id, q = NULL, filters = NULL,
                       limit = 100, offset = 0, sort = NULL) {
  url <- sprintf("%s/action/datastore_search?resource_id=%s&limit=%d&offset=%d",
                 .wprdc_base, resource_id, as.integer(limit), as.integer(offset))
  if (!is.null(q)) url <- paste0(url, "&q=", utils::URLencode(q))
  if (!is.null(filters)) {
    fj <- jsonlite::toJSON(filters, auto_unbox = TRUE)
    url <- paste0(url, "&filters=", utils::URLencode(as.character(fj)))
  }
  if (!is.null(sort)) url <- paste0(url, "&sort=", utils::URLencode(sort))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || !is.data.frame(records) || nrow(records) == 0) return(tibble())
  as_tibble(records) |> select(-any_of("_id"))
}

#' Run a SQL query against WPRDC datastore
#'
#' Uses CKAN datastore_search_sql for flexible querying.
#'
#' @param sql SQL query string (e.g. 'SELECT * FROM "resource_id" LIMIT 10')
#' @return tibble of results
wprdc_sql <- function(sql) {
  url <- paste0(.wprdc_base, "/action/datastore_search_sql?sql=", utils::URLencode(sql))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || !is.data.frame(records) || nrow(records) == 0) return(tibble())
  as_tibble(records)
}

# == Context ===================================================================

#' Generate LLM-friendly context for wprdc.org
#'
#' @return Character string (invisibly), also printed
wprdc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/wprdc.org.R"
  if (!file.exists(src_file)) { cat("# wprdc.org context - source not found\n"); return(invisible(NULL)) }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
