# alleghenycounty.us.R - Allegheny County / WPRDC open data client (CKAN API)
#
# Data source: data.wprdc.org (Western PA Regional Data Center, CKAN)
# Datasets: ~365 (property, air quality, dog licenses, jail census, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wprdc_base <- "https://data.wprdc.org"

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

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  notes = character(), organization = character(),
  num_resources = integer(), metadata_modified = character()
)

# == Dataset discovery =========================================================

#' Search datasets on WPRDC (Allegheny County open data)
#'
#' Searches the Western Pennsylvania Regional Data Center (WPRDC) CKAN catalog
#' for open datasets. Covers ~365 datasets on topics including property
#' assessments, air quality, dog licenses, jail census, and more from
#' Allegheny County, City of Pittsburgh, and regional organizations.
#'
#' @param q Character. Search query string (default \code{""} for all datasets).
#'   Examples: \code{"air quality"}, \code{"property"}, \code{"dog licenses"},
#'   \code{"police"}, \code{"budget"}.
#' @param rows Integer. Number of results to return (default 50).
#' @param start Integer. Offset for pagination (default 0). Use with \code{rows}
#'   to page through results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. CKAN dataset UUID. Pass to \code{ac_metadata()}.}
#'     \item{name}{Character. URL-safe slug (e.g. "allegheny-county-air-quality").}
#'     \item{title}{Character. Human-readable title.}
#'     \item{notes}{Character. Description/notes (may contain Markdown).}
#'     \item{organization}{Character. Publishing organization name
#'       (e.g. "Allegheny County", "City of Pittsburgh").}
#'     \item{num_resources}{Integer. Number of downloadable resources (CSV, GeoJSON, etc.).}
#'     \item{metadata_modified}{Character. ISO 8601 timestamp of last update.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ac_datasets("air quality")
#' ac_datasets("property", rows = 10)
#' ac_datasets()  # browse all
#' }
ac_datasets <- function(q = "", rows = 50, start = 0) {
  url <- sprintf("%s/api/3/action/package_search?q=%s&rows=%d&start=%d",
                 .wprdc_base, utils::URLencode(q), rows, start)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(.schema_datasets)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0) return(.schema_datasets)
  tibble(
    id                = as.character(results$id),
    name              = as.character(results$name),
    title             = as.character(results$title),
    notes             = as.character(results$notes %||% NA_character_),
    organization      = vapply(seq_len(nrow(results)), function(i) {
      o <- results$organization
      if (is.data.frame(o)) o$title[i] %||% NA_character_
      else if (is.list(o)) (o[[i]]$title %||% NA_character_)
      else NA_character_
    }, character(1)),
    num_resources     = as.integer(results$num_resources),
    metadata_modified = as.character(results$metadata_modified)
  )
}

#' Get metadata for a WPRDC dataset
#'
#' Retrieves full metadata for a single WPRDC dataset, including all
#' downloadable resources (CSV files, GeoJSON, HTML pages, etc.). The dataset
#' title and description are attached as attributes on the returned tibble.
#'
#' @param id_or_name Character. Dataset UUID or URL-safe name slug. Get these
#'   from \code{ac_datasets()}. Examples:
#'   \code{"c7b3266c-adc6-41c0-b19a-8d4353bfcdaf"},
#'   \code{"allegheny-county-air-quality"}.
#' @return A tibble of resources with columns:
#'   \describe{
#'     \item{resource_id}{Character. CKAN resource UUID. Pass to
#'       \code{ac_query()} or \code{ac_count()}.}
#'     \item{name}{Character. Resource name (e.g. "Hourly Air Quality Data").}
#'     \item{format}{Character. File format (e.g. "CSV", "GeoJSON", "HTML").}
#'     \item{url}{Character. Direct download URL.}
#'   }
#'   Attributes: \code{dataset_title}, \code{dataset_notes}.
#' @export
#' @examples
#' \dontrun{
#' # Get resources for air quality dataset
#' res <- ac_metadata("c7b3266c-adc6-41c0-b19a-8d4353bfcdaf")
#' res
#' attr(res, "dataset_title")
#' }
ac_metadata <- function(id_or_name) {
  url <- sprintf("%s/api/3/action/package_show?id=%s", .wprdc_base, id_or_name)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  r <- raw$result
  resources <- r$resources
  res_df <- if (!is.null(resources) && is.data.frame(resources)) {
    tibble(
      resource_id = as.character(resources$id),
      name        = as.character(resources$name %||% NA_character_),
      format      = as.character(resources$format %||% NA_character_),
      url         = as.character(resources$url %||% NA_character_)
    )
  } else tibble(resource_id = character(), name = character(), format = character(), url = character())
  attr(res_df, "dataset_title") <- r$title
  attr(res_df, "dataset_notes") <- r$notes
  res_df
}

#' Query a WPRDC datastore resource
#'
#' Uses the CKAN DataStore API to query tabular data from a specific resource.
#' Supports both direct record fetching (with filters, limit, offset) and
#' raw SQL queries via the \code{datastore_search_sql} endpoint.
#'
#' @param resource_id Character. CKAN resource UUID. Get these from
#'   \code{ac_metadata()}. Example:
#'   \code{"36fb4629-8003-4acc-a1ca-3302778a530d"} (Hourly Air Quality Data).
#' @param sql Character or NULL. Raw SQL query string. When provided, all other
#'   parameters except \code{resource_id} are ignored. Use the resource ID as
#'   the table name in FROM clause. Example:
#'   \code{"SELECT * FROM \"36fb4629-...\" WHERE site = 'Liberty' LIMIT 10"}.
#' @param limit Integer. Number of records to return (default 1000, max 50000).
#' @param offset Integer. Offset for pagination (default 0).
#' @param filters Named list or NULL. Field-value filters applied as equality
#'   conditions. Example: \code{list(site = "Liberty", parameter = "PM25")}.
#' @return A tibble with columns matching the resource schema. Columns vary by
#'   dataset. The internal \code{_id} column is removed automatically.
#' @export
#' @examples
#' \dontrun{
#' # Fetch air quality records
#' ac_query("36fb4629-8003-4acc-a1ca-3302778a530d", limit = 10)
#'
#' # With filters
#' ac_query("36fb4629-8003-4acc-a1ca-3302778a530d",
#'          filters = list(site = "Liberty"), limit = 10)
#' }
ac_query <- function(resource_id, sql = NULL, limit = 1000, offset = 0, filters = NULL) {
  if (!is.null(sql)) {
    url <- sprintf("%s/api/3/action/datastore_search_sql?sql=%s",
                   .wprdc_base, utils::URLencode(sql))
  } else {
    url <- sprintf("%s/api/3/action/datastore_search?resource_id=%s&limit=%d&offset=%d",
                   .wprdc_base, resource_id, limit, offset)
    if (!is.null(filters)) {
      url <- paste0(url, "&filters=", utils::URLencode(jsonlite::toJSON(filters, auto_unbox = TRUE)))
    }
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || length(records) == 0) return(tibble())
  df <- as_tibble(records)
  # Remove internal _id column if present
  if ("_id" %in% names(df)) df <- df |> select(-`_id`)
  df
}

#' Get the total record count for a WPRDC resource
#'
#' Queries the CKAN DataStore to return the total number of records in a
#' tabular resource. Useful for planning pagination with \code{ac_query()}.
#'
#' @param resource_id Character. CKAN resource UUID. Get these from
#'   \code{ac_metadata()}. Example:
#'   \code{"36fb4629-8003-4acc-a1ca-3302778a530d"}.
#' @return Integer. Total record count, or \code{NA_integer_} on error.
#' @export
#' @examples
#' \dontrun{
#' ac_count("36fb4629-8003-4acc-a1ca-3302778a530d")
#' }
ac_count <- function(resource_id) {
  url <- sprintf("%s/api/3/action/datastore_search?resource_id=%s&limit=0",
                 .wprdc_base, resource_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(NA_integer_)
  as.integer(raw$result$total %||% NA)
}

# == Context ===================================================================

#' Get alleghenycounty.us client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ac_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ac_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/alleghenycounty.us.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "alleghenycounty.us")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# alleghenycounty.us context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# alleghenycounty.us", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
