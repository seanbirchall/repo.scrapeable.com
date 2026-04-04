# socrata.com.R
# Self-contained Socrata Open Data Discovery & Access client.
# Uses the Socrata Discovery API (api.us.socrata.com) for catalog search
# and the SODA 2.1 API for fetching actual data from any Socrata domain.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data. App tokens optional (increase rate limits).
# Rate limits: Without token ~1000/hr per IP. With token higher.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.socrata_disco <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.double(x)

# -- JSON fetch helper ---------------------------------------------------------

.socrata_fetch <- function(url, params = list(), simplify = TRUE) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = simplify, flatten = FALSE)
}

# -- Type date columns ---------------------------------------------------------

.type_dates <- function(df) {
  for (col in names(df)) {
    if (grepl("date|Date|updated|created|At$", col) && is.character(df[[col]])) {
      vals <- df[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike, na.rm = TRUE) > length(vals) * 0.5) {
        df[[col]] <- as.Date(substr(vals, 1, 10))
      }
    }
  }
  df
}


# == Discovery: Catalog Search ================================================

#' Search the Socrata open data catalog
#'
#' Searches across all Socrata-powered open data portals worldwide.
#' Returns dataset metadata from the Socrata Discovery API.
#'
#' @param query Search term (full-text search across name, description, columns)
#' @param domain Filter to a specific portal domain (e.g. "data.cdc.gov", "data.cityofnewyork.us")
#' @param category Filter by domain category
#' @param tags Character vector of tags to filter by
#' @param only Resource type: "datasets", "maps", "calendars", "links", "filters", "files"
#' @param order Sort order: "relevance" (default), "page_views_total", "updatedAt"
#' @param limit Max results per page (default 50, max 10000)
#' @param offset Starting offset for pagination (default 0)
#' @return tibble: id, name, description, domain, type, updatedAt, createdAt,
#'   download_count, page_views_total, columns_name, permalink
#' @export
socrata_search <- function(query = NULL, domain = NULL, category = NULL,
                           tags = NULL, only = "datasets", order = "relevance",
                           limit = 50, offset = 0) {
  params <- list(limit = limit, offset = offset)
  if (!is.null(query))    params$q <- query
  if (!is.null(domain))   params$domains <- domain
  if (!is.null(category)) params$categories <- category
  if (!is.null(only))     params$only <- only
  if (!is.null(order))    params$order <- order
  if (!is.null(tags))     params$tags <- paste(tags, collapse = ",")

  raw <- .socrata_fetch(.socrata_disco, params, simplify = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  .ex <- function(item, ...) {
    keys <- list(...)
    val <- item
    for (k in keys) val <- val[[k]]
    if (is.null(val)) NA_character_ else as.character(val)
  }

  tibble(
    id               = vapply(results, function(r) .ex(r, "resource", "id"), character(1)),
    name             = vapply(results, function(r) .ex(r, "resource", "name"), character(1)),
    description      = vapply(results, function(r) substr(.ex(r, "resource", "description"), 1, 300), character(1)),
    type             = vapply(results, function(r) .ex(r, "resource", "type"), character(1)),
    domain           = vapply(results, function(r) .ex(r, "metadata", "domain"), character(1)),
    permalink        = vapply(results, function(r) .ex(r, "permalink"), character(1)),
    updatedAt        = as.Date(vapply(results, function(r) substr(.ex(r, "resource", "updatedAt"), 1, 10), character(1))),
    createdAt        = as.Date(vapply(results, function(r) substr(.ex(r, "resource", "createdAt"), 1, 10), character(1))),
    download_count   = vapply(results, function(r) {
      v <- r$resource$download_count; if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    page_views_total = vapply(results, function(r) {
      v <- r$resource$page_views$page_views_total; if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    columns          = vapply(results, function(r) {
      cn <- r$resource$columns_name
      if (is.null(cn)) NA_character_ else paste(cn, collapse = "; ")
    }, character(1)),
    domain_category  = vapply(results, function(r) .ex(r, "classification", "domain_category"), character(1))
  )
}


#' List datasets on a specific Socrata portal
#'
#' Convenience wrapper around socrata_search for browsing a single domain.
#'
#' @param domain Portal domain (e.g. "data.cdc.gov")
#' @param order Sort: "page_views_total" (default), "updatedAt", "relevance"
#' @param limit Max results (default 50)
#' @param offset Pagination offset
#' @return tibble: same columns as socrata_search
#' @export
socrata_list <- function(domain, order = "page_views_total", limit = 50, offset = 0) {
  socrata_search(domain = domain, order = order, limit = limit, offset = offset)
}


# == Discovery: Dataset Metadata ==============================================

#' Get metadata for a specific dataset
#'
#' Fetches full SODA metadata (views API) for a dataset including
#' column definitions, update frequency, license, and owner info.
#'
#' @param domain Portal domain hosting the dataset (e.g. "data.cdc.gov")
#' @param dataset_id Four-by-four dataset ID (e.g. "9bhg-hcku")
#' @return tibble: one row with id, name, description, category, columns (list),
#'   attribution, license, rowsUpdatedAt, createdAt, viewCount
#' @export
socrata_meta <- function(domain, dataset_id) {
  url <- sprintf("https://%s/api/views/%s.json", domain, dataset_id)
  raw <- .socrata_fetch(url, simplify = FALSE)

  cols <- if (!is.null(raw$columns)) {
    tibble(
      field_name = vapply(raw$columns, function(c) c$fieldName %||% NA_character_, character(1)),
      name       = vapply(raw$columns, function(c) c$name %||% NA_character_, character(1)),
      datatype   = vapply(raw$columns, function(c) c$dataTypeName %||% NA_character_, character(1)),
      description = vapply(raw$columns, function(c) c$description %||% NA_character_, character(1))
    )
  } else {
    tibble(field_name = character(), name = character(), datatype = character(),
           description = character())
  }

  tibble(
    id            = .safe_chr(raw$id),
    name          = .safe_chr(raw$name),
    description   = .safe_chr(raw$description),
    category      = .safe_chr(raw$category),
    attribution   = .safe_chr(raw$attribution),
    license       = .safe_chr(raw$license$name),
    rowsUpdatedAt = .safe_chr(raw$rowsUpdatedAt),
    createdAt     = .safe_chr(raw$createdAt),
    viewCount     = .safe_int(raw$viewCount),
    downloadCount = .safe_int(raw$downloadCount),
    columns       = list(cols)
  )
}


# == Access: Fetch Data ========================================================

#' Fetch data from any Socrata dataset
#'
#' Uses the SODA 2.1 API to query and retrieve actual data rows from
#' any public Socrata dataset. Supports SoQL filters.
#'
#' @param domain Portal domain (e.g. "data.cdc.gov")
#' @param dataset_id Four-by-four dataset ID (e.g. "9bhg-hcku")
#' @param select SoQL select clause (e.g. "state, age_group, covid_19_deaths")
#' @param where SoQL where clause (e.g. "state = 'United States'")
#' @param order SoQL order clause (e.g. "date DESC")
#' @param group SoQL group clause
#' @param q Full-text search within the dataset
#' @param limit Max rows to return (default 1000, max 50000)
#' @param offset Row offset for pagination (default 0)
#' @param app_token Optional Socrata app token for higher rate limits
#' @return tibble with dataset rows and typed columns
#' @export
socrata_data <- function(domain, dataset_id, select = NULL, where = NULL,
                         order = NULL, group = NULL, q = NULL,
                         limit = 1000, offset = 0, app_token = NULL) {
  url <- sprintf("https://%s/resource/%s.json", domain, dataset_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]] <- where
  if (!is.null(order))  params[["$order"]] <- order
  if (!is.null(group))  params[["$group"]] <- group
  if (!is.null(q))      params[["$q"]] <- q

  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  if (!is.null(app_token)) {
    req <- req |> httr2::req_headers(`X-App-Token` = app_token)
  }

  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)

  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)

  # Auto-type numeric columns
 for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) > 0 && all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
  }

  .type_dates(df)
}


#' Fetch all rows from a Socrata dataset with automatic pagination
#'
#' Iteratively pages through the SODA API to retrieve all rows.
#' Use with caution on large datasets.
#'
#' @param domain Portal domain
#' @param dataset_id Four-by-four dataset ID
#' @param select SoQL select clause
#' @param where SoQL where clause
#' @param order SoQL order clause
#' @param max_rows Maximum total rows to fetch (default 10000, safety limit)
#' @param page_size Rows per API call (default 5000)
#' @param app_token Optional app token
#' @return tibble with all matching rows
#' @export
socrata_data_all <- function(domain, dataset_id, select = NULL, where = NULL,
                             order = NULL, max_rows = 10000, page_size = 5000,
                             app_token = NULL) {
  all_data <- list()
  offset <- 0
  repeat {
    remaining <- max_rows - offset
    if (remaining <= 0) break
    lim <- min(page_size, remaining)
    chunk <- socrata_data(domain, dataset_id, select = select, where = where,
                          order = order, limit = lim, offset = offset,
                          app_token = app_token)
    if (nrow(chunk) == 0) break
    all_data[[length(all_data) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(all_data) == 0) return(tibble())
  bind_rows(all_data)
}


#' Count rows in a Socrata dataset
#'
#' Uses SoQL count() to get row count without downloading data.
#'
#' @param domain Portal domain
#' @param dataset_id Four-by-four dataset ID
#' @param where Optional SoQL where clause to count a subset
#' @param app_token Optional app token
#' @return integer row count
#' @export
socrata_count <- function(domain, dataset_id, where = NULL, app_token = NULL) {
  df <- socrata_data(domain, dataset_id, select = "count(*) as n",
                     where = where, limit = 1, app_token = app_token)
  if (nrow(df) == 0) return(0L)
  as.integer(df$n[1])
}


# == Context ===================================================================

#' Generate LLM-friendly context for socrata.com
#'
#' @return Character string with full function signatures
#' @export
socrata_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/socrata.com.R"
  if (!file.exists(src_file)) {
    cat("# socrata.com context - source not found\n")
    return(invisible("# socrata.com context - source not found"))
  }
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
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
