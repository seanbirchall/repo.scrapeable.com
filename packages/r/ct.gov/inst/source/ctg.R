# ct.gov.R - State of Connecticut Open Data (Socrata SODA) client
# Self-contained. All public functions return tibbles.
#
# Portal: data.ct.gov (629 Socrata views, 1042 total datasets)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Prefix: ctg_

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ctg_base <- "https://data.ct.gov"
.ctg_disco <- "https://api.us.socrata.com/api/catalog/v1"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

._soda_query <- function(dataset_id, where = NULL, select = NULL,
                         group = NULL, order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .ctg_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("CT SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

._soda_fetch_all <- function(dataset_id, where = NULL, select = NULL,
                             order = NULL, q = NULL,
                             max_rows = 50000, page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(dataset_id, where = where, select = select,
                          order = order, q = q,
                          limit = min(page_size, max_rows - offset),
                          offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), type = character(),
  page_views = integer(), updated_at = character()
)

# == Discovery =================================================================

#' List datasets on the Connecticut Open Data portal
#'
#' Queries the Socrata discovery API for datasets hosted on data.ct.gov.
#'
#' @param limit Number of datasets to return (default 50, max 100)
#' @param category Optional category filter (e.g. "Health and Human Services")
#' @param order Column to sort by: "page_views_total" (default), "relevance", "updatedAt"
#' @return tibble: id, name, category, description, type, page_views, updated_at
ctg_list <- function(limit = 50, category = NULL, order = "page_views_total") {
  url <- sprintf("%s?domains=data.ct.gov&only=datasets&limit=%d&order=%s",
                 .ctg_disco, min(limit, 100), order)
  if (!is.null(category)) {
    url <- paste0(url, "&categories=", utils::URLencode(category, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("CT catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(.schema_datasets)

  res <- raw$results
  tibble(
    id          = vapply(res, function(r) r$resource$id %||% NA_character_, character(1)),
    name        = vapply(res, function(r) r$resource$name %||% NA_character_, character(1)),
    category    = vapply(res, function(r) {
      r$classification$domain_category %||% NA_character_
    }, character(1)),
    description = vapply(res, function(r) {
      d <- r$resource$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    type        = vapply(res, function(r) r$resource$type %||% NA_character_, character(1)),
    page_views  = vapply(res, function(r) {
      as.integer(r$resource$page_views$page_views_total %||% 0L)
    }, integer(1)),
    updated_at  = vapply(res, function(r) r$resource$updatedAt %||% NA_character_, character(1))
  )
}


#' Search Connecticut Open Data datasets
#'
#' Full-text search across dataset names and descriptions.
#'
#' @param query Search terms
#' @param limit Max results (default 20)
#' @return tibble: id, name, category, description, type, page_views, updated_at
ctg_search <- function(query, limit = 20) {
  url <- sprintf("%s?domains=data.ct.gov&search_context=data.ct.gov&q=%s&only=datasets&limit=%d",
                 .ctg_disco, utils::URLencode(query, reserved = TRUE), min(limit, 100))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("CT search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(.schema_datasets)

  res <- raw$results
  tibble(
    id          = vapply(res, function(r) r$resource$id %||% NA_character_, character(1)),
    name        = vapply(res, function(r) r$resource$name %||% NA_character_, character(1)),
    category    = vapply(res, function(r) {
      r$classification$domain_category %||% NA_character_
    }, character(1)),
    description = vapply(res, function(r) {
      d <- r$resource$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    type        = vapply(res, function(r) r$resource$type %||% NA_character_, character(1)),
    page_views  = vapply(res, function(r) {
      as.integer(r$resource$page_views$page_views_total %||% 0L)
    }, integer(1)),
    updated_at  = vapply(res, function(r) r$resource$updatedAt %||% NA_character_, character(1))
  )
}


#' View metadata for a Connecticut dataset
#'
#' Returns detailed metadata for a single dataset including column schema.
#'
#' @param dataset_id Socrata 4x4 identifier (e.g. "emyx-j53e")
#' @return tibble with one row of metadata: id, name, category, description, rows_updated_at,
#'   download_count, columns (semicolon-separated list)
ctg_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .ctg_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("CT view error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(tibble())

  cols_url <- sprintf("%s/api/views/%s/columns.json", .ctg_base, dataset_id)
  cols <- tryCatch({
    c <- .fetch_json(cols_url)
    paste(c$fieldName, collapse = "; ")
  }, error = function(e) NA_character_)

  tibble(
    id              = raw$id %||% NA_character_,
    name            = raw$name %||% NA_character_,
    category        = raw$category %||% NA_character_,
    description     = raw$description %||% NA_character_,
    attribution     = raw$attribution %||% NA_character_,
    rows_updated_at = {
      ts <- raw$rowsUpdatedAt %||% NA_real_
      if (!is.na(ts)) as.POSIXct(ts, origin = "1970-01-01") else as.POSIXct(NA)
    },
    download_count  = as.integer(raw$downloadCount %||% 0L),
    columns         = cols
  )
}


#' Query a Connecticut dataset with SoQL
#'
#' Low-level SODA query with full SoQL support.
#'
#' @param dataset_id Socrata 4x4 identifier
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search within the dataset
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of query results
ctg_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL, q = NULL,
                      limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from a Connecticut dataset (auto-paginated)
#'
#' @param dataset_id Socrata 4x4 identifier
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param max_rows Maximum total rows to fetch (default 50000)
#' @param page_size Rows per request (default 1000)
#' @return tibble of all matching records
ctg_fetch_all <- function(dataset_id, where = NULL, select = NULL,
                          max_rows = 50000, page_size = 1000) {
  ._soda_fetch_all(dataset_id, where = where, select = select,
                   max_rows = max_rows, page_size = page_size)
}

# == Named access functions ====================================================

#' Connecticut mill rates by municipality
#'
#' Property tax mill rates for Connecticut towns (FY 2014-2026).
#' Dataset: emyx-j53e
#'
#' @param municipality Optional town name filter (case-insensitive partial match)
#' @param fiscal_year Optional fiscal year filter (numeric)
#' @param limit Max rows (default 5000)
#' @return tibble: municipality, fiscal_year, grand_list_year, mill_rate,
#'   mill_rate_real_personal, mill_rate_motor_vehicle
ctg_mill_rates <- function(municipality = NULL, fiscal_year = NULL, limit = 5000) {
  wheres <- character()
  if (!is.null(municipality)) {
    wheres <- c(wheres, sprintf("upper(municipality) like upper('%%%s%%')", municipality))
  }
  if (!is.null(fiscal_year)) {
    wheres <- c(wheres, sprintf("fiscal_year = %d", as.integer(fiscal_year)))
  }
  where <- if (length(wheres) > 0) paste(wheres, collapse = " AND ") else NULL

  df <- ._soda_fetch_all("emyx-j53e", where = where,
                         order = "fiscal_year DESC, municipality ASC",
                         max_rows = limit)
  if (nrow(df) == 0) return(tibble(
    municipality = character(), fiscal_year = integer(),
    grand_list_year = integer(), mill_rate = double(),
    mill_rate_real_personal = double(), mill_rate_motor_vehicle = double()
  ))
  tibble(
    municipality            = as.character(df$municipality),
    fiscal_year             = as.integer(df$fiscal_year),
    grand_list_year         = as.integer(df$grand_list_year),
    mill_rate               = as.double(df$mill_rate),
    mill_rate_real_personal = as.double(df$mill_rate_real_personal %||% NA_real_),
    mill_rate_motor_vehicle = as.double(df$mill_rate_motor_vehicle %||% NA_real_)
  )
}


#' Connecticut correctional facility daily population
#'
#' Daily headcounts at DOC facilities. Dataset: n8x6-s299
#'
#' @param facility Optional facility name filter (partial match)
#' @param start_date Optional start date (YYYY-MM-DD)
#' @param end_date Optional end date (YYYY-MM-DD)
#' @param limit Max rows (default 5000)
#' @return tibble: date, facility_name, accused_count, sentenced_count, total_population
ctg_corrections <- function(facility = NULL, start_date = NULL,
                            end_date = NULL, limit = 5000) {
  wheres <- character()
  if (!is.null(facility)) {
    wheres <- c(wheres, sprintf("upper(facility_name) like upper('%%%s%%')", facility))
  }
  if (!is.null(start_date)) {
    wheres <- c(wheres, sprintf("date >= '%s'", start_date))
  }
  if (!is.null(end_date)) {
    wheres <- c(wheres, sprintf("date <= '%s'", end_date))
  }
  where <- if (length(wheres) > 0) paste(wheres, collapse = " AND ") else NULL

  df <- ._soda_fetch_all("n8x6-s299", where = where, order = "date DESC",
                         max_rows = limit)
  if (nrow(df) == 0) return(tibble(
    date = as.Date(character()), facility_name = character(),
    accused_count = integer(), sentenced_count = integer(),
    total_population = integer()
  ))
  tibble(
    date             = as.Date(substr(df$date, 1, 10)),
    facility_name    = as.character(df$facility_name),
    accused_count    = as.integer(df$accused_other_status_count),
    sentenced_count  = as.integer(df$sentenced_status_count),
    total_population = as.integer(df$total_facility_population_count)
  )
}


#' Connecticut state licenses and credentials
#'
#' Active professional licenses. Dataset: ngch-56tr
#'
#' @param name Optional name filter (partial match)
#' @param credential Optional credential type filter (partial match)
#' @param city Optional city filter
#' @param limit Max rows (default 1000)
#' @return tibble: credential_id, name, business_name, credential_type, credential,
#'   status, issue_date, expiration_date, city, state
ctg_licenses <- function(name = NULL, credential = NULL, city = NULL, limit = 1000) {
  wheres <- character()
  if (!is.null(name)) {
    wheres <- c(wheres, sprintf("upper(name) like upper('%%%s%%')", name))
  }
  if (!is.null(credential)) {
    wheres <- c(wheres, sprintf("upper(credential) like upper('%%%s%%')", credential))
  }
  if (!is.null(city)) {
    wheres <- c(wheres, sprintf("upper(city) like upper('%%%s%%')", city))
  }
  where <- if (length(wheres) > 0) paste(wheres, collapse = " AND ") else NULL

  df <- ._soda_query("ngch-56tr", where = where, limit = limit)
  if (nrow(df) == 0) return(tibble(
    credential_id = integer(), name = character(), business_name = character(),
    credential_type = character(), credential = character(), status = character(),
    issue_date = as.Date(character()), expiration_date = as.Date(character()),
    city = character(), state = character()
  ))
  tibble(
    credential_id   = as.integer(df$credentialid),
    name            = as.character(df$name),
    business_name   = as.character(df$businessname %||% NA_character_),
    credential_type = as.character(df$credentialtype),
    credential      = as.character(df$credential),
    status          = as.character(df$status),
    issue_date      = as.Date(substr(df$issuedate %||% NA_character_, 1, 10)),
    expiration_date = as.Date(substr(df$expirationdate %||% NA_character_, 1, 10)),
    city            = as.character(df$city),
    state           = as.character(df$state)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for ct.gov client
#'
#' Reads own source and returns full public function signatures and bodies.
#'
#' @return Character string (invisibly); also printed to console
ctg_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/ct.gov.R"
  if (!file.exists(src_file)) {
    cat("# ct.gov context - source not found\n")
    return(invisible("# ct.gov context - source not found"))
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
