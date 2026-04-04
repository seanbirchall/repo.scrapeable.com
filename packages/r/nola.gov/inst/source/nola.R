# nola.gov.R - Self-contained City of New Orleans open data client
# Source: data.nola.gov (Socrata SODA API)
# Datasets: 185 covering police, permits, 311 calls, code enforcement, etc.

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nola_base <- "https://data.nola.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.safe_date <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  tryCatch(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
           error = function(e) as.POSIXct(NA))
}

.safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

.strip_computed <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(df)
  keep <- !grepl("^:@computed_region_", names(df))
  df[, keep, drop = FALSE]
}

# == Core SODA query ===========================================================

#' Query any NOLA dataset via SODA
#'
#' Low-level function to query any dataset on data.nola.gov using SoQL.
#'
#' @param dataset_id Four-by-four Socrata dataset identifier (e.g. "es9j-6y5d")
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Max rows to return (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
nola_query <- function(dataset_id, where = NULL, select = NULL,
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
  url <- sprintf("%s/resource/%s.json?%s", .nola_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw) |> .strip_computed()
}

# == Discovery =================================================================

#' List datasets on data.nola.gov
#'
#' Returns metadata for datasets available on the New Orleans open data portal.
#'
#' @param limit Number of datasets to return (default 100)
#' @param category Optional category filter (e.g. "Public Safety and Preparedness")
#' @return tibble: id, name, category, description, updated_at, views
nola_list <- function(limit = 100, category = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.nola.gov&limit=%d&only=datasets",
    .nola_base, limit
  )
  if (!is.null(category)) {
    url <- paste0(url, "&categories=", utils::URLencode(category))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) {
    return(tibble(id = character(), name = character(), category = character(),
                  description = character(), updated_at = character(),
                  views = integer()))
  }
  res <- raw$results$resource
  tibble(
    id          = as.character(res$id),
    name        = as.character(res$name),
    category    = as.character(raw$results$classification$domain_category %||% NA_character_),
    description = vapply(res$description, function(d) {
      d <- d %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    updated_at  = as.character(res$updatedAt),
    views       = as.integer(res$page_views$page_views_total %||% 0L)
  )
}

#' Search NOLA datasets by keyword
#'
#' Full-text search across dataset names and descriptions.
#'
#' @param query Search query string
#' @param limit Maximum results (default 20)
#' @return tibble: id, name, category, description, updated_at, views
nola_search <- function(query, limit = 20) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.nola.gov&q=%s&limit=%d&only=datasets",
    .nola_base, utils::URLencode(query), limit
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) {
    return(tibble(id = character(), name = character(), category = character(),
                  description = character(), updated_at = character(),
                  views = integer()))
  }
  res <- raw$results$resource
  tibble(
    id          = as.character(res$id),
    name        = as.character(res$name),
    category    = as.character(raw$results$classification$domain_category %||% NA_character_),
    description = vapply(res$description, function(d) {
      d <- d %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    updated_at  = as.character(res$updatedAt),
    views       = as.integer(res$page_views$page_views_total %||% 0L)
  )
}

#' View metadata for a single NOLA dataset
#'
#' Returns column names, types, and dataset description.
#'
#' @param dataset_id Four-by-four Socrata dataset identifier
#' @return tibble with columns: field_name, data_type, description
nola_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s/columns.json", .nola_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NOLA view error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) {
    return(tibble(field_name = character(), data_type = character(),
                  description = character()))
  }
  tibble(
    field_name  = as.character(raw$fieldName),
    data_type   = as.character(raw$dataTypeName),
    description = as.character(raw$description %||% NA_character_)
  )
}

# == Named data functions =====================================================

#' NOLA 311 service requests
#'
#' Fetch 311 calls from the Orleans Parish Communications District.
#' Dataset: 2jgv-pqrq
#'
#' @param type Optional request type filter (e.g. "Pothole")
#' @param status Optional status filter (e.g. "Closed", "Open")
#' @param since Date string (YYYY-MM-DD) to filter requests after
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of 311 service requests
nola_311 <- function(type = NULL, status = NULL, since = NULL,
                     limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))   clauses <- c(clauses, sprintf("request_type='%s'", type))
  if (!is.null(status)) clauses <- c(clauses, sprintf("request_status='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("date_created>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("2jgv-pqrq", where = where, limit = limit, offset = offset,
                    order = "date_created DESC")
  if (nrow(df) == 0) return(df)
  if ("date_created" %in% names(df)) df$date_created <- .safe_date(df$date_created)
  if ("date_modified" %in% names(df)) df$date_modified <- .safe_date(df$date_modified)
  if ("case_close_date" %in% names(df)) df$case_close_date <- .safe_date(df$case_close_date)
  if ("latitude" %in% names(df)) df$latitude <- .safe_num(df$latitude)
  if ("longitude" %in% names(df)) df$longitude <- .safe_num(df$longitude)
  df
}

#' NOLA calls for service (police)
#'
#' Incidents reported to NOPD. Current year dataset: es9j-6y5d
#'
#' @param type Optional call type filter (e.g. "SHOOTING")
#' @param district Optional police district number
#' @param since Date string (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of police calls for service
nola_calls_for_service <- function(type = NULL, district = NULL, since = NULL,
                                   limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))     clauses <- c(clauses, sprintf("typetext='%s'", type))
  if (!is.null(district)) clauses <- c(clauses, sprintf("policedistrict='%s'", district))
  if (!is.null(since))    clauses <- c(clauses, sprintf("timecreate>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("es9j-6y5d", where = where, limit = limit, offset = offset,
                    order = "timecreate DESC")
  if (nrow(df) == 0) return(df)
  if ("timecreate" %in% names(df)) df$timecreate <- .safe_date(df$timecreate)
  if ("timeclosed" %in% names(df)) df$timeclosed <- .safe_date(df$timeclosed)
  if ("timearrive" %in% names(df)) df$timearrive <- .safe_date(df$timearrive)
  df
}

#' NOLA building permits
#'
#' City of New Orleans permits since 2012. Dataset: rcm3-fn58
#'
#' @param type Optional permit type code (e.g. "MECH", "PLMB", "ELEC")
#' @param status Optional status filter (e.g. "Permit Issued")
#' @param since Date string (YYYY-MM-DD) for filing date
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of permit records
nola_permits <- function(type = NULL, status = NULL, since = NULL,
                         limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(type))   clauses <- c(clauses, sprintf("code='%s'", type))
  if (!is.null(status)) clauses <- c(clauses, sprintf("currentstatus='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("filingdate>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("rcm3-fn58", where = where, limit = limit, offset = offset,
                    order = "filingdate DESC")
  if (nrow(df) == 0) return(df)
  if ("filingdate" %in% names(df)) df$filingdate <- .safe_date(df$filingdate)
  if ("issuedate" %in% names(df)) df$issuedate <- .safe_date(df$issuedate)
  if ("totalfees" %in% names(df)) df$totalfees <- .safe_num(df$totalfees)
  if ("constrval" %in% names(df)) df$constrval <- .safe_num(df$constrval)
  if ("bldgarea" %in% names(df)) df$bldgarea <- .safe_num(df$bldgarea)
  df
}

#' NOLA code enforcement cases
#'
#' All code enforcement cases. Dataset: u6yx-v2tw
#'
#' @param status Optional case status (e.g. "Open", "Closed")
#' @param since Date string (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of code enforcement cases
nola_code_enforcement <- function(status = NULL, since = NULL,
                                  limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(status)) clauses <- c(clauses, sprintf("o_c='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("casefiled>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("u6yx-v2tw", where = where, limit = limit, offset = offset,
                    order = "casefiled DESC")
  if (nrow(df) == 0) return(df)
  if ("casefiled" %in% names(df)) df$casefiled <- .safe_date(df$casefiled)
  if ("statdate" %in% names(df)) df$statdate <- .safe_date(df$statdate)
  df
}

#' NOLA police use of force incidents
#'
#' Use of force reports from NOPD. Dataset: 9mnw-mbde
#'
#' @param level Optional force level (e.g. "L1", "L2", "L3", "L4")
#' @param disposition Optional disposition (e.g. "Use Of Force Authorized")
#' @param since Date string (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of use of force incidents
nola_use_of_force <- function(level = NULL, disposition = NULL, since = NULL,
                              limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(level))       clauses <- c(clauses, sprintf("use_of_force_level='%s'", level))
  if (!is.null(disposition)) clauses <- c(clauses, sprintf("disposition='%s'", disposition))
  if (!is.null(since))       clauses <- c(clauses, sprintf("date_occurred>='%sT00:00:00'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("9mnw-mbde", where = where, limit = limit, offset = offset,
                    order = "date_occurred DESC")
  if (nrow(df) == 0) return(df)
  if ("date_occurred" %in% names(df)) df$date_occurred <- .safe_date(df$date_occurred)
  df
}

#' NOLA misconduct complaints
#'
#' NOPD misconduct complaints. Dataset: gz2m-ef5u
#'
#' @param disposition Optional disposition filter
#' @param since Date string (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of misconduct complaint records
nola_misconduct <- function(disposition = NULL, since = NULL,
                            limit = 1000, offset = 0) {
  clauses <- c()
  if (!is.null(disposition)) clauses <- c(clauses, sprintf("disposition='%s'", disposition))
  if (!is.null(since))       clauses <- c(clauses, sprintf("date_complaint_received_by_nopd_pib>='%s'", since))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL

  df <- nola_query("gz2m-ef5u", where = where, limit = limit, offset = offset,
                    order = "date_complaint_received_by_nopd_pib DESC")
  if (nrow(df) == 0) return(df)
  df
}

#' NOLA business licenses
#'
#' Active occupational licenses. Dataset: iqay-p646
#'
#' @param q Full-text search (e.g. business name)
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of active business licenses
nola_businesses <- function(q = NULL, limit = 1000, offset = 0) {
  df <- nola_query("iqay-p646", q = q, limit = limit, offset = offset)
  if (nrow(df) == 0) return(df)
  df
}

#' NOLA short-term rental licenses
#'
#' Active STR licenses (Airbnb etc). Dataset: ufdg-ajws
#'
#' @param license_type Optional type (e.g. "Commercial", "Non-Commercial")
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble of active short-term rental licenses
nola_str_licenses <- function(license_type = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(license_type)) {
    where <- sprintf("license_type='%s'", license_type)
  }
  df <- nola_query("ufdg-ajws", where = where, limit = limit, offset = offset)
  if (nrow(df) == 0) return(df)
  df
}

# == Pagination helper ========================================================

#' Fetch all rows from a NOLA dataset with auto-pagination
#'
#' @param dataset_id Socrata four-by-four identifier
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @param max_rows Maximum total rows to fetch (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble of all matching rows
nola_fetch_all <- function(dataset_id, where = NULL, q = NULL,
                           max_rows = 10000, page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- nola_query(dataset_id, where = where, q = q,
                        limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Context ===================================================================

#' Generate LLM-friendly context for nola.gov functions
#'
#' Reads its own source file and returns full function signatures and bodies.
#'
#' @return Character string with all public function definitions
nola_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/nola.gov.R"
  if (!file.exists(src_file)) {
    cat("# nola.gov context - source not found\n")
    return(invisible("# nola.gov context - source not found"))
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
