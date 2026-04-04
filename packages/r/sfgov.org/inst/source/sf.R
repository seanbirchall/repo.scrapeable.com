# sfgov.org.R
# Self-contained San Francisco Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.sfgov.org

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sf_base <- "https://data.sfgov.org"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Core SODA query builder
._soda_query <- function(dataset_id, where = NULL, select = NULL,
                         group = NULL, order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query_str <- paste(
    names(params),
    vapply(params, as.character, character(1)),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .sf_base, dataset_id,
                 utils::URLencode(query_str, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF Open Data query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), updated_at = character(), type = character()
)

.schema_view <- tibble(
  field_name = character(), data_type = character(), description = character()
)

.schema_records <- tibble()

# == Discovery =================================================================

#' List San Francisco Open Data datasets
#'
#' Browse the catalog of datasets on data.sfgov.org.
#'
#' @param limit Number of datasets to return (default 50)
#' @param page Page number (default 1)
#' @return tibble: id, name, category, description, updated_at, type
#' @export
sf_list <- function(limit = 50, page = 1) {
  offset <- (page - 1) * limit
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.sfgov.org&search_context=data.sfgov.org&limit=%d&offset=%d",
    .sf_base, limit, offset
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) return(.schema_datasets)

  results <- raw$results
  res_list <- results$resource
  tibble(
    id          = as.character(res_list$id),
    name        = as.character(res_list$name),
    category    = vapply(seq_len(nrow(res_list)), function(i) {
      cats <- results$classification$domain_category
      if (is.null(cats)) NA_character_ else as.character(cats[i]) %||% NA_character_
    }, character(1)),
    description = vapply(as.character(res_list$description), function(d) {
      if (is.na(d) || nchar(d) <= 200) d else paste0(substr(d, 1, 200), "...")
    }, character(1)),
    updated_at  = as.character(res_list$updatedAt),
    type        = as.character(res_list$type)
  )
}


#' Search San Francisco Open Data datasets
#'
#' Full-text search across the data.sfgov.org catalog.
#'
#' @param query Search terms
#' @param limit Max results (default 20)
#' @param only Filter by asset type: "datasets", "maps", "charts", etc. (optional)
#' @return tibble: id, name, category, description, updated_at, type
#' @export
sf_search <- function(query, limit = 20, only = NULL) {
  url <- sprintf(
    "%s/api/catalog/v1?domains=data.sfgov.org&search_context=data.sfgov.org&q=%s&limit=%d",
    .sf_base, utils::URLencode(query, reserved = TRUE), limit
  )
  if (!is.null(only)) url <- paste0(url, "&only=", only)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw$results) == 0) return(.schema_datasets)

  results <- raw$results
  res_list <- results$resource
  tibble(
    id          = as.character(res_list$id),
    name        = as.character(res_list$name),
    category    = vapply(seq_len(nrow(res_list)), function(i) {
      cats <- results$classification$domain_category
      if (is.null(cats)) NA_character_ else as.character(cats[i]) %||% NA_character_
    }, character(1)),
    description = vapply(as.character(res_list$description), function(d) {
      if (is.na(d) || nchar(d) <= 200) d else paste0(substr(d, 1, 200), "...")
    }, character(1)),
    updated_at  = as.character(res_list$updatedAt),
    type        = as.character(res_list$type)
  )
}


#' View metadata and schema for an SF dataset
#'
#' Returns column names, types, and descriptions for a Socrata view.
#'
#' @param dataset_id Four-by-four Socrata view ID (e.g. "wg3w-h783")
#' @return tibble: field_name, data_type, description
#' @export
sf_view <- function(dataset_id) {
  url <- sprintf("%s/api/views/%s.json", .sf_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("SF view metadata error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$columns)) return(.schema_view)

  cols <- raw$columns
  tibble(
    field_name  = as.character(cols$fieldName),
    data_type   = as.character(cols$dataTypeName),
    description = vapply(cols$description %||% rep(NA_character_, nrow(cols)),
                         function(d) if (is.null(d) || is.na(d)) NA_character_ else as.character(d),
                         character(1))
  )
}


# == Data access ===============================================================

#' Query a San Francisco Open Data dataset
#'
#' Runs a SoQL query against any Socrata dataset on data.sfgov.org.
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where SoQL WHERE clause (e.g. "incident_year='2024'")
#' @param select SoQL SELECT clause (e.g. "incident_category,count(*)")
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Max rows to return (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
sf_query <- function(dataset_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from an SF dataset with auto-pagination
#'
#' @param dataset_id Four-by-four Socrata view ID
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param order Optional SoQL ORDER BY clause
#' @param max_rows Maximum total rows to fetch (default 50000)
#' @param page_size Rows per request (default 10000)
#' @return tibble of all matching records
#' @export
sf_fetch_all <- function(dataset_id, where = NULL, select = NULL,
                         order = NULL, max_rows = 50000,
                         page_size = 10000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(dataset_id, where = where, select = select,
                          order = order, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Named convenience functions ===============================================

#' Fetch SF police incident reports
#'
#' Queries dataset wg3w-h783 (Police Department Incident Reports: 2018 to Present).
#'
#' @param category Incident category filter (e.g. "Larceny Theft", "Assault")
#' @param neighborhood Analysis neighborhood filter (e.g. "Mission", "Tenderloin")
#' @param year Incident year filter (e.g. "2024")
#' @param limit Max rows (default 1000)
#' @return tibble of police incidents
#' @export
sf_police_incidents <- function(category = NULL, neighborhood = NULL,
                                year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(category))     clauses <- c(clauses, sprintf("incident_category='%s'", category))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("analysis_neighborhood='%s'", neighborhood))
  if (!is.null(year))         clauses <- c(clauses, sprintf("incident_year='%s'", year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("wg3w-h783", where = where, order = "incident_datetime DESC", limit = limit)
}


#' Fetch SF 311 service requests
#'
#' Queries dataset vw6y-z8j6 (311 Cases).
#'
#' @param category Service request category filter
#' @param neighborhood Neighborhood filter
#' @param status Status filter (e.g. "Open", "Closed")
#' @param limit Max rows (default 1000)
#' @return tibble of 311 cases
#' @export
sf_311_cases <- function(category = NULL, neighborhood = NULL,
                         status = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(category))     clauses <- c(clauses, sprintf("service_name='%s'", category))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhoods_sffind_boundaries='%s'", neighborhood))
  if (!is.null(status))       clauses <- c(clauses, sprintf("status_description='%s'", status))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("vw6y-z8j6", where = where, order = "requested_datetime DESC", limit = limit)
}


#' Fetch SF fire incidents
#'
#' Queries dataset wr8u-xric (Fire Incidents).
#'
#' @param year Incident year filter
#' @param neighborhood Neighborhood filter
#' @param limit Max rows (default 1000)
#' @return tibble of fire incidents
#' @export
sf_fire_incidents <- function(year = NULL, neighborhood = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year))         clauses <- c(clauses, sprintf("incident_year='%s'", year))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhood_district='%s'", neighborhood))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("wr8u-xric", where = where, order = "incident_date DESC", limit = limit)
}


#' Fetch SF budget data
#'
#' Queries dataset xdgd-c79v (Budget).
#'
#' @param department Department filter
#' @param fiscal_year Fiscal year filter
#' @param limit Max rows (default 1000)
#' @return tibble of budget line items
#' @export
sf_budget <- function(department = NULL, fiscal_year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(department))  clauses <- c(clauses, sprintf("department='%s'", department))
  if (!is.null(fiscal_year)) clauses <- c(clauses, sprintf("fiscal_year='%s'", fiscal_year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("xdgd-c79v", where = where, limit = limit)
}


#' Fetch SF building permits
#'
#' Queries dataset tyz3-vt28 (PermitSF Permitting Data).
#'
#' @param permit_type Permit type filter
#' @param status Current status filter
#' @param neighborhood Neighborhood filter
#' @param limit Max rows (default 1000)
#' @return tibble of building permits
#' @export
sf_permits <- function(permit_type = NULL, status = NULL,
                       neighborhood = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(permit_type))  clauses <- c(clauses, sprintf("permit_type_definition='%s'", permit_type))
  if (!is.null(status))       clauses <- c(clauses, sprintf("status='%s'", status))
  if (!is.null(neighborhood)) clauses <- c(clauses, sprintf("neighborhoods_analysis_boundaries='%s'", neighborhood))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tyz3-vt28", where = where, order = "filed_date DESC", limit = limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for the SF Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
sf_context <- function() {
  .build_context("sfgov.org", header_lines = c(
    "# sfgov.org - San Francisco Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Popular datasets (use 4x4 IDs with sf_query):",
    "#   wg3w-h783 = Police Incident Reports (2018+)",
    "#   vw6y-z8j6 = 311 Cases",
    "#   wr8u-xric = Fire Incidents",
    "#   xdgd-c79v = Budget",
    "#   tyz3-vt28 = Building Permits",
    "#   wmam-7g8d = Buyout Agreements",
    "#   wb4c-6hwj = Fire Inspections",
    "#   jjew-r69b = Mobile Food Facility Permits",
    "#",
    "# SoQL query syntax for filtering, aggregating, ordering"
  ))
}
