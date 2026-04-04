# ct.gov.R - State of Connecticut Open Data (Socrata SODA) client
# Self-contained. All public functions return tibbles.
#
# Portal: data.ct.gov (629 Socrata views, 1042 total datasets)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Prefix: ctg_


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
#' Connecticut publishes 629+ Socrata datasets covering health, education,
#' transportation, public safety, and government operations.
#'
#' @param limit Integer. Number of datasets to return (default 50, max 100).
#' @param category Character or NULL. Category filter.
#'   Example: \code{"Health and Human Services"}, \code{"Education"},
#'   \code{"Transportation"}, \code{"Public Safety"}.
#' @param order Character. Sort field (default \code{"page_views_total"}).
#'   Valid: \code{"page_views_total"}, \code{"relevance"}, \code{"updatedAt"}.
#' @return A tibble with 7 columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 ID (e.g. "28fr-iqnx").}
#'     \item{name}{Character. Dataset name.}
#'     \item{category}{Character. Domain category (e.g. "Health and Human Services").}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{type}{Character. Asset type (e.g. "dataset").}
#'     \item{page_views}{Integer. Total page views.}
#'     \item{updated_at}{Character. Last update ISO timestamp.}
#'   }
#' @examples
#' ctg_list(limit = 10)
#' ctg_list(category = "Health and Human Services")
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
  # Handle data.frame result from simplifyVector = TRUE
  r_res <- res$resource
  r_cls <- res$classification
  desc_raw <- r_res$description %||% rep("", nrow(r_res))
  desc_trunc <- ifelse(nchar(desc_raw) > 200, paste0(substr(desc_raw, 1, 200), "..."), desc_raw)
  pv <- if (is.data.frame(r_res$page_views)) r_res$page_views$page_views_total else rep(0L, nrow(r_res))

  tibble(
    id          = as.character(r_res$id),
    name        = as.character(r_res$name),
    category    = as.character(r_cls$domain_category %||% NA_character_),
    description = desc_trunc,
    type        = as.character(r_res$type),
    page_views  = as.integer(pv),
    updated_at  = as.character(r_res$updatedAt)
  )
}


#' Search Connecticut Open Data datasets
#'
#' Full-text search across dataset names and descriptions on data.ct.gov.
#'
#' @param query Character. Search terms.
#'   Example: \code{"education"}, \code{"COVID"}, \code{"crime"}
#' @param limit Integer. Max results (default 20, max 100).
#' @return A tibble with 7 columns (same as \code{\link{ctg_list}}):
#'   id, name, category, description, type, page_views, updated_at.
#' @examples
#' ctg_search("education")
#' ctg_search("COVID", limit = 10)
ctg_search <- function(query, limit = 20) {
  url <- sprintf("%s?domains=data.ct.gov&search_context=data.ct.gov&q=%s&only=datasets&limit=%d",
                 .ctg_disco, utils::URLencode(query, reserved = TRUE), min(limit, 100))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("CT search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(.schema_datasets)

  res <- raw$results
  r_res <- res$resource
  r_cls <- res$classification
  desc_raw <- r_res$description %||% rep("", nrow(r_res))
  desc_trunc <- ifelse(nchar(desc_raw) > 200, paste0(substr(desc_raw, 1, 200), "..."), desc_raw)
  pv <- if (is.data.frame(r_res$page_views)) r_res$page_views$page_views_total else rep(0L, nrow(r_res))

  tibble(
    id          = as.character(r_res$id),
    name        = as.character(r_res$name),
    category    = as.character(r_cls$domain_category %||% NA_character_),
    description = desc_trunc,
    type        = as.character(r_res$type),
    page_views  = as.integer(pv),
    updated_at  = as.character(r_res$updatedAt)
  )
}


#' View metadata for a Connecticut dataset
#'
#' Returns detailed metadata for a single dataset including column schema.
#' Useful for inspecting a dataset before querying with \code{\link{ctg_query}}.
#'
#' @param dataset_id Character. Socrata 4x4 identifier.
#'   Example: \code{"emyx-j53e"} (Mill Rates), \code{"n8x6-s299"} (Corrections)
#' @return A tibble with 1 row and 8 columns:
#'   \describe{
#'     \item{id}{Character. Dataset ID.}
#'     \item{name}{Character. Dataset name.}
#'     \item{category}{Character. Category.}
#'     \item{description}{Character. Full description.}
#'     \item{attribution}{Character. Data source attribution.}
#'     \item{rows_updated_at}{POSIXct. Last row update timestamp.}
#'     \item{download_count}{Integer. Total downloads.}
#'     \item{columns}{Character. Semicolon-delimited column names.}
#'   }
#' @examples
#' ctg_view("emyx-j53e")
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
#' General-purpose SODA query against any data.ct.gov dataset. Supports
#' full SoQL syntax including WHERE, SELECT, GROUP BY, ORDER BY, and
#' full-text search.
#'
#' @param dataset_id Character. Socrata 4x4 identifier.
#'   Example: \code{"emyx-j53e"}, \code{"n8x6-s299"}
#' @param where Character or NULL. SoQL WHERE clause.
#'   Example: \code{"fiscal_year = 2024"}
#' @param select Character or NULL. SoQL SELECT clause.
#'   Example: \code{"municipality, mill_rate"}
#' @param group Character or NULL. SoQL GROUP BY clause.
#'   Example: \code{"municipality"}
#' @param order Character or NULL. SoQL ORDER BY clause.
#'   Example: \code{"mill_rate DESC"}
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Max rows (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of query results. Columns depend on the dataset.
#' @examples
#' ctg_query("emyx-j53e", limit = 5)
#' ctg_query("emyx-j53e", where = "fiscal_year = 2024")
ctg_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL, q = NULL,
                      limit = 1000, offset = 0) {
  ._soda_query(dataset_id, where = where, select = select,
               group = group, order = order, q = q,
               limit = limit, offset = offset)
}


#' Fetch all records from a Connecticut dataset (auto-paginated)
#'
#' Automatically paginates through a dataset to fetch up to max_rows records.
#' Useful for small-to-medium datasets that exceed the single-query limit.
#'
#' @param dataset_id Character. Socrata 4x4 identifier.
#'   Example: \code{"emyx-j53e"}, \code{"n8x6-s299"}
#' @param where Character or NULL. Optional SoQL WHERE clause.
#' @param select Character or NULL. Optional SoQL SELECT clause.
#' @param max_rows Integer. Maximum total rows to fetch (default 50000).
#' @param page_size Integer. Rows per API request (default 1000).
#' @return A tibble of all matching records. Columns depend on the dataset.
#' @examples
#' ctg_fetch_all("emyx-j53e", max_rows = 500)
ctg_fetch_all <- function(dataset_id, where = NULL, select = NULL,
                          max_rows = 50000, page_size = 1000) {
  ._soda_fetch_all(dataset_id, where = where, select = select,
                   max_rows = max_rows, page_size = page_size)
}

# == Named access functions ====================================================

#' Connecticut mill rates by municipality
#'
#' Property tax mill rates for Connecticut towns (FY 2014-2026).
#' Dataset: emyx-j53e. A mill rate is the tax per dollar of assessed
#' property value (e.g. 30.0 = $30 per $1,000 assessed value).
#'
#' @param municipality Character or NULL. Town name filter (case-insensitive
#'   partial match). Example: \code{"Hartford"}, \code{"New Haven"}
#' @param fiscal_year Integer or NULL. Fiscal year filter.
#'   Example: \code{2024}
#' @param limit Integer. Max rows to return (default 5000).
#' @return A tibble with 6 columns:
#'   \describe{
#'     \item{municipality}{Character. Town name.}
#'     \item{fiscal_year}{Integer. Fiscal year (e.g. 2024).}
#'     \item{grand_list_year}{Integer. Grand list year.}
#'     \item{mill_rate}{Numeric. General mill rate.}
#'     \item{mill_rate_real_personal}{Numeric. Real/personal property rate (or NA).}
#'     \item{mill_rate_motor_vehicle}{Numeric. Motor vehicle rate (or NA).}
#'   }
#' @examples
#' ctg_mill_rates(municipality = "Hartford")
#' ctg_mill_rates(fiscal_year = 2024, limit = 20)
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
#' Daily headcounts at Connecticut Department of Correction facilities.
#' Dataset: n8x6-s299. Includes accused (pre-trial) and sentenced populations.
#'
#' @param facility Character or NULL. Facility name filter (case-insensitive
#'   partial match). Example: \code{"Hartford"}, \code{"Cheshire"}
#' @param start_date Character or NULL. Start date in \code{"YYYY-MM-DD"} format.
#'   Example: \code{"2024-01-01"}
#' @param end_date Character or NULL. End date in \code{"YYYY-MM-DD"} format.
#'   Example: \code{"2024-12-31"}
#' @param limit Integer. Max rows to return (default 5000).
#' @return A tibble with 5 columns:
#'   \describe{
#'     \item{date}{Date. Count date.}
#'     \item{facility_name}{Character. Facility name.}
#'     \item{accused_count}{Integer. Accused/pre-trial population.}
#'     \item{sentenced_count}{Integer. Sentenced population.}
#'     \item{total_population}{Integer. Total facility population.}
#'   }
#' @examples
#' ctg_corrections(limit = 10)
#' ctg_corrections(start_date = "2024-01-01", end_date = "2024-01-31")
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
#' Active professional licenses and credentials issued by the State of
#' Connecticut. Dataset: ngch-56tr. Covers physicians, nurses, contractors,
#' cosmetologists, and many other professions.
#'
#' @param name Character or NULL. Licensee name filter (case-insensitive
#'   partial match). Example: \code{"Smith"}
#' @param credential Character or NULL. Credential type filter (case-insensitive
#'   partial match). Example: \code{"Physician"}, \code{"Registered Nurse"}
#' @param city Character or NULL. City filter (case-insensitive partial match).
#'   Example: \code{"Hartford"}, \code{"New Haven"}
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with 10 columns:
#'   \describe{
#'     \item{credential_id}{Integer. Unique credential ID.}
#'     \item{name}{Character. Licensee name.}
#'     \item{business_name}{Character. Business name (or NA).}
#'     \item{credential_type}{Character. Credential type.}
#'     \item{credential}{Character. Specific credential (e.g. "Physician").}
#'     \item{status}{Character. License status.}
#'     \item{issue_date}{Date. Date issued.}
#'     \item{expiration_date}{Date. Expiration date.}
#'     \item{city}{Character. City.}
#'     \item{state}{Character. State.}
#'   }
#' @examples
#' ctg_licenses(credential = "Physician", limit = 10)
#' ctg_licenses(city = "Hartford", limit = 20)
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

#' Get ct.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ctg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ctg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ct.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ct.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ct.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ct.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
