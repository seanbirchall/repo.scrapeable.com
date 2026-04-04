# wa.gov.R
# Washington State Open Data client (data.wa.gov — Socrata/SODA 2.1).
# Discovery + access for 630+ Socrata datasets covering politics, health,
# education, wildlife, labor, cannabis/liquor, data breaches, and more.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.wa_domain <- "data.wa.gov"
.wa_disco  <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_   else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_      else as.double(x)

# -- JSON fetch ----------------------------------------------------------------

.wa_fetch <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

.wa_fetch_list <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE, flatten = FALSE)
}

# -- SODA query helper --------------------------------------------------------

._soda_query <- function(view_id, select = NULL, where = NULL,
                         order = NULL, group = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  url <- sprintf("https://%s/resource/%s.json", .wa_domain, view_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(order))  params[["$order"]]   <- order
  if (!is.null(group))  params[["$group"]]   <- group
  if (!is.null(q))      params[["$q"]]       <- q

  raw <- .wa_fetch(url, params)
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  .type_cols(df)
}

# -- Auto-type columns --------------------------------------------------------

.type_cols <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) == 0) next
      # numeric?
      if (all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
      # date?
      if (grepl("date|Date|updated|created|At$", col) &&
          sum(grepl("^\\d{4}-\\d{2}-\\d{2}", vals)) > length(vals) * 0.5) {
        df[[col]] <- as.Date(substr(df[[col]], 1, 10))
      }
    }
  }
  df
}

# -- Paginated fetch -----------------------------------------------------------

._soda_all <- function(view_id, select = NULL, where = NULL,
                       order = NULL, max_rows = 50000, page_size = 5000) {
  all_data <- list()
  offset <- 0
  repeat {
    remaining <- max_rows - offset
    if (remaining <= 0) break
    lim <- min(page_size, remaining)
    chunk <- ._soda_query(view_id, select = select, where = where,
                          order = order, limit = lim, offset = offset)
    if (nrow(chunk) == 0) break
    all_data[[length(all_data) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < lim) break
  }
  if (length(all_data) == 0) return(tibble())
  bind_rows(all_data)
}


# == Discovery =================================================================

#' List all datasets on data.wa.gov
#'
#' Uses the Socrata Discovery API to list datasets sorted by popularity.
#'
#' @param limit Max results (default 100, max 10000)
#' @param offset Pagination offset
#' @param only Resource type: "datasets" (default), "maps", "files"
#' @return tibble: id, name, description, type, updatedAt, page_views_total
#' @export
wag_list <- function(limit = 100, offset = 0, only = "datasets") {
  params <- list(domains = .wa_domain, limit = limit, offset = offset,
                 order = "page_views_total")
  if (!is.null(only)) params$only <- only

  raw <- .wa_fetch_list(.wa_disco, params)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  tibble(
    id          = vapply(results, function(r) .safe_chr(r$resource$id), character(1)),
    name        = vapply(results, function(r) .safe_chr(r$resource$name), character(1)),
    description = vapply(results, function(r) {
      d <- .safe_chr(r$resource$description)
      if (is.na(d)) d else substr(d, 1, 200)
    }, character(1)),
    type        = vapply(results, function(r) .safe_chr(r$resource$type), character(1)),
    updatedAt   = as.Date(vapply(results, function(r)
      substr(.safe_chr(r$resource$updatedAt), 1, 10), character(1))),
    page_views  = vapply(results, function(r)
      .safe_int(r$resource$page_views$page_views_total), integer(1)),
    category    = vapply(results, function(r)
      .safe_chr(r$classification$domain_category), character(1))
  )
}


#' Search datasets on data.wa.gov
#'
#' Full-text search across dataset names, descriptions, and columns.
#'
#' @param query Search term
#' @param limit Max results (default 50)
#' @param offset Pagination offset
#' @return tibble: same as wag_list
#' @export
wag_search <- function(query, limit = 50, offset = 0) {
  params <- list(domains = .wa_domain, q = query, limit = limit,
                 offset = offset, only = "datasets")
  raw <- .wa_fetch_list(.wa_disco, params)
  results <- raw$results
  if (is.null(results) || length(results) == 0) return(tibble())

  tibble(
    id          = vapply(results, function(r) .safe_chr(r$resource$id), character(1)),
    name        = vapply(results, function(r) .safe_chr(r$resource$name), character(1)),
    description = vapply(results, function(r) {
      d <- .safe_chr(r$resource$description)
      if (is.na(d)) d else substr(d, 1, 200)
    }, character(1)),
    type        = vapply(results, function(r) .safe_chr(r$resource$type), character(1)),
    updatedAt   = as.Date(vapply(results, function(r)
      substr(.safe_chr(r$resource$updatedAt), 1, 10), character(1))),
    page_views  = vapply(results, function(r)
      .safe_int(r$resource$page_views$page_views_total), integer(1))
  )
}


#' Query any WA dataset by view ID (generic SODA access)
#'
#' @param view_id Four-by-four Socrata view ID
#' @param select SoQL select clause
#' @param where SoQL where clause
#' @param order SoQL order clause
#' @param q Full-text search within dataset
#' @param limit Max rows (default 1000)
#' @param offset Row offset
#' @return tibble
#' @export
wag_view <- function(view_id, select = NULL, where = NULL,
                     order = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query(view_id, select = select, where = where,
               order = order, q = q, limit = limit, offset = offset)
}


# == Political Finance =========================================================

#' Campaign contributions to candidates and political committees
#'
#' Washington Public Disclosure Commission (PDC) data on contributions.
#'
#' @param filer_name Filter by candidate/committee name (SoQL LIKE)
#' @param year Election year filter
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_contributions <- function(filer_name = NULL, year = NULL,
                              where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(filer_name)) clauses <- c(clauses,
    sprintf("upper(filer_name) LIKE upper('%%%s%%')", filer_name))
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("election_year='%s'", year))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("kv7h-kjye", where = w, order = "receipt_date DESC", limit = limit)
}


#' Campaign expenditures by candidates and political committees
#'
#' @param filer_name Filter by candidate/committee name
#' @param year Election year
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_expenditures <- function(filer_name = NULL, year = NULL,
                             where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(filer_name)) clauses <- c(clauses,
    sprintf("upper(filer_name) LIKE upper('%%%s%%')", filer_name))
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("election_year='%s'", year))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tijg-9zyp", where = w, order = "expenditure_date DESC", limit = limit)
}


#' PDC enforcement cases
#'
#' @param case_status Filter: "Open", "Closed", etc.
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_enforcement_cases <- function(case_status = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(case_status)) clauses <- c(clauses,
    sprintf("case_status='%s'", case_status))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("a4ma-dq6s", where = w, limit = limit)
}


#' Campaign finance summary
#'
#' @param filer_name Filter by name
#' @param year Election year
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_campaign_summary <- function(filer_name = NULL, year = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(filer_name)) clauses <- c(clauses,
    sprintf("upper(filer_name) LIKE upper('%%%s%%')", filer_name))
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("election_year='%s'", year))
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("3h9x-7bvm", where = w, limit = limit)
}


# == Lobbyist Data =============================================================

#' Lobbyist employment registrations
#'
#' @param year Registration year
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_lobbyist_registrations <- function(year = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year)) clauses <- c(clauses,
    sprintf("employment_year='%s'", year))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("xhn7-64im", where = w, limit = limit)
}


#' Lobbyist compensation and expenses by source
#'
#' @param year Filter year
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_lobbyist_compensation <- function(year = NULL, limit = 1000) {
  w <- if (!is.null(year)) sprintf("reporting_period_year='%s'", year) else NULL
  ._soda_query("9nnw-c693", where = w, limit = limit)
}


# == Health ====================================================================

#' Health care provider credentials
#'
#' @param credential_type Filter by type (e.g., "Physician")
#' @param last_name Filter by last name
#' @param status Filter: "Active", "Expired", etc.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_health_providers <- function(credential_type = NULL, last_name = NULL,
                                 status = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(credential_type)) clauses <- c(clauses,
    sprintf("upper(credentialtype) LIKE upper('%%%s%%')", credential_type))
  if (!is.null(last_name)) clauses <- c(clauses,
    sprintf("upper(lastname) LIKE upper('%%%s%%')", last_name))
  if (!is.null(status)) clauses <- c(clauses,
    sprintf("status='%s'", status))
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("qxh8-f4bd", where = w, limit = limit)
}


# == Data Breaches =============================================================

#' Data breach notifications affecting WA residents
#'
#' @param year Filter by year
#' @param industry_type Filter by industry
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_data_breaches <- function(year = NULL, industry_type = NULL,
                              where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(year)) clauses <- c(clauses, sprintf("year='%s'", year))
  if (!is.null(industry_type)) clauses <- c(clauses,
    sprintf("industrytype='%s'", industry_type))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("sb4j-ca4h", where = w, order = "datesubmitted DESC", limit = limit)
}


#' Consumer complaints to Attorney General
#'
#' @param business_name Filter by business
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_consumer_complaints <- function(business_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(businessname) LIKE upper('%%%s%%')", business_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("gpri-47xz", where = w, limit = limit)
}


# == Education =================================================================

#' Graduation pathway completion rates
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_graduation_rates <- function(where = NULL, limit = 1000) {
  ._soda_query("vjrh-5urx", where = where, limit = limit)
}


#' Washington School Improvement Framework (WSIF)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_school_improvement <- function(where = NULL, limit = 1000) {
  ._soda_query("u25x-vdun", where = where, limit = limit)
}


# == Wildlife & Fish (WDFW) ====================================================

#' WDFW salmonid population indicators
#'
#' @param species Filter by species name
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_salmon_populations <- function(species = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(species)) clauses <- c(clauses,
    sprintf("upper(species) LIKE upper('%%%s%%')", species))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("ncqh-ypvf", where = w, limit = limit)
}


#' WDFW salmonid spawner abundance metrics
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_salmon_metrics <- function(where = NULL, limit = 1000) {
  ._soda_query("x25s-cxg8", where = where, limit = limit)
}


#' WDFW hatchery information
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_hatcheries <- function(where = NULL, limit = 1000) {
  ._soda_query("hjdc-v2n4", where = where, limit = limit)
}


#' WDFW spawning ground surveys
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_spawning_surveys <- function(where = NULL, limit = 1000) {
  ._soda_query("97am-y7xm", where = where, limit = limit)
}


#' WDFW fish releases/plants
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_fish_plants <- function(where = NULL, limit = 1000) {
  ._soda_query("6fex-3r7d", where = where, limit = limit)
}


# == Labor & Industries ========================================================

#' L&I contractor license data
#'
#' @param business_name Filter by business name
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_contractor_licenses <- function(business_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(businessname) LIKE upper('%%%s%%')", business_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("m8qx-ubtq", where = w, limit = limit)
}


#' L&I public works intent projects
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_public_works_intents <- function(where = NULL, limit = 1000) {
  ._soda_query("t9je-9qwa", where = where, limit = limit)
}


#' L&I apprentice utilization on public works
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_apprentice_utilization <- function(where = NULL, limit = 1000) {
  ._soda_query("ijvn-uemp", where = where, limit = limit)
}


# == Liquor & Cannabis =========================================================

#' LCB cannabis license renewals
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_cannabis_renewals <- function(where = NULL, limit = 1000) {
  ._soda_query("brpd-b6zd", where = where, limit = limit)
}


#' LCB liquor license renewals
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_liquor_renewals <- function(where = NULL, limit = 1000) {
  ._soda_query("9dee-kzm5", where = where, limit = limit)
}


# == Environment ===============================================================

#' SEPA (State Environmental Policy Act) register
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_sepa <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("mmcb-z6jf", where = where, q = q, limit = limit)
}


#' Greenhouse gas emissions facility data (2017)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_greenhouse_gas <- function(where = NULL, limit = 1000) {
  ._soda_query("533j-4nbp", where = where, limit = limit)
}


# == Criminal Justice ==========================================================

#' Criminal justice training commission officer certification cases
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_officer_certifications <- function(where = NULL, limit = 1000) {
  ._soda_query("r5ki-dmfz", where = where, limit = limit)
}


#' Criminal justice data book
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_crime_reports <- function(where = NULL, limit = 1000) {
  ._soda_query("v2gc-rgep", where = where, limit = limit)
}


# == Government Finance ========================================================

#' State agency contracts (current fiscal year)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_agency_contracts <- function(where = NULL, limit = 1000) {
  ._soda_query("6fx9-ncas", where = where, limit = limit)
}


#' Statewide master contract sales data
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_contract_sales <- function(where = NULL, limit = 1000) {
  ._soda_query("n8q6-4twj", where = where, limit = limit)
}


#' IT spend by technology towers
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_it_spend <- function(where = NULL, limit = 1000) {
  ._soda_query("vuts-wzbq", where = where, limit = limit)
}


# == Licensing =================================================================

#' Professional license transactions (DOL)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_professional_licenses <- function(where = NULL, limit = 1000) {
  ._soda_query("ixni-jq78", where = where, limit = limit)
}


#' Vehicle registrations by class and county
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_vehicle_registrations <- function(where = NULL, limit = 1000) {
  ._soda_query("hmzg-s6q4", where = where, limit = limit)
}


#' Washington State Certified Public Accountants
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
wag_cpa_firms <- function(where = NULL, limit = 1000) {
  ._soda_query("pzcu-jpab", where = where, limit = limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for wa.gov
#'
#' Reads own source file and returns all public function signatures.
#'
#' @return Character string with function bodies
#' @export
wag_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/wa.gov.R"
  if (!file.exists(src_file)) {
    cat("# wa.gov context - source not found\n")
    return(invisible("# wa.gov context - source not found"))
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
