# oregon.gov.R
# Oregon State Open Data client (data.oregon.gov — Socrata/SODA 2.1).
# Discovery + access for 420+ Socrata datasets covering business, health,
# elections, workers' comp, housing, licensing, education, environment, and more.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.or_domain <- "data.oregon.gov"
.or_disco  <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_   else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_      else as.double(x)

# -- JSON fetch ----------------------------------------------------------------

.or_fetch <- function(url, params = list()) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

.or_fetch_list <- function(url, params = list()) {
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
  url <- sprintf("https://%s/resource/%s.json", .or_domain, view_id)
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(order))  params[["$order"]]   <- order
  if (!is.null(group))  params[["$group"]]   <- group
  if (!is.null(q))      params[["$q"]]       <- q

  raw <- .or_fetch(url, params)
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
      if (all(grepl("^-?[0-9]*\\.?[0-9]+$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
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

#' List all datasets on data.oregon.gov
#'
#' Uses the Socrata Discovery API to list datasets sorted by popularity.
#'
#' @param limit Max results (default 100, max 10000)
#' @param offset Pagination offset
#' @param only Resource type: "datasets" (default), "maps", "files"
#' @return tibble: id, name, description, type, updatedAt, page_views_total
#' @export
oreg_list <- function(limit = 100, offset = 0, only = "datasets") {
  params <- list(domains = .or_domain, limit = limit, offset = offset,
                 order = "page_views_total")
  if (!is.null(only)) params$only <- only

  raw <- .or_fetch_list(.or_disco, params)
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


#' Search datasets on data.oregon.gov
#'
#' Full-text search across dataset names, descriptions, and columns.
#'
#' @param query Search term
#' @param limit Max results (default 50)
#' @param offset Pagination offset
#' @return tibble
#' @export
oreg_search <- function(query, limit = 50, offset = 0) {
  params <- list(domains = .or_domain, q = query, limit = limit,
                 offset = offset, only = "datasets")
  raw <- .or_fetch_list(.or_disco, params)
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


#' Query any Oregon dataset by view ID (generic SODA access)
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
oreg_view <- function(view_id, select = NULL, where = NULL,
                      order = NULL, q = NULL, limit = 1000, offset = 0) {
  ._soda_query(view_id, select = select, where = where,
               order = order, q = q, limit = limit, offset = offset)
}


# == Business ==================================================================

#' Active businesses in Oregon (all)
#'
#' Complete registry of active businesses from the Secretary of State.
#'
#' @param business_name Filter by name (SoQL LIKE)
#' @param entity_type Filter: e.g. "DOMESTIC BUSINESS CORPORATION"
#' @param city Filter by city
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_businesses <- function(business_name = NULL, entity_type = NULL,
                            city = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(business_name) LIKE upper('%%%s%%')", business_name))
  if (!is.null(entity_type)) clauses <- c(clauses,
    sprintf("entity_type='%s'", entity_type))
  if (!is.null(city)) clauses <- c(clauses,
    sprintf("upper(city) LIKE upper('%%%s%%')", city))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tckn-sxa6", where = w, limit = limit)
}


#' Active businesses by county
#'
#' @param county Filter by county name
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_businesses_county <- function(county = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses,
    sprintf("upper(county) LIKE upper('%%%s%%')", county))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("6g49-bcrm", where = w, limit = limit)
}


#' Active nonprofit corporations
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_nonprofits <- function(where = NULL, limit = 1000) {
  ._soda_query("8kyv-b2kw", where = where, limit = limit)
}


#' Active trademark registrations
#'
#' @param q Search trademark descriptions
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_trademarks <- function(q = NULL, where = NULL, limit = 1000) {
  ._soda_query("ny3n-dx3v", where = where, q = q, limit = limit)
}


#' UCC (Uniform Commercial Code) filings entered last month
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_ucc_filings <- function(where = NULL, limit = 1000) {
  ._soda_query("snfi-f79b", where = where, limit = limit)
}


# == Elections =================================================================

#' Voter registration data
#'
#' Registration counts by county, party, and district.
#'
#' @param county Filter by county name
#' @param party Filter by party name
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_voter_registration <- function(county = NULL, party = NULL,
                                    where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses,
    sprintf("upper(county) LIKE upper('%%%s%%')", county))
  if (!is.null(party)) clauses <- c(clauses,
    sprintf("upper(party) LIKE upper('%%%s%%')", party))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("8h6y-5uec", where = w, limit = limit)
}


#' Campaign finance penalty notices
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_campaign_penalties <- function(where = NULL, limit = 1000) {
  ._soda_query("fku5-vh2b", where = where, limit = limit)
}


#' Voting districts by precinct
#'
#' @param county Filter by county
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_voting_districts <- function(county = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county)) clauses <- c(clauses,
    sprintf("upper(county) LIKE upper('%%%s%%')", county))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("r7vb-b9k4", where = w, limit = limit)
}


# == Workers' Compensation ====================================================

#' Active workers' compensation employer database
#'
#' @param business_name Filter by employer name
#' @param county_code Filter by county code
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_workers_comp_employers <- function(business_name = NULL, county_code = NULL,
                                        where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(legal_business_name) LIKE upper('%%%s%%')", business_name))
  if (!is.null(county_code)) clauses <- c(clauses,
    sprintf("county_code='%s'", county_code))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("q9zj-c8r2", where = w, limit = limit)
}


#' Workers' compensation record level claims
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_workers_comp_claims <- function(where = NULL, limit = 1000) {
  ._soda_query("t9t7-8a2y", where = where, limit = limit)
}


#' WARN (Worker Adjustment and Retraining Notification) notices
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_warn_notices <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("ijbz-jpx8", where = where, q = q, limit = limit)
}


# == Health ====================================================================

#' Weekly reportable disease data
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_disease_data <- function(where = NULL, limit = 1000) {
  ._soda_query("2idf-8ewz", where = where, limit = limit)
}


#' Oregon Medical Board license type and status counts
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_medical_licenses <- function(where = NULL, limit = 1000) {
  ._soda_query("ifun-evx5", where = where, limit = limit)
}


#' Oregon Medicaid prioritized list funding line
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_medicaid_funding <- function(where = NULL, limit = 1000) {
  ._soda_query("2mv8-fp67", where = where, limit = limit)
}


# == Housing ===================================================================

#' Affordable housing inventory
#'
#' @param city Filter by city
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_affordable_housing <- function(city = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(city)) clauses <- c(clauses,
    sprintf("upper(city) LIKE upper('%%%s%%')", city))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("p9yn-ftai", where = w, limit = limit)
}


#' Affordable rental housing partner database
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_rental_housing_partners <- function(where = NULL, limit = 1000) {
  ._soda_query("97sx-jz4n", where = where, limit = limit)
}


#' OHCS housing grant opportunities
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_housing_grants <- function(where = NULL, limit = 1000) {
  ._soda_query("8e87-r9yi", where = where, limit = limit)
}


# == Licensing =================================================================

#' Notary public directory
#'
#' @param last_name Filter by last name
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_notaries <- function(last_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(last_name)) clauses <- c(clauses,
    sprintf("upper(last_name) LIKE upper('%%%s%%')", last_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("m934-wp3t", where = w, limit = limit)
}


#' Building Codes Division - Active contractor/individual licenses
#'
#' @param business_name Filter by name
#' @param where Additional SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_contractor_licenses <- function(business_name = NULL, where = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(business_name)) clauses <- c(clauses,
    sprintf("upper(business_name) LIKE upper('%%%s%%')", business_name))
  if (!is.null(where)) clauses <- c(clauses, where)
  w <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("vhbr-cuaq", where = w, limit = limit)
}


#' CCB (Construction Contractors Board) active licenses
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_ccb_licenses <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("g77e-6bhs", where = where, q = q, limit = limit)
}


# == Education =================================================================

#' Community college course listing
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_cc_courses <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("4yhv-tzjd", where = where, q = q, limit = limit)
}


#' Eligible training provider list
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_training_providers <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("dhnh-39zs", where = where, q = q, limit = limit)
}


# == Environment & Agriculture =================================================

#' Farm products master list
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_farm_products <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("3qaz-7u98", where = where, q = q, limit = limit)
}


#' Hazardous materials storage sites
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_hazmat_sites <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("ae2j-x3ks", where = where, q = q, limit = limit)
}


#' Fertilizer program stop sales
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_fertilizer_stops <- function(where = NULL, limit = 1000) {
  ._soda_query("svge-u3j9", where = where, limit = limit)
}


# == Finance ===================================================================

#' Budgeted revenue
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_budgeted_revenue <- function(where = NULL, limit = 1000) {
  ._soda_query("mwsa-rpk9", where = where, limit = limit)
}


#' Secretary of State financial accounting (current biennium)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_sos_accounting <- function(where = NULL, limit = 1000) {
  ._soda_query("vsgy-hcp4", where = where, limit = limit)
}


#' Secretary of State payroll (current biennium)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_sos_payroll <- function(where = NULL, limit = 1000) {
  ._soda_query("fqkk-e5mr", where = where, limit = limit)
}


# == Libraries =================================================================

#' Oregon library directory
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_libraries <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("6x9d-idz4", where = where, q = q, limit = limit)
}


#' Oregon public library statistics
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_library_stats <- function(where = NULL, limit = 1000) {
  ._soda_query("8zw7-zgjw", where = where, limit = limit)
}


# == Government ================================================================

#' Oregon agencies, boards, and commissions
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_agencies <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("wu8n-jqum", where = where, q = q, limit = limit)
}


#' State agency data inventory
#'
#' @param where SoQL where clause
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_data_inventory <- function(where = NULL, q = NULL, limit = 1000) {
  ._soda_query("yp9j-pm7w", where = where, q = q, limit = limit)
}


#' OEM emergency managers (local and tribal)
#'
#' @param where SoQL where clause
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
oreg_emergency_managers <- function(where = NULL, limit = 1000) {
  ._soda_query("vcvj-awba", where = where, limit = limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for oregon.gov
#'
#' Reads own source file and returns all public function signatures.
#'
#' @return Character string with function bodies
#' @export
oreg_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/oregon.gov.R"
  if (!file.exists(src_file)) {
    cat("# oregon.gov context - source not found\n")
    return(invisible("# oregon.gov context - source not found"))
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
