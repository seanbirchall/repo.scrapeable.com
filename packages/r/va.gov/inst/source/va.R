# va.gov.R
# Self-contained VA Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at www.data.va.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.va_base <- "https://www.data.va.gov"

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
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- SODA query helper --------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL, group = NULL,
                         order = NULL, limit = 1000, offset = 0, q = NULL) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .va_base, view_id,
                 utils::URLencode(query, reserved = FALSE))
  tryCatch(.fetch_json(url), error = function(e) {
    warning("VA SODA query error: ", e$message)
    return(NULL)
  })
}

._soda_paginate <- function(view_id, where = NULL, select = NULL,
                            order = NULL, max_rows = 10000,
                            page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(view_id, where = where, select = select,
                          order = order, limit = page_size, offset = offset)
    if (is.null(batch) || length(batch) == 0 ||
        (is.data.frame(batch) && nrow(batch) == 0)) break
    results[[length(results) + 1]] <- as_tibble(batch)
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), rows_updated_at = as.POSIXct(character())
)

.schema_records <- tibble()

# == Discovery =================================================================

#' List VA Open Data datasets
#'
#' Returns metadata for datasets on the VA data portal.
#'
#' @param limit Number of datasets (default 100)
#' @return tibble: id, name, category, description, rows_updated_at
#' @export
va_list <- function(limit = 100) {
  url <- sprintf("%s/api/views?limit=%d", .va_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("VA API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)

  tibble(
    id              = as.character(raw$id),
    name            = as.character(raw$name),
    category        = as.character(raw$category %||% NA_character_),
    description     = ifelse(nchar(raw$description %||% "") > 200,
                             paste0(substr(raw$description, 1, 200), "..."),
                             as.character(raw$description %||% "")),
    rows_updated_at = as.POSIXct(as.numeric(raw$rowsUpdatedAt %||% NA_real_),
                                 origin = "1970-01-01")
  )
}

#' Search VA Open Data datasets
#'
#' Full-text search of the VA data catalog.
#'
#' @param query Search term
#' @param limit Number of results (default 50)
#' @return tibble: id, name, category, description
#' @export
va_search <- function(query, limit = 50) {
  url <- sprintf("%s/api/catalog/v1?q=%s&limit=%d", .va_base,
                 utils::URLencode(query, reserved = TRUE), limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("VA search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_datasets[, c("id", "name", "category", "description")])

  results <- raw$results %||% raw
  if (length(results) == 0) return(.schema_datasets[, c("id", "name", "category", "description")])

  res <- if (is.data.frame(results)) results else results$resource %||% results
  if (!is.data.frame(res) || nrow(res) == 0) {
    return(.schema_datasets[, c("id", "name", "category", "description")])
  }

  tibble(
    id          = as.character(res$id %||% res$nbe_fxf %||% NA_character_),
    name        = as.character(res$name %||% NA_character_),
    category    = as.character(res$classification %||% res$category %||% NA_character_),
    description = as.character(res$description %||% NA_character_)
  )
}

#' Query any VA Socrata dataset by view ID
#'
#' Generic SODA query against any VA dataset. Supports SoQL filtering.
#'
#' @param view_id Socrata 4x4 identifier (e.g. "vp34-vuw8")
#' @param where SoQL where clause
#' @param select SoQL select clause
#' @param group SoQL group by clause
#' @param order SoQL order clause
#' @param q Full-text search within dataset
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
va_view <- function(view_id, where = NULL, select = NULL, group = NULL,
                    order = NULL, q = NULL, limit = 1000, offset = 0) {
  raw <- ._soda_query(view_id, where = where, select = select,
                      group = group, order = order, q = q,
                      limit = limit, offset = offset)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

#' Fetch all records from a VA dataset with auto-pagination
#'
#' @param view_id Socrata 4x4 identifier
#' @param where Optional SoQL where clause
#' @param max_rows Maximum total rows (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble of all matching records
#' @export
va_fetch_all <- function(view_id, where = NULL, max_rows = 10000,
                         page_size = 1000) {
  ._soda_paginate(view_id, where = where, max_rows = max_rows,
                  page_size = page_size)
}

# == Veteran Population ========================================================

#' Veteran population demographics by state
#'
#' Returns veteran count, median age, and other demographics by state and year.
#' Source: VetPop data (view e7mv-5px4).
#'
#' @param limit Max rows (default 1000)
#' @return tibble with demographic categories and state-year columns
#' @export
va_population <- function(limit = 1000) {
  raw <- ._soda_query("e7mv-5px4", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

#' Veteran population by race and ethnicity
#'
#' Projected veteran counts by state, race/ethnicity, and year.
#' Source: view 5hak-ip4u.
#'
#' @param state Optional state name filter
#' @param limit Max rows (default 1000)
#' @return tibble with state, race_ethnicity, and yearly projected counts
#' @export
va_race_ethnicity <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) sprintf("state = '%s'", state) else NULL
  raw <- ._soda_query("5hak-ip4u", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

# == Benefits & Compensation ===================================================

#' Pension recipients by state
#'
#' FY 2024 count of veterans receiving VA Pension benefits by state and sex.
#' Source: view vp34-vuw8.
#'
#' @param limit Max rows (default 1000)
#' @return tibble: state, total_pension_recipients, male, female
#' @export
va_pension <- function(limit = 1000) {
  raw <- ._soda_query("vp34-vuw8", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  df <- as_tibble(raw)
  df |> mutate(
    total_pension_recipients = as.integer(total_pension_recipients),
    male   = as.integer(male %||% NA_character_),
    female = as.integer(female %||% NA_character_)
  )
}

#' Disability compensation recipients by county
#'
#' FY 2024 count of veterans receiving VA Disability Compensation by county,
#' broken down by SCD rating, age group, and sex.
#' Source: view 74ts-jsvb.
#'
#' @param state Optional state name filter
#' @param limit Max rows (default 1000)
#' @return tibble with county-level compensation data
#' @export
va_disability <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) sprintf("state = '%s'", state) else NULL
  raw <- ._soda_query("74ts-jsvb", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

#' Benefits utilization by state
#'
#' Education beneficiaries, unique patients, and other utilization metrics
#' by state and year.
#' Source: view 8x3h-hffb.
#'
#' @param limit Max rows (default 1000)
#' @return tibble with utilization categories and state-year columns
#' @export
va_benefits <- function(limit = 1000) {
  raw <- ._soda_query("8x3h-hffb", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

# == Expenditures ==============================================================

#' VA expenditures by state and county
#'
#' Geographic distribution of VA expenditures including compensation,
#' pensions, medical care, education, and more.
#' Source: GDX FY07 view jbse-k73h (representative year).
#'
#' @param state Optional state abbreviation filter (e.g. "TX")
#' @param limit Max rows (default 1000)
#' @return tibble: state, fips, county, veteran_population, total_expenditure, ...
#' @export
va_expenditures <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) sprintf("state = '%s'", state) else NULL
  raw <- ._soda_query("jbse-k73h", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  df <- as_tibble(raw)
  num_cols <- c("veteran_population", "total_expenditure", "compensation_pensions",
                "medical_care", "education_vocational", "unique_patients")
  for (col in intersect(num_cols, names(df))) {
    df[[col]] <- as.numeric(df[[col]])
  }
  df
}

# == Facilities ================================================================

#' VA facilities by state
#'
#' Count of VA facilities (national/state cemeteries, health care systems,
#' vet centers, etc.) by type and state.
#' Source: view 2weg-acgq.
#'
#' @param limit Max rows (default 100)
#' @return tibble: facility_type, total, and state columns
#' @export
va_facilities <- function(limit = 100) {
  raw <- ._soda_query("2weg-acgq", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

# == Home Loans ================================================================

#' VA home loan guaranty data
#'
#' Home loans guaranteed by VA broken down by state and period of service.
#' Source: VBA LGY 2024 view fp4i-75a5.
#'
#' @param state Optional state name filter
#' @param limit Max rows (default 1000)
#' @return tibble: state_name, period_of_service_or, number_of_loans, total_loan_amount
#' @export
va_home_loans <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) sprintf("state_name = '%s'", state) else NULL
  raw <- ._soda_query("fp4i-75a5", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

# == PTSD Repository ===========================================================

#' PTSD study characteristics
#'
#' Study design information from the PTSD-Repository RCT database.
#' Source: view npcj-egem.
#'
#' @param limit Max rows (default 1000)
#' @return tibble: row_id, study_class, treatment_focus, publication_year, ...
#' @export
va_ptsd_studies <- function(limit = 1000) {
  raw <- ._soda_query("npcj-egem", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  df <- as_tibble(raw)
  if ("publication_year" %in% names(df)) {
    df$publication_year <- as.Date(substr(df$publication_year, 1, 10))
  }
  df
}

#' PTSD study interventions
#'
#' Treatment arm details from the PTSD-Repository.
#' Source: view jckr-i5ky.
#'
#' @param limit Max rows (default 1000)
#' @return tibble with intervention details per study arm
#' @export
va_ptsd_interventions <- function(limit = 1000) {
  raw <- ._soda_query("jckr-i5ky", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

# == Diagnoses =================================================================

#' VHA diagnosis counts by facility
#'
#' ICD-10 diagnosis codes with patient counts across all VHA facilities.
#' Source: Diagnosis FY25 view hz6g-hucz.
#'
#' @param q Full-text search term for diagnosis description
#' @param limit Max rows (default 100)
#' @return tibble: icddisplay, national_all, and facility columns
#' @export
va_diagnoses <- function(q = NULL, limit = 100) {
  raw <- ._soda_query("hz6g-hucz", q = q, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  df <- as_tibble(raw)
  if ("national_all" %in% names(df)) {
    df$national_all <- as.integer(df$national_all)
  }
  df
}

# == Context ===================================================================

#' Generate LLM-friendly context for the VA Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
va_context <- function() {
  .build_context("va.gov", header_lines = c(
    "# va.gov - VA Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Covers: catalog, veterans population, benefits, compensation,",
    "#   expenditures, facilities, home loans, PTSD research, diagnoses.",
    "# SoQL query syntax supported via va_view()."
  ))
}
