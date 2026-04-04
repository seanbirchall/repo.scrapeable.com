#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_retry req_timeout
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_escape
#' @keywords internal
NULL

# healthdata.gov.R
# Self-contained HealthData.gov (HHS Socrata) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA 2.1 at healthdata.gov

# == Private utilities =========================================================

.null_or <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.ua <- "support@scrapeable.com"
.hd_base <- "https://healthdata.gov"

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE, flatten = TRUE)

# -- SODA query engine ---------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL, order = NULL,
                         q = NULL, limit = 1000, offset = 0, group = NULL) {
  base <- paste0(.hd_base, "/resource/", view_id, ".json")
  params <- list()
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q
  if (!is.null(group))  params[["$group"]]  <- group
  params[["$limit"]]  <- as.character(limit)
  params[["$offset"]] <- as.character(offset)
  # Build query string with proper URL encoding
  encoded <- vapply(seq_along(params), function(i) {
    nm <- gsub("\\$", "%24", names(params)[i])
    val <- curl::curl_escape(as.character(params[[i]]))
    paste0(nm, "=", val)
  }, character(1))
  url <- paste0(base, "?", paste(encoded, collapse = "&"))
  .fetch_json(url)
}

._soda_all <- function(view_id, where = NULL, select = NULL, order = NULL,
                       q = NULL, group = NULL, max_rows = 50000) {
  all_rows <- list()
  offset <- 0
  page_size <- 1000
  repeat {
    chunk <- ._soda_query(view_id, where = where, select = select,
                          order = order, q = q, group = group,
                          limit = page_size, offset = offset)
    if (is.null(chunk) || length(chunk) == 0 || nrow(chunk) == 0) break
    all_rows <- c(all_rows, list(chunk))
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < page_size || offset >= max_rows) break
  }
  if (length(all_rows) == 0) return(NULL)
  dplyr::bind_rows(all_rows)
}

.safe_numeric <- function(x) suppressWarnings(as.numeric(x))
.safe_date    <- function(x) as.Date(substr(x, 1, 10))
.safe_chr     <- function(x) as.character(.null_or(x, NA_character_))
.null_to_na   <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else x

# == Dataset catalog ===========================================================

.hd_catalog <- tibble::tibble(
  name = c(
    # --- COVID-19 Hospital ---
    "COVID-19 Hospital Capacity by State",
    "COVID-19 Hospital Capacity by State Timeseries",
    "COVID-19 Hospital Capacity by State (RAW)",
    "COVID-19 Hospital Capacity by State Timeseries (RAW)",
    "COVID-19 Hospital Capacity by Facility",
    "COVID-19 Hospital Capacity by Facility (RAW)",
    "COVID-19 Hospital Data Coverage Summary",
    "COVID-19 Hospital Data Coverage Detail",
    "COVID-19 Hospital Data Coverage Suspense",
    "COVID-19 Hospital Reporting State Certification",
    "Percentage of Hospitals Reporting by State",
    "HHS IDs",
    # --- COVID-19 Estimated ---
    "COVID-19 Estimated Patient Impact by State",
    "COVID-19 Estimated Inpatient Beds by State Timeseries",
    "COVID-19 Estimated Beds Occupied by COVID Patients Timeseries",
    "COVID-19 Estimated ICU Beds Occupied Timeseries",
    # --- COVID-19 Testing ---
    "COVID-19 PCR Testing Time Series",
    # --- COVID-19 Community ---
    "COVID-19 Community Profile National",
    "COVID-19 Community Profile County",
    "COVID-19 State and County Policy Orders",
    # --- COVID-19 Treatments ---
    "ASPR Treatments Locator",
    "COVID-19 Treatments",
    "COVID-19 Public Therapeutic Locator",
    "COVID-19 Test to Treat",
    # --- COVID-19 Surveys ---
    "COVID-19 Monthly Outcome Survey Wave 01",
    "COVID-19 Monthly Outcome Survey Wave 02",
    "COVID-19 Monthly Outcome Survey Wave 03",
    "COVID-19 Monthly Outcome Survey Wave 04",
    "COVID-19 Monthly Outcome Survey Wave 05",
    "COVID-19 Monthly Outcome Survey Wave 06",
    "COVID-19 Monthly Outcome Survey Wave 07",
    "COVID-19 Monthly Outcome Survey Wave 08",
    "COVID-19 Monthly Outcome Survey Wave 09",
    "COVID-19 Monthly Outcome Survey Wave 10",
    "COVID-19 Monthly Outcome Survey Wave 11",
    "COVID-19 Monthly Outcome Survey Wave 12",
    "COVID-19 Monthly Outcome Survey Wave 13",
    "COVID-19 Monthly Outcome Survey Wave 14",
    "COVID-19 Monthly Outcome Survey Wave 15",
    "COVID-19 Monthly Outcome Survey Wave 16",
    "COVID-19 Monthly Outcome Survey Wave 17",
    "COVID-19 Monthly Outcome Survey Wave 18",
    "COVID-19 Monthly Outcome Survey Wave 19",
    "COVID-19 Monthly Outcome Survey Wave 20",
    "COVID-19 Monthly Outcome Survey Wave 21",
    "COVID-19 Monthly Outcome Survey Wave 22",
    "COVID-19 Monthly Outcome Survey Wave 23",
    "COVID-19 Monthly Outcome Survey Wave 24",
    "COVID-19 Monthly Outcome Survey Wave 25",
    "COVID-19 Monthly Outcome Survey Wave 26",
    "COVID-19 Monthly Outcome Survey Wave 27",
    "COVID-19 Monthly Outcome Survey Wave 28",
    # --- COVID-19 SAE Primary ---
    "COVID-19 SAE Primary Vaccine Wave 01",
    "COVID-19 SAE Primary Vaccine Wave 02",
    "COVID-19 SAE Primary Vaccine Wave 04",
    "COVID-19 SAE Primary Vaccine Wave 05",
    "COVID-19 SAE Primary Vaccine Wave 06",
    "COVID-19 SAE Primary Vaccine Wave 07",
    "COVID-19 SAE Primary Vaccine Wave 08",
    "COVID-19 SAE Primary Vaccine Wave 09",
    "COVID-19 SAE Primary Vaccine Wave 10",
    "COVID-19 SAE Primary Vaccine Wave 11",
    "COVID-19 SAE Primary Vaccine Wave 12",
    "COVID-19 SAE Primary Vaccine Wave 13",
    "COVID-19 SAE Primary Vaccine Wave 14",
    "COVID-19 SAE Primary Vaccine Wave 15",
    "COVID-19 SAE Primary Vaccine Wave 16",
    "COVID-19 SAE Primary Vaccine Wave 17",
    "COVID-19 SAE Primary Vaccine Wave 18",
    "COVID-19 SAE Primary Vaccine Wave 19",
    "COVID-19 SAE Primary Vaccine Wave 20",
    "COVID-19 SAE Primary Vaccine Wave 21",
    "COVID-19 SAE Primary Vaccine Wave 22",
    "COVID-19 SAE Primary Vaccine Wave 23",
    "COVID-19 SAE Primary Vaccine Wave 24",
    "COVID-19 SAE Primary Vaccine Wave 25",
    "COVID-19 SAE Primary Vaccine Wave 26",
    "COVID-19 SAE Primary Vaccine Wave 27",
    "COVID-19 SAE Primary Vaccine Wave 28",
    # --- COVID-19 SAE Booster ---
    "COVID-19 SAE Booster Wave 13",
    "COVID-19 SAE Booster Wave 14",
    "COVID-19 SAE Booster Wave 15",
    "COVID-19 SAE Booster Wave 16",
    "COVID-19 SAE Booster Wave 17",
    "COVID-19 SAE Booster Wave 18",
    "COVID-19 SAE Booster Wave 19",
    "COVID-19 SAE Booster Wave 20",
    "COVID-19 SAE Booster Wave 21",
    # --- COVID-19 SAE Bivalent ---
    "COVID-19 SAE Bivalent Wave 22",
    "COVID-19 SAE Bivalent Wave 23",
    "COVID-19 SAE Bivalent Wave 24",
    "COVID-19 SAE Bivalent Wave 25",
    "COVID-19 SAE Bivalent Wave 26",
    "COVID-19 SAE Bivalent Wave 27",
    "COVID-19 SAE Bivalent Wave 28",
    # --- Child Welfare (NCANDS) ---
    "Child Victims by Age",
    "Child Victims Trend",
    "Child Fatalities Trend",
    "Child Fatalities by Submission Type",
    "Children by Disposition",
    "Maltreatment Types of Victims",
    "Perpetrators by Relationship",
    "Perpetrators Trend",
    "Screened-in and Screened-out Referrals",
    "Children Investigation or Alternative Response",
    # --- NCANDS Datasets (older) ---
    "NCANDS Data Tables (2018)",
    "NCANDS Data Tables (FY 2015)",
    # --- HHS Governance & Inventory ---
    "HHS Data Inventory",
    "HHS SORNs Metadata Inventory",
    "HHS Data Governance Board",
    "HHS Expiring Contracts FY23-FY26",
    # --- Other ---
    "Monkeypox Research Summary",
    "ACF-700 Tribal Annual Report",
    "Tribal TANF Caseload Data",
    "HHS Unaccompanied Children",
    "ANA Annual Data Report",
    "Project Tycho Level 1",
    # --- COVID FAQ Templates ---
    "COVID-19 FAQ Template v3",
    "COVID-19 FAQ Template v5",
    "COVID-19 FAQ Template v8",
    "COVID-19 FAQ Template v10",
    "COVID-19 FAQ Template v10 with Mapping"
  ),
  view_id = c(
    # --- COVID-19 Hospital ---
    "9psv-r5iz", "sgxm-t72h", "6xf2-c3ie", "g62h-syeh",
    "anag-cw7u", "uqq2-txqb", "e9gr-58h7", "ieks-f4qs",
    "a6za-z3xi", "gskt-tzcc", "8xss-g7j8", "vz64-k9wr",
    # --- COVID-19 Estimated ---
    "vzfs-79pr", "jjp9-htie", "py8k-j5rq", "7ctx-gtb7",
    # --- COVID-19 Testing ---
    "j8mb-icvb",
    # --- COVID-19 Community ---
    "gzn6-r8g2", "di4u-7yu6", "gyqz-9u7n",
    # --- COVID-19 Treatments ---
    "879u-23sm", "xkzp-zhs7", "rxn6-qnx8", "6m8a-tsjg",
    # --- COVID-19 Surveys (Waves 01-28) ---
    "jkbx-vqsh", "c7xg-28zj", "fjqb-c3h4", "me3u-se5h",
    "ejvn-5bc4", "qhxy-ktqs", "6wrq-qkxb", "m8h6-eez6",
    "6itx-ccwh", "e3v3-avmu", "qbt9-svtx", "b6hq-42rz",
    "t62j-j9ye", "hnmf-jj4n", "6q4t-hwhv", "4ta4-qa59",
    "duxn-gw43", "vihq-44pk", "suf4-7q83", "spb8-ifhv",
    "uiwz-9uh6", "b3zv-yqi4", "ejjy-eyky", "25ew-4v85",
    "r4rm-xbbr", "af8q-tquk", "y54p-5u5c", "s9tn-wwkq",
    # --- COVID-19 SAE Primary (Waves 01-28, some missing) ---
    "38qi-yk2q", "7d7d-924f", "2543-zqv9", "i6f9-253m",
    "ny98-s8s6", "xmx2-7zci", "3rac-z8ut", "8zgg-bkxu",
    "dksh-4m6s", "rj7y-dzzb", "ijyc-ss3b", "jkbs-my6g",
    "tn7j-7qtp", "dqby-4733", "yck7-akru", "xeyx-hae8",
    "iq2u-ngxw", "guqm-pfb6", "tz7w-9cyc", "sx4t-53u5",
    "dcgq-6mwj", "dyi9-ebgk", "6b55-sn2y", "4nyx-g6uz",
    "aem8-fuzu", "5qsf-x4ji", "8hdw-ux9q",
    # --- COVID-19 SAE Booster ---
    "9teb-xsi3", "pwn2-w2ja", "67v7-x8h5", "hv8g-g9hp",
    "nd88-wf45", "n52n-yjbb", "3a65-pvv6", "hcvq-7wix",
    "g2uv-9dmm",
    # --- COVID-19 SAE Bivalent ---
    "efdp-e2cw", "r9n3-7g5s", "vrdd-4mpp", "pse5-ehfc",
    "bnqf-5en8", "63vp-8m6s", "6dii-j4w3",
    # --- Child Welfare ---
    "xn3e-yyaj", "qwij-f3kq", "u7xm-yva2", "9c49-3jtk",
    "usvm-fdmd", "8bce-qw8w", "tw7x-jbvq", "ttus-3dym",
    "k5kg-jgj9", "7viv-bzwe",
    # --- NCANDS older ---
    "ej22-ej5b", "ej22-ej5b",
    # --- HHS Governance & Inventory ---
    "kaw8-4tez", "gs9i-45zc", "w3i2-jayx", "dqp4-e6rh",
    # --- Other ---
    "x7kq-cyv4", "i9p8-46qp", "52gy-ej7q", "ehpz-xc9n",
    "5pmf-mq4r", "g89t-x93h",
    # --- COVID FAQ Templates ---
    "vd2z-c79h", "d7eh-bu6r", "yqg4-xrmp", "p6c2-876z",
    "ssts-5wfp"
  ),
  category = c(
    rep("COVID-19 Hospital", 12),
    rep("COVID-19 Estimated", 4),
    "COVID-19 Testing",
    rep("COVID-19 Community", 3),
    rep("COVID-19 Treatments", 4),
    rep("COVID-19 Survey", 28),
    rep("COVID-19 SAE Primary", 27),
    rep("COVID-19 SAE Booster", 9),
    rep("COVID-19 SAE Bivalent", 7),
    rep("Child Welfare", 10),
    rep("NCANDS", 2),
    rep("HHS Governance", 4),
    rep("Other", 6),
    rep("COVID-19 Templates", 5)
  )
)

# == Discovery layer ===========================================================

#' List all HealthData.gov datasets
#'
#' Returns a hardcoded catalog of 127 Socrata views available on
#' healthdata.gov with their view IDs and categories.
#'
#' @param category Optional filter by category (partial match, case-insensitive)
#' @return tibble: name, view_id, category
#' @export
hd_list <- function(category = NULL) {
  out <- .hd_catalog
  if (!is.null(category)) {
    out <- out[grepl(category, out$category, ignore.case = TRUE), ]
  }
  out
}

#' Search HealthData.gov datasets
#'
#' Searches the catalog by name (case-insensitive partial match).
#'
#' @param query Search string
#' @return tibble: name, view_id, category
#' @export
hd_search <- function(query) {
  idx <- grepl(query, .hd_catalog$name, ignore.case = TRUE)
  .hd_catalog[idx, ]
}

#' List available dataset categories
#'
#' @return character vector of unique categories
#' @export
hd_categories <- function() {
  sort(unique(.hd_catalog$category))
}

# == Generic SODA access =======================================================

#' Query any HealthData.gov SODA dataset
#'
#' Generic escape hatch for any dataset by view_id. Supports full
#' SoQL filtering.
#'
#' @param view_id Socrata 4-4 view identifier (e.g. "9psv-r5iz")
#' @param where SoQL WHERE clause (e.g. "state='NY'")
#' @param select SoQL SELECT clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param group SoQL GROUP BY clause
#' @param limit Max rows per page (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of results
#' @export
hd_view <- function(view_id, where = NULL, select = NULL, order = NULL,
                    q = NULL, group = NULL, limit = 1000, offset = 0) {
  raw <- ._soda_query(view_id, where = where, select = select, order = order,
                      q = q, group = group, limit = limit, offset = offset)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch all rows from a SODA dataset (with pagination)
#'
#' @param view_id Socrata 4-4 view identifier
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param order Optional SoQL ORDER BY clause
#' @param q Optional full-text search
#' @param group Optional SoQL GROUP BY clause
#' @param max_rows Safety cap (default 50000)
#' @return tibble of all rows
#' @export
hd_view_all <- function(view_id, where = NULL, select = NULL, order = NULL,
                        q = NULL, group = NULL, max_rows = 50000) {
  raw <- ._soda_all(view_id, where = where, select = select, order = order,
                    q = q, group = group, max_rows = max_rows)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == COVID-19 Hospital Capacity ================================================

#' COVID-19 hospital capacity by state (latest snapshot)
#'
#' Data on reported patient impact and hospital capacity aggregated by state.
#' Updated daily through May 2024.
#'
#' @param state Two-letter state code filter (e.g. "NY", "CA"). NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble with state, inpatient_beds, covid patients, ICU data
#' @export
hd_covid_hospital_state <- function(state = NULL, limit = 1000) {
  where <- NULL
  if (!is.null(state)) where <- paste0("state='", toupper(state), "'")
  raw <- ._soda_query("9psv-r5iz", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  num_cols <- c("inpatient_beds", "inpatient_beds_used",
                "total_adult_patients_hospitalized_confirmed_covid",
                "total_pediatric_patients_hospitalized_confirmed_covid",
                "staffed_adult_icu_bed_occupancy",
                "total_staffed_adult_icu_beds",
                "staffed_icu_adult_patients_confirmed_covid",
                "previous_day_admission_adult_covid_confirmed")
  for (col in intersect(num_cols, names(out))) {
    out[[col]] <- .safe_numeric(out[[col]])
  }
  pct_cols <- c("inpatient_beds_utilization", "adult_icu_bed_utilization")
  for (col in intersect(pct_cols, names(out))) {
    out[[col]] <- .safe_numeric(out[[col]])
  }
  if ("date" %in% names(out)) out$date <- .safe_date(out$date)
  out
}

#' COVID-19 hospital capacity timeseries by state
#'
#' Full time series of hospital capacity and patient impact by state.
#'
#' @param state Two-letter state code filter. NULL for all states.
#' @param start_date Start date (YYYY-MM-DD string). NULL for all.
#' @param end_date End date (YYYY-MM-DD string). NULL for all.
#' @param limit Max rows (default 5000)
#' @return tibble with date, state, capacity metrics
#' @export
hd_covid_hospital_timeseries <- function(state = NULL, start_date = NULL,
                                         end_date = NULL, limit = 5000) {
  clauses <- character()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", toupper(state), "'"))
  if (!is.null(start_date)) clauses <- c(clauses, paste0("date>='", start_date, "'"))
  if (!is.null(end_date)) clauses <- c(clauses, paste0("date<='", end_date, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("sgxm-t72h", where = where, order = "date DESC", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  if ("date" %in% names(out)) out$date <- .safe_date(out$date)
  num_cols <- c("inpatient_beds", "inpatient_beds_used",
                "total_adult_patients_hospitalized_confirmed_covid",
                "staffed_adult_icu_bed_occupancy", "total_staffed_adult_icu_beds",
                "previous_day_admission_adult_covid_confirmed")
  for (col in intersect(num_cols, names(out))) {
    out[[col]] <- .safe_numeric(out[[col]])
  }
  out
}

#' COVID-19 hospital capacity by facility
#'
#' Facility-level data on hospital capacity and COVID patient impact.
#' Includes hospital name, address, and weekly aggregated metrics.
#'
#' @param state Two-letter state code filter. NULL for all.
#' @param city City name filter (case-sensitive). NULL for all.
#' @param hospital_name Partial hospital name match. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble with facility details and capacity metrics
#' @export
hd_covid_hospital_facility <- function(state = NULL, city = NULL,
                                       hospital_name = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", toupper(state), "'"))
  if (!is.null(city)) clauses <- c(clauses, paste0("city='", city, "'"))
  if (!is.null(hospital_name)) clauses <- c(clauses, paste0("upper(hospital_name) like upper('%", hospital_name, "%')"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("anag-cw7u", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  if ("collection_week" %in% names(out)) out$collection_week <- .safe_date(out$collection_week)
  num_cols <- c("total_beds_7_day_avg", "inpatient_beds_used_covid_7_day_avg",
                "total_icu_beds_7_day_avg", "icu_beds_used_7_day_avg")
  for (col in intersect(num_cols, names(out))) {
    out[[col]] <- .safe_numeric(out[[col]])
  }
  out
}

# == COVID-19 Testing ==========================================================

#' COVID-19 PCR testing time series
#'
#' State-level diagnostic laboratory testing (PCR) results over time.
#'
#' @param state Two-letter state code filter. NULL for all.
#' @param start_date Start date (YYYY-MM-DD). NULL for all.
#' @param end_date End date (YYYY-MM-DD). NULL for all.
#' @param limit Max rows (default 5000)
#' @return tibble: state, state_name, date, overall_outcome, new_results_reported, total_results_reported
#' @export
hd_covid_testing <- function(state = NULL, start_date = NULL,
                             end_date = NULL, limit = 5000) {
  clauses <- character()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", toupper(state), "'"))
  if (!is.null(start_date)) clauses <- c(clauses, paste0("date>='", start_date, "'"))
  if (!is.null(end_date)) clauses <- c(clauses, paste0("date<='", end_date, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("j8mb-icvb", where = where, order = "date DESC", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  if ("date" %in% names(out)) out$date <- .safe_date(out$date)
  if ("new_results_reported" %in% names(out)) out$new_results_reported <- .safe_numeric(out$new_results_reported)
  if ("total_results_reported" %in% names(out)) out$total_results_reported <- .safe_numeric(out$total_results_reported)
  out
}

# == COVID-19 Treatments =======================================================

#' ASPR treatments locator
#'
#' Locations with COVID-19 and influenza antiviral medications.
#' Current and actively updated.
#'
#' @param state Two-letter state code filter. NULL for all.
#' @param zip ZIP code filter. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble: provider_name, address1, city, state, zip, latitude, longitude, medication flags
#' @export
hd_treatments_locator <- function(state = NULL, zip = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", toupper(state), "'"))
  if (!is.null(zip)) clauses <- c(clauses, paste0("zip='", zip, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("879u-23sm", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  if ("latitude" %in% names(out)) out$latitude <- .safe_numeric(out$latitude)
  if ("longitude" %in% names(out)) out$longitude <- .safe_numeric(out$longitude)
  out
}

# == COVID-19 Policy Orders ====================================================

#' COVID-19 state and county policy orders
#'
#' Manually curated dataset of executive orders, ordinances, and other
#' COVID-19 policy actions at state and county level.
#'
#' @param state Two-letter state code filter (e.g. "NY"). NULL for all.
#' @param policy_type Policy type filter (e.g. "Shelter in Place"). NULL for all.
#' @param limit Max rows (default 5000)
#' @return tibble: state_id, county, policy_level, date, policy_type, start_stop, comments
#' @export
hd_covid_policy <- function(state = NULL, policy_type = NULL, limit = 5000) {
  clauses <- character()
  if (!is.null(state)) clauses <- c(clauses, paste0("state_id='", toupper(state), "'"))
  if (!is.null(policy_type)) clauses <- c(clauses, paste0("policy_type='", policy_type, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("gyqz-9u7n", where = where, order = "date DESC", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  if ("date" %in% names(out)) out$date <- .safe_date(out$date)
  out
}

# == COVID-19 Community Profile ================================================

#' COVID-19 community profile (national level)
#'
#' National-level community profile report data (through May 2023).
#'
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_covid_community_national <- function(limit = 1000) {
  raw <- ._soda_query("gzn6-r8g2", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' COVID-19 community profile (county level)
#'
#' County-level community profile report data (through May 2023).
#'
#' @param state Two-letter state code. NULL for all.
#' @param county County name. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_covid_community_county <- function(state = NULL, county = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", toupper(state), "'"))
  if (!is.null(county)) clauses <- c(clauses, paste0("county='", county, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("di4u-7yu6", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Child Welfare (NCANDS) ====================================================

#' Child victims by age
#'
#' Numbers and rates of child maltreatment victims by single-year age
#' and by state from NCANDS.
#'
#' @param state State name filter (e.g. "Alabama"). NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble: state, age counts and rates
#' @export
hd_child_victims_age <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("xn3e-yyaj", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Child victims trend
#'
#' Numbers and rates of child victims over the last five federal fiscal years.
#'
#' @param state State name filter. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_child_victims_trend <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("qwij-f3kq", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Child fatalities trend
#'
#' Number of child fatalities over five federal fiscal years by state.
#'
#' @param state State name filter. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_child_fatalities <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("u7xm-yva2", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Children by disposition
#'
#' Children counted by investigation/response disposition by state.
#'
#' @param state State name filter. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_children_disposition <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("usvm-fdmd", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Maltreatment types of victims
#'
#' Number of maltreatment types by state from NCANDS.
#'
#' @param state State name filter. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_maltreatment_types <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("8bce-qw8w", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Screened-in and screened-out referrals
#'
#' Numbers, percentages, and rates of referrals to CPS by state.
#'
#' @param state State name filter. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_cps_referrals <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("k5kg-jgj9", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == HHS Governance ============================================================

#' HHS Data Inventory
#'
#' Comprehensive metadata catalog of public and non-public HHS data assets.
#'
#' @param organization Organization filter (partial match). NULL for all.
#' @param access_level Filter: "Public", "Non-Public", "Restricted Public". NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble: title, organization, access_level, asset_type, etc.
#' @export
hd_data_inventory <- function(organization = NULL, access_level = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(organization)) clauses <- c(clauses, paste0("upper(organization) like upper('%", organization, "%')"))
  if (!is.null(access_level)) clauses <- c(clauses, paste0("access_level='", access_level, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("kaw8-4tez", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(raw)
  if ("modified_date_at_source" %in% names(out)) out$modified_date_at_source <- .safe_date(out$modified_date_at_source)
  if ("created_date" %in% names(out)) out$created_date <- .safe_date(out$created_date)
  out
}

#' HHS System of Records Notices (SORNs)
#'
#' Machine-readable metadata for HHS SORNs (Privacy Act notices).
#'
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_sorns <- function(limit = 1000) {
  raw <- ._soda_query("gs9i-45zc", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Monkeypox =================================================================

#' Monkeypox research summary
#'
#' US government monkeypox research activities and milestones.
#'
#' @param limit Max rows (default 1000)
#' @return tibble: research_activity, project_title, topic, agency, status, etc.
#' @export
hd_monkeypox_research <- function(limit = 1000) {
  raw <- ._soda_query("x7kq-cyv4", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Other datasets ============================================================

#' HHS Unaccompanied Children Program
#'
#' Data on unaccompanied children in HHS custody.
#'
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_unaccompanied_children <- function(limit = 1000) {
  raw <- ._soda_query("ehpz-xc9n", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Tribal TANF Caseload Data
#'
#' TANF recipient counts by tribe and month.
#'
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_tribal_tanf <- function(limit = 1000) {
  raw <- ._soda_query("52gy-ej7q", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Project Tycho Level 1 - Notifiable Disease Data
#'
#' US weekly nationally notifiable disease surveillance data (1888-2013).
#'
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_tycho <- function(limit = 1000) {
  raw <- ._soda_query("g89t-x93h", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' HHS Expiring Contracts (FY23-FY26)
#'
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_expiring_contracts <- function(limit = 1000) {
  raw <- ._soda_query("dqp4-e6rh", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == COVID-19 Vaccine Survey ===================================================

#' COVID-19 Monthly Outcome Survey by wave
#'
#' Vaccine uptake, beliefs, and behaviors data from the MOS.
#' Waves 01-28 available (Jan 2021 - Apr 2023).
#'
#' @param wave Wave number (1-28). Default 28 (latest).
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_covid_survey <- function(wave = 28, limit = 1000) {
  wave_ids <- c(
    `1`  = "jkbx-vqsh", `2`  = "c7xg-28zj", `3`  = "fjqb-c3h4",
    `4`  = "me3u-se5h", `5`  = "ejvn-5bc4", `6`  = "qhxy-ktqs",
    `7`  = "6wrq-qkxb", `8`  = "m8h6-eez6", `9`  = "6itx-ccwh",
    `10` = "e3v3-avmu", `11` = "qbt9-svtx", `12` = "b6hq-42rz",
    `13` = "t62j-j9ye", `14` = "hnmf-jj4n", `15` = "6q4t-hwhv",
    `16` = "4ta4-qa59", `17` = "duxn-gw43", `18` = "vihq-44pk",
    `19` = "suf4-7q83", `20` = "spb8-ifhv", `21` = "uiwz-9uh6",
    `22` = "b3zv-yqi4", `23` = "ejjy-eyky", `24` = "25ew-4v85",
    `25` = "r4rm-xbbr", `26` = "af8q-tquk", `27` = "y54p-5u5c",
    `28` = "s9tn-wwkq"
  )
  wave_str <- as.character(wave)
  if (!(wave_str %in% names(wave_ids))) {
    stop("Wave must be 1-28. Available: ", paste(names(wave_ids), collapse = ", "))
  }
  raw <- ._soda_query(wave_ids[[wave_str]], limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Hospital Reporting Status =================================================

#' Percentage of hospitals reporting data by state
#'
#' Weekly reporting compliance data.
#'
#' @param state Two-letter state code. NULL for all.
#' @param limit Max rows (default 1000)
#' @return tibble
#' @export
hd_hospital_reporting <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", toupper(state), "'") else NULL
  raw <- ._soda_query("8xss-g7j8", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Context ===================================================================

#' Read hd_ function source code
#'
#' Returns the full source code of this client file, suitable for
#' including in an LLM context window.
#'
#' @return character string of the entire source file
#' @export
hd_context <- function() {
  src <- NULL
  # Try to find the source file
  candidates <- c(
    # When sourced directly
    tryCatch(sys.frame(1)$ofile, error = function(e) NULL),
    # Package install
    tryCatch(system.file("source", "healthdata.R", package = "healthdata.gov"), error = function(e) ""),
    # Common dev locations
    "clients/healthdata.gov.R",
    file.path(Sys.getenv("HOME"), "repo.scrapeable.com", "clients", "healthdata.gov.R")
  )
  for (f in candidates) {
    if (!is.null(f) && nzchar(f) && file.exists(f)) {
      src <- readLines(f)
      break
    }
  }
  if (is.null(src)) {
    # Fallback: deparse all hd_ functions from current env
    fns <- ls(pattern = "^hd_", envir = parent.env(environment()))
    if (length(fns) == 0) fns <- ls(pattern = "^hd_", envir = globalenv())
    bodies <- vapply(fns, function(fn) {
      paste(deparse(get(fn, envir = globalenv())), collapse = "\n")
    }, character(1))
    src <- paste(paste0("# ", fns, "\n", bodies), collapse = "\n\n")
  }
  paste(src, collapse = "\n")
}
