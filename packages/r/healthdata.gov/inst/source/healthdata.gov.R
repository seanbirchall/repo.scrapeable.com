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
#' Returns a catalog of 127 Socrata datasets available on healthdata.gov
#' covering COVID-19 hospital capacity, testing, treatments, community
#' profiles, vaccine surveys, child welfare (NCANDS), HHS governance,
#' and other HHS data assets. Use view_id values with \code{hd_view()}.
#'
#' @param category Optional filter by category (partial match, case-insensitive).
#'   Major categories: "COVID-19 Hospital", "COVID-19 Testing",
#'   "COVID-19 Community", "COVID-19 Treatments", "COVID-19 Survey",
#'   "Child Welfare", "HHS Governance", "Other". NULL for all.
#' @return A tibble with columns:
#'   - name (character): Dataset title
#'   - view_id (character): Socrata 4x4 dataset identifier
#'   - category (character): Topic area
#' @examples
#' \dontrun{
#' # Browse all datasets
#' hd_list()
#'
#' # Filter to child welfare datasets
#' hd_list(category = "Child Welfare")
#' }
#' @export
hd_list <- function(category = NULL) {
  out <- .hd_catalog
  if (!is.null(category)) {
    out <- out[grepl(category, out$category, ignore.case = TRUE), ]
  }
  out
}

#' Search HealthData.gov datasets by keyword
#'
#' Searches the dataset catalog by name using case-insensitive partial
#' matching. Use this to find specific datasets by topic.
#'
#' @param query Search string (case-insensitive). Examples: "hospital",
#'   "testing", "child", "monkeypox", "TANF", "inventory".
#' @return A tibble with columns: name, view_id, category. Empty if no matches.
#' @examples
#' \dontrun{
#' # Find hospital-related datasets
#' hd_search("hospital")
#'
#' # Find child welfare datasets
#' hd_search("child")
#' }
#' @export
hd_search <- function(query) {
  idx <- grepl(query, .hd_catalog$name, ignore.case = TRUE)
  .hd_catalog[idx, ]
}

#' List available HealthData.gov dataset categories
#'
#' Returns a sorted character vector of all unique category names in the
#' catalog. Useful for discovering what topic areas are available before
#' filtering with \code{hd_list(category = ...)}.
#'
#' @return A character vector of unique category names, sorted alphabetically.
#' @examples
#' \dontrun{
#' hd_categories()
#' }
#' @export
hd_categories <- function() {
  sort(unique(.hd_catalog$category))
}

# == Generic SODA access =======================================================

#' Query any HealthData.gov SODA dataset by view ID
#'
#' Generic escape hatch for querying any healthdata.gov dataset by its
#' Socrata view ID. Supports full SoQL including WHERE, SELECT, ORDER BY,
#' GROUP BY, and full-text search. Use \code{hd_list()} to find view IDs.
#'
#' @param view_id Socrata 4x4 view identifier (e.g. "9psv-r5iz" for
#'   COVID hospital by state). Use \code{hd_list()} to browse.
#' @param where SoQL WHERE clause (e.g. "state='NY'", "date>'2023-01-01'").
#'   NULL for no filter.
#' @param select SoQL SELECT clause for column projection. NULL for all.
#' @param order SoQL ORDER BY clause (e.g. "date DESC"). NULL for default.
#' @param q Full-text search query. NULL for no search.
#' @param group SoQL GROUP BY clause for aggregation. NULL for no grouping.
#' @param limit Max rows per page (default 1000).
#' @param offset Pagination offset (default 0).
#' @return A tibble of results. Columns depend on the dataset.
#' @examples
#' \dontrun{
#' # Query COVID hospital data for New York
#' hd_view("9psv-r5iz", where = "state='NY'", limit = 50)
#' }
#' @export
hd_view <- function(view_id, where = NULL, select = NULL, order = NULL,
                    q = NULL, group = NULL, limit = 1000, offset = 0) {
  raw <- ._soda_query(view_id, where = where, select = select, order = order,
                      q = q, group = group, limit = limit, offset = offset)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch all rows from a HealthData.gov dataset (paginated)
#'
#' Automatically paginates through all rows of a SODA dataset, up to
#' \code{max_rows}. Use this when you need the complete dataset rather
#' than a single page. Warning: large datasets may take several minutes.
#'
#' @param view_id Socrata 4x4 view identifier.
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param select Optional SoQL SELECT clause. NULL for all columns.
#' @param order Optional SoQL ORDER BY clause. NULL for default.
#' @param q Optional full-text search. NULL for no search.
#' @param group Optional SoQL GROUP BY clause. NULL for no grouping.
#' @param max_rows Safety cap on total rows (default 50000).
#' @return A tibble of all rows up to max_rows.
#' @examples
#' \dontrun{
#' # Fetch all COVID hospital state data
#' all_data <- hd_view_all("9psv-r5iz")
#' }
#' @export
hd_view_all <- function(view_id, where = NULL, select = NULL, order = NULL,
                        q = NULL, group = NULL, max_rows = 50000) {
  raw <- ._soda_all(view_id, where = where, select = select, order = order,
                    q = q, group = group, max_rows = max_rows)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == COVID-19 Hospital Capacity ================================================

#' Fetch COVID-19 hospital capacity by state (latest snapshot)
#'
#' Returns the latest reported patient impact and hospital capacity data
#' aggregated by state. Includes inpatient beds, ICU beds, COVID patient
#' counts, and utilization rates. Updated daily through May 2024.
#' Numeric columns are automatically parsed from strings.
#'
#' @param state Two-letter state code filter (e.g. "NY", "CA", "TX").
#'   NULL for all states.
#' @param limit Max rows (default 1000).
#' @return A tibble with columns including:
#'   - state (character): Two-letter state code
#'   - date (Date): Report date
#'   - inpatient_beds (numeric): Total inpatient beds
#'   - inpatient_beds_used (numeric): Inpatient beds in use
#'   - inpatient_beds_utilization (numeric): Utilization rate (0-1)
#'   - total_adult_patients_hospitalized_confirmed_covid (numeric)
#'   - total_staffed_adult_icu_beds (numeric)
#'   - staffed_icu_adult_patients_confirmed_covid (numeric)
#' @examples
#' \dontrun{
#' # Get latest hospital data for New York
#' hd_covid_hospital_state(state = "NY")
#'
#' # Get all states
#' hd_covid_hospital_state(limit = 60)
#' }
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

#' Fetch COVID-19 hospital capacity time series by state
#'
#' Returns the full time series of hospital capacity and patient impact
#' by state. Supports date range filtering. Ordered by date descending.
#' Use for longitudinal analysis of hospital capacity trends during
#' the pandemic.
#'
#' @param state Two-letter state code filter (e.g. "NY"). NULL for all states.
#' @param start_date Start date as "YYYY-MM-DD" string (e.g. "2021-01-01").
#'   NULL for no start bound.
#' @param end_date End date as "YYYY-MM-DD" string. NULL for no end bound.
#' @param limit Max rows (default 5000). Set higher for multi-state time series.
#' @return A tibble with columns including: date (Date), state, inpatient_beds,
#'   inpatient_beds_used, covid confirmed patients, ICU data (numeric).
#' @examples
#' \dontrun{
#' # Get NY hospital capacity for 2022
#' hd_covid_hospital_timeseries(state = "NY",
#'   start_date = "2022-01-01", end_date = "2022-12-31")
#' }
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

#' Fetch COVID-19 hospital capacity by individual facility
#'
#' Returns facility-level data on hospital capacity and COVID patient
#' impact. Includes hospital name, address, geocoded location, and
#' weekly aggregated metrics (7-day averages). Useful for comparing
#' hospitals within a state or city.
#'
#' @param state Two-letter state code filter (e.g. "NY"). NULL for all.
#' @param city City name filter (case-sensitive, e.g. "New York"). NULL for all.
#' @param hospital_name Partial hospital name match (case-insensitive,
#'   e.g. "Mount Sinai"). NULL for all.
#' @param limit Max rows (default 1000).
#' @return A tibble with columns including: hospital_name, address, city,
#'   state, zip, collection_week (Date), total_beds_7_day_avg (numeric),
#'   inpatient_beds_used_covid_7_day_avg (numeric), total_icu_beds_7_day_avg,
#'   and geocoded location data.
#' @examples
#' \dontrun{
#' # Get New York City hospitals
#' hd_covid_hospital_facility(state = "NY", city = "New York", limit = 50)
#'
#' # Search for a specific hospital
#' hd_covid_hospital_facility(hospital_name = "Mount Sinai")
#' }
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

#' Fetch COVID-19 PCR testing time series by state
#'
#' Returns state-level diagnostic laboratory testing (PCR) results over
#' time, including positive and negative outcomes. Supports date range
#' filtering. Ordered by date descending.
#'
#' @param state Two-letter state code filter (e.g. "CA"). NULL for all states.
#' @param start_date Start date as "YYYY-MM-DD". NULL for no start bound.
#' @param end_date End date as "YYYY-MM-DD". NULL for no end bound.
#' @param limit Max rows (default 5000).
#' @return A tibble with columns:
#'   - state (character): Two-letter state code
#'   - state_name (character): Full state name
#'   - date (Date): Test result date
#'   - overall_outcome (character): "Positive" or "Negative"
#'   - new_results_reported (numeric): New test results for this date
#'   - total_results_reported (numeric): Cumulative total
#' @examples
#' \dontrun{
#' # Get California testing data for 2022
#' hd_covid_testing(state = "CA",
#'   start_date = "2022-01-01", end_date = "2022-12-31")
#' }
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

#' Fetch ASPR COVID-19/flu treatment locator data
#'
#' Returns locations with COVID-19 and influenza antiviral medications
#' available (e.g. Paxlovid, Tamiflu). Includes geocoded pharmacy and
#' provider locations. Actively updated by ASPR.
#'
#' @param state Two-letter state code filter (e.g. "NY"). NULL for all states.
#' @param zip ZIP code filter (e.g. "10001"). NULL for all.
#' @param limit Max rows (default 1000).
#' @return A tibble with columns including: provider_name, address1, city,
#'   state, zip, latitude (numeric), longitude (numeric), and medication
#'   availability flags.
#' @examples
#' \dontrun{
#' # Find treatment locations in New York
#' hd_treatments_locator(state = "NY", limit = 100)
#'
#' # Find by ZIP code
#' hd_treatments_locator(zip = "10001")
#' }
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

#' Fetch COVID-19 state and county policy orders
#'
#' Returns a manually curated dataset of executive orders, ordinances,
#' and other COVID-19 policy actions at state and county level. Covers
#' mask mandates, shelter-in-place orders, school closures, business
#' restrictions, and more. Ordered by date descending.
#'
#' @param state Two-letter state code filter (e.g. "NY"). NULL for all.
#' @param policy_type Policy type filter (exact match). Examples:
#'   "Shelter in Place", "Mask Requirement", "School Closure",
#'   "Bar/Restaurant Limits". NULL for all types.
#' @param limit Max rows (default 5000).
#' @return A tibble with columns including: state_id, county, policy_level,
#'   date (Date), policy_type, start_stop, comments, and source URLs.
#' @examples
#' \dontrun{
#' # Get New York COVID policy orders
#' hd_covid_policy(state = "NY", limit = 100)
#'
#' # Get all shelter-in-place orders
#' hd_covid_policy(policy_type = "Shelter in Place")
#' }
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

#' Fetch COVID-19 community profile (national level)
#'
#' Returns national-level community profile report data including case
#' rates, testing rates, hospital admissions, and vaccination metrics.
#' Data through May 2023.
#'
#' @param limit Max rows (default 1000).
#' @return A tibble with national-level COVID community indicators.
#' @examples
#' \dontrun{
#' hd_covid_community_national(limit = 100)
#' }
#' @export
hd_covid_community_national <- function(limit = 1000) {
  raw <- ._soda_query("gzn6-r8g2", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch COVID-19 community profile (county level)
#'
#' Returns county-level community profile report data including case
#' rates, test positivity, hospital capacity, and community transmission
#' level. Data through May 2023.
#'
#' @param state Two-letter state code (e.g. "NY"). NULL for all states.
#' @param county County name (case-sensitive, e.g. "New York"). NULL for all.
#' @param limit Max rows (default 1000).
#' @return A tibble with county-level COVID community indicators.
#' @examples
#' \dontrun{
#' # Get Los Angeles County data
#' hd_covid_community_county(state = "CA", county = "Los Angeles")
#' }
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

#' Fetch child maltreatment victims by age (NCANDS)
#'
#' Returns numbers and rates of child maltreatment victims by single-year
#' age group and by state from the National Child Abuse and Neglect Data
#' System (NCANDS). Useful for analyzing age-specific victimization patterns.
#'
#' @param state State name filter (full name, e.g. "Alabama", "California").
#'   NULL for all states.
#' @param limit Max rows (default 1000).
#' @return A tibble with columns including: state, and victim counts and
#'   rates by individual age (0-17).
#' @examples
#' \dontrun{
#' # Get all states
#' hd_child_victims_age(limit = 60)
#'
#' # Get California data
#' hd_child_victims_age(state = "California")
#' }
#' @export
hd_child_victims_age <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("xn3e-yyaj", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch child maltreatment victim trends (NCANDS)
#'
#' Returns numbers and rates of child victims over the last five federal
#' fiscal years by state from NCANDS. Use for tracking changes in child
#' maltreatment over time.
#'
#' @param state State name filter (full name, e.g. "Texas"). NULL for all.
#' @param limit Max rows (default 1000).
#' @return A tibble with state-level victim counts and rates across
#'   multiple fiscal years.
#' @examples
#' \dontrun{
#' hd_child_victims_trend(limit = 60)
#' }
#' @export
hd_child_victims_trend <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("qwij-f3kq", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch child fatality trends (NCANDS)
#'
#' Returns the number of child fatalities due to abuse or neglect over
#' five federal fiscal years by state from NCANDS.
#'
#' @param state State name filter (full name, e.g. "Florida"). NULL for all.
#' @param limit Max rows (default 1000).
#' @return A tibble with state-level child fatality counts across fiscal years.
#' @examples
#' \dontrun{
#' hd_child_fatalities(limit = 60)
#' }
#' @export
hd_child_fatalities <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("u7xm-yva2", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch children by investigation/response disposition (NCANDS)
#'
#' Returns counts of children by CPS investigation or alternative response
#' disposition (substantiated, unsubstantiated, indicated, etc.) by state.
#'
#' @param state State name filter (full name). NULL for all states.
#' @param limit Max rows (default 1000).
#' @return A tibble with state-level disposition counts.
#' @examples
#' \dontrun{
#' hd_children_disposition(limit = 60)
#' }
#' @export
hd_children_disposition <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("usvm-fdmd", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch maltreatment types of child victims (NCANDS)
#'
#' Returns the number of child victims by maltreatment type (neglect,
#' physical abuse, sexual abuse, psychological maltreatment, etc.)
#' by state from NCANDS.
#'
#' @param state State name filter (full name). NULL for all states.
#' @param limit Max rows (default 1000).
#' @return A tibble with maltreatment type counts by state.
#' @examples
#' \dontrun{
#' hd_maltreatment_types(limit = 60)
#' }
#' @export
hd_maltreatment_types <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("8bce-qw8w", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch CPS screened-in and screened-out referrals (NCANDS)
#'
#' Returns numbers, percentages, and rates of referrals to Child
#' Protective Services (CPS) that were screened in (accepted for
#' investigation) vs screened out, by state from NCANDS.
#'
#' @param state State name filter (full name). NULL for all states.
#' @param limit Max rows (default 1000).
#' @return A tibble with referral counts, percentages, and rates by state.
#' @examples
#' \dontrun{
#' hd_cps_referrals(limit = 60)
#' }
#' @export
hd_cps_referrals <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  raw <- ._soda_query("k5kg-jgj9", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == HHS Governance ============================================================

#' Fetch HHS Data Inventory (metadata catalog)
#'
#' Returns the comprehensive metadata catalog of public and non-public
#' HHS data assets including dataset titles, descriptions, access levels,
#' responsible organizations, and update frequencies. Useful for
#' discovering HHS data assets beyond what is on healthdata.gov.
#'
#' @param organization Organization filter (partial match, case-insensitive).
#'   Examples: "CDC", "FDA", "NIH", "CMS", "SAMHSA". NULL for all.
#' @param access_level Filter by access level: "Public", "Non-Public", or
#'   "Restricted Public". NULL for all levels.
#' @param limit Max rows (default 1000).
#' @return A tibble with columns including: title, organization, access_level,
#'   asset_type, modified_date_at_source (Date), created_date (Date),
#'   description, and download URLs.
#' @examples
#' \dontrun{
#' # Get CDC public datasets
#' hd_data_inventory(organization = "CDC", access_level = "Public")
#'
#' # Browse all HHS data assets
#' hd_data_inventory(limit = 500)
#' }
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

#' Fetch HHS System of Records Notices (SORNs)
#'
#' Returns machine-readable metadata for HHS System of Records Notices
#' published under the Privacy Act. Includes system name, responsible
#' organization, categories of individuals and records, and authority.
#'
#' @param limit Max rows (default 1000).
#' @return A tibble with SORN metadata including system name, organization,
#'   categories, and authority citations.
#' @examples
#' \dontrun{
#' hd_sorns(limit = 100)
#' }
#' @export
hd_sorns <- function(limit = 1000) {
  raw <- ._soda_query("gs9i-45zc", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Monkeypox =================================================================

#' Fetch monkeypox (mpox) research summary
#'
#' Returns US government monkeypox/mpox research activities and milestones
#' including project descriptions, responsible agencies, topics, and
#' current status.
#'
#' @param limit Max rows (default 1000).
#' @return A tibble with columns including: research_activity, project_title,
#'   topic, agency, status, and milestone descriptions.
#' @examples
#' \dontrun{
#' hd_monkeypox_research(limit = 50)
#' }
#' @export
hd_monkeypox_research <- function(limit = 1000) {
  raw <- ._soda_query("x7kq-cyv4", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Other datasets ============================================================

#' Fetch HHS Unaccompanied Children Program data
#'
#' Returns data on unaccompanied children (UC) in HHS/ORR custody
#' including referral counts, average length of care, and bed capacity.
#'
#' @param limit Max rows (default 1000).
#' @return A tibble with UC program metrics.
#' @examples
#' \dontrun{
#' hd_unaccompanied_children(limit = 100)
#' }
#' @export
hd_unaccompanied_children <- function(limit = 1000) {
  raw <- ._soda_query("ehpz-xc9n", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch Tribal TANF caseload data
#'
#' Returns Temporary Assistance for Needy Families (TANF) recipient
#' counts by tribe and month. Covers tribal TANF programs operated
#' by federally recognized tribes.
#'
#' @param limit Max rows (default 1000).
#' @return A tibble with tribal TANF caseload data including tribe name,
#'   month, and recipient counts.
#' @examples
#' \dontrun{
#' hd_tribal_tanf(limit = 100)
#' }
#' @export
hd_tribal_tanf <- function(limit = 1000) {
  raw <- ._soda_query("52gy-ej7q", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch Project Tycho Level 1 notifiable disease data
#'
#' Returns US weekly nationally notifiable disease surveillance data
#' spanning 1888-2013 from the Project Tycho dataset. Covers diseases
#' like measles, polio, smallpox, pertussis, and hepatitis with
#' state-level weekly case counts.
#'
#' @param limit Max rows (default 1000). Full dataset is very large.
#' @return A tibble with disease surveillance records including disease
#'   name, state, year, week, and case count.
#' @examples
#' \dontrun{
#' hd_tycho(limit = 100)
#' }
#' @export
hd_tycho <- function(limit = 1000) {
  raw <- ._soda_query("g89t-x93h", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

#' Fetch HHS expiring contracts (FY23-FY26)
#'
#' Returns HHS contracts expiring during fiscal years 2023-2026.
#' Includes vendor names, contract values, agencies, and expiration dates.
#'
#' @param limit Max rows (default 1000).
#' @return A tibble with expiring contract details including vendor,
#'   agency, value, and expiration date.
#' @examples
#' \dontrun{
#' hd_expiring_contracts(limit = 100)
#' }
#' @export
hd_expiring_contracts <- function(limit = 1000) {
  raw <- ._soda_query("dqp4-e6rh", limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == COVID-19 Vaccine Survey ===================================================

#' Fetch COVID-19 Monthly Outcome Survey data by wave
#'
#' Returns COVID-19 vaccine uptake, beliefs, and behavioral data from
#' the Monthly Outcome Survey (MOS). Waves 1-28 available, spanning
#' January 2021 through April 2023. Each wave is a separate dataset.
#' Use for tracking vaccine hesitancy and public health behaviors.
#'
#' @param wave Wave number as integer (1-28). Default 28 (latest available).
#'   Wave 1 = Jan 2021, Wave 28 = Apr 2023.
#' @param limit Max rows (default 1000).
#' @return A tibble with survey response data. Columns vary by wave but
#'   typically include demographic breakdowns and vaccine-related metrics.
#' @examples
#' \dontrun{
#' # Get latest survey wave
#' hd_covid_survey(wave = 28, limit = 100)
#'
#' # Get early pandemic wave
#' hd_covid_survey(wave = 1, limit = 100)
#' }
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

#' Fetch percentage of hospitals reporting data by state
#'
#' Returns weekly hospital data reporting compliance rates by state.
#' Shows what percentage of hospitals in each state submitted required
#' COVID-19 capacity and patient data to HHS.
#'
#' @param state Two-letter state code (e.g. "NY"). NULL for all states.
#' @param limit Max rows (default 1000).
#' @return A tibble with state-level reporting compliance percentages
#'   by week.
#' @examples
#' \dontrun{
#' # Get reporting rates for all states
#' hd_hospital_reporting(limit = 60)
#'
#' # Get New York reporting history
#' hd_hospital_reporting(state = "NY")
#' }
#' @export
hd_hospital_reporting <- function(state = NULL, limit = 1000) {
  where <- if (!is.null(state)) paste0("state='", toupper(state), "'") else NULL
  raw <- ._soda_query("8xss-g7j8", where = where, limit = limit)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Context ===================================================================

#' Get healthdata.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/healthdata.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "healthdata.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# healthdata.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# healthdata.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
