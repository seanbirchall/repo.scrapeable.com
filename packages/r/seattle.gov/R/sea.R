# seattle.gov.R — City of Seattle Open Data (Socrata SODA) client
# Self-contained. All public functions return tibbles.
#
# Base: data.seattle.gov (Socrata)
# Auth: none required
# Rate limits: 1000 req/hr without app token
# Dependencies: httr2, jsonlite, dplyr, tibble


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.sea_ua   <- "seattle.gov.R client (support@scrapeable.com)"
.sea_base <- "https://data.seattle.gov"

.sea_fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .sea_ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  tmp
}

.sea_fetch_json <- function(url) jsonlite::fromJSON(.sea_fetch(url), simplifyDataFrame = TRUE)

._soda_query <- function(view_id, where = NULL, select = NULL, order = NULL,
                         q = NULL, group = NULL, having = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(having)) params[["$having"]] <- having

  query_str <- paste(
    names(params),
    vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE), character(1)),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .sea_base, view_id, query_str)
  raw <- tryCatch(.sea_fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(tibble::tibble())
  tibble::as_tibble(raw)
}

# == Full catalog ==============================================================

.sea_catalog <- tibble::tribble(
  ~view_id,      ~name,                                               ~category,
  "tazs-3rd5",   "SPD Crime Data: 2008-Present",                      "public_safety",
  "33kz-ixgy",   "Call Data (SPD 911)",                                "public_safety",
  "9bjs-7a7w",   "SPD Arrest Data",                                   "public_safety",
  "ppi5-g2bj",   "Use Of Force",                                      "public_safety",
  "i2q9-thny",   "Crisis Data",                                       "public_safety",
  "hyay-5x7b",   "Office of Police Accountability Complaints",        "public_safety",
  "mg5r-efcm",   "SPD Officer Involved Shooting (OIS) Data",          "public_safety",
  "2qns-g7s7",   "Seattle Police Disciplinary Appeals",               "public_safety",
  "kzjm-xkqj",   "Seattle Real Time Fire 911 Calls",                  "fire",
  "76t5-zqzr",   "Building Permits",                                  "permits",
  "ht3q-kdvx",   "Land Use Permits",                                  "permits",
  "c87v-5hwh",   "Trade Permits",                                     "permits",
  "c4tj-daue",   "Electrical Permits",                                "permits",
  "tqk8-y2z5",   "Plan Review",                                       "permits",
  "crg2-ssqd",   "Permit Review Time Data",                           "permits",
  "axkr-2p68",   "Building Certificates of Occupancy",                "permits",
  "e285-aq8h",   "Plan Comments",                                     "permits",
  "5ngg-rpne",   "Customer Service Requests (311)",                   "services",
  "k7ra-jqqe",   "Unauthorized Encampment Reports",                   "services",
  "jucr-xeky",   "Graffiti Reports",                                  "services",
  "bpvk-ju3y",   "Illegal Dumping Reports",                           "services",
  "74ix-6xdj",   "72hr Parking Ordinance Violations",                 "services",
  "ez4a-iug7",   "Code Complaints and Violations",                    "code_compliance",
  "ud3x-cvhp",   "Code Compliance Complaints by Year",                "code_compliance",
  "afww-hv6u",   "Code Compliance Complaints by Month",               "code_compliance",
  "4m8w-zxcb",   "Code Compliance Violations by Month",               "code_compliance",
  "wnbq-64tb",   "Active Business License Tax Certificate",           "business",
  "s7df-xba4",   "Short Term Rental License",                         "business",
  "j2xh-c7vt",   "Rental Property Registration",                      "business",
  "rke9-rsvs",   "Paid Parking Occupancy (Last 30 Days)",             "transportation",
  "hiyf-7edq",   "Paid Parking (Last 48 Hours)",                      "transportation",
  "gg89-k5p6",   "Paid Parking Transaction Data",                     "transportation",
  "xucb-vzhc",   "Traffic Counts by Study",                           "transportation",
  "s72h-pqjm",   "Traffic Counts Studies by Type",                    "transportation",
  "gi49-5uh6",   "Traffic Counts Studies by 15 Minute Bins",          "transportation",
  "g32r-fjzp",   "Traffic Count Studies by Hour Bins",                "transportation",
  "gm8h-9449",   "SDOT Drawbridge Status",                            "transportation",
  "ium9-iqtc",   "Street Closures",                                   "transportation",
  "eyde-ia6x",   "Street Use Permit Applications Public Notices",     "transportation",
  "ra8u-g2xi",   "Street Use Review Times",                           "transportation",
  "egc4-d24i",   "Road Weather Information Stations",                 "transportation",
  "kqdm-4wfs",   "Blockface Space Inventory",                         "transportation",
  "7jzm-ucez",   "Annual Parking Study Data",                         "transportation",
  "dm95-f8w5",   "Special Events Permits",                            "transportation",
  "qhwj-ipk4",   "Current City Properties",                           "property",
  "54qs-2h7f",   "Unreinforced Masonry Buildings",                    "property",
  "ajvs-k55z",   "Privately Owned Public Spaces",                     "property",
  "teqw-tu6e",   "Building Energy Benchmarking Data, 2015-Present",   "energy",
  "m6va-m4qe",   "City of Seattle Capital Budget",                    "finance",
  "8u2j-imqx",   "City of Seattle Operating Budget",                  "finance",
  "bsgq-948x",   "Open Budget - Capital Projects Details",            "finance",
  "q76w-bfs5",   "Capital Projects Input Data",                       "finance",
  "5avq-r9hj",   "City of Seattle Staff Demographics",                "hr",
  "2khk-5ukd",   "City of Seattle Wage Data",                         "hr",
  "mqnm-tr2p",   "City of Seattle Racial Equity Actions",             "hr",
  "6vkj-f5xf",   "Library Collection Inventory",                      "library",
  "5src-czff",   "Checkouts By Title (Physical Items)",               "library",
  "tmmm-ytt6",   "Checkouts by Title",                                "library",
  "6853-bgsc",   "Seattle Channel TV Schedule",                       "media",
  "jguv-t9rb",   "Seattle Pet Licenses",                              "community",
  "pr2n-4pn6",   "City of Seattle Neighborhood Matching Funds",       "community",
  "kkzf-ntnu",   "Emergency Food and Meals Seattle and King County",  "community",
  "xrnu-8eiq",   "Park Features By PMAID",                            "parks",
  "v5tj-kqhc",   "Seattle Parks And Recreation Park Addresses",       "parks",
  "65db-xm6k",   "Fremont Bridge Bicycle Counter",                    "bikes",
  "avwm-i8ym",   "2nd Ave Cycle Track North of Marion St Bicycle Counter", "bikes",
  "2z5v-ecg8",   "Burke Gilman Trail north of NE 70th St Bicycle and Pedestrian Counter", "bikes",
  "m2yw-err8",   "12th Ave S Bike Counter",                           "bikes",
  "u38e-ybnc",   "MTS Trail west of I-90 Bridge Bicycle and Pedestrian Counter", "bikes",
  "35qv-xtws",   "Taps Tracker -- Water Service Application Status",  "utilities",
  "nube-4pmr",   "Landsburg Fish Ladder Annual Salmon Counts",        "environment",
  "rsq4-c9zm",   "Sockeye Broodstock Weir Operations - Cedar River",  "environment",
  "y6ef-jf2w",   "Sold Fleet Equipment",                              "fleet",
  "enxu-fgzb",   "Active Fleet Complement",                           "fleet",
  "6gnm-7jex",   "Current Fleet Surplus/Auction List",                "fleet",
  "xvj2-ai6y",   "Underground Storage Tank (UST) Records - Residential", "environment",
  "uy6s-ine5",   "Tree Service Provider Registry",                    "environment",
  "a2k6-wwdn",   "Seattle Graffiti Daily Summary",                    "services",
  "d9ti-hi4p",   "Downtown Activation Plan Dashboard Data",           "planning",
  "4sup-g4bi",   "Public Disclosure Requests",                        "transparency",
  "9gb2-237x",   "Monthly Tow Statistics 2021-Current",               "transportation",
  "qqw9-cbst",   "Discrimination Case Settlements by Month, 2017-Present", "civil_rights",
  "eqiu-9fdd",   "Discrimination Case Closures by Month and Type, 2017-Present", "civil_rights",
  "bb86-nj6i",   "Discrimination Complaints Filed by Month and Type, 2017-Present", "civil_rights",
  "k6nc-7s8x",   "Civil Rights Customer Inquiries by Month",          "civil_rights",
  "yb5c-ur9z",   "SDCI Employee Contact",                             "directory",
  "rjzv-bp3r",   "Commercial Vehicle License Plates for 3rd Ave Camera Enforcement", "transportation",
  "rk6r-ehyv",   "Notice of Medallion Transfers",                     "business",
  "xsgx-5um2",   "Closed Case Summaries (2026)",                      "public_safety",
  "58nk-8i58",   "Closed Case Summaries (2024-2025)",                 "public_safety",
  "m33m-84uk",   "Closed Case Summaries (2022-2023)",                 "public_safety",
  "f8kp-sfr3",   "Closed Case Summaries (2020-2021)",                 "public_safety",
  "pafy-bfmu",   "OPA Complaint Tracker",                             "public_safety",
  "98cw-h8mg",   "Ethnic Artist Roster",                              "community"
)

# == Discovery =================================================================

#' List all datasets in the Seattle Open Data catalog
#'
#' Returns the built-in catalog of 90+ Socrata datasets available on
#' data.seattle.gov, organized by category.
#'
#' @param category Character or NULL. Filter by category slug. Valid values:
#'   \code{"public_safety"}, \code{"fire"}, \code{"permits"},
#'   \code{"services"}, \code{"code_compliance"}, \code{"business"},
#'   \code{"transportation"}, \code{"property"}, \code{"energy"},
#'   \code{"finance"}, \code{"hr"}, \code{"library"}, \code{"media"},
#'   \code{"community"}, \code{"parks"}, \code{"bikes"},
#'   \code{"utilities"}, \code{"environment"}, \code{"fleet"},
#'   \code{"planning"}, \code{"transparency"}, \code{"civil_rights"},
#'   \code{"directory"}. NULL (default) returns all.
#' @return A tibble with columns:
#'   \describe{
#'     \item{view_id}{Character. Socrata 4x4 dataset identifier (e.g. \code{"tazs-3rd5"}).}
#'     \item{name}{Character. Dataset name.}
#'     \item{category}{Character. Category slug.}
#'   }
#' @examples
#' sea_list()
#' sea_list("public_safety")
#' @export
sea_list <- function(category = NULL) {
  out <- .sea_catalog
  if (!is.null(category)) {
    out <- out |> dplyr::filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  out
}

#' Search the Seattle Open Data catalog by keyword
#'
#' Filters the built-in catalog for datasets whose names match a keyword.
#' Case-insensitive.
#'
#' @param query Character. Search string matched against dataset names
#'   (e.g. \code{"crime"}, \code{"parking"}, \code{"library"}).
#' @return A tibble with columns: view_id, name, category.
#' @examples
#' sea_search("crime")
#' sea_search("bicycle")
#' @export
sea_search <- function(query) {
  .sea_catalog |>
    dplyr::filter(grepl(query, .data$name, ignore.case = TRUE))
}

# == Generic SODA access =======================================================

#' Query any Seattle Socrata dataset by view ID
#'
#' Runs a SoQL (Socrata Query Language) query against any dataset on
#' data.seattle.gov. Use \code{sea_list()} or \code{sea_search()} to find
#' the view_id for a dataset.
#'
#' @param view_id Character. Socrata 4x4 identifier
#'   (e.g. \code{"tazs-3rd5"} for SPD crime data). Obtain from
#'   \code{sea_list()} or \code{sea_search()}.
#' @param where Character or NULL. SoQL WHERE clause for filtering
#'   (e.g. \code{"precinct='North'"}).
#' @param select Character or NULL. SoQL SELECT clause for column selection
#'   (e.g. \code{"count(*) as n"}).
#' @param order Character or NULL. SoQL ORDER BY clause
#'   (e.g. \code{"date DESC"}).
#' @param q Character or NULL. Full-text search query.
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param having Character or NULL. SoQL HAVING clause.
#' @param limit Integer. Rows to return (default 1000, max 50000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns varying by dataset.
#' @examples
#' sea_view("tazs-3rd5", limit = 5)
#' sea_view("jguv-t9rb", where = "species='Cat'", limit = 5)
#' @export
sea_view <- function(view_id, where = NULL, select = NULL, order = NULL,
                     q = NULL, group = NULL, having = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select, order = order,
               q = q, group = group, having = having,
               limit = limit, offset = offset)
}

# == Crime & Public Safety =====================================================

#' Seattle Police crime data (2008-present)
#'
#' Queries SPD crime offense records from the NIBRS system. Returns
#' individual offenses with location, type, and time information.
#'
#' @param offense_category Character or NULL. Filter by offense category
#'   (e.g. \code{"PROPERTY CRIME"}, \code{"PERSON CRIME"}).
#' @param neighborhood Character or NULL. Filter by neighborhood name
#'   (e.g. \code{"CAPITOL HILL"}, \code{"BALLARD"}, \code{"UNIVERSITY DISTRICT"}).
#' @param precinct Character or NULL. Filter by police precinct:
#'   \code{"North"}, \code{"South"}, \code{"East"}, \code{"West"},
#'   \code{"Southwest"}.
#' @param since Character or NULL. Minimum offense date in
#'   \code{"YYYY-MM-DD"} format (e.g. \code{"2024-01-01"}).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: report_number,
#'   report_date_time, offense_id, offense_date, offense_category,
#'   offense_sub_category, block_address, latitude, longitude, beat,
#'   precinct, sector, neighborhood, nibrs_offense_code,
#'   nibrs_offense_code_description.
#' @examples
#' sea_crime(since = "2024-01-01", limit = 10)
#' sea_crime(precinct = "North", limit = 5)
#' @export
sea_crime <- function(offense_category = NULL, neighborhood = NULL,
                      precinct = NULL, since = NULL,
                      limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(offense_category)) clauses <- c(clauses, sprintf("offense_category='%s'", offense_category))
  if (!is.null(neighborhood))     clauses <- c(clauses, sprintf("neighborhood='%s'", neighborhood))
  if (!is.null(precinct))         clauses <- c(clauses, sprintf("precinct='%s'", precinct))
  if (!is.null(since))            clauses <- c(clauses, sprintf("offense_date>='%sT00:00:00'", since))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tazs-3rd5", where = where, order = "offense_date DESC",
               limit = limit, offset = offset)
}

#' Seattle Police 911 call data
#'
#' Queries SPD Computer-Aided Dispatch (CAD) events. Each row represents
#' one 911 call with dispatch details.
#'
#' @param event_group Character or NULL. Filter by event group
#'   (e.g. \code{"Traffic"}, \code{"Disorder"}, \code{"Suspicious"}).
#' @param precinct Character or NULL. Filter by dispatch precinct.
#' @param since Character or NULL. Minimum event time in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: cad_event_number,
#'   initial_call_type, final_call_type, priority, dispatch_precinct,
#'   dispatch_sector, dispatch_beat, dispatch_neighborhood,
#'   dispatch_address, event_group, cad_event_original_time_queued.
#' @examples
#' sea_calls(limit = 5)
#' sea_calls(event_group = "Traffic", limit = 5)
#' @export
sea_calls <- function(event_group = NULL, precinct = NULL, since = NULL,
                      limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(event_group)) clauses <- c(clauses, sprintf("event_group='%s'", event_group))
  if (!is.null(precinct))    clauses <- c(clauses, sprintf("dispatch_precinct='%s'", precinct))
  if (!is.null(since))       clauses <- c(clauses, sprintf("cad_event_original_time_queued>='%sT00:00:00'", since))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("33kz-ixgy", where = where, order = "cad_event_original_time_queued DESC",
               limit = limit, offset = offset)
}

#' Seattle Fire 911 calls (real-time)
#'
#' Returns recent Seattle Fire Department 911 dispatches.
#'
#' @param type Character or NULL. Filter by call type
#'   (e.g. \code{"Aid Response"}, \code{"Medic Response"}, \code{"Fire"}).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: address, type, datetime,
#'   latitude, longitude, incident_number.
#' @examples
#' sea_fire911(limit = 5)
#' @export
sea_fire911 <- function(type = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(type)) sprintf("type='%s'", type) else NULL
  ._soda_query("kzjm-xkqj", where = where, order = "datetime DESC",
               limit = limit, offset = offset)
}

#' SPD arrest data
#'
#' Queries Seattle Police Department arrest records.
#'
#' @param since Character or NULL. Minimum arrest date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of arrest records with columns varying by dataset.
#' @examples
#' sea_arrests(since = "2024-01-01", limit = 5)
#' @export
sea_arrests <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("arrest_occurred_date_time>='%sT00:00:00'", since) else NULL
  ._soda_query("9bjs-7a7w", where = where, order = "arrest_occurred_date_time DESC",
               limit = limit, offset = offset)
}

#' SPD Use of Force data
#'
#' Queries Seattle Police Department use-of-force incident reports.
#'
#' @param since Character or NULL. Minimum incident date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of use-of-force incidents with columns varying by
#'   dataset.
#' @examples
#' sea_use_of_force(since = "2024-01-01", limit = 5)
#' @export
sea_use_of_force <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("occured_date_time>='%sT00:00:00'", since) else NULL
  ._soda_query("ppi5-g2bj", where = where, order = "occured_date_time DESC",
               limit = limit, offset = offset)
}

# == Permits ===================================================================

#' Seattle building permits
#'
#' Queries City of Seattle building permit records.
#'
#' @param permit_type Character or NULL. Filter by permit type description
#'   (partial match, e.g. \code{"New"}, \code{"Addition/Alteration"}).
#' @param status Character or NULL. Filter by current status
#'   (e.g. \code{"Issued"}, \code{"Completed"}, \code{"Application Accepted"}).
#' @param since Character or NULL. Minimum application date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: permitnum, permitclass,
#'   permittypedesc, description, statuscurrent, originaladdress1,
#'   latitude, longitude, applieddate, issueddate, value.
#' @examples
#' sea_permits(since = "2024-01-01", limit = 5)
#' sea_permits(status = "Issued", limit = 5)
#' @export
sea_permits <- function(permit_type = NULL, status = NULL, since = NULL,
                        limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(permit_type)) clauses <- c(clauses, sprintf("permittypedesc like '%%%s%%'", permit_type))
  if (!is.null(status))      clauses <- c(clauses, sprintf("statuscurrent='%s'", status))
  if (!is.null(since))       clauses <- c(clauses, sprintf("applieddate>='%sT00:00:00'", since))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("76t5-zqzr", where = where, order = "applieddate DESC",
               limit = limit, offset = offset)
}

#' Seattle land use permits
#'
#' Queries City of Seattle land use permit applications and decisions.
#'
#' @param status Character or NULL. Filter by current status.
#' @param since Character or NULL. Minimum application date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of land use permit records.
#' @examples
#' sea_land_use(since = "2024-01-01", limit = 5)
#' @export
sea_land_use <- function(status = NULL, since = NULL, limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(status)) clauses <- c(clauses, sprintf("statuscurrent='%s'", status))
  if (!is.null(since))  clauses <- c(clauses, sprintf("applieddate>='%sT00:00:00'", since))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("ht3q-kdvx", where = where, order = "applieddate DESC",
               limit = limit, offset = offset)
}

# == 311 & Service Requests ====================================================

#' Seattle 311 customer service requests
#'
#' Queries City of Seattle 311 service request records, covering
#' complaints, inquiries, and requests across city departments.
#'
#' @param department Character or NULL. Filter by department name.
#' @param since Character or NULL. Minimum created date in
#'   \code{"YYYY-MM-DD"} format.
#' @param q Character or NULL. Full-text search query.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of 311 service request records.
#' @examples
#' sea_311(since = "2024-01-01", limit = 5)
#' sea_311(q = "pothole", limit = 5)
#' @export
sea_311 <- function(department = NULL, since = NULL, q = NULL,
                    limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(department)) clauses <- c(clauses, sprintf("departmentname='%s'", department))
  if (!is.null(since))      clauses <- c(clauses, sprintf("createddate>='%sT00:00:00'", since))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("5ngg-rpne", where = where, q = q, order = "createddate DESC",
               limit = limit, offset = offset)
}

#' Seattle graffiti reports
#'
#' Queries City of Seattle graffiti incident reports.
#'
#' @param since Character or NULL. Minimum created date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of graffiti reports.
#' @examples
#' sea_graffiti(since = "2024-01-01", limit = 5)
#' @export
sea_graffiti <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("created_date>='%sT00:00:00'", since) else NULL
  ._soda_query("jucr-xeky", where = where, order = "created_date DESC",
               limit = limit, offset = offset)
}

#' Seattle illegal dumping reports
#'
#' Queries City of Seattle illegal dumping incident reports.
#'
#' @param since Character or NULL. Minimum created date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of illegal dumping reports.
#' @examples
#' sea_dumping(since = "2024-01-01", limit = 5)
#' @export
sea_dumping <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("created_date>='%sT00:00:00'", since) else NULL
  ._soda_query("bpvk-ju3y", where = where, order = "created_date DESC",
               limit = limit, offset = offset)
}

# == Transportation ============================================================

#' Seattle paid parking occupancy (last 30 days)
#'
#' Returns recent paid parking occupancy data from City of Seattle meters.
#'
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of parking occupancy records.
#' @examples
#' sea_parking(limit = 5)
#' @export
sea_parking <- function(limit = 1000, offset = 0) {
  ._soda_query("rke9-rsvs", order = "occupancydatetime DESC",
               limit = limit, offset = offset)
}

#' Seattle traffic counts by study
#'
#' Returns SDOT traffic count study data.
#'
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of traffic count records.
#' @examples
#' sea_traffic(limit = 5)
#' @export
sea_traffic <- function(limit = 1000, offset = 0) {
  ._soda_query("xucb-vzhc", limit = limit, offset = offset)
}

#' SDOT drawbridge opening status
#'
#' Queries Seattle Department of Transportation drawbridge opening records.
#'
#' @param bridge Character or NULL. Filter by bridge name (partial match,
#'   e.g. \code{"Fremont"}, \code{"Ballard"}).
#' @param since Character or NULL. Minimum open date in
#'   \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of drawbridge opening records.
#' @examples
#' sea_drawbridges(limit = 5)
#' sea_drawbridges(bridge = "Fremont", limit = 5)
#' @export
sea_drawbridges <- function(bridge = NULL, since = NULL, limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(bridge)) clauses <- c(clauses, sprintf("entityname like '%%%s%%'", bridge))
  if (!is.null(since))  clauses <- c(clauses, sprintf("opendatetime>='%sT00:00:00'", since))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("gm8h-9449", where = where, order = "opendatetime DESC",
               limit = limit, offset = offset)
}

# == Finance & Budget ==========================================================

#' City of Seattle capital budget
#'
#' Queries City of Seattle capital budget data.
#'
#' @param department Character or NULL. Filter by department name (partial
#'   match, e.g. \code{"Transportation"}, \code{"Parks"}).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of capital budget records.
#' @examples
#' sea_capital_budget(limit = 5)
#' @export
sea_capital_budget <- function(department = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(department)) sprintf("department like '%%%s%%'", department) else NULL
  ._soda_query("m6va-m4qe", where = where, limit = limit, offset = offset)
}

#' City of Seattle operating budget
#'
#' Queries City of Seattle operating budget data.
#'
#' @param department Character or NULL. Filter by department name (partial
#'   match).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of operating budget records.
#' @examples
#' sea_operating_budget(limit = 5)
#' @export
sea_operating_budget <- function(department = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(department)) sprintf("department like '%%%s%%'", department) else NULL
  ._soda_query("8u2j-imqx", where = where, limit = limit, offset = offset)
}

# == Business & Licensing ======================================================

#' Active business license tax certificates
#'
#' Queries active City of Seattle business license and tax certificate
#' records.
#'
#' @param q Character or NULL. Full-text search across all fields
#'   (e.g. business name, address).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of active business license records.
#' @examples
#' sea_business_licenses(q = "coffee", limit = 5)
#' @export
sea_business_licenses <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("wnbq-64tb", q = q, limit = limit, offset = offset)
}

# == Community =================================================================

#' Seattle pet licenses
#'
#' Queries City of Seattle pet license records.
#'
#' @param species Character or NULL. Filter by species: \code{"Cat"},
#'   \code{"Dog"}, \code{"Goat"}, etc.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns including: license_issue_date,
#'   license_number, animal_s_name, species, primary_breed,
#'   secondary_breed, zip_code.
#' @examples
#' sea_pets(species = "Cat", limit = 5)
#' sea_pets(species = "Dog", limit = 5)
#' @export
sea_pets <- function(species = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(species)) sprintf("animal_s_name IS NOT NULL AND species='%s'", species) else NULL
  ._soda_query("jguv-t9rb", where = where, limit = limit, offset = offset)
}

# == Library ===================================================================

#' Seattle Public Library checkouts by title
#'
#' Queries Seattle Public Library circulation data by title.
#'
#' @param q Character or NULL. Full-text search on title and creator
#'   fields.
#' @param material_type Character or NULL. Filter by material type
#'   (e.g. \code{"BOOK"}, \code{"EBOOK"}, \code{"AUDIOBOOK"},
#'   \code{"VIDEODISC"}).
#' @param year Integer or NULL. Filter by checkout year (e.g. 2024).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of checkout records ordered by checkout count
#'   descending.
#' @examples
#' sea_library_checkouts(q = "Harry Potter", limit = 5)
#' sea_library_checkouts(material_type = "EBOOK", year = 2024, limit = 5)
#' @export
sea_library_checkouts <- function(q = NULL, material_type = NULL,
                                  year = NULL,
                                  limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(material_type)) clauses <- c(clauses, sprintf("materialtype='%s'", material_type))
  if (!is.null(year))          clauses <- c(clauses, sprintf("checkoutyear=%d", as.integer(year)))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("tmmm-ytt6", where = where, q = q, order = "checkouts DESC",
               limit = limit, offset = offset)
}

# == Energy ====================================================================

#' Building energy benchmarking data (2015-present)
#'
#' Queries City of Seattle building energy benchmarking data, required
#' for commercial and multifamily buildings over 20,000 sq ft.
#'
#' @param building_type Character or NULL. Filter by building type
#'   (partial match, e.g. \code{"Office"}, \code{"Multifamily"}).
#' @param year Integer or Character or NULL. Filter by reporting year
#'   (e.g. 2023).
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of building energy performance records.
#' @examples
#' sea_energy(year = 2023, limit = 5)
#' @export
sea_energy <- function(building_type = NULL, year = NULL,
                       limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(building_type)) clauses <- c(clauses, sprintf("buildingtype like '%%%s%%'", building_type))
  if (!is.null(year))          clauses <- c(clauses, sprintf("datayear='%s'", as.character(year)))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("teqw-tu6e", where = where, limit = limit, offset = offset)
}

# == Bikes =====================================================================

#' Fremont Bridge bicycle counter data
#'
#' Returns hourly bicycle counts from the Fremont Bridge inductive
#' loop counter.
#'
#' @param since Character or NULL. Minimum date in \code{"YYYY-MM-DD"}
#'   format.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of bicycle count records.
#' @examples
#' sea_bike_fremont(since = "2024-01-01", limit = 5)
#' @export
sea_bike_fremont <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("date>='%sT00:00:00'", since) else NULL
  ._soda_query("65db-xm6k", where = where, order = "date DESC",
               limit = limit, offset = offset)
}

# == City Properties ===========================================================

#' Current City of Seattle properties
#'
#' Queries City of Seattle-owned property inventory.
#'
#' @param q Character or NULL. Full-text search query.
#' @param limit Integer. Rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of city property records.
#' @examples
#' sea_properties(q = "park", limit = 5)
#' @export
sea_properties <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("qhwj-ipk4", q = q, limit = limit, offset = offset)
}

# == Context ===================================================================

#' Get seattle.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' sea_context()
#' @export
sea_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sea_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/seattle.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "seattle.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# seattle.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# seattle.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
