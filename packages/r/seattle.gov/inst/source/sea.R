# seattle.gov.R — City of Seattle Open Data (Socrata SODA) client
# Self-contained. All public functions return tibbles.
#
# Base: data.seattle.gov (Socrata)
# Auth: none required
# Rate limits: 1000 req/hr without app token
# Dependencies: httr2, jsonlite, dplyr, tibble

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

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
#' @param category Optional category filter (e.g. "public_safety", "permits",
#'   "transportation", "finance", "library", "community", etc.)
#' @return tibble: view_id, name, category
sea_list <- function(category = NULL) {
  out <- .sea_catalog
  if (!is.null(category)) {
    out <- out |> dplyr::filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  out
}

#' Search the Seattle Open Data catalog by keyword
#'
#' @param query Search string matched against dataset names
#' @return tibble: view_id, name, category
sea_search <- function(query) {
  .sea_catalog |>
    dplyr::filter(grepl(query, .data$name, ignore.case = TRUE))
}

# == Generic SODA access =======================================================

#' Query any Seattle Socrata dataset by view ID
#'
#' Runs a SoQL query against a specific dataset on data.seattle.gov.
#'
#' @param view_id Socrata 4x4 identifier (e.g. "tazs-3rd5")
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param group SoQL GROUP BY clause
#' @param having SoQL HAVING clause
#' @param limit Rows to return (default 1000, max 50000)
#' @param offset Pagination offset
#' @return tibble
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
#' SPD crime offenses from the NIBRS system.
#'
#' @param offense_category Filter by offense category (e.g. "PROPERTY CRIME", "PERSON CRIME")
#' @param neighborhood Filter by neighborhood name (e.g. "CAPITOL HILL")
#' @param precinct Filter by precinct (e.g. "North", "South", "East", "West", "Southwest")
#' @param since Date string for minimum offense date (e.g. "2024-01-01")
#' @param limit Rows (default 1000)
#' @param offset Offset for pagination
#' @return tibble: report_number, report_date_time, offense_id, offense_date,
#'   offense_category, offense_sub_category, block_address, latitude, longitude,
#'   beat, precinct, sector, neighborhood
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
#' @param event_group Filter by event group (e.g. "Traffic", "Disorder", "Suspicious")
#' @param precinct Filter by dispatch precinct
#' @param since Date string for minimum event time
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble: cad_event_number, initial_call_type, final_call_type, priority,
#'   dispatch_precinct, dispatch_sector, dispatch_beat, dispatch_neighborhood,
#'   dispatch_address, event_group, cad_event_original_time_queued
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

#' Seattle Fire 911 calls (real-time, last 24 hours)
#'
#' @param type Filter by call type (e.g. "Aid Response", "Medic Response", "Fire")
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble: address, type, datetime, latitude, longitude, incident_number
sea_fire911 <- function(type = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(type)) sprintf("type='%s'", type) else NULL
  ._soda_query("kzjm-xkqj", where = where, order = "datetime DESC",
               limit = limit, offset = offset)
}

#' SPD arrest data
#'
#' @param since Date string for minimum arrest date
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_arrests <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("arrest_occurred_date_time>='%sT00:00:00'", since) else NULL
  ._soda_query("9bjs-7a7w", where = where, order = "arrest_occurred_date_time DESC",
               limit = limit, offset = offset)
}

#' SPD Use of Force data
#'
#' @param since Date string
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_use_of_force <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("occured_date_time>='%sT00:00:00'", since) else NULL
  ._soda_query("ppi5-g2bj", where = where, order = "occured_date_time DESC",
               limit = limit, offset = offset)
}

# == Permits ===================================================================

#' Seattle building permits
#'
#' @param permit_type Filter by permit type description
#' @param status Filter by current status (e.g. "Issued", "Completed")
#' @param since Date string for minimum application date
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble: permitnum, permitclass, permittypedesc, description,
#'   statuscurrent, originaladdress1, latitude, longitude, etc.
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
#' @param status Filter by status
#' @param since Date string
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
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
#' @param department Filter by department
#' @param since Date string
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
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
#' @param since Date string
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_graffiti <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("created_date>='%sT00:00:00'", since) else NULL
  ._soda_query("jucr-xeky", where = where, order = "created_date DESC",
               limit = limit, offset = offset)
}

#' Seattle illegal dumping reports
#'
#' @param since Date string
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_dumping <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("created_date>='%sT00:00:00'", since) else NULL
  ._soda_query("bpvk-ju3y", where = where, order = "created_date DESC",
               limit = limit, offset = offset)
}

# == Transportation ============================================================

#' Seattle paid parking occupancy (last 30 days)
#'
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_parking <- function(limit = 1000, offset = 0) {
  ._soda_query("rke9-rsvs", order = "occupancydatetime DESC",
               limit = limit, offset = offset)
}

#' Seattle traffic counts by study
#'
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_traffic <- function(limit = 1000, offset = 0) {
  ._soda_query("xucb-vzhc", limit = limit, offset = offset)
}

#' SDOT drawbridge status
#'
#' @param bridge Filter by bridge name
#' @param since Date string
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
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
#' @param department Filter by department
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_capital_budget <- function(department = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(department)) sprintf("department like '%%%s%%'", department) else NULL
  ._soda_query("m6va-m4qe", where = where, limit = limit, offset = offset)
}

#' City of Seattle operating budget
#'
#' @param department Filter by department
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_operating_budget <- function(department = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(department)) sprintf("department like '%%%s%%'", department) else NULL
  ._soda_query("8u2j-imqx", where = where, limit = limit, offset = offset)
}

# == Business & Licensing ======================================================

#' Active business license tax certificates
#'
#' @param q Full-text search (business name, etc.)
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_business_licenses <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("wnbq-64tb", q = q, limit = limit, offset = offset)
}

# == Community =================================================================

#' Seattle pet licenses
#'
#' @param species Filter: "Cat", "Dog", "Goat", etc.
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_pets <- function(species = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(species)) sprintf("animal_s_name IS NOT NULL AND species='%s'", species) else NULL
  ._soda_query("jguv-t9rb", where = where, limit = limit, offset = offset)
}

# == Library ===================================================================

#' Seattle Public Library checkouts by title
#'
#' @param q Full-text search on title
#' @param material_type Filter by material type (e.g. "BOOK", "EBOOK")
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
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
#' @param building_type Filter by building type
#' @param year Filter by reporting year
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
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
#' @param since Date string
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_bike_fremont <- function(since = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(since)) sprintf("date>='%sT00:00:00'", since) else NULL
  ._soda_query("65db-xm6k", where = where, order = "date DESC",
               limit = limit, offset = offset)
}

# == City Properties ===========================================================

#' Current City of Seattle properties
#'
#' @param q Full-text search
#' @param limit Rows (default 1000)
#' @param offset Offset
#' @return tibble
sea_properties <- function(q = NULL, limit = 1000, offset = 0) {
  ._soda_query("qhwj-ipk4", q = q, limit = limit, offset = offset)
}

# == Context ===================================================================

#' Generate LLM-friendly context for seattle.gov
#'
#' Reads this source file and returns all public function signatures and bodies.
#'
#' @return Character string (invisibly); also printed to console
sea_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/seattle.gov.R"
  if (!file.exists(src_file)) {
    cat("# seattle.gov context - source not found\n")
    return(invisible("# seattle.gov context - source not found"))
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
