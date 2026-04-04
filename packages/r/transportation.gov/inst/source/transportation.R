# transportation.gov.R
# Self-contained data.transportation.gov (US DOT) Socrata client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data). Optional app token reduces throttling.
# Base: https://data.transportation.gov (Socrata SODA, follows redirects)
# Docs: https://dev.socrata.com/docs/queries/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.dot_base <- "https://data.transportation.gov"

# -- SODA query engine ---------------------------------------------------------

.dot_soda <- function(dataset_id, where = NULL, select = NULL, group = NULL,
                      order = NULL, q = NULL, limit = 1000, offset = 0,
                      token = NULL, max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 50000)

  repeat {
    base_url <- paste0(.dot_base, "/resource/", dataset_id, ".json")
    req <- httr2::request(base_url) |>
      httr2::req_url_query(`$limit` = page_size, `$offset` = current_offset)
    if (!is.null(where))  req <- req |> httr2::req_url_query(`$where` = where)
    if (!is.null(select)) req <- req |> httr2::req_url_query(`$select` = select)
    if (!is.null(group))  req <- req |> httr2::req_url_query(`$group` = group)
    if (!is.null(order))  req <- req |> httr2::req_url_query(`$order` = order)
    if (!is.null(q))      req <- req |> httr2::req_url_query(`$q` = q)
    if (!is.null(token))  req <- req |> httr2::req_url_query(`$$app_token` = token)

    tmp <- tempfile(fileext = ".json")
    resp <- tryCatch(
      req |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_options(followlocation = TRUE) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform(path = tmp),
      error = function(e) NULL
    )
    if (is.null(resp)) break
    if (httr2::resp_status(resp) >= 400) break
    raw <- tryCatch(jsonlite::fromJSON(tmp), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) break

    all_data[[length(all_data) + 1]] <- as_tibble(raw)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (is.data.frame(raw) && nrow(raw) < page_size) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)
  result
}

# == Dataset catalog ===========================================================

.dot_catalog <- tibble::tribble(
  ~view_id,    ~name,                                                              ~category,
  # --- NHTSA Recalls ---
  "6axg-epim", "Recalls Data",                                                     "Vehicle Safety",
  # --- Railroad Safety (FRA) ---
  "85tf-25kj", "Rail Equipment Accident/Incident Data (Form 54)",                  "Railroad Safety",
  "byy5-w977", "Rail Accident Subset - Unique Train Accidents",                    "Railroad Safety",
  "aqxq-n5hy", "Rail Equipment Accident/Incident Source Data (Form 54)",           "Railroad Safety",
  "7wn6-i5b9", "Highway-Rail Grade Crossing Incident Data (Form 57)",             "Railroad Safety",
  "icqf-xf4w", "Highway-Rail Grade Crossing Incident Source Data (Form 57)",      "Railroad Safety",
  "m2f8-22s6", "Crossing Inventory Data (Form 71) - Current",                     "Railroad Safety",
  "xp92-5xme", "Crossing Inventory Source Data (Form 71) - Current",              "Railroad Safety",
  "vhwz-raag", "Crossing Inventory Data (Form 71) - Historical",                  "Railroad Safety",
  "8uv2-y4is", "Crossing Inventory Source Data (Form 71) - Historical",           "Railroad Safety",
  "m8i6-zdsy", "Injury/Illness Summary - Operational Data (Form 55)",             "Railroad Safety",
  "unww-uhxd", "Injury/Illness Summary - Operational Source Data (Form 55)",      "Railroad Safety",
  "rash-pd2d", "Injury/Illness Summary - Casualty Data (Form 55a)",              "Railroad Safety",
  "kuvg-3uwp", "Injury/Illness Summary - Casualty Source Data (Form 55A)",       "Railroad Safety",
  # --- Transit (NTD / FTA) ---
  "5ti2-5uiv", "Monthly Modal Time Series (NTD)",                                 "Transit",
  "9ivb-8ae9", "Major Safety Events (FTA)",                                        "Transit",
  "65fa-qbkf", "FRA Regulated Mode Major Security Events",                        "Transit",
  "wwdp-t4re", "NTD Annual Data - Service (by Mode and Time Period)",             "Transit",
  "5x22-djnv", "NTD Annual Data - Federal Funding Allocation",                    "Transit",
  "2u7n-ub22", "General Transit Feed Specification Weblinks",                      "Transit",
  # --- Aviation ---
  "3xj5-daif", "T100 - Preliminary Estimates",                                    "Aviation",
  "q4tb-tbff", "AFF - T100 Segment Summary By Carrier",                           "Aviation",
  "r495-tyji", "AFF - T100 Segment Summary By Origin Airport",                    "Aviation",
  "56rv-9p75", "AFF - T100 Segment Summary By Country",                           "Aviation",
  "udzf-9fvh", "International Passengers Freight All Types",                       "Aviation",
  "w8ea-nba4", "Air Carrier Fuel Efficiency by Year",                              "Aviation",
  "hinw-eisy", "AFF - Aircraft Fleet Age by Carrier",                              "Aviation",
  "xrt2-b7j8", "AFF - Air Carrier Aircraft Fleet Age",                             "Aviation",
  "u2pk-kyws", "AFF - GA Inventory",                                               "Aviation",
  "kxxg-a7c6", "AFF - GA Performance",                                             "Aviation",
  "h77j-murt", "AFF - Average Air Fare by Quarter",                                "Aviation",
  "gjp5-nh2u", "AFF - Average Air Fare by Carrier",                                "Aviation",
  "33xp-y9fx", "AFF - Fuel Consumption by Carrier Group",                          "Aviation",
  "jtvy-isaj", "AFF - Average Fuel Cost by Carrier Group",                         "Aviation",
  "2ydv-qfge", "PTFF Enplanements at Top 50 US Airports",                          "Aviation",
  "yj5y-b2ir", "Consumer Airfare Report Table 6 - City-Pair Markets 10+ Pax/Day", "Aviation",
  "wqw2-rjgd", "Consumer Airfare Report Table 2 - Top 1000 City-Pair Markets",    "Aviation",
  "tfrh-tu9e", "Consumer Airfare Report Table 1a - All Airport Pair Markets",      "Aviation",
  "d6nc-s8v6", "Consumer Airfare Report Table 7 - Fare Premiums",                  "Aviation",
  "bkh6-tj42", "Consumer Airfare Report Table 5 - Highest/Lowest Fares <750mi",   "Aviation",
  # --- Border Crossing ---
  "keg4-3bc2", "Border Crossing Entry Data",                                       "Border Crossing",
  # --- Supply Chain & Freight ---
  "y5ut-ibwt", "Supply Chain and Freight Indicators",                              "Freight",
  "gbe2-48iq", "Tank Car Data 2017-2021",                                          "Freight",
  "qq62-cjjy", "CFS Export File 2012-2022",                                        "Freight",
  "j246-y2rf", "CFS Area File 2012-2022",                                          "Freight",
  "dkgi-gbeh", "CFS Hazmat File 2012-2022",                                        "Freight",
  # --- Highway Statistics ---
  "3hqe-sh4k", "Public Road Length by Functional System (HM-20)",                  "Highways",
  "26bt-cq5y", "Road Length by Measured Pavement Roughness (HM-64)",               "Highways",
  "nps9-3pm2", "Vehicle Miles of Travel by Functional System (VM-2)",              "Highways",
  "9ey8-2qc4", "Annual Vehicle Distance Traveled (VM-1)",                          "Highways",
  "54nx-se7f", "Public Road Mileage, Lane Miles, and VMT (VMT-421C)",             "Highways",
  "7b4c-ii6e", "Estimated Lane-Miles by Functional System (HM-60)",               "Highways",
  "r94d-n4f9", "National Highway Construction Cost Index (NHCCI)",                 "Highways",
  "ra4w-8xud", "Monthly NHS Traffic Speed",                                        "Highways",
  # --- Highway Finance ---
  "q989-k45x", "Total Disbursements for Highways (DISB-C)",                        "Highway Finance",
  "9pk4-tjjw", "Total Receipts for Highways (REC-C)",                              "Highway Finance",
  "hr7b-uxn2", "Disposition of Receipts from Highway User Taxes (HDF)",            "Highway Finance",
  "tjcx-g994", "State Disbursements for State-Administered Highways (SF-4C)",      "Highway Finance",
  "s85w-az69", "Change in Indebtedness - State Highways (SB-2)",                   "Highway Finance",
  "ix2d-bsqq", "Revenues Used by States for Highways (SF-1)",                      "Highway Finance",
  "255k-mnvp", "Disbursements by States for Highways (SF-2)",                      "Highway Finance",
  "ymn9-wjhq", "Obligation of Highway Funds by Functional System (FA-4C)",        "Highway Finance",
  "4qeg-kgwc", "Obligation of Federal Funds by Functional Class (FA6A)",           "Highway Finance",
  "8fhs-nes8", "Federal Highway Trust Fund Receipts by State (FE-9)",              "Highway Finance",
  "taz8-hut2", "Status of the Highway Trust Fund (FE-210)",                        "Highway Finance",
  "qdqw-a4s6", "Receipts and Expenditures for Highways by Federal Agencies (FA-5)","Highway Finance",
  "jpvr-2n6z", "Disposition of State Highway-User Revenues (SDF)",                 "Highway Finance",
  # --- Motor Fuel ---
  "2eye-i5cs", "State Motor Fuel Tax Receipts (MF-201)",                           "Motor Fuel",
  "n34k-rt9f", "State Motor Fuel Tax Receipts (MF-201) - Adjusted",               "Motor Fuel",
  "5qxh-t94v", "Monthly Motor Fuel - Gasoline Volumes Distributed",               "Motor Fuel",
  "5n49-mh85", "Monthly Special Fuel Volumes Distributed",                         "Motor Fuel",
  "xb52-s9ue", "Monthly Gasoline Reported by States (MF-33GA)",                   "Motor Fuel",
  "rsp4-2gbw", "Monthly Special Fuel Reported by State (MF-33SF)",                "Motor Fuel",
  "ef8t-7afc", "Motor Fuel Use (MF-21)",                                           "Motor Fuel",
  "2ntn-7s3e", "Highway Use of Gasoline by State (MF-226)",                        "Motor Fuel",
  "kbvr-tyu5", "Monthly Motor Fuel Sales Reported by States",                      "Motor Fuel",
  "e5cn-ri8q", "Tax Rates by Motor Fuel and State",                                "Motor Fuel",
  "yuth-jt4g", "Weekly Gasoline Product Supplied",                                 "Motor Fuel",
  "t4xw-qp9u", "Weekly Gasoline Product Supplied (alt)",                           "Motor Fuel",
  # --- Driver/Vehicle Registration ---
  "xfkb-3bxx", "Licensed Drivers by State, Sex, and Age Group (DL-22)",           "Drivers & Vehicles",
  "jm62-yva2", "Licensed Drivers by Sex and Age Groups (DL-220)",                 "Drivers & Vehicles",
  "dwyf-zaik", "Licensed Drivers by State (DL-201)",                               "Drivers & Vehicles",
  "4dra-vxq7", "Motor Vehicle Registrations (MV-1)",                               "Drivers & Vehicles",
  "9hnm-fr6r", "Licensed Drivers, Vehicle Registrations, Population (DV-1C)",     "Drivers & Vehicles",
  "a6pv-7q2j", "Licensed Drivers By Sex And Ratio To Population (DL-1C)",         "Drivers & Vehicles",
  # --- Transportation Economics ---
  "bw6n-ddqk", "Transportation Services Index (TSI)",                              "Economics",
  "tcq5-4pgu", "Transportation Economic Trends (TET)",                             "Economics",
  "5yqg-88j3", "Transportation Satellite Accounts",                                "Economics",
  "kdtd-3e96", "State-Level Transportation Public Finance Statistics (TPFS)",      "Economics",
  "6aiz-ybqx", "Aggregate State Transportation Public Finance Statistics (TPFS)", "Economics",
  "nu8j-7gmn", "Government Transportation Financial Statistics (GTFS)",            "Economics",
  "3qgg-2u2a", "Sales Tax Collections by State",                                   "Economics",
  # --- Passenger Travel ---
  "pqmc-mnds", "Passenger Travel Facts and Figures",                               "Passenger Travel",
  "b3ps-driu", "Pocket Guide - Moving People",                                     "Passenger Travel",
  "bqx9-a7yw", "Pocket Guide - Passenger Miles Traveled",                          "Passenger Travel",
  "amn9-4jcb", "Pocket Guide - Daily Passenger Travel",                            "Passenger Travel",
  "iyz9-3pd9", "Pocket Guide - Transportation Facilities",                         "Passenger Travel",
  "xnav-e47e", "Persons Traveling Into US at Land Borders and Airports",           "Passenger Travel",
  "cnzt-d3cb", "Top 25 Busiest Amtrak Stations",                                   "Passenger Travel",
  "9bvt-h7tb", "Amtrak Stations Along the Northeast Corridor",                     "Passenger Travel",
  "ex2n-fgi7", "Commuting Public Transportation in Metro Areas",                   "Passenger Travel",
  "va72-z8hz", "Local Area Transportation Characteristics 2017",                   "Passenger Travel",
  "frme-pssc", "Local Area Transportation Characteristics 2009",                   "Passenger Travel",
  # --- Bikeshare / Micromobility ---
  "6cfa-ipzd", "Docked Bikeshare Ridership",                                       "Bikeshare",
  "w3m5-t2w3", "Docked Bikeshare Ridership by System/Month/Station",              "Bikeshare",
  "g3h6-334u", "Docked Bikeshare Ridership by System/Month/Day",                  "Bikeshare",
  "j6uy-twhg", "Docked Bikeshare Ridership by System/Month/Hour",                 "Bikeshare",
  "cvai-skrf", "Docked Bikeshare Ridership by System/Year/Month",                 "Bikeshare",
  "7m5x-ubud", "Locations of Docked Bikeshare Stations by System/Year",           "Bikeshare",
  "cqdc-cm7d", "Bikeshare and E-scooter Systems by Year/City",                    "Bikeshare",
  "8cjz-h8bz", "Bikeshare and E-scooter Systems 2020 by Month/Status",           "Bikeshare",
  "h2kz-rw8a", "National Expenditures on Bicycles and Accessories",               "Bikeshare",
  "xjbt-2jz9", "Bikeshare Isochrones - Stations Accessible",                      "Bikeshare",
  # --- Water Transportation ---
  "iiy2-kmkn", "Container Ships Awaiting Dock at US Ports (Weekly)",               "Maritime",
  "iahn-a7j4", "TEU Handled by Select US Container Ports (Monthly)",               "Maritime",
  "x6rh-cpwu", "TEU Capacity of Container Ships at US Ports (Monthly)",            "Maritime",
  "jt8n-q46j", "Monthly Downbound Barge Grain Tonnage at Lock 27",                "Maritime",
  "c7tj-sc2j", "Top 25 Ports by Dry Bulk Tonnage",                                 "Maritime",
  "swpm-impx", "Great Lakes St Lawrence Seaway Performance",                       "Maritime",
  "d38r-yvh5", "National Census of Ferry Operators 2018",                          "Maritime",
  "rjzd-p9xx", "Port Maritime 2020",                                               "Maritime",
  # --- Noise ---
  "iqwy-f8z4", "Population Exposed to Aviation Noise 2016/2018",                  "Environment",
  "ppe3-tvgj", "Population Exposed to Road Noise 2016/2018",                      "Environment",
  # --- HPMS ---
  "x2p2-83w8", "HPMS Statewide Summaries 2020",                                   "HPMS",
  "u7gh-36sf", "HPMS Statewide Summaries 2018",                                   "HPMS",
  "ptrd-vf53", "HPMS Statewide Summaries 2019",                                   "HPMS",
  "vj2n-3mx5", "HPMS Urban Area Summaries 2022",                                  "HPMS",
  "vhn6-qyw4", "HPMS Urban Area Summaries 2020",                                  "HPMS",
  "pwi8-yn3a", "HPMS Urban Area Summaries 2021",                                  "HPMS",
  "55v8-hdzv", "HPMS Urban Area Summaries 2023",                                  "HPMS",
  "eq35-vm98", "HPMS County Summary 2024",                                         "HPMS",
  "d84g-cswr", "HPMS Non-Federal Aid Summary 2024",                               "HPMS",
  "pxa9-b968", "HPMS Non-Federal Aid Summary 2023",                               "HPMS",
  "n3wf-ffbz", "HPMS Non-Federal Aid Summary 2021",                               "HPMS",
  "9z7c-6tf9", "HPMS Non-Federal Aid Summary 2022",                               "HPMS",
  "trfk-mhda", "HPMS Roadway Events Data 2021",                                   "HPMS",
  "fsfq-kawt", "HPMS Roadway Events Data 2022",                                   "HPMS",
  "8gz6-23ex", "HPMS Roadway Events Data 2023",                                   "HPMS",
  "4wpf-u82y", "HPMS Roadway Events Data 2024",                                   "HPMS",
  "ruqx-86ui", "HPMS Roadway Sample 2023",                                         "HPMS",
  "qikb-qkpf", "HPMS Roadway Sample 2022",                                        "HPMS",
  "phws-6zwr", "HPMS Roadway Sample 2020",                                        "HPMS",
  "c359-yvt8", "HPMS Roadway Sample 2021",                                        "HPMS",
  "bpg8-byn2", "HPMS Roadway Sample 2019",                                        "HPMS",
  "7fpa-e6hd", "HPMS Roadway Sample 2024",                                        "HPMS",
  "39wk-vhst", "HPMS Roadway Sample 2018",                                        "HPMS",
  "q8hw-cdnd", "HPMS Roadway Sections 2020",                                      "HPMS",
  "42um-tgh5", "HPMS Spatial All Sections 2024",                                   "HPMS",
  "dgnj-ses5", "HPMS County Summary 2021",                                         "HPMS",
  "v8xa-dgz4", "HPMS County Summary 2022",                                        "HPMS",
  "bkqm-dpy8", "HPMS County Summary 2020",                                        "HPMS",
  "8j5p-a5aj", "HPMS County Summary 2023",                                        "HPMS",
  "9wrh-mnib", "HPMS Urban Summary 2018",                                          "HPMS",
  "7b9r-bf38", "HPMS Urban Summary 2019",                                          "HPMS",
  # --- Intercity Access ---
  "xnub-2sc4", "Intercity Air, Bus, and Rail Facilities",                          "Intercity Access",
  "m2bh-93w3", "Access to Intercity Transportation in Rural Areas",                "Intercity Access",
  # --- AI Inventory ---
  "anj8-k6f5", "DOT AI Use Cases Inventory",                                       "Other",
  # --- Connected Vehicle / ITS ---
  "69qe-yiui", "Work Zone Data Feed Registry",                                     "ITS",
  "gu5j-r7xj", "NYC CV Pilot IE Processed Data",                                  "ITS",
  "vxmm-9bi5", "NYC CV Pilot EVENT Data One Month Sample",                         "ITS",
  # --- FMCSA Motor Carriers (text/download only -- not SODA) ---
  # "6qg9-x4f8", "Carrier", "Motor Carriers",  # text download
  # --- Seaway ---
  "swpm-impx", "Great Lakes St Lawrence Seaway Performance",                       "Maritime"
)

# Remove duplicate swpm-impx entry
.dot_catalog <- .dot_catalog |> distinct(view_id, .keep_all = TRUE)


# == Discovery =================================================================

#' List all cataloged DOT datasets
#'
#' Returns a tibble of all cataloged SODA-accessible datasets on
#' data.transportation.gov with view IDs, names, and categories.
#'
#' @return tibble: view_id, name, category
#' @export
dot_list <- function() {
  .dot_catalog
}

#' Search DOT datasets by keyword
#'
#' Searches the catalog by name or category.
#'
#' @param query Character search term (case-insensitive grep)
#' @return tibble: view_id, name, category
#' @export
dot_search <- function(query) {
  pattern <- tolower(query)
  .dot_catalog |>
    filter(grepl(pattern, tolower(name)) | grepl(pattern, tolower(category)))
}

# == Generic SODA accessor =====================================================

#' Query any DOT Socrata dataset by view ID
#'
#' Generic SODA query against data.transportation.gov. Supports SoQL
#' parameters: where, select, group, order, full-text q, limit, offset.
#'
#' @param view_id   4x4 Socrata view ID (e.g. "keg4-3bc2")
#' @param where     SoQL WHERE clause
#' @param select    SoQL SELECT clause
#' @param group     SoQL GROUP BY clause
#' @param order     SoQL ORDER BY clause
#' @param q         Full-text search string
#' @param limit     Max rows per page (default 1000, max 50000)
#' @param offset    Starting offset
#' @param max_results  Overall max rows to return (pages automatically)
#' @param token     Optional Socrata app token
#' @return tibble of results
#' @export
dot_view <- function(view_id, where = NULL, select = NULL, group = NULL,
                     order = NULL, q = NULL, limit = 1000, offset = 0,
                     max_results = NULL, token = NULL) {
  .dot_soda(view_id, where = where, select = select, group = group,
            order = order, q = q, limit = limit, offset = offset,
            token = token, max_results = max_results)
}

# == Named domain functions ====================================================

# --- Vehicle Safety / NHTSA Recalls ---

#' NHTSA vehicle recall data
#'
#' Returns NHTSA recall campaigns from data.transportation.gov.
#'
#' @param manufacturer Filter by manufacturer name (partial match, case-insensitive)
#' @param component    Filter by component (e.g. "AIR BAGS", "BRAKES")
#' @param recall_type  "Vehicle", "Tire", "Equipment", or "Child Seat"
#' @param limit        Max rows (default 1000)
#' @param max_results  Overall max rows
#' @return tibble of recall records
#' @export
dot_recalls <- function(manufacturer = NULL, component = NULL,
                        recall_type = NULL, limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(manufacturer)) clauses <- c(clauses, paste0("upper(manufacturer) like '%", toupper(manufacturer), "%'"))
  if (!is.null(component))    clauses <- c(clauses, paste0("upper(component) like '%", toupper(component), "%'"))
  if (!is.null(recall_type))  clauses <- c(clauses, paste0("recall_type='", recall_type, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("6axg-epim", where = where, order = "report_received_date DESC",
            limit = limit, max_results = max_results)
}

# --- Railroad Safety ---

#' Railroad accidents/incidents (Form 54)
#'
#' Rail equipment accidents including derailments, collisions, fires.
#'
#' @param state   State abbreviation filter (e.g. "TX", "CA")
#' @param year    Year filter
#' @param type    Accident type (e.g. "Derailment", "Collision")
#' @param limit   Max rows per page
#' @param max_results Overall max
#' @return tibble
#' @export
dot_rail_accidents <- function(state = NULL, year = NULL, type = NULL,
                               limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("stateabbr='", toupper(state), "'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  if (!is.null(type))  clauses <- c(clauses, paste0("upper(accidenttype) like '%", toupper(type), "%'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("85tf-25kj", where = where, order = "date DESC",
            limit = limit, max_results = max_results)
}

#' Highway-rail grade crossing incidents (Form 57)
#'
#' Collisions between trains and highway users at grade crossings.
#'
#' @param state   State abbreviation filter
#' @param year    Year filter
#' @param limit   Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_crossing_incidents <- function(state = NULL, year = NULL,
                                   limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("upper(statename) like '%", toupper(state), "%'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("7wn6-i5b9", where = where, limit = limit, max_results = max_results)
}

#' Railroad casualty data (Form 55a)
#'
#' Reportable fatalities, injuries, and illnesses from railroad operations.
#'
#' @param year  Year filter
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_rail_casualties <- function(year = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(year)) paste0("year='", year, "'") else NULL
  .dot_soda("rash-pd2d", where = where, limit = limit, max_results = max_results)
}

# --- Transit ---

#' Monthly transit time series (NTD)
#'
#' Modal service data and safety/security from the National Transit Database.
#' Includes ridership, revenue miles/hours, collisions, fatalities by agency/mode.
#'
#' @param agency  Agency name filter (partial match)
#' @param mode    Mode code (e.g. "HR" heavy rail, "MB" bus, "LR" light rail)
#' @param year    Year filter
#' @param limit   Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_transit <- function(agency = NULL, mode = NULL, year = NULL,
                        limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(agency)) clauses <- c(clauses, paste0("upper(agency) like '%", toupper(agency), "%'"))
  if (!is.null(mode))   clauses <- c(clauses, paste0("mode='", toupper(mode), "'"))
  if (!is.null(year))   clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("5ti2-5uiv", where = where, order = "month_year DESC",
            limit = limit, max_results = max_results)
}

#' Major transit safety events (FTA)
#'
#' @param year Year filter
#' @param event_type Event type filter (e.g. "Collision", "Derailment")
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_transit_safety <- function(year = NULL, event_type = NULL,
                               limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(year))       clauses <- c(clauses, paste0("year='", year, "'"))
  if (!is.null(event_type)) clauses <- c(clauses, paste0("upper(event_type) like '%", toupper(event_type), "%'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("9ivb-8ae9", where = where, order = "incident_date DESC",
            limit = limit, max_results = max_results)
}

# --- Border Crossing ---

#' Border crossing entry data
#'
#' Monthly inbound crossings at US-Canada and US-Mexico borders by port.
#' Covers trucks, trains, containers, buses, personal vehicles, pedestrians.
#'
#' @param state   State filter
#' @param border  "US-Canada Border" or "US-Mexico Border"
#' @param measure  Measure type (e.g. "Trucks", "Personal Vehicles", "Pedestrians")
#' @param year    Year filter (e.g. 2024)
#' @param limit   Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_border_crossings <- function(state = NULL, border = NULL, measure = NULL,
                                 year = NULL, limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state))   clauses <- c(clauses, paste0("state='", state, "'"))
  if (!is.null(border))  clauses <- c(clauses, paste0("border='", border, "'"))
  if (!is.null(measure)) clauses <- c(clauses, paste0("measure='", measure, "'"))
  if (!is.null(year))    clauses <- c(clauses, paste0("date_extract_y(date)=", year))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("keg4-3bc2", where = where, order = "date DESC",
            limit = limit, max_results = max_results)
}

# --- Supply Chain & Freight ---

#' Supply chain and freight indicators
#'
#' Weekly/monthly supply chain indicators from DOT interagency working group.
#'
#' @param indicator Filter by indicator name (partial match)
#' @param year      Year filter
#' @param limit     Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_freight_indicators <- function(indicator = NULL, year = NULL,
                                   limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(indicator)) clauses <- c(clauses, paste0("upper(indicator) like '%", toupper(indicator), "%'"))
  if (!is.null(year))      clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("y5ut-ibwt", where = where, order = "date DESC",
            limit = limit, max_results = max_results)
}

# --- Aviation ---

#' Aviation segment data by carrier (T100)
#'
#' Commercial air passengers, seats, freight, and mail by carrier.
#'
#' @param carrier Carrier name filter (partial match)
#' @param year    Year filter
#' @param limit   Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_aviation_carriers <- function(carrier = NULL, year = NULL,
                                  limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(carrier)) clauses <- c(clauses, paste0("upper(carrier_name) like '%", toupper(carrier), "%'"))
  if (!is.null(year))    clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  # Uses gjp5-nh2u (average airfare by carrier) which has carrier names
  .dot_soda("gjp5-nh2u", where = where, limit = limit, max_results = max_results)
}

#' Average domestic air fares by quarter
#'
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_airfares <- function(limit = 1000, max_results = NULL) {
  .dot_soda("h77j-murt", order = "year DESC", limit = limit, max_results = max_results)
}

#' Consumer airfare report - top city-pair markets
#'
#' Detailed fare data for top 1000 city-pair markets.
#'
#' @param city City name filter (partial match)
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_airfares_citypair <- function(city = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(city)) paste0("upper(city) like '%", toupper(city), "%'") else NULL
  .dot_soda("wqw2-rjgd", where = where, limit = limit, max_results = max_results)
}

# --- Highway Statistics ---

#' Public road length by functional system and state (HM-20)
#'
#' Annual road miles by functional system for all states.
#'
#' @param state State name filter
#' @param year  Year filter
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_road_length <- function(state = NULL, year = NULL,
                            limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", state, "'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("3hqe-sh4k", where = where, limit = limit, max_results = max_results)
}

#' Vehicle miles traveled by functional system and state (VM-2)
#'
#' @param state State name filter
#' @param year  Year filter
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_vmt <- function(state = NULL, year = NULL,
                    limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("upper(state) like '%", toupper(state), "%'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("nps9-3pm2", where = where, limit = limit, max_results = max_results)
}

#' National Highway Construction Cost Index (NHCCI)
#'
#' Price index tracking highway construction cost changes.
#'
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_construction_cost <- function(limit = 1000, max_results = NULL) {
  .dot_soda("r94d-n4f9", order = "quarter DESC", limit = limit, max_results = max_results)
}

# --- Drivers & Vehicle Registration ---

#' Licensed drivers by state, sex, and age group (DL-22)
#'
#' @param state State name filter
#' @param year  Year filter
#' @param sex   "Male" or "Female"
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_licensed_drivers <- function(state = NULL, year = NULL, sex = NULL,
                                 limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", state, "'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  if (!is.null(sex))   clauses <- c(clauses, paste0("sex='", sex, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("xfkb-3bxx", where = where, limit = limit, max_results = max_results)
}

#' Motor vehicle registrations by state (MV-1)
#'
#' Annual state-reported registration data since 1900.
#'
#' @param state State name filter
#' @param year  Year filter
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_vehicle_registrations <- function(state = NULL, year = NULL,
                                      limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", state, "'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("4dra-vxq7", where = where, limit = limit, max_results = max_results)
}

# --- Transportation Economics ---

#' Transportation Services Index (TSI)
#'
#' Monthly index measuring freight and passenger movement (seasonally adjusted).
#'
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_tsi <- function(limit = 1000, max_results = NULL) {
  .dot_soda("bw6n-ddqk", order = "obs_date DESC", limit = limit, max_results = max_results)
}

#' Transportation Economic Trends (TET)
#'
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_economic_trends <- function(limit = 1000, max_results = NULL) {
  .dot_soda("tcq5-4pgu", limit = limit, max_results = max_results)
}

# --- Motor Fuel ---

#' Motor fuel tax rates by state
#'
#' Tax rates on gasoline, diesel, and alternative fuels by state.
#'
#' @param state State name filter
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_fuel_tax_rates <- function(state = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  .dot_soda("e5cn-ri8q", where = where, limit = limit, max_results = max_results)
}

#' Monthly motor fuel sales by state
#'
#' @param state State name filter
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_fuel_sales <- function(state = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(state)) paste0("upper(state) like '%", toupper(state), "%'") else NULL
  .dot_soda("kbvr-tyu5", where = where, limit = limit, max_results = max_results)
}

# --- Maritime ---

#' Great Lakes St Lawrence Seaway performance
#'
#' Monthly performance statistics for the Great Lakes-St. Lawrence Seaway.
#'
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_seaway <- function(limit = 1000, max_results = NULL) {
  .dot_soda("swpm-impx", limit = limit, max_results = max_results)
}

#' Container ship port activity
#'
#' Weekly/monthly container ship data at US ports (ships awaiting,
#' TEU throughput, TEU capacity).
#'
#' @param dataset One of "waiting" (ships awaiting dock), "throughput" (TEU handled),
#'   "capacity" (TEU capacity). Default "waiting".
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_container_ports <- function(dataset = c("waiting", "throughput", "capacity"),
                                limit = 1000, max_results = NULL) {
  dataset <- match.arg(dataset)
  vid <- switch(dataset,
    waiting    = "iiy2-kmkn",
    throughput = "iahn-a7j4",
    capacity   = "x6rh-cpwu"
  )
  .dot_soda(vid, limit = limit, max_results = max_results)
}

# --- Bikeshare ---

#' Docked bikeshare ridership data
#'
#' @param system  Bikeshare system name filter (partial match)
#' @param year    Year filter
#' @param limit   Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_bikeshare <- function(system = NULL, year = NULL,
                          limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(system)) clauses <- c(clauses, paste0("upper(system) like '%", toupper(system), "%'"))
  if (!is.null(year))   clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("cvai-skrf", where = where, limit = limit, max_results = max_results)
}

# --- Highway Finance ---

#' Highway Trust Fund status (FE-210)
#'
#' @param limit Max rows
#' @param max_results Overall max
#' @return tibble
#' @export
dot_highway_trust_fund <- function(limit = 1000, max_results = NULL) {
  .dot_soda("taz8-hut2", limit = limit, max_results = max_results)
}

# == Context / source reader ===================================================

#' Return DOT client source for LLM context
#'
#' Reads its own source file and returns it as a single string.
#'
#' @return Character string: full source code of this client
#' @export
dot_context <- function() {
  src <- tryCatch({
    fn <- sys.frame(sys.nframe())$ofile
    if (is.null(fn)) {
      fn <- this_file <- (function() {
        for (i in seq_len(sys.nframe())) {
          f <- sys.frame(i)$ofile
          if (!is.null(f)) return(f)
        }
        NULL
      })()
    }
    if (!is.null(fn)) readLines(fn, warn = FALSE)
    else NULL
  }, error = function(e) NULL)

  if (is.null(src)) {
    pkg <- system.file("source", "transportation.R", package = "transportation.gov")
    if (nzchar(pkg)) src <- readLines(pkg, warn = FALSE)
  }
  if (is.null(src)) return("Source not available.")
  paste(src, collapse = "\n")
}
