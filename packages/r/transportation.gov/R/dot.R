# transportation.gov.R
# Self-contained data.transportation.gov (US DOT) Socrata client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data). Optional app token reduces throttling.
# Base: https://data.transportation.gov (Socrata SODA, follows redirects)
# Docs: https://dev.socrata.com/docs/queries/


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

#' List all cataloged US DOT datasets
#'
#' Returns a catalog of 120+ SODA-accessible datasets on
#' data.transportation.gov covering vehicle safety recalls, railroad
#' safety, transit ridership, aviation, border crossings, highway
#' statistics, motor fuel, and more. Use this to discover available
#' datasets and their Socrata view IDs for use with \code{dot_view()}.
#'
#' @return A tibble with columns:
#'   - view_id (character): Socrata 4x4 dataset identifier (e.g. "6axg-epim")
#'   - name (character): Dataset title
#'   - category (character): Topic area (e.g. "Vehicle Safety", "Transit",
#'     "Aviation", "Border Crossing", "Highways", "Motor Fuel", etc.)
#' @examples
#' \dontrun{
#' # Browse all datasets
#' dot_list()
#'
#' # Filter to aviation datasets
#' dot_list() |> dplyr::filter(category == "Aviation")
#' }
#' @export
dot_list <- function() {
  .dot_catalog
}

#' Search US DOT datasets by keyword
#'
#' Searches the dataset catalog by name or category using case-insensitive
#' partial matching. Use this when you know the topic but not the exact
#' dataset name or view ID.
#'
#' @param query Character search term (case-insensitive). Examples: "recall",
#'   "rail", "transit", "aviation", "border", "bikeshare", "fuel", "highway".
#' @return A tibble with columns: view_id, name, category. Empty tibble if
#'   no matches found.
#' @examples
#' \dontrun{
#' # Find railroad-related datasets
#' dot_search("rail")
#'
#' # Find all border crossing data
#' dot_search("border")
#' }
#' @export
dot_search <- function(query) {
  pattern <- tolower(query)
  .dot_catalog |>
    filter(grepl(pattern, tolower(name)) | grepl(pattern, tolower(category)))
}

# == Generic SODA accessor =====================================================

#' Query any DOT Socrata dataset by view ID
#'
#' Generic escape hatch for querying any data.transportation.gov dataset
#' by its Socrata view ID. Supports full SoQL (Socrata Query Language)
#' including WHERE filters, SELECT projections, GROUP BY aggregation,
#' ORDER BY sorting, and full-text search. Automatically paginates to
#' fetch up to \code{max_results} rows. Use \code{dot_list()} to find
#' view IDs for datasets not covered by named functions.
#'
#' @param view_id Socrata 4x4 view identifier (e.g. "keg4-3bc2" for border
#'   crossings, "6axg-epim" for recalls). Use \code{dot_list()} to browse.
#' @param where SoQL WHERE clause for filtering (e.g. "state='TX'",
#'   "year='2023'"). NULL for no filter.
#' @param select SoQL SELECT clause for column projection (e.g.
#'   "state, SUM(value)"). NULL for all columns.
#' @param group SoQL GROUP BY clause for aggregation (e.g. "state").
#'   NULL for no grouping.
#' @param order SoQL ORDER BY clause (e.g. "date DESC"). NULL for default order.
#' @param q Full-text search string. NULL for no text search.
#' @param limit Max rows per SODA page (default 1000, max 50000).
#' @param offset Starting row offset for pagination (default 0).
#' @param max_results Overall maximum rows to return across all pages.
#'   NULL for single-page result. When set, automatically paginates.
#' @param token Optional Socrata app token to increase rate limits.
#'   NULL for unauthenticated access (~1000 req/hour).
#' @return A tibble of query results. Column names and types depend on
#'   the specific dataset.
#' @examples
#' \dontrun{
#' # Get 50 border crossing records for Texas
#' dot_view("keg4-3bc2", where = "state='Texas'", limit = 50)
#'
#' # Aggregate vehicle miles traveled by state
#' dot_view("nps9-3pm2", select = "state, SUM(total) as total_vmt",
#'          group = "state", order = "total_vmt DESC")
#' }
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

#' Fetch NHTSA vehicle recall campaigns
#'
#' Returns National Highway Traffic Safety Administration (NHTSA) recall
#' data including manufacturer, component, defect description, and remedy.
#' Covers vehicles, tires, equipment, and child seats. Ordered by most
#' recent report received date.
#'
#' @param manufacturer Filter by manufacturer name (partial match,
#'   case-insensitive). Examples: "FORD", "TOYOTA", "TESLA".
#' @param component Filter by vehicle component (partial match,
#'   case-insensitive). Examples: "AIR BAGS", "BRAKES", "STEERING",
#'   "ELECTRICAL", "FUEL SYSTEM".
#' @param recall_type Type of recall: "Vehicle", "Tire", "Equipment", or
#'   "Child Seat". NULL for all types.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows to return. NULL for single page.
#' @return A tibble with columns including: record_id, campaign_number,
#'   manufacturer, component, recall_type, defect_summary, consequence_summary,
#'   remedy, report_received_date, and more.
#' @examples
#' \dontrun{
#' # Get recent Tesla recalls
#' dot_recalls(manufacturer = "TESLA", limit = 50)
#'
#' # Find all air bag recalls
#' dot_recalls(component = "AIR BAGS", limit = 100)
#'
#' # Get tire recalls only
#' dot_recalls(recall_type = "Tire", limit = 50)
#' }
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

#' Fetch FRA railroad accident/incident data (Form 54)
#'
#' Returns Federal Railroad Administration (FRA) rail equipment accident
#' and incident data from Form 54 reports. Covers derailments, collisions,
#' fires, explosions, and other rail equipment events. Includes damage
#' estimates, casualties, and hazmat involvement. Ordered by date descending.
#'
#' @param state Two-letter state abbreviation filter (e.g. "TX", "CA", "OH").
#'   NULL for all states.
#' @param year Year filter (e.g. 2023, 2024). NULL for all years.
#' @param type Accident type filter (partial match, case-insensitive).
#'   Examples: "Derailment", "Collision", "Fire", "Explosion". NULL for all.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: date, stateabbr, accidenttype,
#'   railroad, temperature, visibility, hazmatcars, casualties, damage,
#'   and many more accident detail fields.
#' @examples
#' \dontrun{
#' # Get Ohio derailments
#' dot_rail_accidents(state = "OH", type = "Derailment", limit = 50)
#'
#' # Get all 2023 rail accidents
#' dot_rail_accidents(year = 2023, max_results = 5000)
#' }
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

#' Fetch highway-rail grade crossing incidents (Form 57)
#'
#' Returns FRA data on collisions between trains and highway users
#' (vehicles, pedestrians) at grade crossings. Includes location,
#' warning device type, train speed, casualties, and circumstance details.
#'
#' @param state State name filter (partial match, case-insensitive).
#'   Examples: "TEXAS", "CALIFORNIA". NULL for all states.
#' @param year Year filter (e.g. 2023). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: date, statename, railroad,
#'   crossing_id, train_speed, highway_user_action, warning_device,
#'   casualties, and position details.
#' @examples
#' \dontrun{
#' # Get Texas grade crossing incidents
#' dot_crossing_incidents(state = "TEXAS", limit = 100)
#'
#' # Get 2023 incidents nationwide
#' dot_crossing_incidents(year = 2023, max_results = 5000)
#' }
#' @export
dot_crossing_incidents <- function(state = NULL, year = NULL,
                                   limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("upper(statename) like '%", toupper(state), "%'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("7wn6-i5b9", where = where, limit = limit, max_results = max_results)
}

#' Fetch FRA railroad casualty data (Form 55a)
#'
#' Returns reportable fatalities, injuries, and occupational illnesses
#' from railroad operations. Data from the FRA Injury/Illness Summary
#' casualty supplement (Form 55a).
#'
#' @param year Year filter (e.g. 2023). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: year, railroad, state,
#'   casualty counts, person type, and incident details.
#' @examples
#' \dontrun{
#' # Get 2023 railroad casualties
#' dot_rail_casualties(year = 2023)
#' }
#' @export
dot_rail_casualties <- function(year = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(year)) paste0("year='", year, "'") else NULL
  .dot_soda("rash-pd2d", where = where, limit = limit, max_results = max_results)
}

# --- Transit ---

#' Fetch monthly transit ridership time series (NTD)
#'
#' Returns monthly National Transit Database (NTD) modal time series data
#' including unlinked passenger trips (ridership), vehicle revenue miles,
#' vehicle revenue hours, and safety events. Data by transit agency and
#' mode. Ordered by month/year descending.
#'
#' @param agency Transit agency name filter (partial match, case-insensitive).
#'   Examples: "MTA", "WMATA", "CHICAGO", "BAY AREA". NULL for all agencies.
#' @param mode Transit mode code filter. Common values: "HR" (heavy rail),
#'   "MB" (motor bus), "LR" (light rail), "CR" (commuter rail),
#'   "DR" (demand response), "VP" (vanpool). NULL for all modes.
#' @param year Year filter (e.g. 2023). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: agency, mode, month_year, upt
#'   (unlinked passenger trips), vrm (vehicle revenue miles), vrh (vehicle
#'   revenue hours), and safety/security event counts.
#' @examples
#' \dontrun{
#' # Get NYC MTA heavy rail ridership
#' dot_transit(agency = "MTA", mode = "HR", limit = 100)
#'
#' # Get all bus ridership for 2023
#' dot_transit(mode = "MB", year = 2023, max_results = 5000)
#' }
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

#' Fetch major transit safety events (FTA)
#'
#' Returns Federal Transit Administration (FTA) major safety event data
#' including collisions, derailments, fires, security events, and
#' fatalities/injuries at transit agencies. Ordered by incident date
#' descending.
#'
#' @param year Year filter (e.g. 2023). NULL for all years.
#' @param event_type Event type filter (partial match, case-insensitive).
#'   Examples: "Collision", "Derailment", "Fire", "Security". NULL for all.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: incident_date, agency, mode,
#'   event_type, fatalities, injuries, and event description.
#' @examples
#' \dontrun{
#' # Get 2023 transit collisions
#' dot_transit_safety(year = 2023, event_type = "Collision")
#'
#' # Get all recent transit safety events
#' dot_transit_safety(limit = 200)
#' }
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

#' Fetch US border crossing entry data
#'
#' Returns monthly inbound crossing counts at US-Canada and US-Mexico
#' border ports of entry. Covers trucks, trains, containers, buses,
#' personal vehicles, and pedestrians. Data from Bureau of Transportation
#' Statistics (BTS). Ordered by date descending.
#'
#' @param state State name filter (e.g. "Texas", "California", "New York").
#'   NULL for all states.
#' @param border Border filter: "US-Canada Border" or "US-Mexico Border".
#'   NULL for both borders.
#' @param measure Crossing measure type. Examples: "Trucks", "Truck Containers Full",
#'   "Personal Vehicles", "Personal Vehicle Passengers", "Pedestrians",
#'   "Trains", "Rail Containers Full", "Buses", "Bus Passengers".
#'   NULL for all measures.
#' @param year Year filter (e.g. 2024). Uses SoQL date_extract_y().
#'   NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: port_name, state, border,
#'   date, measure, value, latitude, longitude.
#' @examples
#' \dontrun{
#' # Get truck crossings at US-Mexico border
#' dot_border_crossings(border = "US-Mexico Border", measure = "Trucks")
#'
#' # Get all Texas border crossings for 2024
#' dot_border_crossings(state = "Texas", year = 2024, max_results = 5000)
#' }
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

#' Fetch supply chain and freight indicators
#'
#' Returns weekly and monthly supply chain performance indicators from
#' the DOT Supply Chain Data initiative. Covers container throughput,
#' rail volumes, trucking conditions, and other freight metrics. Ordered
#' by date descending.
#'
#' @param indicator Filter by indicator name (partial match, case-insensitive).
#'   Examples: "container", "rail", "truck", "port". NULL for all indicators.
#' @param year Year filter (e.g. 2024). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: indicator, date, value, source,
#'   frequency, and trend metrics.
#' @examples
#' \dontrun{
#' # Get container-related supply chain indicators
#' dot_freight_indicators(indicator = "container", limit = 100)
#'
#' # Get all 2024 freight indicators
#' dot_freight_indicators(year = 2024, max_results = 5000)
#' }
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

#' Fetch average airfare data by carrier
#'
#' Returns average domestic airfare data by airline carrier from the
#' BTS Consumer Airfare Report. Includes carrier name, average fare,
#' and passenger counts. Use \code{dot_airfares()} for quarterly averages
#' or \code{dot_airfares_citypair()} for route-level data.
#'
#' @param carrier Airline carrier name filter (partial match, case-insensitive).
#'   Examples: "DELTA", "UNITED", "SOUTHWEST", "AMERICAN". NULL for all.
#' @param year Year filter (e.g. 2023). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: carrier_name, year, quarter,
#'   average_fare, passenger_count, and market share data.
#' @examples
#' \dontrun{
#' # Get Delta Air Lines fare data
#' dot_aviation_carriers(carrier = "DELTA")
#'
#' # Compare all carriers for 2023
#' dot_aviation_carriers(year = 2023)
#' }
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

#' Fetch average domestic air fares by quarter
#'
#' Returns national average domestic airfare data by quarter from the
#' BTS Average Air Fare report. Tracks fare trends over time. Ordered
#' by year descending. For carrier-level data use \code{dot_aviation_carriers()};
#' for city-pair data use \code{dot_airfares_citypair()}.
#'
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: year, quarter, average_fare,
#'   and inflation-adjusted fare data.
#' @examples
#' \dontrun{
#' # Get quarterly airfare trends
#' dot_airfares(limit = 100)
#' }
#' @export
dot_airfares <- function(limit = 1000, max_results = NULL) {
  .dot_soda("h77j-murt", order = "year DESC", limit = limit, max_results = max_results)
}

#' Fetch consumer airfare data for top city-pair markets
#'
#' Returns detailed fare data for the top 1000 domestic city-pair markets
#' from the BTS Consumer Airfare Report (Table 2). Includes average fare,
#' passenger volume, distance, and year-over-year changes.
#'
#' @param city City name filter (partial match, case-insensitive). Matches
#'   against origin or destination city. Examples: "NEW YORK", "CHICAGO",
#'   "LOS ANGELES". NULL for all city pairs.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: city, city2, nonstop_miles,
#'   passengers, average_fare, and quarter/year.
#' @examples
#' \dontrun{
#' # Get airfares for routes involving Chicago
#' dot_airfares_citypair(city = "CHICAGO", limit = 100)
#'
#' # Get all top city-pair market fares
#' dot_airfares_citypair(max_results = 5000)
#' }
#' @export
dot_airfares_citypair <- function(city = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(city)) paste0("upper(city) like '%", toupper(city), "%'") else NULL
  .dot_soda("wqw2-rjgd", where = where, limit = limit, max_results = max_results)
}

# --- Highway Statistics ---

#' Fetch public road length by functional system (HM-20)
#'
#' Returns annual road miles by functional system (Interstate, Arterial,
#' Collector, Local) for all states from FHWA Highway Statistics Table
#' HM-20. Use for state-level road infrastructure comparisons.
#'
#' @param state State name filter (e.g. "Texas", "California"). NULL for all.
#' @param year Year filter (e.g. 2022). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: state, year, and road miles
#'   by functional system (interstate, other_arterial, collector, local).
#' @examples
#' \dontrun{
#' # Get Texas road miles
#' dot_road_length(state = "Texas")
#'
#' # Compare states for 2022
#' dot_road_length(year = 2022)
#' }
#' @export
dot_road_length <- function(state = NULL, year = NULL,
                            limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("state='", state, "'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("3hqe-sh4k", where = where, limit = limit, max_results = max_results)
}

#' Fetch vehicle miles traveled by functional system (VM-2)
#'
#' Returns annual vehicle miles traveled (VMT) by functional system
#' and state from FHWA Highway Statistics Table VM-2. VMT is a key
#' metric for traffic volume, safety analysis, and infrastructure
#' planning.
#'
#' @param state State name filter (partial match, case-insensitive).
#'   Examples: "TEXAS", "CALIFORNIA". NULL for all states.
#' @param year Year filter (e.g. 2022). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: state, year, and VMT by
#'   functional system (interstate, arterial, collector, local) in
#'   millions of miles.
#' @examples
#' \dontrun{
#' # Get Texas VMT
#' dot_vmt(state = "TEXAS")
#'
#' # Get 2022 VMT for all states
#' dot_vmt(year = 2022)
#' }
#' @export
dot_vmt <- function(state = NULL, year = NULL,
                    limit = 1000, max_results = NULL) {
  clauses <- c()
  if (!is.null(state)) clauses <- c(clauses, paste0("upper(state) like '%", toupper(state), "%'"))
  if (!is.null(year))  clauses <- c(clauses, paste0("year='", year, "'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  .dot_soda("nps9-3pm2", where = where, limit = limit, max_results = max_results)
}

#' Fetch National Highway Construction Cost Index (NHCCI)
#'
#' Returns the NHCCI price index tracking changes in highway construction
#' costs over time. Based on competitive bid prices from FHWA-funded
#' projects. Useful for inflation adjustment of highway spending.
#' Ordered by quarter descending.
#'
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: quarter, year, index values,
#'   and percent change.
#' @examples
#' \dontrun{
#' # Get construction cost index
#' dot_construction_cost(limit = 50)
#' }
#' @export
dot_construction_cost <- function(limit = 1000, max_results = NULL) {
  .dot_soda("r94d-n4f9", order = "quarter DESC", limit = limit, max_results = max_results)
}

# --- Drivers & Vehicle Registration ---

#' Fetch licensed drivers by state, sex, and age group (DL-22)
#'
#' Returns FHWA data on licensed drivers broken down by state, sex, and
#' age group from Highway Statistics Table DL-22. Use for demographic
#' analysis of the driving population.
#'
#' @param state State name filter (e.g. "Texas"). NULL for all states.
#' @param year Year filter (e.g. 2022). NULL for all years.
#' @param sex Sex filter: "Male" or "Female". NULL for both.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: state, year, sex, age_group,
#'   and licensed_drivers count.
#' @examples
#' \dontrun{
#' # Get Texas driver demographics
#' dot_licensed_drivers(state = "Texas", year = 2022)
#'
#' # Compare male vs female drivers nationwide
#' dot_licensed_drivers(year = 2022) |>
#'   dplyr::group_by(sex) |>
#'   dplyr::summarise(total = sum(as.numeric(licensed_drivers), na.rm = TRUE))
#' }
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

#' Fetch motor vehicle registrations by state (MV-1)
#'
#' Returns annual state-reported motor vehicle registration data from
#' FHWA Highway Statistics Table MV-1. Historical data available since
#' 1900. Includes automobiles, buses, trucks, and motorcycles.
#'
#' @param state State name filter (e.g. "Texas"). NULL for all states.
#' @param year Year filter (e.g. 2022). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: state, year, and registration
#'   counts by vehicle type (automobiles, buses, trucks, motorcycles, total).
#' @examples
#' \dontrun{
#' # Get Texas vehicle registrations
#' dot_vehicle_registrations(state = "Texas", year = 2022)
#'
#' # Historical registration trend for one state
#' dot_vehicle_registrations(state = "California", max_results = 5000)
#' }
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

#' Fetch Transportation Services Index (TSI)
#'
#' Returns the monthly BTS Transportation Services Index, a key economic
#' indicator measuring the volume of freight and passenger movement in
#' the US. Seasonally adjusted. Ordered by observation date descending.
#' Use for tracking transportation demand and economic conditions.
#'
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: obs_date, freight_index,
#'   passenger_index, combined_index, and month-over-month changes.
#' @examples
#' \dontrun{
#' # Get recent TSI data
#' dot_tsi(limit = 60)  # ~5 years of monthly data
#' }
#' @export
dot_tsi <- function(limit = 1000, max_results = NULL) {
  .dot_soda("bw6n-ddqk", order = "obs_date DESC", limit = limit, max_results = max_results)
}

#' Fetch Transportation Economic Trends (TET) data
#'
#' Returns BTS Transportation Economic Trends data including GDP
#' contribution, employment, productivity, and spending metrics for
#' the transportation sector.
#'
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with economic trend indicators for the transportation
#'   sector including GDP, employment, and productivity measures.
#' @examples
#' \dontrun{
#' # Get transportation economic trends
#' dot_economic_trends(limit = 100)
#' }
#' @export
dot_economic_trends <- function(limit = 1000, max_results = NULL) {
  .dot_soda("tcq5-4pgu", limit = limit, max_results = max_results)
}

# --- Motor Fuel ---

#' Fetch motor fuel tax rates by state
#'
#' Returns state-level tax rates on gasoline, diesel, gasohol, and
#' alternative fuels from FHWA. Useful for comparing fuel taxation
#' across states and tracking rate changes over time.
#'
#' @param state State name filter (e.g. "Texas"). NULL for all states.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: state, gasoline_tax_rate,
#'   diesel_tax_rate, gasohol_tax_rate, and other fuel type rates
#'   (cents per gallon).
#' @examples
#' \dontrun{
#' # Get all state fuel tax rates
#' dot_fuel_tax_rates()
#'
#' # Get Texas fuel taxes
#' dot_fuel_tax_rates(state = "Texas")
#' }
#' @export
dot_fuel_tax_rates <- function(state = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(state)) paste0("state='", state, "'") else NULL
  .dot_soda("e5cn-ri8q", where = where, limit = limit, max_results = max_results)
}

#' Fetch monthly motor fuel sales reported by states
#'
#' Returns monthly motor fuel sales volumes reported by states to FHWA.
#' Covers gasoline, diesel/special fuels, and alternative fuels.
#'
#' @param state State name filter (partial match, case-insensitive).
#'   NULL for all states.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: state, month, year,
#'   gasoline volume, special fuel volume (thousands of gallons).
#' @examples
#' \dontrun{
#' # Get Texas monthly fuel sales
#' dot_fuel_sales(state = "Texas", limit = 100)
#' }
#' @export
dot_fuel_sales <- function(state = NULL, limit = 1000, max_results = NULL) {
  where <- if (!is.null(state)) paste0("upper(state) like '%", toupper(state), "%'") else NULL
  .dot_soda("kbvr-tyu5", where = where, limit = limit, max_results = max_results)
}

# --- Maritime ---

#' Fetch Great Lakes-St. Lawrence Seaway performance data
#'
#' Returns monthly performance statistics for the Great Lakes-St. Lawrence
#' Seaway system including cargo tonnage, vessel transits, and commodity
#' breakdowns.
#'
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: year, month, cargo tonnage,
#'   vessel transits, and commodity type breakdowns.
#' @examples
#' \dontrun{
#' # Get seaway performance data
#' dot_seaway(limit = 100)
#' }
#' @export
dot_seaway <- function(limit = 1000, max_results = NULL) {
  .dot_soda("swpm-impx", limit = limit, max_results = max_results)
}

#' Fetch container ship port activity data
#'
#' Returns weekly or monthly container ship data at major US ports.
#' Three sub-datasets available: ships awaiting dock, TEU throughput,
#' and TEU capacity. Useful for monitoring port congestion and supply
#' chain conditions.
#'
#' @param dataset Sub-dataset to query. One of:
#'   - "waiting": Container ships awaiting dock at US ports (weekly)
#'   - "throughput": TEU handled by select US container ports (monthly)
#'   - "capacity": TEU capacity of container ships at US ports (monthly)
#'   Default "waiting".
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with port-level container ship metrics. Columns vary
#'   by sub-dataset but typically include port name, date, and volume counts.
#' @examples
#' \dontrun{
#' # Check ships waiting at US ports
#' dot_container_ports("waiting", limit = 50)
#'
#' # Get monthly TEU throughput
#' dot_container_ports("throughput", limit = 100)
#' }
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

#' Fetch docked bikeshare ridership data
#'
#' Returns ridership data for docked bikeshare systems across US cities,
#' aggregated by system, year, and month. Covers systems like Citi Bike,
#' Capital Bikeshare, Divvy, Bay Wheels, and others.
#'
#' @param system Bikeshare system name filter (partial match, case-insensitive).
#'   Examples: "CITI BIKE", "CAPITAL", "DIVVY", "BAY WHEELS". NULL for all.
#' @param year Year filter (e.g. 2023). NULL for all years.
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: system, year, month, trips,
#'   and membership type breakdowns.
#' @examples
#' \dontrun{
#' # Get Citi Bike ridership
#' dot_bikeshare(system = "CITI BIKE", limit = 100)
#'
#' # Get all bikeshare data for 2023
#' dot_bikeshare(year = 2023)
#' }
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

#' Fetch Highway Trust Fund status (FE-210)
#'
#' Returns the status of the Federal Highway Trust Fund including
#' receipts, outlays, and balance data from FHWA Table FE-210.
#' The Highway Trust Fund finances federal highway and transit programs.
#'
#' @param limit Max rows per SODA page (default 1000).
#' @param max_results Overall maximum rows. NULL for single page.
#' @return A tibble with columns including: fiscal year, receipts,
#'   transfers, outlays, and trust fund balance.
#' @examples
#' \dontrun{
#' # Get Highway Trust Fund status
#' dot_highway_trust_fund(limit = 50)
#' }
#' @export
dot_highway_trust_fund <- function(limit = 1000, max_results = NULL) {
  .dot_soda("taz8-hut2", limit = limit, max_results = max_results)
}

# == Context / source reader ===================================================

#' Get transportation.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dot_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dot_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/transportation.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "transportation.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# transportation.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# transportation.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
