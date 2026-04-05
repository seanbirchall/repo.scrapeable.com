# honolulu.gov.R
# Self-contained Honolulu Open Data (Socrata) client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public SODA API)
# Base: https://data.honolulu.gov
# Docs: https://dev.socrata.com/docs/endpoints.html

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

.ua <- "support@scrapeable.com"
.hnl_base <- "https://data.honolulu.gov"

# -- Core fetch engine ---------------------------------------------------------

.hnl_fetch <- function(url) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.hnl_fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# -- SODA query helper --------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL,
                         order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  url <- paste0(.hnl_base, "/resource/", view_id, ".json")
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  req <- req |> httr2::req_url_query(`$where` = where)
  if (!is.null(select)) req <- req |> httr2::req_url_query(`$select` = select)
  if (!is.null(order))  req <- req |> httr2::req_url_query(`$order` = order)
  if (!is.null(q))      req <- req |> httr2::req_url_query(`$q` = q)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  out <- jsonlite::fromJSON(tmp, simplifyVector = TRUE)
  if (is.data.frame(out)) tibble::as_tibble(out) else tibble::tibble()
}

# -- Catalog -------------------------------------------------------------------

.hnl_catalog <- tibble::tribble(
  ~name, ~view_id, ~description, ~category,
  "Traffic Incidents", "ykb6-n5th", "Real-time traffic incidents updated every 5 minutes", "Public Safety",
  "HPD Crime Incidents", "vg88-5rn5", "Honolulu Police Department crime incidents", "Public Safety",
  "HNL 311 Reports", "jdy7-ftwe", "311 service requests (current)", "Government",
  "Honolulu 311 Reports", "6hui-dvrh", "311 service requests (alternate feed)", "Government",

  "Honolulu 311 Archive", "pxs8-ci86", "Archived 311 service requests", "Government",
  "Building Permits", "4vab-c87q", "Building permits Jan 2005 through Jun 2025", "Business",
  "Hawaii Business Registration", "9k54-ztb8", "Hawaii business entity registrations", "Business",
  "Solar PV Permits 2019", "tywc-8mat", "Accepted value for solar PV permits issued 2019 by contractor", "Business",
  "2010-2016 Building Permits", "3fr8-2hnx", "Building permits 2010-2016", "Business",
  "2016 Building Permits", "ycwt-ujqt", "Building permits for 2016", "Business",
  "2016 By Contractor", "9cvt-tn7w", "2016 permits by contractor", "Business",
  "PV Permits 2008-2017", "yqa3-uduc", "Photovoltaic permits 2008 through June 2017", "Business",
  "EV Charging Permits", "hrf6-um48", "Electrical vehicle charging permits 2005-2017", "Business",
  "Proposed FY27 Budget", "puaw-t6sa", "Proposed budget for fiscal year 2027", "Finance",
  "Proposed FY26 Budget", "7sq2-y9vx", "Proposed budget for fiscal year 2026", "Finance",
  "Adopted FY25 Budget", "4mj2-5un8", "Adopted budget for fiscal year 2025", "Finance",
  "Proposed FY25 Budget", "mpdv-p8pt", "Proposed budget for fiscal year 2025", "Finance",
  "Adopted FY24 Budget", "p45d-q3xd", "Adopted budget for fiscal year 2024", "Finance",
  "Proposed FY24 Budget", "ejp8-2fun", "Proposed budget for fiscal year 2024", "Finance",
  "Adopted FY23 Budget", "rqrs-32nd", "Adopted budget for fiscal year 2023", "Finance",
  "Proposed FY23 Budget", "6s4t-8syz", "Proposed budget for fiscal year 2023", "Finance",
  "Adopted FY22 Budget", "k8va-67va", "Adopted budget for fiscal year 2022", "Finance",
  "Proposed FY22 Budget", "hi8x-h7cf", "Proposed budget for fiscal year 2022", "Finance",
  "Adopted FY21 Budget", "dxdd-mf4t", "Adopted budget for fiscal year 2021", "Finance",
  "Proposed FY21 Budget", "cgu9-vsgm", "Proposed budget for fiscal year 2021", "Finance",
  "Adopted FY20 Budget", "cxe8-45ut", "Adopted budget for fiscal year 2020", "Finance",
  "Proposed FY20 Budget", "k4if-3w6n", "Proposed budget for fiscal year 2020", "Finance",
  "Adopted FY19 Budget", "9nxw-kgwa", "Adopted budget for fiscal year 2019", "Finance",
  "Proposed FY19 Budget", "exh2-2d4k", "Proposed budget for fiscal year 2019", "Finance",
  "Adopted FY18 Budget", "mqjq-jn8u", "Adopted budget for fiscal year 2018", "Finance",
  "Proposed FY18 Budget", "5ens-aztz", "Proposed budget for fiscal year 2018", "Finance",
  "Adopted FY17 Budget", "fpu3-7m4z", "Adopted budget for fiscal year 2017", "Finance",
  "Proposed FY17 Budget", "e8xz-zi2e", "Proposed budget for fiscal year 2017", "Finance",
  "Adopted FY16 Budget", "rk7y-5ruv", "Adopted budget for fiscal year 2016", "Finance",
  "Proposed FY16 Budget", "rkqa-i8sr", "Proposed budget for fiscal year 2016", "Finance",
  "Adopted FY15 Budget", "utqy-e8pg", "Adopted budget for fiscal year 2015", "Finance",
  "Proposed FY15 Budget", "dw6j-5gaz", "Proposed budget for fiscal year 2015", "Finance",
  "Proposed FY14 Budget", "rh9s-z3mn", "Proposed budget for fiscal year 2014", "Finance",
  "FY2024 Operating Expenditures", "8rb8-csdw", "FY2024 detail operating expenditures budget vs actual (draft)", "Finance",
  "FY2023 Operating Expenditures", "pdmj-wd6d", "FY2023 detail operating expenditures budget vs actual", "Finance",
  "FY2022 Operating Expenditures", "gi6r-wxcw", "FY2022 detail operating expenditures budget vs actual", "Finance",
  "FY2021 Operating Expenditures", "xvaf-fqye", "FY2021 detail operating expenditures budget vs actual", "Finance",
  "FY2020 Operating Expenditures", "qi4x-zvpn", "FY2020 detail operating expenditures budget vs actual", "Finance",
  "FY2020-2023 Operating Expenditures", "79s5-6upm", "FY2020-2023 combined detail operating expenditures", "Finance",
  "Traffic Camera Locations", "cat5-2v98", "Traffic camera locations on Oahu", "Location",
  "Bus Routes", "s5c7-gtgi", "City bus routes", "Transportation",
  "City Public Parking Stalls", "cma8-twyj", "Public parking stall locations", "Transportation",
  "Shoreline Access Points", "x972-pcwa", "Public shoreline access points", "Recreation",
  "Parks by Activity", "2drc-h56d", "Parks searchable by activity type", "Recreation",
  "EV Charging Stations", "u93k-n9f3", "Electric vehicle charging station locations", "Transportation",
  "Bus Stop Locations", "yczt-gks6", "City bus stop locations", "Transportation",
  "Road Paving Status", "mp5x-wfar", "Road paving/repaving status", "Transportation",
  "Existing Bike Facilities", "5hpa-8dfv", "Existing bicycle facilities on Oahu", "Transportation",
  "Road Repaving", "4tkz-zg68", "Road repaving rehabilitation projects", "Transportation",
  "Oahu Bikeways Map", "d3ep-gp3d", "Oahu bikeways mapped", "Transportation",
  "Abandoned Vehicles", "7mwf-c3z3", "Abandoned vehicle reports", "Transportation",
  "Fire Stations", "9wbz-g4gv", "Fire station locations", "Location",
  "Hospitals", "k3km-g7pi", "Hospital locations", "Location",
  "Parks", "tg29-p63i", "Park locations", "Location",
  "Exceptional Trees", "84fd-3fzf", "Exceptional trees on Oahu", "Recreation",
  "City Spending 2010-2013", "smuq-xtz4", "City spending by division 2010-2013", "Finance",
  "Curbside Collection Schedules", "quth-64pn", "Refuse curbside collection schedules", "Government",
  "Refuse Drop-off Stations", "rm2f-q6mh", "Refuse drop-off and transfer station locations", "Location",
  "City & County Owned Land", "2wz2-h4kn", "City and county owned land parcels", "Location",
  "State Owned Land", "xj6c-uzj9", "State owned land parcels", "Location",
  "Federal Owned Land", "fbqz-69qn", "Federal owned land parcels", "Location",
  "Tsunami Evacuation Zone", "8t8u-ez86", "Tsunami evacuation zone boundaries", "Public Safety",
  "Tsunami Shelters", "8uk2-gka6", "Tsunami shelter locations", "Public Safety",
  "Emergency Shelters", "gxrb-rbvm", "Emergency shelter locations", "Public Safety",
  "Public Safety Service Locations", "gegk-aj9r", "Public safety service locations", "Public Safety",
  "Storm Water System", "e68k-r6rp", "Storm water system infrastructure", "Public Safety",
  "Zoning Map", "y7ps-rgqn", "Zoning map data", "Location",
  "Soil Types", "cmf2-4bf9", "Soil type classifications", "Location",
  "Honolulu 2011 Expenditures", "pwgx-uwn6", "City expenditures for 2011", "Finance"
)

# -- Budget view_id lookup -----------------------------------------------------

.hnl_budget_ids <- tibble::tribble(
  ~year, ~type, ~view_id,
  2027L, "proposed", "puaw-t6sa",
  2026L, "proposed", "7sq2-y9vx",
  2025L, "adopted",  "4mj2-5un8",
  2025L, "proposed", "mpdv-p8pt",
  2024L, "adopted",  "p45d-q3xd",
  2024L, "proposed", "ejp8-2fun",
  2023L, "adopted",  "rqrs-32nd",
  2023L, "proposed", "6s4t-8syz",
  2022L, "adopted",  "k8va-67va",
  2022L, "proposed", "hi8x-h7cf",
  2021L, "adopted",  "dxdd-mf4t",
  2021L, "proposed", "cgu9-vsgm",
  2020L, "adopted",  "cxe8-45ut",
  2020L, "proposed", "k4if-3w6n",
  2019L, "adopted",  "9nxw-kgwa",
  2019L, "proposed", "exh2-2d4k",
  2018L, "adopted",  "mqjq-jn8u",
  2018L, "proposed", "5ens-aztz",
  2017L, "adopted",  "fpu3-7m4z",
  2017L, "proposed", "e8xz-zi2e",
  2016L, "adopted",  "rk7y-5ruv",
  2016L, "proposed", "rkqa-i8sr",
  2015L, "adopted",  "utqy-e8pg",
  2015L, "proposed", "dw6j-5gaz",
  2014L, "proposed", "rh9s-z3mn"
)

# == Public functions ==========================================================

#' List all Honolulu Open Data datasets
#'
#' Returns a catalog of 70+ datasets available on data.honolulu.gov
#' (Socrata SODA API). Covers public safety, government services, business
#' registrations, budgets, transportation, recreation, and land use.
#' Use view_id values with \code{hnl_view()} for generic queries.
#'
#' @param category Optional category filter (case-insensitive partial match).
#'   Valid categories: "Public Safety", "Government", "Business", "Finance",
#'   "Transportation", "Recreation", "Location". NULL for all.
#' @return A tibble with columns:
#'   - name (character): Dataset title (e.g. "Traffic Incidents", "HPD Crime Incidents")
#'   - view_id (character): Socrata 4x4 dataset identifier for SODA queries
#'   - description (character): Brief description of dataset contents
#'   - category (character): Topic area
#' @examples
#' \dontrun{
#' # Browse all datasets
#' hnl_list()
#'
#' # Filter to transportation datasets
#' hnl_list(category = "Transportation")
#' }
#' @export
hnl_list <- function(category = NULL) {
  out <- .hnl_catalog

if (!is.null(category)) {
    out <- out |> dplyr::filter(grepl(category, .data$category, ignore.case = TRUE))
  }
  out
}

#' Search the Honolulu Open Data catalog by keyword
#'
#' Searches dataset names and descriptions using case-insensitive matching.
#' Use this to find datasets by topic before querying with named functions
#' or \code{hnl_view()}.
#'
#' @param query Search string (case-insensitive). Examples: "crime",
#'   "budget", "permit", "parking", "bus", "311".
#' @return A tibble of matching datasets with columns: name, view_id,
#'   description, category. Empty tibble if no matches.
#' @examples
#' \dontrun{
#' # Find crime-related datasets
#' hnl_search("crime")
#'
#' # Find budget data
#' hnl_search("budget")
#' }
#' @export
hnl_search <- function(query) {
  .hnl_catalog |>
    dplyr::filter(
      grepl(query, name, ignore.case = TRUE) |
      grepl(query, description, ignore.case = TRUE)
    )
}

#' Query any Honolulu Socrata dataset by view ID
#'
#' Generic escape hatch for querying any data.honolulu.gov dataset by
#' its Socrata view ID. Supports full SoQL including WHERE, SELECT,
#' ORDER BY, and full-text search. Use \code{hnl_list()} to find view IDs.
#'
#' @param view_id The Socrata 4x4 view identifier (e.g. "ykb6-n5th" for
#'   traffic incidents). Use \code{hnl_list()} to browse available datasets.
#' @param where Optional SoQL WHERE clause (e.g. "status='Active'").
#'   NULL for no filter.
#' @param select Optional SoQL SELECT clause for column projection.
#'   NULL for all columns.
#' @param order Optional SoQL ORDER BY clause (e.g. "date DESC").
#'   NULL for default order.
#' @param q Optional full-text search query. NULL for no search.
#' @param limit Maximum rows to return (default 1000, max 50000).
#' @param offset Row offset for manual pagination (default 0).
#' @return A tibble of results. Columns depend on the dataset.
#' @examples
#' \dontrun{
#' # Query parks dataset
#' hnl_view("tg29-p63i", limit = 50)
#'
#' # Get exceptional trees on Oahu
#' hnl_view("84fd-3fzf", limit = 100)
#' }
#' @export
hnl_view <- function(view_id, where = NULL, select = NULL, order = NULL,
                     q = NULL, limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select,
               order = order, q = q, limit = limit, offset = offset)
}

#' Fetch real-time Honolulu traffic incidents
#'
#' Returns live traffic incident data from Honolulu, updated every 5 minutes.
#' Includes motor vehicle collisions, stalls, hazards, and road closures.
#' Dates are parsed to Date class; use for real-time traffic monitoring.
#'
#' @param limit Maximum rows (default 1000).
#' @param type Optional incident type filter (exact match). Examples: "MVC"
#'   (motor vehicle collision), "Stall", "Hazard", "Closure". NULL for all.
#' @return A tibble with columns:
#'   - date (Date): Incident date
#'   - time (character): Incident time (HH:MM format)
#'   - type (character): Incident type (e.g. "MVC", "Stall")
#'   - address (character): Street address or cross streets
#'   - location (character): Additional location description
#'   - area (character): Geographic area of Oahu
#' @examples
#' \dontrun{
#' # Get all current traffic incidents
#' hnl_traffic(limit = 50)
#'
#' # Get motor vehicle collisions only
#' hnl_traffic(type = "MVC")
#' }
#' @export
hnl_traffic <- function(limit = 1000, type = NULL) {
  where <- NULL
  if (!is.null(type)) where <- paste0("type='", type, "'")
  raw <- ._soda_query("ykb6-n5th", where = where, limit = limit)
  if (nrow(raw) == 0) return(tibble(
    date = as.Date(character()), time = character(),
    type = character(), address = character(),
    location = character(), area = character()
  ))
  raw |>
    dplyr::transmute(
      date     = as.Date(date, format = "%m/%d/%Y"),
      time     = as.character(time),
      type     = as.character(type),
      address  = as.character(address),
      location = as.character(location %||% NA_character_),
      area     = as.character(area)
    )
}

#' Fetch Honolulu Police Department crime incidents
#'
#' Returns HPD crime incident data including offense type, location,
#' and case status. Ordered by date descending (most recent first).
#'
#' @param limit Maximum rows (default 1000).
#' @param type Optional crime type filter (exact match). Examples:
#'   "ROBBERY", "FRAUD", "BURGLARY", "THEFT/LARCENY", "ASSAULT",
#'   "MOTOR VEHICLE THEFT", "VANDALISM". NULL for all types.
#' @return A tibble with columns:
#'   - objectid (integer): Record identifier
#'   - incidentnum (character): HPD incident number
#'   - blockaddress (character): Block-level address of incident
#'   - date (POSIXct): Date and time of incident
#'   - type (character): Crime/offense type
#'   - status (character): Case status
#'   - score (integer): Geocoding match score
#'   - side (character): Side of street
#' @examples
#' \dontrun{
#' # Get recent crime incidents
#' hnl_crime(limit = 100)
#'
#' # Get robbery incidents only
#' hnl_crime(type = "ROBBERY", limit = 50)
#' }
#' @export
hnl_crime <- function(limit = 1000, type = NULL) {
  where <- NULL
  if (!is.null(type)) where <- paste0("type='", type, "'")
  raw <- ._soda_query("vg88-5rn5", where = where, limit = limit,
                      order = "date DESC")
  if (nrow(raw) == 0) return(tibble(
    objectid = integer(), incidentnum = character(),
    blockaddress = character(), date = as.POSIXct(character()),
    type = character(), status = character()
  ))
  raw |>
    dplyr::transmute(
      objectid     = as.integer(objectid),
      incidentnum  = as.character(incidentnum),
      blockaddress = as.character(blockaddress),
      date         = as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%S"),
      type         = as.character(type),
      status       = as.character(status),
      score        = as.integer(score %||% NA_character_),
      side         = as.character(side %||% NA_character_)
    )
}

#' Fetch Honolulu 311 service requests
#'
#' Returns 311 non-emergency service requests submitted by the public
#' including pothole reports, graffiti, illegal dumping, street light
#' issues, and more. Ordered by creation date descending.
#'
#' @param limit Maximum rows (default 1000).
#' @param status Optional status filter (exact match). Examples: "Closed",
#'   "In Progress", "Open". NULL for all statuses.
#' @param request_type Optional request type filter (partial match via SoQL
#'   LIKE). Examples: "Pothole", "Graffiti", "Street Light", "Abandoned
#'   Vehicle". NULL for all types.
#' @return A tibble with columns:
#'   - id (character): Service request ID
#'   - date_created (character): Date request was created
#'   - request_type (character): Type of service request
#'   - street (character): Street address
#'   - city (character): City name
#'   - state (character): State abbreviation
#'   - zip_code (character): ZIP code
#'   - status (character): Current request status
#'   - description (character): Free-text description of the issue
#' @examples
#' \dontrun{
#' # Get recent 311 requests
#' hnl_311(limit = 100)
#'
#' # Get open pothole requests
#' hnl_311(status = "Open", request_type = "Pothole")
#' }
#' @export
hnl_311 <- function(limit = 1000, status = NULL, request_type = NULL) {
  clauses <- c()
  if (!is.null(status)) clauses <- c(clauses, paste0("status='", status, "'"))
  if (!is.null(request_type)) clauses <- c(clauses,
    paste0("request_type like '%", request_type, "%'"))
  where <- if (length(clauses) > 0) paste(clauses, collapse = " AND ") else NULL
  raw <- ._soda_query("jdy7-ftwe", where = where, limit = limit,
                      order = "date_created DESC")
  if (nrow(raw) == 0) return(tibble(
    id = character(), date_created = character(),
    request_type = character(), street = character(),
    city = character(), state = character(),
    zip_code = character(), status = character(),
    description = character()
  ))
  raw |>
    dplyr::transmute(
      id           = as.character(id),
      date_created = as.character(date_created),
      request_type = as.character(request_type),
      street       = as.character(street),
      city         = as.character(city),
      state        = as.character(state),
      zip_code     = as.character(zip_code),
      status       = as.character(status),
      description  = as.character(description)
    )
}

#' Fetch Honolulu building permits
#'
#' Returns building permit data from January 2005 through present.
#' Includes permit details, estimated construction values, fees,
#' and property characteristics. Ordered by creation date descending.
#'
#' @param limit Maximum rows (default 1000).
#' @param status Optional status filter (exact match on statusdescription).
#'   Examples: "Inspection(s) in Progress", "Permit Issued", "Approved",
#'   "Finaled". NULL for all statuses.
#' @return A tibble with columns:
#'   - permit_no (character): Building permit number
#'   - issue_date (Date): Date permit was issued
#'   - created_date (POSIXct): Date permit application was created
#'   - proposed_use (character): Proposed use of building
#'   - status (character): Current permit status
#'   - estimated_value (numeric): Estimated value of work in USD
#'   - accepted_value (numeric): Accepted value of work in USD
#'   - fees_collected (numeric): Building permit fees collected in USD
#'   - address (character): Property address
#'   - commercial_residential (character): "Commercial" or "Residential"
#'   - new_building (character): New building flag
#'   - solar (character): Solar installation flag
#'   - pool (character): Pool installation flag
#'   - tmk (character): Tax Map Key parcel identifier
#' @examples
#' \dontrun{
#' # Get recent building permits
#' hnl_permits(limit = 100)
#'
#' # Get permits currently under inspection
#' hnl_permits(status = "Inspection(s) in Progress", limit = 50)
#' }
#' @export
hnl_permits <- function(limit = 1000, status = NULL) {
  where <- NULL
  if (!is.null(status)) where <- paste0("statusdescription='", status, "'")
  raw <- ._soda_query("4vab-c87q", where = where, limit = limit,
                      order = "createddate DESC")
  if (nrow(raw) == 0) return(tibble(
    permit_no = character(), issue_date = as.Date(character()),
    proposed_use = character(), status = character(),
    estimated_value = numeric(), address = character(),
    commercial_residential = character()
  ))
  raw |>
    dplyr::transmute(
      permit_no            = as.character(buildingpermitno),
      issue_date           = as.Date(substr(issuedate, 1, 10)),
      created_date         = as.POSIXct(createddate, format = "%Y-%m-%dT%H:%M:%S"),
      proposed_use         = as.character(proposeduse),
      status               = as.character(statusdescription),
      estimated_value      = suppressWarnings(as.numeric(estimatedvalueofwork)),
      accepted_value       = suppressWarnings(as.numeric(acceptedvalue)),
      fees_collected       = suppressWarnings(as.numeric(bpfeescollected)),
      address              = as.character(address %||% joblocation),
      commercial_residential = as.character(commercialresidential),
      new_building         = as.character(newbuilding),
      solar                = as.character(solar),
      pool                 = as.character(pool),
      tmk                  = as.character(tmk)
    )
}

#' Fetch Honolulu city budget data by fiscal year
#'
#' Returns budget line items for a specific fiscal year, either proposed
#' or adopted. Dollar columns are automatically cleaned from string to
#' numeric. Available fiscal years span FY2014 through FY2027.
#'
#' @param year Fiscal year as integer (2014-2027). Default 2027.
#' @param type Budget type: "proposed" or "adopted". Not all years have
#'   both types. Default "proposed".
#' @param limit Maximum rows (default 1000).
#' @param offset Row offset for pagination (default 0).
#' @return A tibble of budget line items with columns that vary by year
#'   but typically include: department, division, object_code,
#'   description, and dollar amount columns (automatically parsed to
#'   numeric).
#' @examples
#' \dontrun{
#' # Get proposed FY2027 budget
#' hnl_budget(year = 2027, type = "proposed", limit = 100)
#'
#' # Get adopted FY2025 budget
#' hnl_budget(year = 2025, type = "adopted", limit = 500)
#'
#' # Compare proposed vs adopted for FY2024
#' proposed <- hnl_budget(2024, "proposed", limit = 500)
#' adopted <- hnl_budget(2024, "adopted", limit = 500)
#' }
#' @export
hnl_budget <- function(year = 2027, type = "proposed", limit = 1000, offset = 0) {
  type <- tolower(type)
  match <- .hnl_budget_ids |>
    dplyr::filter(.data$year == !!year, .data$type == !!type)
  if (nrow(match) == 0) {
    avail <- .hnl_budget_ids |>
      dplyr::mutate(label = paste0(.data$type, " FY", .data$year)) |>
      dplyr::pull(label)
    stop("No budget found for ", type, " FY", year,
         ". Available: ", paste(avail, collapse = ", "))
  }
  vid <- match$view_id[1]
  raw <- ._soda_query(vid, limit = limit, offset = offset)
  if (nrow(raw) == 0) return(tibble())
  # Clean dollar columns if present
  .clean_dollar <- function(x) {
    suppressWarnings(as.numeric(gsub("[$ ,]", "", x)))
  }
  dollar_cols <- grep("^fy_|budget|actual|current_services|proposed", names(raw), value = TRUE)
  for (col in dollar_cols) {
    if (is.character(raw[[col]])) raw[[col]] <- .clean_dollar(raw[[col]])
  }
  tibble::as_tibble(raw)
}

#' Fetch Hawaii business entity registrations
#'
#' Returns business registrations filed with the State of Hawaii DCCA.
#' Includes corporations, LLCs, partnerships, and other entity types.
#' Supports full-text search by business name and status filtering.
#'
#' @param q Full-text search query for business name (e.g. "Aloha",
#'   "Hawaii Coffee"). NULL for no text search.
#' @param status Optional status filter (exact match). Examples: "Active",
#'   "Dissolved", "Terminated", "Withdrawn". NULL for all statuses.
#' @param limit Maximum rows (default 1000).
#' @return A tibble with columns:
#'   - fileno (character): Filing number
#'   - name (character): Business entity name
#'   - business_type (character): Entity type (e.g. "Domestic LLC",
#'     "Domestic Profit Corporation", "Foreign LLC")
#'   - status (character): Registration status
#'   - purpose (character): Stated business purpose
#'   - place_incorporated (character): State/country of incorporation
#'   - registration_date (character): Date of registration
#'   - agent_name (character): Registered agent name
#' @examples
#' \dontrun{
#' # Search for businesses by name
#' hnl_businesses(q = "Aloha", limit = 50)
#'
#' # Get active LLCs (filter after fetching)
#' hnl_businesses(status = "Active", limit = 100) |>
#'   dplyr::filter(grepl("LLC", business_type))
#' }
#' @export
hnl_businesses <- function(q = NULL, status = NULL, limit = 1000) {
  where <- NULL
  if (!is.null(status)) where <- paste0("status='", status, "'")
  raw <- ._soda_query("9k54-ztb8", where = where, q = q, limit = limit)
  if (nrow(raw) == 0) return(tibble(
    fileno = character(), name = character(),
    business_type = character(), status = character(),
    purpose = character(), registration_date = character()
  ))
  raw |>
    dplyr::transmute(
      fileno            = as.character(fileno),
      name              = as.character(name),
      business_type     = as.character(business_type),
      status            = as.character(status),
      purpose           = as.character(purpose),
      place_incorporated = as.character(place_incorporated),
      registration_date = as.character(registration_date),
      agent_name        = as.character(agent_name)
    )
}

#' Get honolulu.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hnl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hnl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/honolulu.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "honolulu.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# honolulu.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# honolulu.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
