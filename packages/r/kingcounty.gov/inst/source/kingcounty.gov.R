# kingcounty.gov.R - King County, WA Open Data (Socrata SODA) client
# Self-contained. All public functions return tibbles.
#
# Portal: data.kingcounty.gov (245 Socrata views)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public datasets)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.kc_base <- "https://data.kingcounty.gov"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Core SODA query -----------------------------------------------------------

._kc_soda_query <- function(view_id, where = NULL, select = NULL,
                            order = NULL, group = NULL, q = NULL,
                            limit = 1000, offset = 0,
                            base = .kc_base) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(q))      params[["$q"]]      <- q

  qs <- paste(names(params),
              vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE),
                     character(1)),
              sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", base, view_id, qs)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(),
  updated_at = as.POSIXct(character()), view_count = integer()
)

# == Discovery =================================================================

#' List available datasets on King County Open Data
#'
#' Queries the Socrata views API on data.kingcounty.gov to enumerate
#' available datasets. The portal hosts approximately 245 datasets
#' covering public safety, health, environment, property, and elections.
#'
#' @param limit Integer. Number of datasets to return. Default 100, max 200.
#' @param category Character or NULL. Category filter
#'   (e.g. `"Health"`, `"Environment"`).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata view ID (e.g. `"yaai-7frk"`).}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Dataset description.}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type.}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{view_count}{Integer. Total views.}
#'   }
#'
#' @family kingcounty discovery
#' @seealso [kc_search()] for keyword search, [kc_view()] for single-dataset metadata
#'
#' @examples
#' \dontrun{
#' kc_list(limit = 20)
#' kc_list(category = "Health")
#' }
#' @export
kc_list <- function(limit = 100, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .kc_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_datasets)

  result <- as_tibble(raw) |>
    transmute(
      id         = as.character(id),
      name       = as.character(name),
      description = as.character(description %||% NA),
      category   = as.character(category %||% NA),
      type       = as.character(displayType %||% NA),
      updated_at = as.POSIXct(as.numeric(viewLastModified %||% NA),
                              origin = "1970-01-01", tz = "UTC"),
      view_count = as.integer(viewCount %||% NA)
    )

  if (!is.null(category)) {
    result <- result |> filter(grepl(!!category, .data$category, ignore.case = TRUE))
  }
  result
}

#' Search King County datasets by keyword
#'
#' Full-text search across dataset names and metadata on
#' data.kingcounty.gov.
#'
#' @param query Character. Search term (e.g. `"food"`, `"water"`, `"crime"`).
#' @param limit Integer. Maximum results. Default 20.
#'
#' @return A tibble with the same columns as [kc_list()].
#'
#' @family kingcounty discovery
#' @seealso [kc_list()] for browsing by category
#'
#' @examples
#' \dontrun{
#' kc_search("food inspection")
#' }
#' @export
kc_search <- function(query, limit = 20) {
  url <- sprintf("%s/api/views?limit=%d&q=%s", .kc_base, limit,
                 utils::URLencode(query, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_datasets)

  as_tibble(raw) |>
    transmute(
      id         = as.character(id),
      name       = as.character(name),
      description = as.character(description %||% NA),
      category   = as.character(category %||% NA),
      type       = as.character(displayType %||% NA),
      updated_at = as.POSIXct(as.numeric(viewLastModified %||% NA),
                              origin = "1970-01-01", tz = "UTC"),
      view_count = as.integer(viewCount %||% NA)
    )
}

#' Get metadata for a specific King County dataset
#'
#' Retrieves detailed metadata for a single dataset including row and
#' column counts.
#'
#' @param view_id Character. Socrata view ID (e.g. `"yaai-7frk"`).
#'
#' @return A single-row tibble with columns: id, name, description,
#'   category, row_count, column_count.
#'
#' @family kingcounty discovery
#' @seealso [kc_query()] to fetch data from the dataset
#'
#' @examples
#' \dontrun{
#' kc_view("yaai-7frk")
#' }
#' @export
kc_view <- function(view_id) {
  url <- sprintf("%s/api/views/%s.json", .kc_base, view_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$id)) {
    return(tibble(id = character(), name = character(), description = character(),
                  category = character(), row_count = integer(), column_count = integer()))
  }
  tibble(
    id           = as.character(raw$id %||% NA),
    name         = as.character(raw$name %||% NA),
    description  = as.character(raw$description %||% NA),
    category     = as.character(raw$category %||% NA),
    row_count    = as.integer(raw$rowCount %||% NA),
    column_count = as.integer(length(raw$columns %||% list()))
  )
}

#' Generic SODA query against any King County dataset
#'
#' Runs a SoQL query against any dataset on data.kingcounty.gov.
#' Supports filtering, column selection, ordering, grouping, and
#' full-text search.
#'
#' @param view_id Character. Socrata view ID.
#' @param where Character or NULL. SoQL WHERE clause.
#' @param select Character or NULL. SoQL SELECT clause.
#' @param order Character or NULL. SoQL ORDER BY clause.
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param q Character or NULL. Full-text search query.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family kingcounty data
#' @seealso [kc_view()] to inspect dataset metadata
#'
#' @examples
#' \dontrun{
#' kc_query("yaai-7frk", limit = 50)
#' }
#' @export
kc_query <- function(view_id, where = NULL, select = NULL,
                     order = NULL, group = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._kc_soda_query(view_id, where = where, select = select,
                  order = order, group = group, q = q,
                  limit = limit, offset = offset)
}

# == Animals ===================================================================

#' King County lost, found, and adoptable pets
#'
#' Queries the King County lost/found/adoptable pets dataset
#' (`yaai-7frk`). Includes animal details, dates, and general location.
#'
#' @param animal_type Character or NULL. Filter by animal type
#'   (e.g. `"Cat"`, `"Dog"`, `"Rabbit"`).
#' @param record_type Character or NULL. Filter by record type
#'   (`"FOUND"`, `"LOST"`, or `"ADOPTABLE"`).
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including impound_no, animal_id,
#'   record_type, animal_type, age, animal_gender, animal_breed,
#'   animal_color, date, and obfuscated_address.
#'
#' @family kingcounty data
#'
#' @examples
#' \dontrun{
#' kc_pets(animal_type = "Dog", record_type = "ADOPTABLE", limit = 50)
#' }
#' @export
kc_pets <- function(animal_type = NULL, record_type = NULL,
                    limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(animal_type))  clauses <- c(clauses, sprintf("animal_type = '%s'", animal_type))
  if (!is.null(record_type))  clauses <- c(clauses, sprintf("record_type = '%s'", record_type))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._kc_soda_query("yaai-7frk", where = where, order = "date DESC",
                   limit = limit, offset = offset)
}

# == Public Safety =============================================================

#' King County Sheriff offense reports (2020-present)
#'
#' Queries the King County Sheriff's Office offense report dataset
#' (`4kmt-kfqf`). Uses NIBRS (National Incident-Based Reporting System)
#' codes for crime classification.
#'
#' @param nibrs_code Character or NULL. NIBRS code filter
#'   (e.g. `"13A"` for Aggravated Assault, `"23H"` for All Other Larceny).
#' @param city Character or NULL. City name filter. Case-insensitive.
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including case_number,
#'   incident_datetime (POSIXct), nibrs_code, nibrs_code_name,
#'   block_address, city, state, zip, district, and precinct.
#'
#' @family kingcounty public_safety
#' @seealso [kc_jail_bookings()]
#'
#' @examples
#' \dontrun{
#' kc_offense_reports(city = "SEATTLE", year = 2024, limit = 50)
#' }
#' @export
kc_offense_reports <- function(nibrs_code = NULL, city = NULL, year = NULL,
                               limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(nibrs_code)) clauses <- c(clauses, sprintf("nibrs_code = '%s'", nibrs_code))
  if (!is.null(city))       clauses <- c(clauses, sprintf("upper(city) = '%s'", toupper(city)))
  if (!is.null(year))       clauses <- c(clauses,
    sprintf("incident_datetime >= '%d-01-01' AND incident_datetime < '%d-01-01'", year, year + 1))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._kc_soda_query("4kmt-kfqf", where = where, order = "incident_datetime DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(incident_datetime = as.POSIXct(incident_datetime, format = "%Y-%m-%dT%H:%M:%S"))
}

#' King County adult jail bookings (current year)
#'
#' Queries the King County adult jail booking dataset (`j56h-zgnm`)
#' for the current year.
#'
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with booking records.
#'
#' @family kingcounty public_safety
#' @seealso [kc_offense_reports()]
#'
#' @examples
#' \dontrun{
#' kc_jail_bookings(limit = 50)
#' }
#' @export
kc_jail_bookings <- function(q = NULL, limit = 1000, offset = 0) {
  ._kc_soda_query("j56h-zgnm", q = q, order = "dt_book DESC",
                   limit = limit, offset = offset)
}

# == Health ====================================================================

#' King County food establishment inspection data
#'
#' Queries restaurant and food establishment health inspection results
#' from King County Public Health (`f29f-zza5`). Includes inspection
#' scores, results, and violation details.
#'
#' @param name Character or NULL. Business name filter.
#'   Case-insensitive partial match.
#' @param city Character or NULL. City filter. Case-insensitive.
#' @param result Character or NULL. Inspection result filter
#'   (e.g. `"Satisfactory"`, `"Unsatisfactory"`,
#'   `"Incomplete"`, `"Complete"`).
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including name, inspection_date (Date),
#'   address, city, zip_code, inspection_type, inspection_score (integer),
#'   inspection_result, and violation_description.
#'
#' @family kingcounty health
#'
#' @examples
#' \dontrun{
#' kc_food_inspections(city = "SEATTLE", limit = 50)
#' kc_food_inspections(result = "Unsatisfactory")
#' }
#' @export
kc_food_inspections <- function(name = NULL, city = NULL, result = NULL,
                                limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(name))   clauses <- c(clauses, sprintf("upper(name) LIKE '%%%s%%'", toupper(name)))
  if (!is.null(city))   clauses <- c(clauses, sprintf("upper(city) = '%s'", toupper(city)))
  if (!is.null(result)) clauses <- c(clauses, sprintf("inspection_result = '%s'", result))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._kc_soda_query("f29f-zza5", where = where, order = "inspection_date DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  if ("inspection_date" %in% names(df)) {
    df$inspection_date <- as.Date(sub("T.*", "", df$inspection_date))
  }
  if ("inspection_score" %in% names(df)) {
    df$inspection_score <- as.integer(df$inspection_score)
  }
  df
}

#' King County mental health and substance use disorder providers
#'
#' Queries the King County behavioral health provider directory
#' (`sep3-3pj3`). Lists providers offering mental health and/or
#' substance use disorder services.
#'
#' @param q Character or NULL. Full-text search across provider name,
#'   location, and services.
#' @param adults Logical or NULL. Filter for adult services when `TRUE`.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including provider, phone, website,
#'   locations, notes, and service-type flags.
#'
#' @family kingcounty health
#'
#' @examples
#' \dontrun{
#' kc_mental_health_providers(q = "counseling", limit = 50)
#' }
#' @export
kc_mental_health_providers <- function(q = NULL, adults = NULL,
                                       limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(adults)) clauses <- c(clauses, sprintf("adults = %s", tolower(as.character(adults))))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._kc_soda_query("sep3-3pj3", where = where, q = q,
                   limit = limit, offset = offset)
}

#' Lead content of consumer products tested in King County
#'
#' Queries lead testing results for consumer products in King County
#' (`i6sy-ckp7`).
#'
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with lead testing results.
#'
#' @family kingcounty health
#'
#' @examples
#' \dontrun{
#' kc_lead_products(q = "toy")
#' }
#' @export
kc_lead_products <- function(q = NULL, limit = 1000, offset = 0) {
  ._kc_soda_query("i6sy-ckp7", q = q, limit = limit, offset = offset)
}

# == Environment & Water =======================================================

#' King County water quality monitoring data
#'
#' Queries the King County water quality monitoring dataset
#' (`vwmt-pvjw`). Covers streams, lakes, and marine sites with
#' various physical, chemical, and biological parameters.
#'
#' @param site_type Character or NULL. Filter by site type
#'   (e.g. `"Stream"`, `"Lake"`, `"Marine"`). Partial match.
#' @param parameter Character or NULL. Filter by parameter name
#'   (e.g. `"Temperature"`, `"Dissolved Oxygen"`). Partial match.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble of water quality observations.
#'
#' @family kingcounty environment
#' @seealso [kc_streams_wqi()], [kc_swim_beaches()]
#'
#' @examples
#' \dontrun{
#' kc_water_quality(site_type = "Lake", limit = 50)
#' }
#' @export
kc_water_quality <- function(site_type = NULL, parameter = NULL,
                             limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(site_type)) clauses <- c(clauses, sprintf("upper(site_type) LIKE '%%%s%%'",
                                                          toupper(site_type)))
  if (!is.null(parameter)) clauses <- c(clauses, sprintf("upper(parameter) LIKE '%%%s%%'",
                                                          toupper(parameter)))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._kc_soda_query("vwmt-pvjw", where = where, order = "collect_datetime DESC",
                   limit = limit, offset = offset)
}

#' King County streams water quality index
#'
#' Queries the King County streams Water Quality Index (WQI) dataset
#' (`ww4k-7bef`). Provides composite water quality scores for
#' monitored stream sites.
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble of stream WQI records.
#'
#' @family kingcounty environment
#' @seealso [kc_water_quality()], [kc_stream_gages()]
#'
#' @examples
#' \dontrun{
#' kc_streams_wqi()
#' }
#' @export
kc_streams_wqi <- function(limit = 1000, offset = 0) {
  ._kc_soda_query("ww4k-7bef", limit = limit, offset = offset)
}

#' King County freshwater swim beach data
#'
#' Queries freshwater swim beach monitoring data (`mbzm-4r9y`).
#' Includes water temperature and bacteria geometric mean readings
#' used to assess beach safety.
#'
#' @param beach Character or NULL. Beach name filter. Partial match.
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including beach, jurisdiction,
#'   date (Date), geomean30d, watertempc (numeric), and
#'   watertempf (numeric).
#'
#' @family kingcounty environment
#'
#' @examples
#' \dontrun{
#' kc_swim_beaches(year = 2024, limit = 50)
#' }
#' @export
kc_swim_beaches <- function(beach = NULL, year = NULL,
                            limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(beach)) clauses <- c(clauses, sprintf("upper(beach) LIKE '%%%s%%'",
                                                      toupper(beach)))
  if (!is.null(year))  clauses <- c(clauses, sprintf("date >= '%d-01-01' AND date < '%d-01-01'",
                                                      year, year + 1))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._kc_soda_query("mbzm-4r9y", where = where, order = "date DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(
    date       = as.Date(date),
    watertempc = as.numeric(watertempc %||% NA),
    watertempf = as.numeric(watertempf %||% NA)
  )
}

#' King County stream gage hydrology data
#'
#' Queries stream gage hydrology data from King County (`hkim-5ysi`).
#' Includes discharge, stage, and other hydrological parameters from
#' automated monitoring stations.
#'
#' @param site_id Character or NULL. Filter by monitoring site ID.
#' @param parameter Character or NULL. Filter by parameter name
#'   (e.g. `"discharge"`, `"stage"`).
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including site_id,
#'   datetime (POSIXct), corrected_data (numeric), parameter,
#'   and provisional.
#'
#' @family kingcounty environment
#' @seealso [kc_water_quality()], [kc_streams_wqi()]
#'
#' @examples
#' \dontrun{
#' kc_stream_gages(parameter = "discharge", limit = 100)
#' }
#' @export
kc_stream_gages <- function(site_id = NULL, parameter = NULL,
                            limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(site_id))   clauses <- c(clauses, sprintf("site_id = '%s'", site_id))
  if (!is.null(parameter)) clauses <- c(clauses, sprintf("parameter = '%s'", parameter))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._kc_soda_query("hkim-5ysi", where = where, order = "datetime DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(
    datetime       = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%S"),
    corrected_data = as.numeric(corrected_data %||% NA)
  )
}

#' King County freshwater CTD profiles (Lake Washington)
#'
#' Queries conductivity-temperature-depth (CTD) profile data from
#' Lake Washington and other freshwater bodies (`nrpy-ni8p`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble of CTD profile measurements.
#'
#' @family kingcounty environment
#'
#' @examples
#' \dontrun{
#' kc_freshwater_ctd(limit = 100)
#' }
#' @export
kc_freshwater_ctd <- function(limit = 1000, offset = 0) {
  ._kc_soda_query("nrpy-ni8p", order = "date DESC",
                   limit = limit, offset = offset)
}

# == Property & Tax ============================================================

#' King County real property tax receivables
#'
#' Queries real property tax receivable data from King County
#' (`dkna-i698`). Shows billed and paid amounts by account.
#'
#' @param bill_year Integer or NULL. Filter by billing year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including account_number, levy_code,
#'   bill_year, billed_amount, and paid_amount.
#'
#' @family kingcounty property
#' @seealso [kc_foreclosures()]
#'
#' @examples
#' \dontrun{
#' kc_property_tax(bill_year = 2024, limit = 50)
#' }
#' @export
kc_property_tax <- function(bill_year = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(bill_year)) sprintf("bill_year = '%d'", bill_year) else NULL
  ._kc_soda_query("dkna-i698", where = where, limit = limit, offset = offset)
}

#' King County foreclosure parcels
#'
#' Queries current foreclosure parcel data from King County
#' (`nx4x-daw6`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble of foreclosure parcel records.
#'
#' @family kingcounty property
#' @seealso [kc_property_tax()]
#'
#' @examples
#' \dontrun{
#' kc_foreclosures()
#' }
#' @export
kc_foreclosures <- function(limit = 1000, offset = 0) {
  ._kc_soda_query("nx4x-daw6", limit = limit, offset = offset)
}

# == Elections =================================================================

#' King County election results (February 2026 Special)
#'
#' Queries precinct-level election results from King County
#' (`vqgp-yqq9`). Contains vote counts by race, precinct, and
#' counter type (ballots cast, registered voters, etc.).
#'
#' @param race Character or NULL. Filter by race name. Partial match.
#' @param precinct Character or NULL. Filter by precinct identifier.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including precinct, race,
#'   countergroup, countertype, and sumofcount (integer).
#'
#' @family kingcounty data
#'
#' @examples
#' \dontrun{
#' kc_election_results(limit = 50)
#' }
#' @export
kc_election_results <- function(race = NULL, precinct = NULL,
                                limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(race))     clauses <- c(clauses, sprintf("upper(race) LIKE '%%%s%%'",
                                                         toupper(race)))
  if (!is.null(precinct)) clauses <- c(clauses, sprintf("precinct = '%s'", precinct))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._kc_soda_query("vqgp-yqq9", where = where, limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(sumofcount = as.integer(sumofcount %||% NA))
}

# == Recycling & Environment ===================================================

#' King County recycling options ("What do I do with...?")
#'
#' Queries the King County recycling/disposal options dataset
#' (`zqwi-c5q3`). Helps find providers that accept specific materials
#' for recycling, reuse, or disposal.
#'
#' @param material Character or NULL. Filter by material handled
#'   (e.g. `"Electronics"`, `"Paint"`, `"Appliances"`). Partial match.
#' @param q Character or NULL. Full-text search.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including provider_name,
#'   provider_address, phone, material_handled, service_description,
#'   and fee.
#'
#' @family kingcounty data
#'
#' @examples
#' \dontrun{
#' kc_recycling(material = "Electronics")
#' }
#' @export
kc_recycling <- function(material = NULL, q = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(material)) where <- sprintf("upper(material_handled) LIKE '%%%s%%'",
                                            toupper(material))
  ._kc_soda_query("zqwi-c5q3", where = where, q = q,
                   limit = limit, offset = offset)
}

# == Demographics ==============================================================

#' King County population counts (2010 Census)
#'
#' Queries 2010 Census population count data for King County
#' (`cavj-x985`).
#'
#' @param limit Integer. Maximum rows. Default 100.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble of population count records.
#'
#' @family kingcounty demographics
#' @seealso [kc_housing()]
#'
#' @examples
#' \dontrun{
#' kc_population_2010()
#' }
#' @export
kc_population_2010 <- function(limit = 100, offset = 0) {
  ._kc_soda_query("cavj-x985", limit = limit, offset = offset)
}

#' Total King County housing units (2000-2010)
#'
#' Queries housing unit count data for King County for the
#' 2000--2010 period (`bs3e-nncv`).
#'
#' @param limit Integer. Maximum rows. Default 100.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble of housing unit count records.
#'
#' @family kingcounty demographics
#' @seealso [kc_population_2010()]
#'
#' @examples
#' \dontrun{
#' kc_housing()
#' }
#' @export
kc_housing <- function(limit = 100, offset = 0) {
  ._kc_soda_query("bs3e-nncv", limit = limit, offset = offset)
}

# == Context ===================================================================

#' Get kingcounty.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
kc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(kc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/kingcounty.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "kingcounty.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# kingcounty.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# kingcounty.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
