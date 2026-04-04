# in.gov.R - State of Indiana / Bloomington Open Data (Socrata SODA) client
# Self-contained. All public functions return tibbles.
#
# Portal: data.bloomington.in.gov (195 Socrata views)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public datasets)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ind_base <- "https://data.bloomington.in.gov"

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

._ind_soda_query <- function(view_id, where = NULL, select = NULL,
                             order = NULL, group = NULL, q = NULL,
                             limit = 1000, offset = 0,
                             base = .ind_base) {
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

.schema_calls <- tibble(
  case_number = character(), datetime = as.POSIXct(character()),
  month = character(), weekday = character(), nature = character(),
  district = character(), agency = character(), report = character()
)

# == Discovery =================================================================

#' List available datasets on Indiana / Bloomington Open Data
#'
#' Queries the Socrata views API on data.bloomington.in.gov to enumerate
#' available datasets. The portal hosts approximately 195 datasets
#' covering public safety, utilities, transportation, and government.
#'
#' @param limit Integer. Number of datasets to return. Default 100, max 200.
#' @param category Character or NULL. Category filter applied after fetch
#'   (e.g. `"Public Safety"`, `"Utilities"`).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata view ID (e.g. `"t5xf-ggw6"`).}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Dataset description.}
#'     \item{category}{Character. Thematic category.}
#'     \item{type}{Character. Display type (e.g. `"table"`, `"map"`).}
#'     \item{updated_at}{POSIXct. Last modification timestamp (UTC).}
#'     \item{view_count}{Integer. Total views.}
#'   }
#'
#' @family indiana discovery
#' @seealso [ind_search()] for keyword search, [ind_view()] for single-dataset metadata
#'
#' @examples
#' \dontrun{
#' ind_list(limit = 20)
#' ind_list(category = "Public Safety")
#' }
#' @export
ind_list <- function(limit = 100, category = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .ind_base, limit)
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

#' Search Bloomington datasets by keyword
#'
#' Full-text search across dataset names and metadata on the
#' Bloomington, Indiana Socrata portal.
#'
#' @param query Character. Search term (e.g. `"police"`, `"water"`, `"rental"`).
#' @param limit Integer. Maximum results to return. Default 20.
#'
#' @return A tibble with the same columns as [ind_list()].
#'
#' @family indiana discovery
#' @seealso [ind_list()] to browse by category
#'
#' @examples
#' \dontrun{
#' ind_search("fire")
#' }
#' @export
ind_search <- function(query, limit = 20) {
  url <- sprintf("%s/api/views?limit=%d&q=%s", .ind_base, limit,
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

#' Get metadata for a specific Bloomington dataset
#'
#' Retrieves detailed metadata for a single dataset, including row
#' and column counts. Use this to inspect a dataset before querying
#' it with [ind_query()].
#'
#' @param view_id Character. Socrata view ID (e.g. `"t5xf-ggw6"`).
#'
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{id}{Character. View ID.}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Full description.}
#'     \item{category}{Character. Category.}
#'     \item{row_count}{Integer. Number of rows.}
#'     \item{column_count}{Integer. Number of columns.}
#'   }
#'
#' @family indiana discovery
#' @seealso [ind_query()] to fetch data from the dataset
#'
#' @examples
#' \dontrun{
#' ind_view("t5xf-ggw6")
#' }
#' @export
ind_view <- function(view_id) {
  url <- sprintf("%s/api/views/%s.json", .ind_base, view_id)
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

#' Generic SODA query against any Bloomington dataset
#'
#' Runs a SoQL (Socrata Query Language) query against any dataset on
#' the Bloomington, IN open data portal. Supports filtering, column
#' selection, ordering, grouping, and full-text search.
#'
#' @param view_id Character. Socrata view ID (e.g. `"t5xf-ggw6"`).
#' @param where Character or NULL. SoQL WHERE clause
#'   (e.g. `"year = '2024'"`).
#' @param select Character or NULL. SoQL SELECT clause
#'   (e.g. `"nature, count(*) as n"`).
#' @param order Character or NULL. SoQL ORDER BY clause
#'   (e.g. `"datetime DESC"`).
#' @param group Character or NULL. SoQL GROUP BY clause.
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Maximum rows to return. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family indiana data
#' @seealso [ind_view()] to inspect dataset metadata first
#'
#' @examples
#' \dontrun{
#' ind_query("t5xf-ggw6", where = "nature = 'THEFT'", limit = 50)
#' }
#' @export
ind_query <- function(view_id, where = NULL, select = NULL,
                      order = NULL, group = NULL, q = NULL,
                      limit = 1000, offset = 0) {
  ._ind_soda_query(view_id, where = where, select = select,
                   order = order, group = group, q = q,
                   limit = limit, offset = offset)
}


# == Public Safety =============================================================

#' Bloomington Police calls for service
#'
#' Queries the Bloomington Police Department calls for service dataset
#' (view ID `t5xf-ggw6`). Each row represents a single dispatched call.
#'
#' @param nature Character or NULL. Filter by nature/type of call
#'   (e.g. `"THEFT"`, `"ACCIDENT"`, `"DISTURBANCE"`). Partial match, case-insensitive.
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows to return. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns: case_number, datetime (POSIXct),
#'   month, weekday, nature, district, agency, report.
#'
#' @family indiana public_safety
#' @seealso [ind_accidents()], [ind_fire_incidents()]
#'
#' @examples
#' \dontrun{
#' ind_calls_for_service(nature = "THEFT", limit = 50)
#' ind_calls_for_service(year = 2024, limit = 100)
#' }
#' @export
ind_calls_for_service <- function(nature = NULL, year = NULL,
                                  limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(nature)) clauses <- c(clauses, sprintf("upper(nature) LIKE '%%%s%%'",
                                                       toupper(nature)))
  if (!is.null(year))   clauses <- c(clauses, sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                                       year, year + 1))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._ind_soda_query("t5xf-ggw6", where = where, order = "datetime DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(.schema_calls)
  df |> mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%S"))
}

#' Bloomington accidents reported to police
#'
#' Queries traffic and other accident reports from the Bloomington PD
#' (view ID `vf95-pwwj`).
#'
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset including
#'   case_number, datetime, month, weekday, district, agency.
#'
#' @family indiana public_safety
#' @seealso [ind_calls_for_service()]
#'
#' @examples
#' \dontrun{
#' ind_accidents(year = 2024, limit = 50)
#' }
#' @export
ind_accidents <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                        year, year + 1)
  ._ind_soda_query("vf95-pwwj", where = where, order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington domestic violence reports
#'
#' Queries domestic violence incident reports from the Bloomington PD
#' (view ID `vq37-rm9u`).
#'
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including case_number, datetime,
#'   nature, district, and demographic fields.
#'
#' @family indiana public_safety
#'
#' @examples
#' \dontrun{
#' ind_domestic_violence(year = 2024, limit = 50)
#' }
#' @export
ind_domestic_violence <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                        year, year + 1)
  ._ind_soda_query("vq37-rm9u", where = where, order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington police use of force incidents
#'
#' Queries police use of force incident data from the Bloomington PD
#' (view ID `7jzv-6jei`).
#'
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family indiana public_safety
#'
#' @examples
#' \dontrun{
#' ind_use_of_force(year = 2023)
#' }
#' @export
ind_use_of_force <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                        year, year + 1)
  ._ind_soda_query("7jzv-6jei", where = where, order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington hate crimes
#'
#' Queries hate crime incident data from the Bloomington PD
#' (view ID `vzyb-ttns`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family indiana public_safety
#'
#' @examples
#' \dontrun{
#' ind_hate_crimes()
#' }
#' @export
ind_hate_crimes <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("vzyb-ttns", order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington stolen guns reports
#'
#' Queries stolen firearm reports from the Bloomington PD
#' (view ID `y66s-bnfm`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family indiana public_safety
#'
#' @examples
#' \dontrun{
#' ind_stolen_guns()
#' }
#' @export
ind_stolen_guns <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("y66s-bnfm", order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington citizen complaints against police
#'
#' Queries citizen complaints filed against the Bloomington PD
#' (view ID `kit3-8bua`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family indiana public_safety
#'
#' @examples
#' \dontrun{
#' ind_citizen_complaints()
#' }
#' @export
ind_citizen_complaints <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("kit3-8bua", limit = limit, offset = offset)
}

# == Fire & Emergency ==========================================================

#' Bloomington fire incident responses
#'
#' Queries fire department incident response data from Bloomington
#' (view ID `jr6a-uyr5`). Includes incident type, alarm datetime,
#' responding unit, address, and response time.
#'
#' @param type Character or NULL. Filter by incident type description
#'   (e.g. `"FIRE"`, `"EMS"`, `"HAZMAT"`). Case-insensitive partial match.
#' @param year Integer or NULL. Filter to a specific calendar year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including inci_no, unit, inci_type,
#'   inci_desc, alm_datetime (POSIXct), address, and resp_time (integer seconds).
#'
#' @family indiana emergency
#' @seealso [ind_calls_for_service()] for police calls
#'
#' @examples
#' \dontrun{
#' ind_fire_incidents(type = "FIRE", year = 2024, limit = 50)
#' }
#' @export
ind_fire_incidents <- function(type = NULL, year = NULL,
                               limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(type)) clauses <- c(clauses, sprintf("upper(inci_desc) LIKE '%%%s%%'",
                                                     toupper(type)))
  if (!is.null(year)) clauses <- c(clauses, sprintf("alm_datetime >= '%d-01-01' AND alm_datetime < '%d-01-01'",
                                                     year, year + 1))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._ind_soda_query("jr6a-uyr5", where = where, order = "alm_datetime DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(
    alm_datetime = as.POSIXct(alm_datetime, format = "%Y-%m-%dT%H:%M:%S"),
    resp_time = as.integer(resp_time %||% NA)
  )
}

# == Government ================================================================

#' Bloomington annual employee compensation
#'
#' Queries City of Bloomington employee compensation data
#' (view ID `fcnf-g862`). Reports annual compensation by employee
#' with department and title information.
#'
#' @param department Character or NULL. Filter by department name
#'   (e.g. `"POLICE"`, `"FIRE"`). Case-insensitive partial match.
#' @param year Integer or NULL. Filter by year using the `asofdate` field.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including employeeid, firstname,
#'   lastname, department, title, compensation (numeric), and asofdate.
#'
#' @family indiana government
#' @seealso [ind_contracts()]
#'
#' @examples
#' \dontrun{
#' ind_compensation(department = "POLICE", limit = 50)
#' }
#' @export
ind_compensation <- function(department = NULL, year = NULL,
                             limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(department)) clauses <- c(clauses, sprintf("upper(department) LIKE '%%%s%%'",
                                                           toupper(department)))
  if (!is.null(year)) clauses <- c(clauses, sprintf("asofdate >= '%d-01-01' AND asofdate < '%d-01-01'",
                                                     year, year + 1))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._ind_soda_query("fcnf-g862", where = where, order = "asofdate DESC",
                         limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(compensation = as.numeric(compensation))
}

#' Bloomington public contracts
#'
#' Queries City of Bloomington public contracts data
#' (view ID `ruzy-efni`).
#'
#' @param q Character or NULL. Full-text search within the dataset.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns from the dataset.
#'
#' @family indiana government
#' @seealso [ind_compensation()]
#'
#' @examples
#' \dontrun{
#' ind_contracts(q = "construction", limit = 50)
#' }
#' @export
ind_contracts <- function(q = NULL, limit = 1000, offset = 0) {
  ._ind_soda_query("ruzy-efni", q = q, limit = limit, offset = offset)
}

#' Bloomington rental permits
#'
#' Queries City of Bloomington rental permit data (view ID `9q6j-a8rc`).
#' Includes permit number, property address, status, and contact information.
#'
#' @param status Character or NULL. Filter by permit status
#'   (e.g. `"Complete"`, `"In Review"`).
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including permit_num, property_address,
#'   status, contact_name, and units.
#'
#' @family indiana government
#'
#' @examples
#' \dontrun{
#' ind_rental_permits(status = "Complete", limit = 50)
#' }
#' @export
ind_rental_permits <- function(status = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(status)) sprintf("status = '%s'", status) else NULL
  ._ind_soda_query("9q6j-a8rc", where = where, order = "permit_num DESC",
                   limit = limit, offset = offset)
}

# == 311 / Service Requests ====================================================

#' Bloomington Open311 service requests
#'
#' Queries the City of Bloomington Open311 / service request dataset
#' (view ID `aw6y-t4ix`). Covers trash, recycling, potholes, and other
#' municipal service requests.
#'
#' @param service Character or NULL. Filter by service name
#'   (e.g. `"Trash"`, `"Recycling"`, `"Pothole"`). Case-insensitive partial match.
#' @param status Character or NULL. Filter by status
#'   (e.g. `"open"`, `"closed"`).
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including service_request_id,
#'   requested_datetime, status_description, service_name, description,
#'   and address.
#'
#' @family indiana services
#'
#' @examples
#' \dontrun{
#' ind_service_requests(service = "Pothole", limit = 50)
#' }
#' @export
ind_service_requests <- function(service = NULL, status = NULL,
                                 limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(service)) clauses <- c(clauses, sprintf("upper(service_name) LIKE '%%%s%%'",
                                                        toupper(service)))
  if (!is.null(status))  clauses <- c(clauses, sprintf("status_description = '%s'", status))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._ind_soda_query("aw6y-t4ix", where = where, order = "requested_datetime DESC",
                   limit = limit, offset = offset)
}

# == Animals ===================================================================

#' Bloomington animal shelter data
#'
#' Queries the City of Bloomington animal shelter dataset
#' (view ID `e245-r9ub`). Includes intake dates, animal details,
#' and movement/outcome information.
#'
#' @param species Character or NULL. Filter by species name
#'   (e.g. `"Cat"`, `"Dog"`).
#' @param movement Character or NULL. Filter by movement/outcome type
#'   (e.g. `"Adoption"`, `"Reclaimed"`, `"Transfer"`).
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including id, intakedate, sheltercode,
#'   animalname, breedname, speciesname, sexname, and movementtype.
#'
#' @family indiana services
#'
#' @examples
#' \dontrun{
#' ind_animal_shelter(species = "Dog", limit = 50)
#' ind_animal_shelter(movement = "Adoption")
#' }
#' @export
ind_animal_shelter <- function(species = NULL, movement = NULL,
                               limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(species))  clauses <- c(clauses, sprintf("speciesname = '%s'", species))
  if (!is.null(movement)) clauses <- c(clauses, sprintf("movementtype = '%s'", movement))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._ind_soda_query("e245-r9ub", where = where, order = "intakedate DESC",
                   limit = limit, offset = offset)
}

# == Transportation ============================================================

#' Bloomington traffic counts
#'
#' Queries annual average daily traffic (AADT) count data for
#' Bloomington (view ID `dcr5-fg4c`). Includes location coordinates
#' and community identifiers.
#'
#' @param year Integer or NULL. Filter by count year.
#' @param community Character or NULL. Filter by community name.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with columns including loc_id, year, dir,
#'   lat (numeric), long (numeric), crossroad, aadt (integer),
#'   and community.
#'
#' @family indiana transportation
#'
#' @examples
#' \dontrun{
#' ind_traffic_counts(year = 2023, limit = 50)
#' }
#' @export
ind_traffic_counts <- function(year = NULL, community = NULL,
                               limit = 1000, offset = 0) {
  clauses <- character()
  if (!is.null(year))      clauses <- c(clauses, sprintf("year = '%d'", year))
  if (!is.null(community)) clauses <- c(clauses, sprintf("community = '%s'", community))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._ind_soda_query("dcr5-fg4c", where = where, limit = limit, offset = offset)
  if (nrow(df) == 0) return(tibble())
  df |> mutate(
    aadt = as.integer(aadt %||% NA),
    lat  = as.numeric(lat %||% NA),
    long = as.numeric(long %||% NA)
  )
}

#' Bloomington scooter activity (daily)
#'
#' Queries daily e-scooter activity data for Bloomington
#' (view ID `guch-m6cb`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with daily scooter usage columns.
#'
#' @family indiana transportation
#'
#' @examples
#' \dontrun{
#' ind_scooter_activity(limit = 30)
#' }
#' @export
ind_scooter_activity <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("guch-m6cb", order = "date DESC",
                   limit = limit, offset = offset)
}

#' Bloomington road closings
#'
#' Queries current and scheduled road closings in Bloomington
#' (view ID `2qb3-brs9`).
#'
#' @param limit Integer. Maximum rows. Default 500.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with road closure information.
#'
#' @family indiana transportation
#'
#' @examples
#' \dontrun{
#' ind_road_closings()
#' }
#' @export
ind_road_closings <- function(limit = 500, offset = 0) {
  ._ind_soda_query("2qb3-brs9", limit = limit, offset = offset)
}

# == Utilities / Water =========================================================

#' Bloomington water main breaks
#'
#' Queries water main break incident data for Bloomington
#' (view ID `587m-ajx8`).
#'
#' @param year Integer or NULL. Filter by year.
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with water main break records.
#'
#' @family indiana utilities
#'
#' @examples
#' \dontrun{
#' ind_water_main_breaks(year = 2024)
#' }
#' @export
ind_water_main_breaks <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("year = '%d'", year)
  ._ind_soda_query("587m-ajx8", where = where, limit = limit, offset = offset)
}

#' Bloomington sanitary sewer overflows
#'
#' Queries sanitary sewer overflow (SSO) incident data for Bloomington
#' (view ID `kfaf-msfz`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with sewer overflow records.
#'
#' @family indiana utilities
#'
#' @examples
#' \dontrun{
#' ind_sewer_overflows()
#' }
#' @export
ind_sewer_overflows <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("kfaf-msfz", limit = limit, offset = offset)
}

#' Bloomington COVID-19 wastewater monitoring
#'
#' Queries COVID-19 wastewater surveillance data for Bloomington
#' (view ID `yv82-z42g`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with wastewater monitoring readings by date.
#'
#' @family indiana utilities
#'
#' @examples
#' \dontrun{
#' ind_covid_wastewater(limit = 100)
#' }
#' @export
ind_covid_wastewater <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("yv82-z42g", order = "date DESC",
                   limit = limit, offset = offset)
}

#' Bloomington nuisance complaints
#'
#' Queries nuisance and code violation complaints for Bloomington
#' (view ID `8mur-twyk`).
#'
#' @param limit Integer. Maximum rows. Default 1000.
#' @param offset Integer. Pagination offset. Default 0.
#'
#' @return A tibble with nuisance complaint records.
#'
#' @family indiana services
#'
#' @examples
#' \dontrun{
#' ind_nuisance_complaints(limit = 100)
#' }
#' @export
ind_nuisance_complaints <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("8mur-twyk", limit = limit, offset = offset)
}

# == Context ===================================================================

#' Get in.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ind_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ind_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/in.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "in.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# in.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# in.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
