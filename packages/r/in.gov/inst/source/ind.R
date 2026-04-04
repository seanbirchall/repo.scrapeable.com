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
#' @param limit Number of datasets to return (default 100, max 200)
#' @param category Optional category filter
#' @return tibble: id, name, description, category, type, updated_at, view_count
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

#' Search datasets by keyword
#'
#' @param query Search term
#' @param limit Max results (default 20)
#' @return tibble: id, name, description, category, type, updated_at, view_count
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

#' Get metadata for a specific dataset
#'
#' @param view_id Socrata view ID (e.g. "t5xf-ggw6")
#' @return tibble with one row: id, name, description, category, row_count, columns
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
#' @param view_id Socrata view ID
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param order SoQL ORDER BY clause
#' @param group SoQL GROUP BY clause
#' @param q Full-text search query
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble
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
#' @param nature Filter by nature of call (e.g. "THEFT", "ACCIDENT")
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble: case_number, datetime, month, weekday, nature, district, agency, report
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
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @return tibble: case_number, datetime, month, weekday, district, agency
ind_accidents <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                        year, year + 1)
  ._ind_soda_query("vf95-pwwj", where = where, order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington domestic violence reports
#'
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @return tibble: case_number, datetime, nature, district, demographics
ind_domestic_violence <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                        year, year + 1)
  ._ind_soda_query("vq37-rm9u", where = where, order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington police use of force incidents
#'
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @return tibble
ind_use_of_force <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("datetime >= '%d-01-01' AND datetime < '%d-01-01'",
                                        year, year + 1)
  ._ind_soda_query("7jzv-6jei", where = where, order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington hate crimes
#'
#' @param limit Max rows (default 1000)
#' @return tibble
ind_hate_crimes <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("vzyb-ttns", order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington stolen guns reports
#'
#' @param limit Max rows (default 1000)
#' @return tibble
ind_stolen_guns <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("y66s-bnfm", order = "datetime DESC",
                   limit = limit, offset = offset)
}

#' Bloomington citizen complaints against police
#'
#' @param limit Max rows (default 1000)
#' @return tibble
ind_citizen_complaints <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("kit3-8bua", limit = limit, offset = offset)
}

# == Fire & Emergency ==========================================================

#' Bloomington fire incident responses
#'
#' @param type Filter by incident type description
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @return tibble: inci_no, unit, inci_type, inci_desc, alm_datetime, address, resp_time
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
#' @param department Filter by department name (partial match)
#' @param year Filter by year (uses asofdate field)
#' @param limit Max rows (default 1000)
#' @return tibble: employeeid, firstname, lastname, department, title, compensation, asofdate
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
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
ind_contracts <- function(q = NULL, limit = 1000, offset = 0) {
  ._ind_soda_query("ruzy-efni", q = q, limit = limit, offset = offset)
}

#' Bloomington rental permits
#'
#' @param status Filter by status (e.g. "Complete", "In Review")
#' @param limit Max rows (default 1000)
#' @return tibble: permit_num, property_address, status, contact_name, units
ind_rental_permits <- function(status = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(status)) sprintf("status = '%s'", status) else NULL
  ._ind_soda_query("9q6j-a8rc", where = where, order = "permit_num DESC",
                   limit = limit, offset = offset)
}

# == 311 / Service Requests ====================================================

#' Bloomington Open311 service requests
#'
#' @param service Filter by service name (e.g. "Trash", "Recycling", "Pothole")
#' @param status Filter by status ("open" or "closed")
#' @param limit Max rows (default 1000)
#' @return tibble: service_request_id, requested_datetime, status_description,
#'   service_name, description, address
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
#' @param species Filter by species ("Cat", "Dog", etc.)
#' @param movement Filter by movement type ("Adoption", "Reclaimed", etc.)
#' @param limit Max rows (default 1000)
#' @return tibble: id, intakedate, sheltercode, animalname, breedname,
#'   speciesname, sexname, movementtype
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
#' @param year Filter by year
#' @param community Filter by community
#' @param limit Max rows (default 1000)
#' @return tibble: loc_id, year, dir, lat, long, crossroad, aadt, community
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
#' @param limit Max rows (default 1000)
#' @return tibble
ind_scooter_activity <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("guch-m6cb", order = "date DESC",
                   limit = limit, offset = offset)
}

#' Bloomington road closings
#'
#' @param limit Max rows (default 500)
#' @return tibble
ind_road_closings <- function(limit = 500, offset = 0) {
  ._ind_soda_query("2qb3-brs9", limit = limit, offset = offset)
}

# == Utilities / Water =========================================================

#' Bloomington water main breaks
#'
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @return tibble
ind_water_main_breaks <- function(year = NULL, limit = 1000, offset = 0) {
  where <- NULL
  if (!is.null(year)) where <- sprintf("year = '%d'", year)
  ._ind_soda_query("587m-ajx8", where = where, limit = limit, offset = offset)
}

#' Bloomington sanitary sewer overflows
#'
#' @param limit Max rows (default 1000)
#' @return tibble
ind_sewer_overflows <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("kfaf-msfz", limit = limit, offset = offset)
}

#' Bloomington COVID-19 wastewater monitoring
#'
#' @param limit Max rows (default 1000)
#' @return tibble
ind_covid_wastewater <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("yv82-z42g", order = "date DESC",
                   limit = limit, offset = offset)
}

#' Bloomington nuisance complaints
#'
#' @param limit Max rows (default 1000)
#' @return tibble
ind_nuisance_complaints <- function(limit = 1000, offset = 0) {
  ._ind_soda_query("8mur-twyk", limit = limit, offset = offset)
}

# == Context ===================================================================

#' Generate LLM-friendly context for in.gov client
#'
#' @return Character string with all public function signatures and bodies
ind_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/in.gov.R"
  if (!file.exists(src_file)) {
    cat("# in.gov context - source not found\n")
    return(invisible("# in.gov context - source not found"))
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
