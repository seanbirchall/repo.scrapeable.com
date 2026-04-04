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
#' @param limit Number of datasets to return (default 100, max 200)
#' @param category Optional category filter
#' @return tibble: id, name, description, category, type, updated_at, view_count
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
#' @param query Search term
#' @param limit Max results (default 20)
#' @return tibble: id, name, description, category, type, updated_at, view_count
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

#' Get metadata for a specific dataset
#'
#' @param view_id Socrata view ID (e.g. "yaai-7frk")
#' @return tibble with one row: id, name, description, category, row_count, column_count
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
#' @param view_id Socrata view ID
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param order SoQL ORDER BY clause
#' @param group SoQL GROUP BY clause
#' @param q Full-text search query
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble
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
#' @param animal_type Filter by type ("Cat", "Dog", "Rabbit", etc.)
#' @param record_type Filter by record type ("FOUND", "LOST", "ADOPTABLE")
#' @param limit Max rows (default 1000)
#' @return tibble: impound_no, animal_id, record_type, animal_type, age,
#'   animal_gender, animal_breed, animal_color, date, obfuscated_address
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
#' @param nibrs_code Filter by NIBRS code (e.g. "13A" for Aggravated Assault)
#' @param city Filter by city name
#' @param year Filter by year
#' @param limit Max rows (default 1000)
#' @return tibble: case_number, incident_datetime, nibrs_code, nibrs_code_name,
#'   block_address, city, state, zip, district, precinct
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
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
kc_jail_bookings <- function(q = NULL, limit = 1000, offset = 0) {
  ._kc_soda_query("j56h-zgnm", q = q, order = "dt_book DESC",
                   limit = limit, offset = offset)
}

# == Health ====================================================================

#' King County food establishment inspection data
#'
#' @param name Filter by business name (partial match)
#' @param city Filter by city
#' @param result Filter by inspection result ("Satisfactory", "Unsatisfactory", etc.)
#' @param limit Max rows (default 1000)
#' @return tibble: name, inspection_date, address, city, zip_code,
#'   inspection_type, inspection_score, inspection_result, violation_description
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
#' @param q Full-text search (provider name, location, etc.)
#' @param adults Filter for adult services (TRUE/FALSE)
#' @param limit Max rows (default 1000)
#' @return tibble: provider, phone, website, locations, notes, service flags
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
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble
kc_lead_products <- function(q = NULL, limit = 1000, offset = 0) {
  ._kc_soda_query("i6sy-ckp7", q = q, limit = limit, offset = offset)
}

# == Environment & Water =======================================================

#' King County water quality monitoring data
#'
#' @param site_type Filter by site type (e.g. "Stream", "Lake", "Marine")
#' @param parameter Filter by parameter name
#' @param limit Max rows (default 1000)
#' @return tibble
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
#' @param limit Max rows (default 1000)
#' @return tibble
kc_streams_wqi <- function(limit = 1000, offset = 0) {
  ._kc_soda_query("ww4k-7bef", limit = limit, offset = offset)
}

#' King County freshwater swim beach data
#'
#' @param beach Filter by beach name
#' @param year Filter by year (based on date field)
#' @param limit Max rows (default 1000)
#' @return tibble: beach, jurisdiction, date, geomean30d, watertempc, watertempf
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
#' @param site_id Filter by site ID
#' @param parameter Filter by parameter (e.g. "discharge", "stage")
#' @param limit Max rows (default 1000)
#' @return tibble: site_id, datetime, corrected_data, parameter, provisional
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
#' @param limit Max rows (default 1000)
#' @return tibble
kc_freshwater_ctd <- function(limit = 1000, offset = 0) {
  ._kc_soda_query("nrpy-ni8p", order = "date DESC",
                   limit = limit, offset = offset)
}

# == Property & Tax ============================================================

#' King County real property tax receivables
#'
#' @param bill_year Filter by billing year
#' @param limit Max rows (default 1000)
#' @return tibble: account_number, levy_code, bill_year, billed_amount, paid_amount
kc_property_tax <- function(bill_year = NULL, limit = 1000, offset = 0) {
  where <- if (!is.null(bill_year)) sprintf("bill_year = '%d'", bill_year) else NULL
  ._kc_soda_query("dkna-i698", where = where, limit = limit, offset = offset)
}

#' King County foreclosure parcels
#'
#' @param limit Max rows (default 1000)
#' @return tibble
kc_foreclosures <- function(limit = 1000, offset = 0) {
  ._kc_soda_query("nx4x-daw6", limit = limit, offset = offset)
}

# == Elections =================================================================

#' King County election results (February 2026 Special)
#'
#' @param race Filter by race name (partial match)
#' @param precinct Filter by precinct
#' @param limit Max rows (default 1000)
#' @return tibble: precinct, race, countergroup, countertype, sumofcount
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
#' @param material Search by material handled (e.g. "Electronics", "Paint")
#' @param q Full-text search
#' @param limit Max rows (default 1000)
#' @return tibble: provider_name, provider_address, phone, material_handled,
#'   service_description, fee
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
#' @param limit Max rows (default 100)
#' @return tibble
kc_population_2010 <- function(limit = 100, offset = 0) {
  ._kc_soda_query("cavj-x985", limit = limit, offset = offset)
}

#' Total King County housing units (2000-2010)
#'
#' @param limit Max rows (default 100)
#' @return tibble
kc_housing <- function(limit = 100, offset = 0) {
  ._kc_soda_query("bs3e-nncv", limit = limit, offset = offset)
}

# == Context ===================================================================

#' Generate LLM-friendly context for kingcounty.gov client
#'
#' @return Character string with all public function signatures and bodies
kc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/kingcounty.gov.R"
  if (!file.exists(src_file)) {
    cat("# kingcounty.gov context - source not found\n")
    return(invisible("# kingcounty.gov context - source not found"))
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
