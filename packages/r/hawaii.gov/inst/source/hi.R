# hawaii.gov.R
# Self-contained Hawaii Open Data (CKAN) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 at opendata.hawaii.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hi_base <- "https://opendata.hawaii.gov"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- CKAN helpers --------------------------------------------------------------

._ckan_action <- function(action, params = list()) {
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- if (nchar(query) > 0) {
    sprintf("%s/api/action/%s?%s", .hi_base, action, query)
  } else {
    sprintf("%s/api/action/%s", .hi_base, action)
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Hawaii CKAN error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || !isTRUE(raw$success)) {
    if (!is.null(raw)) warning("CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

._datastore_search <- function(resource_id, q = NULL, filters = NULL,
                               limit = 1000, offset = 0) {
  params <- list(resource_id = resource_id, limit = limit, offset = offset)
  if (!is.null(q)) params[["q"]] <- q
  if (!is.null(filters)) params[["filters"]] <- filters

  result <- ._ckan_action("datastore_search", params)
  if (is.null(result)) return(tibble())

  records <- result$records
  if (is.null(records) || length(records) == 0) return(tibble())
  df <- as_tibble(records)
  df[["_id"]] <- NULL
  df
}

._datastore_paginate <- function(resource_id, q = NULL, max_rows = 10000,
                                 page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._datastore_search(resource_id, q = q, limit = page_size,
                                offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_packages <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(), notes = character()
)

.schema_records <- tibble()

# == Discovery =================================================================

#' List Hawaii Open Data datasets
#'
#' Returns metadata for datasets on the Hawaii open data portal.
#'
#' @param rows Number of datasets (default 50)
#' @param start Offset (default 0)
#' @return tibble: id, name, title, organization, num_resources, notes
#' @export
hi_list <- function(rows = 50, start = 0) {
  result <- ._ckan_action("package_search", list(rows = rows, start = start))
  if (is.null(result)) return(.schema_packages)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_packages)

  org <- tryCatch({
    if (is.data.frame(datasets$organization)) {
      as.character(datasets$organization$title)
    } else {
      vapply(datasets$organization %||% list(), function(o) {
        if (is.null(o)) NA_character_ else as.character(o$title %||% NA_character_)
      }, character(1))
    }
  }, error = function(e) rep(NA_character_, nrow(datasets)))

  tibble(
    id            = as.character(datasets$id),
    name          = as.character(datasets$name),
    title         = as.character(datasets$title),
    organization  = org,
    num_resources = as.integer(datasets$num_resources),
    notes         = ifelse(nchar(datasets$notes %||% "") > 200,
                           paste0(substr(datasets$notes, 1, 200), "..."),
                           as.character(datasets$notes %||% ""))
  )
}

#' Search Hawaii Open Data datasets
#'
#' Full-text search of the CKAN catalog.
#'
#' @param query Search term
#' @param rows Number of results (default 50)
#' @return tibble: id, name, title, organization, num_resources, notes
#' @export
hi_search <- function(query, rows = 50) {
  result <- ._ckan_action("package_search",
                          list(q = utils::URLencode(query, reserved = TRUE),
                               rows = rows))
  if (is.null(result)) return(.schema_packages)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_packages)

  org <- tryCatch({
    if (is.data.frame(datasets$organization)) {
      as.character(datasets$organization$title)
    } else {
      vapply(datasets$organization %||% list(), function(o) {
        if (is.null(o)) NA_character_ else as.character(o$title %||% NA_character_)
      }, character(1))
    }
  }, error = function(e) rep(NA_character_, nrow(datasets)))

  tibble(
    id            = as.character(datasets$id),
    name          = as.character(datasets$name),
    title         = as.character(datasets$title),
    organization  = org,
    num_resources = as.integer(datasets$num_resources),
    notes         = ifelse(nchar(datasets$notes %||% "") > 200,
                           paste0(substr(datasets$notes, 1, 200), "..."),
                           as.character(datasets$notes %||% ""))
  )
}

#' Query a Hawaii datastore resource
#'
#' Generic query against any CKAN datastore-backed resource.
#'
#' @param resource_id CKAN resource UUID
#' @param q Full-text search within resource
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
hi_view <- function(resource_id, q = NULL, limit = 1000, offset = 0) {
  ._datastore_search(resource_id, q = q, limit = limit, offset = offset)
}

#' Fetch all records from a Hawaii datastore resource
#'
#' Auto-paginated fetch.
#'
#' @param resource_id CKAN resource UUID
#' @param q Optional full-text search
#' @param max_rows Maximum total rows (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble
#' @export
hi_fetch_all <- function(resource_id, q = NULL, max_rows = 10000,
                         page_size = 1000) {
  ._datastore_paginate(resource_id, q = q, max_rows = max_rows,
                       page_size = page_size)
}

# == Campaign Finance ==========================================================

#' Campaign contributions to Hawaii candidates
#'
#' Contributions received by Hawaii state and county candidates (2015-present).
#' Source: resource 443bd998-1ef3-47da-9170-c2c376b2e41c.
#'
#' @param candidate Optional candidate name filter (full-text search)
#' @param limit Max rows (default 1000)
#' @return tibble: Candidate.Name, Contributor.Type, Date, Amount, Office, ...
#' @export
hi_contributions <- function(candidate = NULL, limit = 1000) {
  q <- if (!is.null(candidate)) candidate else NULL
  ._datastore_search("443bd998-1ef3-47da-9170-c2c376b2e41c",
                     q = q, limit = limit)
}

#' Campaign expenditures by Hawaii candidates
#'
#' Expenditures made by Hawaii state and county candidates (2015-present).
#' Source: resource ca3ac02a-eb44-4b44-b3a7-5f60653cc1d3.
#'
#' @param candidate Optional candidate name filter (full-text search)
#' @param limit Max rows (default 1000)
#' @return tibble: candidate name, payee, amount, expenditure type, etc.
#' @export
hi_expenditures <- function(candidate = NULL, limit = 1000) {
  q <- if (!is.null(candidate)) candidate else NULL
  ._datastore_search("ca3ac02a-eb44-4b44-b3a7-5f60653cc1d3",
                     q = q, limit = limit)
}

# == Energy ====================================================================

#' Hawaii annual electricity cost by county
#'
#' Annual electricity cost in dollars for each county and statewide.
#' Source: resource 999dccc3-20b0-43d9-a4ba-2b007be05811.
#'
#' @param limit Max rows (default 100)
#' @return tibble: Year, Honolulu, Maui.County, Hawaii.County, Kauai.County, State
#' @export
hi_electricity_cost <- function(limit = 100) {
  ._datastore_search("999dccc3-20b0-43d9-a4ba-2b007be05811", limit = limit)
}

#' Hawaii annual electricity consumption
#'
#' Annual electricity consumption in MWh by source.
#' Source: resource 2ecf5eff-3599-40f5-a357-f1a266708b69.
#'
#' @param limit Max rows (default 100)
#' @return tibble: Year and consumption columns
#' @export
hi_electricity_consumption <- function(limit = 100) {
  ._datastore_search("2ecf5eff-3599-40f5-a357-f1a266708b69", limit = limit)
}

#' Hawaii fossil fuel consumption and expenditures
#'
#' Source: resource c9b38d5b-cd1c-418e-a736-d8acceb518cb.
#'
#' @param limit Max rows (default 100)
#' @return tibble with fuel consumption data
#' @export
hi_fossil_fuel <- function(limit = 100) {
  ._datastore_search("c9b38d5b-cd1c-418e-a736-d8acceb518cb", limit = limit)
}

# == Agriculture & Environment =================================================

#' Plant pathogens of Hawaii
#'
#' Checklist database of plant pathogens reported across Hawaiian islands.
#' Source: resource e00d43f6-b324-4694-b2e9-777bad862007.
#'
#' @param q Optional full-text search (e.g. pathogen or host name)
#' @param limit Max rows (default 1000)
#' @return tibble: host_family, host_genus, pathogen_genus, pathogen_species, islands, ...
#' @export
hi_plant_pathogens <- function(q = NULL, limit = 1000) {
  ._datastore_search("e00d43f6-b324-4694-b2e9-777bad862007",
                     q = q, limit = limit)
}

#' Hawaii pesticide products
#'
#' Chemical pesticide products registered in Hawaii.
#' Source: resource 63701ab4-72a4-45e5-80a9-24ebbfce72c4.
#'
#' @param q Optional search term
#' @param limit Max rows (default 1000)
#' @return tibble with pesticide product details
#' @export
hi_pesticides <- function(q = NULL, limit = 1000) {
  ._datastore_search("63701ab4-72a4-45e5-80a9-24ebbfce72c4",
                     q = q, limit = limit)
}

# == Education =================================================================

#' University of Hawaii student enrollment by characteristics
#'
#' Source: resource 292575da-41cc-4f03-8012-5c1a8743fc74.
#'
#' @param limit Max rows (default 1000)
#' @return tibble with enrollment data
#' @export
hi_enrollment <- function(limit = 1000) {
  ._datastore_search("292575da-41cc-4f03-8012-5c1a8743fc74", limit = limit)
}

#' University of Hawaii degrees awarded
#'
#' Source: resource 3a0b8368-71a7-4402-b9c7-9fc5a398a952.
#'
#' @param limit Max rows (default 1000)
#' @return tibble with degree data
#' @export
hi_degrees <- function(limit = 1000) {
  ._datastore_search("3a0b8368-71a7-4402-b9c7-9fc5a398a952", limit = limit)
}

# == Economic Indicators =======================================================

#' Hawaii monthly economic indicators
#'
#' Labor force, unemployment, wage/salary jobs by sector, visitor arrivals,
#' and other economic indicators over time.
#' Source: resource 08f83740-6dcc-455f-8331-a4491ccce0f6.
#'
#' @param limit Max rows (default 1000)
#' @return tibble with monthly economic data
#' @export
hi_economic_indicators <- function(limit = 1000) {
  ._datastore_search("08f83740-6dcc-455f-8331-a4491ccce0f6", limit = limit)
}

# == Infrastructure ============================================================

#' Hawaii public parking locations
#'
#' State parking lot locations.
#' Source: resource 723ebf23-9dea-457a-9235-16659cfe0024.
#'
#' @param limit Max rows (default 500)
#' @return tibble with parking location data
#' @export
hi_parking <- function(limit = 500) {
  ._datastore_search("723ebf23-9dea-457a-9235-16659cfe0024", limit = limit)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the Hawaii Open Data package
#'
#' @return Character string (invisibly), also printed
#' @export
hi_context <- function() {
  .build_context("hawaii.gov", header_lines = c(
    "# hawaii.gov - Hawaii Open Data (CKAN) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles.",
    "#",
    "# Covers: catalog, campaign finance, energy, agriculture,",
    "#   education, economic indicators, infrastructure.",
    "# Generic queries supported via hi_view(resource_id)."
  ))
}
