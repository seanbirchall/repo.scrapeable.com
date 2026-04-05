# hawaii.gov.R
# Self-contained Hawaii Open Data (CKAN) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 at opendata.hawaii.gov


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Returns metadata for datasets on the Hawaii open data portal
#' (\url{https://opendata.hawaii.gov}), powered by CKAN.
#'
#' @param rows Integer. Number of datasets to return (default 50).
#' @param start Integer. Offset for pagination (default 0).
#' @return A tibble with columns: \code{id} (character, CKAN UUID),
#'   \code{name} (character, URL slug), \code{title} (character),
#'   \code{organization} (character, publishing org name),
#'   \code{num_resources} (integer), \code{notes} (character, truncated
#'   description).
#' @examples
#' \dontrun{
#' hi_list()
#' hi_list(rows = 10, start = 20)
#' }
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
#' Full-text search of the CKAN catalog at opendata.hawaii.gov.
#'
#' @param query Character. Search term (e.g. \code{"energy"},
#'   \code{"tourism"}, \code{"education"}, \code{"agriculture"}).
#' @param rows Integer. Number of results (default 50).
#' @return A tibble with columns: \code{id}, \code{name}, \code{title},
#'   \code{organization}, \code{num_resources}, \code{notes} (same schema
#'   as \code{\link{hi_list}}).
#' @examples
#' \dontrun{
#' hi_search("energy")
#' hi_search("tourism", rows = 10)
#' }
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
#' Generic query against any CKAN datastore-backed resource. Find
#' resource IDs by browsing \code{\link{hi_list}} or \code{\link{hi_search}}.
#'
#' @param resource_id Character. CKAN resource UUID (e.g.
#'   \code{"999dccc3-20b0-43d9-a4ba-2b007be05811"}).
#' @param q Character or NULL. Full-text search within resource records.
#' @param limit Integer. Max rows (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with resource-specific columns. Column names and types
#'   vary by dataset.
#' @examples
#' \dontrun{
#' hi_view("999dccc3-20b0-43d9-a4ba-2b007be05811", limit = 10)
#' }
#' @export
hi_view <- function(resource_id, q = NULL, limit = 1000, offset = 0) {
  ._datastore_search(resource_id, q = q, limit = limit, offset = offset)
}

#' Fetch all records from a Hawaii datastore resource
#'
#' Auto-paginated fetch that retrieves all records from a CKAN
#' datastore resource, up to \code{max_rows}.
#'
#' @param resource_id Character. CKAN resource UUID.
#' @param q Character or NULL. Optional full-text search within records.
#' @param max_rows Integer. Maximum total rows to fetch (default 10000).
#' @param page_size Integer. Rows per API request (default 1000).
#' @return A tibble with resource-specific columns.
#' @examples
#' \dontrun{
#' hi_fetch_all("999dccc3-20b0-43d9-a4ba-2b007be05811", max_rows = 500)
#' }
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
#' Source: CKAN resource \code{443bd998-1ef3-47da-9170-c2c376b2e41c}.
#'
#' @param candidate Character or NULL. Candidate name filter (full-text search,
#'   e.g. \code{"Abercrombie"}).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including: \code{Candidate Name},
#'   \code{Contributor Type}, \code{Contributor Name}, \code{Date},
#'   \code{Amount} (integer), \code{Aggregate} (integer), \code{Employer},
#'   \code{Occupation}, \code{City}, \code{State}, \code{Zip Code},
#'   \code{Office}, \code{Reg No}, \code{Election Period}, \code{InOutState},
#'   \code{Range}.
#' @examples
#' \dontrun{
#' hi_contributions(limit = 50)
#' hi_contributions(candidate = "Abercrombie", limit = 100)
#' }
#' @export
hi_contributions <- function(candidate = NULL, limit = 1000) {
  q <- if (!is.null(candidate)) candidate else NULL
  ._datastore_search("443bd998-1ef3-47da-9170-c2c376b2e41c",
                     q = q, limit = limit)
}

#' Campaign expenditures by Hawaii candidates
#'
#' Expenditures made by Hawaii state and county candidates (2015-present).
#' Source: CKAN resource \code{ca3ac02a-eb44-4b44-b3a7-5f60653cc1d3}.
#'
#' @param candidate Character or NULL. Candidate name filter (full-text search).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including candidate name, payee, amount,
#'   expenditure type, date, office, and election period.
#' @examples
#' \dontrun{
#' hi_expenditures(limit = 50)
#' hi_expenditures(candidate = "Ige")
#' }
#' @export
hi_expenditures <- function(candidate = NULL, limit = 1000) {
  q <- if (!is.null(candidate)) candidate else NULL
  ._datastore_search("ca3ac02a-eb44-4b44-b3a7-5f60653cc1d3",
                     q = q, limit = limit)
}

# == Energy ====================================================================

#' Hawaii annual electricity cost by county
#'
#' Annual electricity cost in dollars for each county and statewide total.
#' Source: CKAN resource \code{999dccc3-20b0-43d9-a4ba-2b007be05811}.
#'
#' @param limit Integer. Max rows (default 100).
#' @return A tibble with columns: \code{Year} (character),
#'   \code{County of Honolulu (dollars)}, \code{Maui County (dollars)},
#'   \code{Hawaii County (dollars)}, \code{Kauai County (dollars)},
#'   \code{State  (dollars)}. Values are character strings with commas.
#' @examples
#' \dontrun{
#' hi_electricity_cost()
#' }
#' @export
hi_electricity_cost <- function(limit = 100) {
  ._datastore_search("999dccc3-20b0-43d9-a4ba-2b007be05811", limit = limit)
}

#' Hawaii annual electricity consumption
#'
#' Annual electricity consumption in MWh by source and county.
#' Source: CKAN resource \code{2ecf5eff-3599-40f5-a357-f1a266708b69}.
#'
#' @param limit Integer. Max rows (default 100).
#' @return A tibble with year and consumption columns by county and source.
#' @examples
#' \dontrun{
#' hi_electricity_consumption()
#' }
#' @export
hi_electricity_consumption <- function(limit = 100) {
  ._datastore_search("2ecf5eff-3599-40f5-a357-f1a266708b69", limit = limit)
}

#' Hawaii fossil fuel consumption and expenditures
#'
#' Fossil fuel consumption and expenditure data for Hawaii.
#' Source: CKAN resource \code{c9b38d5b-cd1c-418e-a736-d8acceb518cb}.
#'
#' @param limit Integer. Max rows (default 100).
#' @return A tibble with fuel type, consumption, and expenditure columns.
#' @examples
#' \dontrun{
#' hi_fossil_fuel()
#' }
#' @export
hi_fossil_fuel <- function(limit = 100) {
  ._datastore_search("c9b38d5b-cd1c-418e-a736-d8acceb518cb", limit = limit)
}

# == Agriculture & Environment =================================================

#' Plant pathogens of Hawaii
#'
#' Checklist database of plant pathogens reported across Hawaiian islands.
#' Source: CKAN resource \code{e00d43f6-b324-4694-b2e9-777bad862007}.
#'
#' @param q Character or NULL. Full-text search (e.g. pathogen or host name
#'   like \code{"rust"}, \code{"Coffea"}).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with columns including host family, host genus,
#'   pathogen genus, pathogen species, and island occurrence data.
#' @examples
#' \dontrun{
#' hi_plant_pathogens(limit = 50)
#' hi_plant_pathogens(q = "rust")
#' }
#' @export
hi_plant_pathogens <- function(q = NULL, limit = 1000) {
  ._datastore_search("e00d43f6-b324-4694-b2e9-777bad862007",
                     q = q, limit = limit)
}

#' Hawaii pesticide products
#'
#' Chemical pesticide products registered in Hawaii.
#' Source: CKAN resource \code{63701ab4-72a4-45e5-80a9-24ebbfce72c4}.
#'
#' @param q Character or NULL. Search term (e.g. product name or active
#'   ingredient).
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with pesticide product registration details.
#' @examples
#' \dontrun{
#' hi_pesticides(limit = 50)
#' hi_pesticides(q = "glyphosate")
#' }
#' @export
hi_pesticides <- function(q = NULL, limit = 1000) {
  ._datastore_search("63701ab4-72a4-45e5-80a9-24ebbfce72c4",
                     q = q, limit = limit)
}

# == Education =================================================================

#' University of Hawaii student enrollment by characteristics
#'
#' Student enrollment data by campus and demographic characteristics.
#' Source: CKAN resource \code{292575da-41cc-4f03-8012-5c1a8743fc74}.
#'
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with enrollment data by campus, year, and student
#'   characteristics.
#' @examples
#' \dontrun{
#' hi_enrollment(limit = 100)
#' }
#' @export
hi_enrollment <- function(limit = 1000) {
  ._datastore_search("292575da-41cc-4f03-8012-5c1a8743fc74", limit = limit)
}

#' University of Hawaii degrees awarded
#'
#' Degrees awarded by the University of Hawaii system by campus, year,
#' and program.
#' Source: CKAN resource \code{3a0b8368-71a7-4402-b9c7-9fc5a398a952}.
#'
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with degree data by campus, year, and field.
#' @examples
#' \dontrun{
#' hi_degrees(limit = 100)
#' }
#' @export
hi_degrees <- function(limit = 1000) {
  ._datastore_search("3a0b8368-71a7-4402-b9c7-9fc5a398a952", limit = limit)
}

# == Economic Indicators =======================================================

#' Hawaii monthly economic indicators
#'
#' Labor force, unemployment, wage/salary jobs by sector, visitor arrivals,
#' and other economic indicators over time.
#' Source: CKAN resource \code{08f83740-6dcc-455f-8331-a4491ccce0f6}.
#'
#' @param limit Integer. Max rows (default 1000).
#' @return A tibble with monthly economic data including labor force,
#'   unemployment rate, jobs by sector, and visitor statistics.
#' @examples
#' \dontrun{
#' hi_economic_indicators(limit = 100)
#' }
#' @export
hi_economic_indicators <- function(limit = 1000) {
  ._datastore_search("08f83740-6dcc-455f-8331-a4491ccce0f6", limit = limit)
}

# == Infrastructure ============================================================

#' Hawaii public parking locations
#'
#' State parking lot locations and details.
#' Source: CKAN resource \code{723ebf23-9dea-457a-9235-16659cfe0024}.
#'
#' @param limit Integer. Max rows (default 500).
#' @return A tibble with parking location data including names, addresses,
#'   and coordinates.
#' @examples
#' \dontrun{
#' hi_parking()
#' }
#' @export
hi_parking <- function(limit = 500) {
  ._datastore_search("723ebf23-9dea-457a-9235-16659cfe0024", limit = limit)
}

# == Context ===================================================================

#' Get hawaii.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/hawaii.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "hawaii.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# hawaii.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# hawaii.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
