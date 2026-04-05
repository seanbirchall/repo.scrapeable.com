# blm.gov.R
# Self-contained Bureau of Land Management GIS client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public ArcGIS REST services)
# Docs: https://gis.blm.gov/arcgis/rest/services

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.blm_base <- "https://gis.blm.gov/arcgis/rest/services"

# -- Core ArcGIS query engine --------------------------------------------------

.blm_query <- function(service, layer = 0, where = "1=1", out_fields = "*",
                       result_count = 1000, result_offset = 0,
                       order_by = NULL) {
  url <- paste0(.blm_base, "/", service, "/FeatureServer/", layer, "/query")
  params <- list(
    where = where,
    outFields = out_fields,
    resultRecordCount = result_count,
    resultOffset = result_offset,
    f = "json"
  )
  if (!is.null(order_by)) params$orderByFields <- order_by

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# Paginated query returning tibble
.blm_fetch <- function(service, layer = 0, where = "1=1", out_fields = "*",
                       max_results = 500, order_by = NULL) {
  all_rows <- list()
  offset <- 0
  page <- min(max_results, 1000)

  repeat {
    raw <- .blm_query(service, layer, where, out_fields,
                      result_count = page, result_offset = offset,
                      order_by = order_by)
    feats <- raw$features
    if (is.null(feats) || length(feats) == 0) break

    rows <- lapply(feats, function(f) {
      attrs <- f$attributes
      lapply(attrs, function(v) {
        if (is.null(v)) NA_character_ else as.character(v)
      })
    })
    all_rows <- c(all_rows, rows)
    offset <- offset + length(feats)

    if (length(all_rows) >= max_results) break
    if (!isTRUE(raw$exceededTransferLimit) && length(feats) < page) break
  }

  if (length(all_rows) == 0) return(tibble())
  if (length(all_rows) > max_results) all_rows <- all_rows[seq_len(max_results)]
  bind_rows(lapply(all_rows, as_tibble))
}

# Count features
.blm_count <- function(service, layer = 0, where = "1=1") {
  url <- paste0(.blm_base, "/", service, "/FeatureServer/", layer, "/query")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(where = where, returnCountOnly = "true", f = "json") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  raw$count %||% 0L
}

# -- Context generator ---------------------------------------------------------

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

# == Public functions ==========================================================

#' @title List available BLM GIS service categories
#'
#' Returns the top-level folder structure of the Bureau of Land Management's
#' national ArcGIS REST services. Each folder contains one or more
#' FeatureServer or MapServer services. Use \code{blm_services()} to explore
#' services within a folder.
#'
#' @return A tibble with one column:
#'   \itemize{
#'     \item \code{folder} (character): Service category name, e.g. \code{"recreation"},
#'       \code{"lands"}, \code{"Cadastral"}, \code{"minerals"}, \code{"admin_boundaries"}
#'   }
#' @examples
#' \dontrun{
#' # See all available GIS service categories
#' blm_list()
#'
#' # Then explore a category
#' blm_services("recreation")
#' }
#' @export
blm_list <- function() {
  url <- paste0(.blm_base, "?f=json")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  folders <- vapply(raw$folders, as.character, character(1))
  tibble(folder = folders)
}

#' @title List services within a BLM GIS folder
#'
#' Returns the FeatureServer and MapServer services available within a
#' specific BLM GIS folder. Service names can be passed to \code{blm_query()}
#' or \code{blm_count()} for data access.
#'
#' @param folder Folder name from \code{blm_list()}. Examples:
#'   \code{"recreation"}, \code{"lands"}, \code{"minerals"}, \code{"Cadastral"}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{name} (character): Full service path, e.g. \code{"recreation/BLM_Natl_Recreation_Offline"}
#'     \item \code{type} (character): Service type, e.g. \code{"FeatureServer"}, \code{"MapServer"}
#'   }
#' @examples
#' \dontrun{
#' # List recreation services
#' blm_services("recreation")
#'
#' # List land management services
#' blm_services("lands")
#' }
#' @export
blm_services <- function(folder) {
  url <- paste0(.blm_base, "/", folder, "?f=json")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  if (length(raw$services) == 0) return(tibble(name = character(), type = character()))
  bind_rows(lapply(raw$services, function(s) {
    tibble(name = s$name %||% NA_character_, type = s$type %||% NA_character_)
  }))
}

#' @title Search BLM national recreation sites
#'
#' Queries the BLM National Recreation Sites FeatureServer for campgrounds,
#' trailheads, day-use areas, and other recreation areas on BLM-managed
#' public lands. Filter by state or search by name. Results are ordered
#' alphabetically by site name.
#'
#' @param query Search term to match against recreation area names (partial
#'   match, case-sensitive in ArcGIS). Examples: \code{"Red Rock"},
#'   \code{"Canyon"}, \code{"Lake"}. \code{NULL} returns all sites.
#'   Default \code{NULL}.
#' @param state Two-letter state abbreviation filter. Examples: \code{"NV"},
#'   \code{"UT"}, \code{"CA"}. \code{NULL} for all states. Default \code{NULL}.
#' @param max_results Maximum number of results to return. Default \code{100}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{RecAreaName} (character): Name of the recreation area
#'     \item \code{RecAreaDescription} (character): Description of the site
#'     \item \code{State} (character): Two-letter state abbreviation
#'     \item \code{RecAreaLatitude} (character): Latitude coordinate
#'     \item \code{RecAreaLongitude} (character): Longitude coordinate
#'     \item \code{RecAreaPhone} (character): Contact phone number
#'     \item \code{RecAreaEmail} (character): Contact email address
#'     \item \code{BLMRecURL} (character): URL to the BLM recreation page
#'     \item \code{ActivityNames} (character): Available activities (hiking, camping, etc.)
#'     \item \code{RecAreaDirections} (character): Driving directions
#'     \item \code{RecAreaFeeDescription} (character): Fee information
#'     \item \code{StayLimit} (character): Maximum stay duration
#'     \item \code{Keywords} (character): Search keywords
#'   }
#'   Plus additional metadata columns (OrgRecAreaID, LastUpdatedDate, etc.).
#' @examples
#' \dontrun{
#' # All recreation sites in Nevada
#' blm_search(state = "NV")
#'
#' # Search for sites with "Canyon" in the name
#' blm_search(query = "Canyon", max_results = 50)
#'
#' # Utah camping areas
#' blm_search(state = "UT", max_results = 200)
#' }
#' @export
blm_search <- function(query = NULL, state = NULL, max_results = 100) {
  clauses <- "1=1"
  if (!is.null(state)) {
    clauses <- paste0("State = '", toupper(state), "'")
  }
  if (!is.null(query)) {
    q_esc <- gsub("'", "''", query)
    clause <- paste0("RecAreaName LIKE '%", q_esc, "%'")
    clauses <- if (clauses == "1=1") clause else paste(clauses, "AND", clause)
  }

  .blm_fetch("recreation/BLM_Natl_Recreation_Offline", layer = 0,
             where = clauses, max_results = max_results,
             order_by = "RecAreaName ASC")
}

#' @title Get BLM National Landscape Conservation System areas
#'
#' Returns national monuments, national conservation areas, wilderness areas,
#' wild and scenic rivers, and other NLCS-designated lands managed by BLM.
#' Each row is one designated area with its classification, state, and links.
#'
#' @param state Two-letter state abbreviation filter. Examples: \code{"NV"},
#'   \code{"UT"}, \code{"CA"}. \code{NULL} for all states. Default \code{NULL}.
#' @param max_results Maximum results to return. Default \code{200}.
#' @return A tibble with columns (all character):
#'   \itemize{
#'     \item \code{Label} (character): Display label for the area
#'     \item \code{NCA_NAME} (character): National Conservation Area name
#'     \item \code{sma_code} (character): Special Management Area classification code
#'     \item \code{STATE_ADMN} (character): Administrative state
#'     \item \code{STATE_GEOG} (character): Geographic state
#'     \item \code{NLCS_ID} (character): NLCS identifier
#'     \item \code{DESCRIPTION} (character): Area description
#'     \item \code{WEBLINK} (character): URL to BLM page for this area
#'     \item \code{PHOTO_LINK} (character): URL to photo
#'     \item \code{PHOTO_TEXT} (character): Photo caption
#'     \item \code{Shape__Area} (character): Area in map units
#'     \item \code{Shape__Length} (character): Perimeter length
#'   }
#' @examples
#' \dontrun{
#' # All NLCS areas in Nevada
#' blm_nlcs(state = "NV")
#'
#' # All NLCS areas nationally
#' blm_nlcs(max_results = 500)
#'
#' # Utah conservation areas
#' blm_nlcs(state = "UT")
#' }
#' @export
blm_nlcs <- function(state = NULL, max_results = 200) {
  where <- "1=1"
  if (!is.null(state)) {
    where <- paste0("STATE_GEOG = '", toupper(state), "'")
  }
  .blm_fetch("lands/BLM_Natl_NLCS_Generalized", layer = 0,
             where = where, max_results = max_results)
}

#' @title Get BLM recreation site details by name
#'
#' Returns detailed information for a specific recreation site by partial
#' name match. Returns all available fields for matched sites. Use this
#' to get full details (directions, fees, activities) for a known site.
#'
#' @param name Exact or partial name of the recreation area (case-sensitive
#'   in ArcGIS). Examples: \code{"Red Rock"}, \code{"Slickrock"}.
#' @return A tibble (usually 1 row) with columns (all character):
#'   \itemize{
#'     \item \code{RecAreaName} (character): Site name
#'     \item \code{RecAreaDescription} (character): Full description
#'     \item \code{RecAreaDirections} (character): Driving directions
#'     \item \code{State} (character): State abbreviation
#'     \item \code{RecAreaLatitude} (character): Latitude
#'     \item \code{RecAreaLongitude} (character): Longitude
#'     \item \code{RecAreaPhone} (character): Phone number
#'     \item \code{RecAreaEmail} (character): Email address
#'     \item \code{BLMRecURL} (character): BLM website URL
#'     \item \code{ActivityNames} (character): Available activities
#'     \item \code{RecAreaFeeDescription} (character): Fee details
#'     \item \code{StayLimit} (character): Stay limit
#'   }
#'   Plus additional metadata columns.
#' @examples
#' \dontrun{
#' # Get details for Red Rock Canyon
#' blm_recreation_site("Red Rock")
#'
#' # Get details for a specific campground
#' blm_recreation_site("Slickrock")
#' }
#' @export
blm_recreation_site <- function(name) {
  q_esc <- gsub("'", "''", name)
  where <- paste0("RecAreaName LIKE '%", q_esc, "%'")
  .blm_fetch("recreation/BLM_Natl_Recreation_Offline", layer = 0,
             where = where, max_results = 10)
}

#' @title Count features in a BLM service layer
#'
#' Returns the total number of features matching a query in any BLM
#' FeatureServer layer. Useful for checking data volume before downloading
#' with \code{blm_query()}.
#'
#' @param service Service path. Find valid paths with \code{blm_services()}.
#'   Examples: \code{"recreation/BLM_Natl_Recreation_Offline"},
#'   \code{"lands/BLM_Natl_NLCS_Generalized"}.
#' @param layer Layer index within the service. Default \code{0}.
#' @param where SQL WHERE clause for filtering. Default \code{"1=1"} (all features).
#'   Example: \code{"State = 'NV'"}.
#' @return Integer. Total count of matching features.
#' @examples
#' \dontrun{
#' # Count all recreation sites
#' blm_count("recreation/BLM_Natl_Recreation_Offline")
#'
#' # Count recreation sites in Nevada
#' blm_count("recreation/BLM_Natl_Recreation_Offline", where = "State = 'NV'")
#' }
#' @export
blm_count <- function(service, layer = 0, where = "1=1") {
  .blm_count(service, layer, where)
}

#' @title Query any BLM FeatureServer layer
#'
#' Generic query function for any BLM ArcGIS FeatureServer endpoint.
#' Supports pagination, field selection, and SQL WHERE filtering. Use this
#' when the convenience functions (\code{blm_search}, \code{blm_nlcs}) do
#' not cover your use case. Find available services with \code{blm_services()}.
#'
#' @param service Service path. Examples:
#'   \code{"recreation/BLM_Natl_Recreation_Offline"},
#'   \code{"lands/BLM_Natl_NLCS_Generalized"},
#'   \code{"minerals/BLM_Natl_Mining_Claims"}.
#'   Find valid paths with \code{blm_services()}.
#' @param layer Layer index within the service. Default \code{0}. Most services
#'   have a single layer at index 0.
#' @param where SQL WHERE clause for filtering. Default \code{"1=1"} (all features).
#'   Examples: \code{"State = 'CA'"}, \code{"RecAreaName LIKE '\%Canyon\%'"}.
#' @param fields Comma-separated field names to return, or \code{"*"} for all
#'   fields. Default \code{"*"}. Example: \code{"RecAreaName,State,RecAreaLatitude"}.
#' @param max_results Maximum features to return. Default \code{500}. Pagination
#'   is handled automatically.
#' @return A tibble of feature attributes. Columns depend on the service/layer
#'   queried. All values are returned as character type.
#' @examples
#' \dontrun{
#' # Query recreation sites with specific fields
#' blm_query("recreation/BLM_Natl_Recreation_Offline",
#'           fields = "RecAreaName,State,ActivityNames",
#'           where = "State = 'UT'", max_results = 50)
#'
#' # Query NLCS conservation areas
#' blm_query("lands/BLM_Natl_NLCS_Generalized",
#'           where = "STATE_GEOG = 'AZ'")
#' }
#' @export
blm_query <- function(service, layer = 0, where = "1=1", fields = "*",
                      max_results = 500) {
  .blm_fetch(service, layer, where = where, out_fields = fields,
             max_results = max_results)
}

#' Get blm.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
blm_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(blm_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/blm.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "blm.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# blm.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# blm.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
