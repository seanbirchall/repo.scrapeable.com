# greatschools.org.R
# Self-contained GreatSchools API client.
# Returns school data including ratings, locations, and details.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required (register at https://www.greatschools.org/api/registration.page)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gs_base <- "https://gs-api.greatschools.org"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json", api_key = NULL) {
  tmp <- tempfile(fileext = ext)
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua)
  if (!is.null(api_key)) {
    req <- req |> httr2::req_headers(`x-api-key` = api_key)
  }
  req |> httr2::req_perform(path = tmp)
  tmp
}

.fetch_gs_json <- function(url, api_key) {
  jsonlite::fromJSON(.fetch(url, api_key = api_key))
}

.gs_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("GREATSCHOOLS_API_KEY", "")
  if (key == "") stop("GreatSchools API key required. Set GREATSCHOOLS_API_KEY or pass api_key parameter.\nRegister at https://www.greatschools.org/api/registration.page")
  key
}

# == Schemas ===================================================================

.schema_schools <- tibble(
  id = integer(),
  name = character(),
  state = character(),
  city = character(),
  address = character(),
  zip = character(),
  phone = character(),
  type = character(),
  level = character(),
  rating = integer(),
  lat = numeric(),
  lon = numeric(),
  district_name = character(),
  enrollment = integer(),
  url = character()
)

# == Public functions ==========================================================

#' Search for schools by location
#'
#' Search the GreatSchools database for schools in a specific city and state.
#' Returns school profiles including GreatSchools ratings (1-10 scale),
#' enrollment, location coordinates, and contact information. Requires a
#' GreatSchools API key (register at
#' \url{https://www.greatschools.org/api/registration.page}).
#'
#' @param state Character. Two-letter state abbreviation (e.g., \code{"PA"}).
#' @param city Character. City name (e.g., \code{"Philadelphia"}).
#' @param limit Integer. Maximum results to return (default 20).
#' @param school_type Character or \code{NULL}. Filter by type: \code{"public"},
#'   \code{"charter"}, or \code{"private"}. \code{NULL} (default) returns all.
#' @param level Character or \code{NULL}. Filter by level: \code{"elementary"},
#'   \code{"middle"}, or \code{"high"}. \code{NULL} (default) returns all.
#' @param api_key Character or \code{NULL}. GreatSchools API key. Defaults to
#'   \code{GREATSCHOOLS_API_KEY} environment variable.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. GreatSchools school ID.}
#'     \item{name}{Character. School name.}
#'     \item{state}{Character. State abbreviation.}
#'     \item{city}{Character. City name.}
#'     \item{address}{Character. Street address.}
#'     \item{zip}{Character. ZIP code.}
#'     \item{phone}{Character. Phone number.}
#'     \item{type}{Character. School type (public, charter, private).}
#'     \item{level}{Character. Grade level (elementary, middle, high).}
#'     \item{rating}{Integer. GreatSchools rating (1-10, 10 = best).}
#'     \item{lat}{Numeric. Latitude.}
#'     \item{lon}{Numeric. Longitude.}
#'     \item{district_name}{Character. School district name.}
#'     \item{enrollment}{Integer. Student enrollment count.}
#'     \item{url}{Character. GreatSchools profile URL.}
#'   }
#' @export
#' @family greatschools functions
#' @seealso [gs_nearby()] for radius search, [gs_search()] to search by name
#' @examples
#' \dontrun{
#' gs_schools("CA", "San Francisco", limit = 10)
#' gs_schools("NY", "New York", school_type = "public", level = "high")
#' }
gs_schools <- function(state, city, limit = 20, school_type = NULL,
                       level = NULL, api_key = NULL) {
  key <- .gs_key(api_key)
  url <- sprintf("%s/schools?state=%s&city=%s&limit=%d",
                 .gs_base, state, utils::URLencode(city), limit)
  if (!is.null(school_type)) url <- paste0(url, "&school_type=", school_type)
  if (!is.null(level)) url <- paste0(url, "&level_code=", level)

  raw <- .fetch_gs_json(url, key)
  schools <- raw$schools %||% raw
  if (is.null(schools) || length(schools) == 0) return(.schema_schools)

  if (is.data.frame(schools)) {
    as_tibble(schools) |>
      transmute(
        id = as.integer(id %||% universal_id %||% NA),
        name = as.character(if ("name" %in% names(schools)) name else NA),
        state = as.character(if ("state" %in% names(schools)) state else NA),
        city = as.character(if ("city" %in% names(schools)) city else NA),
        address = as.character(if ("street" %in% names(schools)) street else
                               if ("address" %in% names(schools)) address else NA),
        zip = as.character(if ("zip" %in% names(schools)) zip else NA),
        phone = as.character(if ("phone" %in% names(schools)) phone else NA),
        type = as.character(if ("type" %in% names(schools)) type else
                            if ("school_type" %in% names(schools)) school_type else NA),
        level = as.character(if ("level" %in% names(schools)) level else
                             if ("level_code" %in% names(schools)) level_code else NA),
        rating = as.integer(if ("rating" %in% names(schools)) rating else
                            if ("gs_rating" %in% names(schools)) gs_rating else NA),
        lat = as.numeric(if ("lat" %in% names(schools)) lat else NA),
        lon = as.numeric(if ("lon" %in% names(schools)) lon else NA),
        district_name = as.character(if ("district_name" %in% names(schools)) district_name else
                                     if ("districtName" %in% names(schools)) districtName else NA),
        enrollment = as.integer(if ("enrollment" %in% names(schools)) enrollment else NA),
        url = as.character(if ("links" %in% names(schools)) {
          if (is.list(links)) sapply(links, function(l) l$href %||% NA_character_) else NA
        } else NA)
      )
  } else {
    .schema_schools
  }
}

#' Search schools near coordinates
#'
#' Find schools within a given radius of a geographic point. Useful for
#' location-based school discovery. Returns the same school profile schema
#' as \code{\link{gs_schools}}.
#'
#' @param lat Numeric. Latitude of the center point.
#' @param lon Numeric. Longitude of the center point.
#' @param radius Integer. Search radius in miles (default 5).
#' @param limit Integer. Maximum results to return (default 20).
#' @param api_key Character or \code{NULL}. GreatSchools API key. Defaults to
#'   \code{GREATSCHOOLS_API_KEY} environment variable.
#' @return A tibble with the same columns as \code{\link{gs_schools}}.
#' @export
#' @family greatschools functions
#' @seealso [gs_schools()] for city-based search
#' @examples
#' \dontrun{
#' # Schools near downtown San Francisco
#' gs_nearby(lat = 37.7749, lon = -122.4194, radius = 2)
#' }
gs_nearby <- function(lat, lon, radius = 5, limit = 20, api_key = NULL) {
  key <- .gs_key(api_key)
  url <- sprintf("%s/schools?lat=%f&lon=%f&radius=%d&limit=%d",
                 .gs_base, lat, lon, radius, limit)
  raw <- .fetch_gs_json(url, key)
  schools <- raw$schools %||% raw
  if (is.null(schools) || length(schools) == 0) return(.schema_schools)

  if (is.data.frame(schools)) {
    as_tibble(schools)
  } else {
    .schema_schools
  }
}

#' Search schools by name
#'
#' Search the GreatSchools database by school name. Supports partial
#' matching. Optionally filter to a specific state.
#'
#' @param query Character. School name to search for (e.g. \code{"Lincoln"},
#'   \code{"STEM Academy"}).
#' @param state Character or \code{NULL}. Optional two-letter state
#'   abbreviation to narrow results.
#' @param limit Integer. Maximum results to return (default 20).
#' @param api_key Character or \code{NULL}. GreatSchools API key. Defaults to
#'   \code{GREATSCHOOLS_API_KEY} environment variable.
#' @return A tibble with the same columns as \code{\link{gs_schools}}.
#' @export
#' @family greatschools functions
#' @seealso [gs_schools()] for city-based search, [gs_nearby()] for radius search
#' @examples
#' \dontrun{
#' gs_search("Lincoln High")
#' gs_search("Montessori", state = "CA")
#' }
gs_search <- function(query, state = NULL, limit = 20, api_key = NULL) {
  key <- .gs_key(api_key)
  url <- sprintf("%s/schools?q=%s&limit=%d",
                 .gs_base, utils::URLencode(query), limit)
  if (!is.null(state)) url <- paste0(url, "&state=", state)
  raw <- .fetch_gs_json(url, key)
  schools <- raw$schools %||% raw
  if (is.null(schools) || length(schools) == 0) return(.schema_schools)

  if (is.data.frame(schools)) {
    as_tibble(schools)
  } else {
    .schema_schools
  }
}

#' Get greatschools.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gs_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gs_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/greatschools.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "greatschools.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# greatschools.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# greatschools.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
