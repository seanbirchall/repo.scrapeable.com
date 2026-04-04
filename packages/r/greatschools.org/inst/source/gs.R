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
#' Returns schools in a given state and city with their ratings.
#'
#' @param state Two-letter state abbreviation (e.g., "PA")
#' @param city City name (e.g., "Philadelphia")
#' @param limit Max results (default 20)
#' @param school_type Optional: "public", "charter", or "private"
#' @param level Optional: "elementary", "middle", or "high"
#' @param api_key GreatSchools API key. Defaults to GREATSCHOOLS_API_KEY env var.
#' @return tibble: id, name, state, city, address, zip, phone, type, level,
#'   rating, lat, lon, district_name, enrollment, url
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
#' @param lat Latitude
#' @param lon Longitude
#' @param radius Search radius in miles (default 5)
#' @param limit Max results (default 20)
#' @param api_key GreatSchools API key
#' @return tibble: same as gs_schools()
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
#' @param query School name to search for
#' @param state Optional two-letter state abbreviation
#' @param limit Max results (default 20)
#' @param api_key GreatSchools API key
#' @return tibble: same as gs_schools()
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

#' Show context for the greatschools.org client
#'
#' @return Invisible string of context
gs_context <- function() {
  src_dir <- system.file("source", package = "greatschools.org")
  if (src_dir == "") {
    this_file <- sys.frame(1)$ofile %||%
      attr(body(gs_schools), "srcfile")$filename %||%
      ""
    if (this_file != "" && file.exists(this_file)) {
      lines <- readLines(this_file, warn = FALSE)
    } else {
      cat("# greatschools.org - GreatSchools API client\n")
      cat("# Source not found. Use ?gs_schools for help.\n")
      return(invisible(""))
    }
  } else {
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) {
      cat("# No R source found.\n")
      return(invisible(""))
    }
    lines <- readLines(src_files[1], warn = FALSE)
  }

  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]
    k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) {
      k <- k + 1
      sig <- paste(sig, trimws(lines[k]))
    }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c(
    "# greatschools.org - GreatSchools API",
    "# School ratings, locations, and details",
    "# Requires API key (GREATSCHOOLS_API_KEY env var)",
    "#",
    "# == Functions ==",
    "#",
    unlist(blocks)
  ), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
