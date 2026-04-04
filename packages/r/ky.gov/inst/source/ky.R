

# ky-gov.R
# Self-contained Kentucky GIS Server API client (ArcGIS REST).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown; be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ky_base <- "https://kygisserver.ky.gov/arcgis/rest/services/WGS84WM_Services"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyDataFrame = TRUE)

.ky_query <- function(service, where = "1=1", fields = "*", limit = 100,
                      offset = 0, geometry = FALSE) {
  url <- sprintf(
    "%s/%s/MapServer/0/query?where=%s&outFields=%s&f=json&resultRecordCount=%d&resultOffset=%d&returnGeometry=%s",
    .ky_base, service,
    utils::URLencode(where, reserved = TRUE),
    utils::URLencode(fields, reserved = TRUE),
    as.integer(limit), as.integer(offset),
    ifelse(geometry, "true", "false")
  )
  raw <- .fetch_json(url)
  if (is.null(raw$features) || length(raw$features) == 0) return(NULL)
  raw$features$attributes
}


# == Schemas ===================================================================

.schema_cities <- tibble::tibble(
  name = character(), county = character(), fips = character(),
  class = character(), elevation = numeric(),
  pop2010 = numeric(), area_sq_miles = numeric(),
  incorporated = character()
)

.schema_webcams <- tibble::tibble(
  id = integer(), name = character(), status = character(),
  state = character(), county = character(), highway = character(),
  direction = character(), description = character(),
  latitude = numeric(), longitude = numeric(),
  snapshot_url = character()
)

.schema_landmarks <- tibble::tibble(
  name = character(), county = character(), address = character(),
  city = character(), listed_date = character(), category = character()
)


# == Public functions ==========================================================

#' List Kentucky city/municipality boundaries
#'
#' @param county Optional county name filter (case-insensitive partial match)
#' @param limit Maximum results (default 100, max 1000)
#' @param offset Result offset for pagination (default 0)
#' @return tibble: name, county, fips, class, elevation, pop2010,
#'   area_sq_miles, incorporated
#' @export
ky_cities <- function(county = NULL, limit = 100, offset = 0) {
  where <- "1=1"
  if (!is.null(county)) {
    where <- sprintf("UPPER(COUNTY) LIKE '%%%s%%'", toupper(county))
  }
  attrs <- .ky_query("Ky_CityBnd_Polygon_WGS84WM", where = where,
                     fields = "NAME,COUNTY,FIPS,CLASS,ELEV,POP2010,Area_SqMiles,INCORP",
                     limit = limit, offset = offset)
  if (is.null(attrs)) return(.schema_cities)

  tibble::tibble(
    name          = as.character(attrs$NAME %||% NA),
    county        = as.character(attrs$COUNTY %||% NA),
    fips          = as.character(attrs$FIPS %||% NA),
    class         = as.character(attrs$CLASS %||% NA),
    elevation     = suppressWarnings(as.numeric(attrs$ELEV)),
    pop2010       = suppressWarnings(as.numeric(attrs$POP2010)),
    area_sq_miles = suppressWarnings(as.numeric(attrs$Area_SqMiles)),
    incorporated  = as.character(attrs$INCORP %||% NA)
  )
}

#' Search Kentucky cities by name
#'
#' @param query City name search string (case-insensitive partial match)
#' @param limit Maximum results (default 50)
#' @return tibble: name, county, fips, class, elevation, pop2010,
#'   area_sq_miles, incorporated
#' @export
ky_city_search <- function(query, limit = 50) {
  where <- sprintf("UPPER(NAME) LIKE '%%%s%%'", toupper(query))
  attrs <- .ky_query("Ky_CityBnd_Polygon_WGS84WM", where = where,
                     fields = "NAME,COUNTY,FIPS,CLASS,ELEV,POP2010,Area_SqMiles,INCORP",
                     limit = limit)
  if (is.null(attrs)) return(.schema_cities)

  tibble::tibble(
    name          = as.character(attrs$NAME %||% NA),
    county        = as.character(attrs$COUNTY %||% NA),
    fips          = as.character(attrs$FIPS %||% NA),
    class         = as.character(attrs$CLASS %||% NA),
    elevation     = suppressWarnings(as.numeric(attrs$ELEV)),
    pop2010       = suppressWarnings(as.numeric(attrs$POP2010)),
    area_sq_miles = suppressWarnings(as.numeric(attrs$Area_SqMiles)),
    incorporated  = as.character(attrs$INCORP %||% NA)
  )
}

#' List Jefferson County KY traffic web cameras
#'
#' @param limit Maximum results (default 100)
#' @param offset Result offset for pagination (default 0)
#' @return tibble: id, name, status, state, county, highway,
#'   direction, description, latitude, longitude, snapshot_url
#' @export
ky_webcams <- function(limit = 100, offset = 0) {
  attrs <- .ky_query("Ky_WebCams_WGS84WM", where = "1=1",
                     fields = "id,name,status,state,county,highway,direction,description,latitude,longitude,snapshot",
                     limit = limit, offset = offset)
  if (is.null(attrs)) return(.schema_webcams)

  tibble::tibble(
    id           = suppressWarnings(as.integer(attrs$id)),
    name         = as.character(attrs$name %||% NA),
    status       = as.character(attrs$status %||% NA),
    state        = as.character(attrs$state %||% NA),
    county       = as.character(attrs$county %||% NA),
    highway      = as.character(attrs$highway %||% NA),
    direction    = as.character(attrs$direction %||% NA),
    description  = as.character(attrs$description %||% NA),
    latitude     = suppressWarnings(as.numeric(attrs$latitude)),
    longitude    = suppressWarnings(as.numeric(attrs$longitude)),
    snapshot_url = as.character(attrs$snapshot %||% NA)
  )
}

#' List Kentucky National Register landmarks
#'
#' @param county Optional county name filter (case-insensitive partial match)
#' @param limit Maximum results (default 100)
#' @param offset Result offset for pagination (default 0)
#' @return tibble: name, county, address, city, listed_date, category
#' @export
ky_landmarks <- function(county = NULL, limit = 100, offset = 0) {
  where <- "1=1"
  if (!is.null(county)) {
    where <- sprintf("UPPER(County) LIKE '%%%s%%'", toupper(county))
  }
  attrs <- .ky_query("Ky_National_Register_Landmarks_WGS84WM", where = where,
                     fields = "*", limit = limit, offset = offset)
  if (is.null(attrs)) return(.schema_landmarks)

  # Field names may vary; handle gracefully
  tibble::tibble(
    name        = as.character(attrs$Name %||% attrs$NAME %||% NA),
    county      = as.character(attrs$County %||% attrs$COUNTY %||% NA),
    address     = as.character(attrs$Address %||% attrs$ADDRESS %||% NA),
    city        = as.character(attrs$City %||% attrs$CITY %||% NA),
    listed_date = as.character(attrs$Listed %||% attrs$LISTED %||% NA),
    category    = as.character(attrs$Category %||% attrs$CATEGORY %||% NA)
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for ky.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
ky_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) {
    src_file <- system.file("source", "ky.R", package = "ky.gov")
  }
  if (is.null(src_file) || !nzchar(src_file) || !file.exists(src_file)) {
    cat("# ky.gov context - source not found\n")
    return(invisible("# ky.gov context - source not found"))
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
