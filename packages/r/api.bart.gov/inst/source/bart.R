


# bart.R
# Self-contained BART (Bay Area Rapid Transit) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: Public demo key included (MW9S-E7SL-26DU-VV8V)
# Rate limits: generous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bart_base <- "https://api.bart.gov/api"
.bart_key <- "MW9S-E7SL-26DU-VV8V"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_departures <- tibble(
  station = character(), destination = character(), minutes = character(),
  platform = integer(), direction = character(), length = integer(),
  color = character(), delay = character()
)

.schema_stations <- tibble(
  abbr = character(), name = character(), city = character(),
  county = character(), state = character(),
  latitude = numeric(), longitude = numeric(), address = character(),
  zipcode = character()
)

.schema_routes <- tibble(
  number = character(), name = character(), abbr = character(),
  color = character(), holidays = character(), num_stations = integer()
)


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Get real-time departures from a BART station
#'
#' @param station Station abbreviation (e.g. "MONT", "EMBR", "12TH").
#'   Use bart_stations() to see all abbreviations.
#' @return tibble: station, destination, minutes, platform, direction,
#'   length, color, delay
#' @export
bart_departures <- function(station) {
  url <- sprintf(
    "%s/etd.aspx?cmd=etd&orig=%s&json=y&key=%s",
    .bart_base, toupper(station), .bart_key
  )
  raw <- .fetch_json(url)
  root <- raw$root
  if (is.null(root)) return(.schema_departures)
  stations <- root$station
  if (is.null(stations) || length(stations) == 0) return(.schema_departures)

  # stations is typically a list/df with one entry
  sta <- if (is.data.frame(stations)) stations else stations[[1]]
  etd <- sta$etd
  if (is.null(etd)) return(.schema_departures)

  # etd may be a list of destinations
  if (is.data.frame(etd)) {
    rows <- lapply(seq_len(nrow(etd)), function(i) {
      est <- etd$estimate[[i]]
      if (!is.data.frame(est)) return(NULL)
      tibble(
        station = toupper(station),
        destination = as.character(etd$destination[i]),
        minutes = as.character(est$minutes),
        platform = as.integer(est$platform),
        direction = as.character(est$direction),
        length = as.integer(est$length),
        color = as.character(est$color),
        delay = as.character(est$delay %||% NA_character_)
      )
    })
    bind_rows(rows)
  } else {
    .schema_departures
  }
}

#' List all BART stations
#'
#' @return tibble: abbr, name, city, county, state, latitude, longitude,
#'   address, zipcode
#' @export
bart_stations <- function() {
  url <- sprintf("%s/stn.aspx?cmd=stns&json=y&key=%s", .bart_base, .bart_key)
  raw <- .fetch_json(url)
  stations <- raw$root$stations$station
  if (is.null(stations) || length(stations) == 0 ||
      (is.data.frame(stations) && nrow(stations) == 0)) return(.schema_stations)

  tibble(
    abbr = as.character(stations$abbr),
    name = as.character(stations$name),
    city = as.character(stations$city),
    county = as.character(stations$county),
    state = as.character(stations$state),
    latitude = as.numeric(stations$gtfs_latitude),
    longitude = as.numeric(stations$gtfs_longitude),
    address = as.character(stations$address),
    zipcode = as.character(stations$zipcode)
  )
}

#' List all BART routes
#'
#' @return tibble: number, name, abbr, color, holidays, num_stations
#' @export
bart_routes <- function() {
  url <- sprintf("%s/route.aspx?cmd=routes&json=y&key=%s", .bart_base, .bart_key)
  raw <- .fetch_json(url)
  routes <- raw$root$routes$route
  if (is.null(routes) || length(routes) == 0 ||
      (is.data.frame(routes) && nrow(routes) == 0)) return(.schema_routes)

  tibble(
    number = as.character(routes$number),
    name = as.character(routes$name),
    abbr = as.character(routes$abbr),
    color = as.character(routes$color),
    holidays = as.character(routes$holidays %||% NA_character_),
    num_stations = as.integer(routes$num_stns %||% NA_integer_)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for api.bart.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
bart_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.bart.gov.R"
  if (!file.exists(src_file)) {
    cat("# api.bart.gov context - source not found\n")
    return(invisible("# api.bart.gov context - source not found"))
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

