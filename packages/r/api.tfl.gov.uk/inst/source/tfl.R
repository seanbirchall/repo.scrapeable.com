


# tfl.R
# Self-contained Transport for London API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (open data)
# Rate limits: 500 requests/minute


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tfl_base <- "https://api.tfl.gov.uk"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_lines <- tibble(
  id = character(), name = character(), mode = character(),
  status = character(), reason = character()
)

.schema_arrivals <- tibble(
  id = character(), line_name = character(), station_name = character(),
  destination = character(), towards = character(),
  expected_arrival = as.POSIXct(character()), time_to_station = integer(),
  platform_name = character(), direction = character()
)

.schema_stops <- tibble(
  id = character(), name = character(), lat = numeric(),
  lon = numeric(), modes = character()
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Get TfL lines by mode
#'
#' @param mode Transport mode: "tube", "bus", "dlr", "overground",
#'   "elizabeth-line", "tram", "cable-car" (default "tube")
#' @return tibble: id, name, mode, status, reason
#' @export
tfl_lines <- function(mode = "tube") {
  url <- sprintf("%s/Line/Mode/%s/Status", .tfl_base, mode)
  raw <- jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
  if (length(raw) == 0) return(.schema_lines)

  rows <- lapply(raw, function(line) {
    st <- if (length(line$lineStatuses) > 0) line$lineStatuses[[1]] else list()
    tibble(
      id = as.character(line$id),
      name = as.character(line$name),
      mode = as.character(line$modeName),
      status = as.character(st$statusSeverityDescription %||% NA_character_),
      reason = as.character(st$reason %||% NA_character_)
    )
  })
  bind_rows(rows)
}

#' Get live arrivals for a TfL line or stop
#'
#' @param line_id Line ID (e.g. "victoria", "central", "northern")
#' @param stop_id Optional stop/station NAPTAN ID to filter arrivals
#' @return tibble: id, line_name, station_name, destination, towards,
#'   expected_arrival, time_to_station, platform_name, direction
#' @export
tfl_arrivals <- function(line_id, stop_id = NULL) {
  if (!is.null(stop_id)) {
    url <- sprintf("%s/Line/%s/Arrivals/%s", .tfl_base, line_id, stop_id)
  } else {
    url <- sprintf("%s/Line/%s/Arrivals", .tfl_base, line_id)
  }
  raw <- .fetch_json(url)
  if (length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) return(.schema_arrivals)

  tibble(
    id = as.character(raw$id),
    line_name = as.character(raw$lineName),
    station_name = as.character(raw$stationName),
    destination = as.character(raw$destinationName),
    towards = as.character(raw$towards %||% NA_character_),
    expected_arrival = as.POSIXct(raw$expectedArrival, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    time_to_station = as.integer(raw$timeToStation),
    platform_name = as.character(raw$platformName %||% NA_character_),
    direction = as.character(raw$direction %||% NA_character_)
  )
}

#' Search TfL stops/stations
#'
#' @param query Search term (e.g. "Kings Cross", "Victoria")
#' @return tibble: id, name, lat, lon, modes
#' @export
tfl_stops <- function(query) {
  url <- sprintf("%s/StopPoint/Search/%s", .tfl_base,
                 utils::URLencode(query, reserved = TRUE))
  raw <- .fetch_json(url)
  matches <- raw$matches
  if (is.null(matches) || length(matches) == 0 ||
      (is.data.frame(matches) && nrow(matches) == 0)) return(.schema_stops)

  tibble(
    id = as.character(matches$id),
    name = as.character(matches$name),
    lat = as.numeric(matches$lat),
    lon = as.numeric(matches$lon),
    modes = vapply(matches$modes, function(m) paste(m, collapse = ", "), character(1))
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for api.tfl.gov.uk
#'
#' @return Character string with full function signatures and bodies
#' @export
tfl_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.tfl.gov.uk.R"
  if (!file.exists(src_file)) {
    cat("# api.tfl.gov.uk context - source not found\n")
    return(invisible("# api.tfl.gov.uk context - source not found"))
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

