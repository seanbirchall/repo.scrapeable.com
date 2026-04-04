



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

#' Get tide predictions for a station
#' @param station Station ID (e.g. '9414290' for San Francisco)
#' @param begin_date Start date YYYYMMDD
#' @param end_date End date YYYYMMDD
#' @return tibble of tide predictions
#' @export
tides_predictions <- function(station, begin_date = format(Sys.Date(), "%Y%m%d"),
                              end_date = format(Sys.Date() + 1, "%Y%m%d")) {
  url <- sprintf("%s?station=%s&begin_date=%s&end_date=%s&product=predictions&datum=MLLW&units=metric&time_zone=gmt&application=scrapeable&format=json",
                 .base, station, begin_date, end_date)
  raw <- .fetch_json(url)
  preds <- raw$predictions
  if (length(preds) == 0) return(tibble::tibble(time = character(), value = numeric()))
  tibble::tibble(
    time = vapply(preds, function(x) x$t %||% NA_character_, character(1)),
    value = vapply(preds, function(x) as.numeric(x$v %||% NA), numeric(1))
  )
}

#' Get water levels for a station
#' @param station Station ID
#' @param begin_date Start date YYYYMMDD
#' @param end_date End date YYYYMMDD
#' @return tibble of water levels
#' @export
tides_water_levels <- function(station, begin_date = format(Sys.Date() - 1, "%Y%m%d"),
                               end_date = format(Sys.Date(), "%Y%m%d")) {
  url <- sprintf("%s?station=%s&begin_date=%s&end_date=%s&product=water_level&datum=MLLW&units=metric&time_zone=gmt&application=scrapeable&format=json",
                 .base, station, begin_date, end_date)
  raw <- .fetch_json(url)
  data <- raw$data
  if (length(data) == 0) return(tibble::tibble(time = character(), value = numeric()))
  tibble::tibble(
    time = vapply(data, function(x) x$t %||% NA_character_, character(1)),
    value = vapply(data, function(x) as.numeric(x$v %||% NA), numeric(1)),
    quality = vapply(data, function(x) x$q %||% NA_character_, character(1))
  )
}

#' List NOAA tide stations
#' @return tibble of stations
#' @export
tides_stations <- function() {
  url <- "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.json"
  raw <- .fetch_json(url)
  stns <- raw$stations
  if (length(stns) == 0) return(tibble::tibble(id = character(), name = character()))
  tibble::tibble(
    id = vapply(stns, function(x) x$id %||% NA_character_, character(1)),
    name = vapply(stns, function(x) x$name %||% NA_character_, character(1)),
    state = vapply(stns, function(x) x$state %||% NA_character_, character(1)),
    latitude = vapply(stns, function(x) as.numeric(x$lat %||% NA), numeric(1)),
    longitude = vapply(stns, function(x) as.numeric(x$lng %||% NA), numeric(1))
  )
}

#' Generate LLM context for api.tidesandcurrents.noaa.gov
#' @return Character string
#' @export
tides_context <- function() {
  .build_context("api.tidesandcurrents.noaa.gov")
}


# == Context ===================================================================

#' Generate LLM-friendly context for api.tidesandcurrents.noaa.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
tidesandcurrents_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.tidesandcurrents.noaa.gov.R"
  if (!file.exists(src_file)) {
    cat("# api.tidesandcurrents.noaa.gov context - source not found\n")
    return(invisible("# api.tidesandcurrents.noaa.gov context - source not found"))
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

