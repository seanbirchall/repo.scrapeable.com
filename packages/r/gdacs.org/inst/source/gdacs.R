# gdacs-org.R
# Self-contained GDACS (Global Disaster Alert and Coordination System) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gdacs_base <- "https://www.gdacs.org/gdacsapi/api/events"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_events <- tibble(
  event_id = character(), event_type = character(), name = character(),
  country = character(), alert_level = character(),
  severity = character(), date = as.POSIXct(character()),
  lat = numeric(), lng = numeric(), url = character()
)

# == Events ====================================================================

#' Fetch GDACS disaster events
#'
#' Query the GDACS event list API for natural disasters including
#' earthquakes, tropical cyclones, floods, volcanoes, droughts, and wildfires.
#'
#' @param type Event type code: "EQ" (earthquake), "TC" (tropical cyclone),
#'   "FL" (flood), "VO" (volcano), "DR" (drought), "WF" (wildfire).
#'   Multiple types can be comma-separated (e.g. "EQ,TC").
#' @param from_date Start date (Date or "YYYY-MM-DD")
#' @param to_date End date (Date or "YYYY-MM-DD")
#' @param alert_level Filter by alert level: "Green", "Orange", "Red"
#' @param limit Max results (default 100)
#' @return tibble: event_id, event_type, name, country, alert_level,
#'   severity, date, lat, lng, url
gdacs_events <- function(type = "EQ", from_date = NULL, to_date = NULL,
                         alert_level = NULL, limit = 100) {
  if (is.null(from_date)) from_date <- Sys.Date() - 30
  if (is.null(to_date)) to_date <- Sys.Date()
  url <- sprintf(
    "%s/geteventlist/SEARCH?eventlist=%s&fromDate=%s&toDate=%s&alertlevel=%s&limit=%d",
    .gdacs_base, type,
    format(as.Date(from_date), "%Y-%m-%d"),
    format(as.Date(to_date), "%Y-%m-%d"),
    alert_level %||% "",
    as.integer(limit)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_events)

  features <- raw$features
  if (is.null(features) || length(features) == 0) return(.schema_events)

  rows <- lapply(features, function(f) {
    p <- f$properties %||% list()
    g <- f$geometry %||% list()
    coords <- g$coordinates %||% list(NA, NA)
    tibble(
      event_id = as.character(p$eventid %||% p$eventId %||% NA),
      event_type = as.character(p$eventtype %||% p$eventType %||% NA),
      name = as.character(p$name %||% p$eventname %||% NA),
      country = as.character(p$country %||% NA),
      alert_level = as.character(p$alertlevel %||% NA),
      severity = as.character(p$severitydata %||% p$severity %||% NA),
      date = tryCatch(
        as.POSIXct(p$fromdate %||% p$eventDate %||% NA, tz = "UTC"),
        error = function(e) as.POSIXct(NA)
      ),
      lat = as.numeric(if (length(coords) >= 2) coords[[2]] else NA),
      lng = as.numeric(if (length(coords) >= 1) coords[[1]] else NA),
      url = as.character(p$url %||% p$link %||% NA)
    )
  })
  result <- bind_rows(rows)
  if (nrow(result) > limit) result <- result[seq_len(limit), ]
  result
}

# == Context ===================================================================

#' Generate LLM-friendly context for the gdacs.org package
#'
#' @return Character string (invisibly), also printed
gdacs_context <- function() {
  .build_context("gdacs.org", header_lines = c(
    "# gdacs.org - GDACS Disaster Alert API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://www.gdacs.org/gdacsapi/api/events",
    "# All functions return tibbles with typed columns.",
    "# Event types: EQ (earthquake), TC (tropical cyclone), FL (flood),",
    "#   VO (volcano), DR (drought), WF (wildfire)",
    "# Alert levels: Green, Orange, Red"
  ))
}
