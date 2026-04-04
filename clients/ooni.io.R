# api.ooni.io.R - Self-contained api.ooni.io client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# api-ooni-io.R
# Self-contained OONI (Open Observatory of Network Interference) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ooni_base <- "https://api.ooni.io/api/v1"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a


# == Schemas ===================================================================

.schema_measurements <- tibble(
  measurement_uid = character(), report_id = character(),
  probe_cc = character(), probe_asn = character(),
  test_name = character(), input = character(),
  measurement_start_time = character(),
  anomaly = logical(), confirmed = logical(), failure = logical()
)

.schema_incidents <- tibble(
  id = character(), title = character(), text = character(),
  start_time = character(), create_time = character(),
  update_time = character(), event_type = character()
)

# == Measurements ==============================================================

#' Search OONI network measurements
#'
#' Query the OONI (Open Observatory of Network Interference) measurements
#' database for internet censorship and surveillance test results from
#' volunteer probes worldwide.
#'
#' @details
#' OONI Probe runs standardised network tests to detect website blocking,
#' middleboxes, and the reachability of circumvention tools. Key test names:
#' \itemize{
#'   \item \code{"web_connectivity"} -- tests whether a URL is blocked
#'   \item \code{"tor"} / \code{"vanilla_tor"} -- Tor reachability
#'   \item \code{"signal"} / \code{"whatsapp"} / \code{"telegram"} /
#'     \code{"facebook_messenger"} -- messaging app reachability
#'   \item \code{"ndt"} -- network speed test
#'   \item \code{"http_invalid_request_line"} -- middlebox detection
#' }
#'
#' An \code{anomaly} indicates the test detected something unusual; a
#' \code{confirmed} flag means independent verification of blocking.
#'
#' @param country Character or \code{NULL}. ISO 3166-1 alpha-2 country code
#'   (e.g. \code{"IR"}, \code{"CN"}, \code{"RU"}, \code{"US"}).
#' @param test_name Character or \code{NULL}. OONI test name (see Details).
#' @param limit Integer. Maximum results (default 20, max 100).
#' @param input Character or \code{NULL}. URL or input tested (used with
#'   \code{"web_connectivity"}).
#' @param since Character or \code{NULL}. Start date \code{"YYYY-MM-DD"}.
#' @param until Character or \code{NULL}. End date \code{"YYYY-MM-DD"}.
#' @return A tibble with columns:
#' \describe{
#'   \item{measurement_uid}{Character. Unique measurement identifier.}
#'   \item{report_id}{Character. Report identifier.}
#'   \item{probe_cc}{Character. Country code of the probe.}
#'   \item{probe_asn}{Character. Autonomous System Number of the probe.}
#'   \item{test_name}{Character. OONI test name.}
#'   \item{input}{Character. URL or input tested (may be \code{NA}).}
#'   \item{measurement_start_time}{Character. ISO 8601 timestamp.}
#'   \item{anomaly}{Logical. Whether an anomaly was detected.}
#'   \item{confirmed}{Logical. Whether blocking was confirmed.}
#'   \item{failure}{Logical. Whether the test itself failed.}
#' }
#' @export
#' @seealso \code{\link{ooni_incidents}}, \code{\link{ooni_aggregation}}
#' @examples
#' ooni_measurements(country = "IR", test_name = "web_connectivity", limit = 5)
ooni_measurements <- function(country = NULL, test_name = NULL, limit = 20,
                              input = NULL, since = NULL, until = NULL) {
  params <- list()
  if (!is.null(country)) params$probe_cc <- country
  if (!is.null(test_name)) params$test_name <- test_name
  if (!is.null(input)) params$input <- input
  if (!is.null(since)) params$since <- since
  if (!is.null(until)) params$until <- until
  params$limit <- limit

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- paste0(.ooni_base, "/measurements?", query)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("OONI measurements query failed: ", e$message)
    return(list())
  })

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_measurements)

  as_tibble(results) |>
    transmute(
      measurement_uid      = as.character(measurement_uid),
      report_id            = as.character(report_id),
      probe_cc             = as.character(probe_cc),
      probe_asn            = as.character(probe_asn),
      test_name            = as.character(test_name),
      input                = as.character(if ("input" %in% names(results)) input else NA_character_),
      measurement_start_time = as.character(measurement_start_time),
      anomaly              = as.logical(anomaly),
      confirmed            = as.logical(confirmed),
      failure              = as.logical(failure)
    )
}

# == Incidents =================================================================

#' Search OONI censorship incidents
#'
#' Retrieve reported internet censorship incidents curated by the OONI
#' team. Each incident summarises a confirmed or suspected blocking
#' event in a specific country or region.
#'
#' @details
#' Incidents are editorially curated reports that synthesise measurement
#' data into human-readable event descriptions. They may include links
#' to detailed OONI Explorer analyses. The \code{event_type} column
#' categorises incidents (e.g. website blocking, app blocking).
#'
#' @param limit Integer. Maximum incidents to return (default 20).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. Incident identifier.}
#'   \item{title}{Character. Incident title / headline.}
#'   \item{text}{Character. Descriptive text.}
#'   \item{start_time}{Character. When the incident began.}
#'   \item{create_time}{Character. When the report was created.}
#'   \item{update_time}{Character. When the report was last updated.}
#'   \item{event_type}{Character. Type of censorship event.}
#' }
#' @export
#' @seealso \code{\link{ooni_measurements}}, \code{\link{ooni_aggregation}}
#' @examples
#' ooni_incidents(limit = 5)
ooni_incidents <- function(limit = 20) {
  url <- paste0(.ooni_base, "/incidents/search?limit=", limit)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("OONI incidents query failed: ", e$message)
    return(list())
  })

  incidents <- raw$incidents
  if (is.null(incidents) || length(incidents) == 0) return(.schema_incidents)

  as_tibble(incidents) |>
    transmute(
      id          = as.character(id %||% NA_character_),
      title       = as.character(title %||% NA_character_),
      text        = as.character(if ("text" %in% names(incidents)) text else NA_character_),
      start_time  = as.character(if ("start_time" %in% names(incidents)) start_time else NA_character_),
      create_time = as.character(create_time %||% NA_character_),
      update_time = as.character(update_time %||% NA_character_),
      event_type  = as.character(if ("event_type" %in% names(incidents)) event_type else NA_character_)
    )
}

# == Aggregation ===============================================================

#' Get daily measurement aggregation statistics
#'
#' Retrieve daily counts of OONI measurements, anomalies, and confirmed
#' blocking events for a given country and test, aggregated by day.
#'
#' @details
#' This endpoint provides a time-series view of measurement activity and
#' detected interference. It is useful for spotting trends in censorship
#' over time (e.g. increased blocking around elections or protests).
#' The default date range covers the last 30 days.
#'
#' @param probe_cc Character. ISO 3166-1 alpha-2 country code
#'   (e.g. \code{"IR"}, \code{"CN"}, \code{"RU"}).
#' @param test_name Character. OONI test name (default
#'   \code{"web_connectivity"}).
#' @param since Character or \code{NULL}. Start date \code{"YYYY-MM-DD"}
#'   (default: 30 days ago).
#' @param until Character or \code{NULL}. End date \code{"YYYY-MM-DD"}
#'   (default: today).
#' @return A tibble with columns:
#' \describe{
#'   \item{measurement_start_day}{Character. Date string (\code{"YYYY-MM-DD"}).}
#'   \item{measurement_count}{Integer. Total measurements on that day.}
#'   \item{anomaly_count}{Integer. Measurements flagged as anomalous.}
#'   \item{confirmed_count}{Integer. Measurements with confirmed blocking.}
#' }
#' @export
#' @seealso \code{\link{ooni_measurements}}, \code{\link{ooni_incidents}}
#' @examples
#' ooni_aggregation("IR", since = "2024-01-01", until = "2024-01-31")
ooni_aggregation <- function(probe_cc, test_name = "web_connectivity",
                             since = NULL, until = NULL) {
  schema <- tibble(measurement_start_day = character(), measurement_count = integer(),
                   anomaly_count = integer(), confirmed_count = integer())
  if (is.null(since)) since <- format(Sys.Date() - 30, "%Y-%m-%d")
  if (is.null(until)) until <- format(Sys.Date(), "%Y-%m-%d")
  url <- sprintf("%s/aggregation?probe_cc=%s&test_name=%s&since=%s&until=%s&axis_x=measurement_start_day",
                 .ooni_base, probe_cc, test_name, since, until)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(schema)
  d <- raw$result
  if (is.null(d) || length(d) == 0) return(schema)
  if (!is.data.frame(d)) return(schema)

  as_tibble(d) |>
    transmute(
      measurement_start_day = as.character(if ("measurement_start_day" %in% names(d)) measurement_start_day else NA_character_),
      measurement_count = as.integer(if ("measurement_count" %in% names(d)) measurement_count else NA_integer_),
      anomaly_count = as.integer(if ("anomaly_count" %in% names(d)) anomaly_count else NA_integer_),
      confirmed_count = as.integer(if ("confirmed_count" %in% names(d)) confirmed_count else NA_integer_)
    )
}

# == Context ===================================================================

#' Get ooni.io client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ooni_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ooni_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ooni.io.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ooni.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ooni.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ooni.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
