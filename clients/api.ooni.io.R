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

#' Search OONI internet censorship measurements
#'
#' Queries the OONI (Open Observatory of Network Interference) database
#' for network measurement test results. Each row represents one censorship
#' test run by a volunteer probe, with flags indicating whether the
#' tested URL/service was blocked (anomaly), confirmed blocked, or
#' failed. Use \code{ooni_aggregation()} for daily summary statistics
#' and \code{ooni_incidents()} for curated censorship event reports.
#'
#' @param country Character or NULL. ISO 3166-1 alpha-2 country code,
#'   e.g. \code{"IR"} (Iran), \code{"CN"} (China), \code{"RU"} (Russia),
#'   \code{"TR"} (Turkey). Default NULL (all countries).
#' @param test_name Character or NULL. OONI test type. Common values:
#'   \code{"web_connectivity"} (website blocking), \code{"tor"} (Tor
#'   reachability), \code{"signal"} (Signal messenger), \code{"whatsapp"},
#'   \code{"facebook_messenger"}, \code{"telegram"}, \code{"ndt"} (speed
#'   test). Default NULL (all tests).
#' @param limit Integer. Maximum results to return, default \code{20},
#'   maximum \code{100}.
#' @param input Character or NULL. URL or input tested, for
#'   \code{web_connectivity} tests, e.g. \code{"https://twitter.com"}.
#'   Default NULL.
#' @param since Character or NULL. Only include measurements after this
#'   date, format \code{"YYYY-MM-DD"}. Default NULL.
#' @param until Character or NULL. Only include measurements before this
#'   date, format \code{"YYYY-MM-DD"}. Default NULL.
#' @return A tibble with one row per measurement:
#'   \describe{
#'     \item{measurement_uid}{\code{character} -- Unique measurement identifier}
#'     \item{report_id}{\code{character} -- Report identifier for grouping}
#'     \item{probe_cc}{\code{character} -- Country code where the probe ran (e.g. "IR")}
#'     \item{probe_asn}{\code{character} -- Autonomous System Number of the probe network (e.g. "AS31549")}
#'     \item{test_name}{\code{character} -- Test type (e.g. "web_connectivity")}
#'     \item{input}{\code{character} -- URL or input tested, or NA}
#'     \item{measurement_start_time}{\code{character} -- ISO 8601 timestamp of measurement}
#'     \item{anomaly}{\code{logical} -- TRUE if measurement showed signs of interference}
#'     \item{confirmed}{\code{logical} -- TRUE if blocking was confirmed}
#'     \item{failure}{\code{logical} -- TRUE if the test itself failed}
#'   }
#' @examples
#' \dontrun{
#' # Recent web censorship tests from Iran
#' ooni_measurements(country = "IR", test_name = "web_connectivity", limit = 20)
#'
#' # Check if Twitter is blocked in a country
#' ooni_measurements(country = "RU", input = "https://twitter.com", limit = 10)
#'
#' # Tor reachability tests from China
#' ooni_measurements(country = "CN", test_name = "tor", limit = 50)
#' }
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
#' Returns curated internet censorship incident reports from OONI's
#' database. Each incident is a documented, analyzed censorship event
#' in a specific country (e.g. "Russia blocks Cloudflare", "Brazil
#' blocks X/Twitter"). These are higher-level narratives compared to
#' raw measurements from \code{ooni_measurements()}.
#'
#' @param limit Integer. Maximum number of incidents to return, default
#'   \code{20}. Note: the API may return more than requested.
#' @return A tibble with one row per incident:
#'   \describe{
#'     \item{id}{\code{character} -- Unique incident identifier}
#'     \item{title}{\code{character} -- Incident title (e.g. "Russia blocks Cloudflare")}
#'     \item{text}{\code{character} -- Incident description/body text (may be NA in list view)}
#'     \item{start_time}{\code{character} -- When the censorship event started (ISO 8601)}
#'     \item{create_time}{\code{character} -- When the report was created}
#'     \item{update_time}{\code{character} -- When the report was last updated}
#'     \item{event_type}{\code{character} -- Type of event (typically "incident")}
#'   }
#' @examples
#' \dontrun{
#' # Browse recent censorship incidents
#' ooni_incidents(limit = 10)
#'
#' # Find incidents mentioning a specific country
#' ooni_incidents(limit = 50) |> dplyr::filter(grepl("Iran", title))
#' }
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

#' Get aggregated measurement statistics by day
#'
#' Returns daily aggregated counts of measurements, anomalies, and
#' confirmed blocks for a given country and test type. Useful for
#' visualizing censorship trends over time. Each row is one time
#' bucket (hourly within the date range). Pair with
#' \code{ooni_measurements()} to drill into specific anomalies.
#'
#' @param probe_cc Character. ISO 3166-1 alpha-2 country code, e.g.
#'   \code{"IR"} (Iran), \code{"CN"} (China), \code{"RU"} (Russia).
#' @param test_name Character. OONI test name, default
#'   \code{"web_connectivity"}. See \code{ooni_measurements()} for
#'   valid test names.
#' @param since Character or NULL. Start date in \code{"YYYY-MM-DD"}
#'   format. Default NULL (30 days ago).
#' @param until Character or NULL. End date in \code{"YYYY-MM-DD"}
#'   format. Default NULL (today).
#' @return A tibble with one row per time bucket:
#'   \describe{
#'     \item{measurement_start_day}{\code{character} -- ISO 8601 timestamp for the time bucket}
#'     \item{measurement_count}{\code{integer} -- Total number of measurements in that bucket}
#'     \item{anomaly_count}{\code{integer} -- Number of measurements showing interference}
#'     \item{confirmed_count}{\code{integer} -- Number of confirmed blocks}
#'   }
#' @examples
#' \dontrun{
#' # Daily censorship stats for Iran over the past week
#' ooni_aggregation("IR", since = "2025-03-01", until = "2025-03-07")
#'
#' # Tor blocking trends in China
#' ooni_aggregation("CN", test_name = "tor",
#'                  since = "2025-01-01", until = "2025-01-31")
#'
#' # Default: last 30 days of web_connectivity for Russia
#' ooni_aggregation("RU")
#' }
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

#' Get api.ooni.io client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.ooni.io.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.ooni.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.ooni.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.ooni.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
