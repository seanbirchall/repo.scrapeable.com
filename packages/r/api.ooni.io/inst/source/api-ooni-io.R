# api-ooni-io.R
# Self-contained OONI (Open Observatory of Network Interference) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ooni_base <- "https://api.ooni.io/api/v1"

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
#' Query the OONI measurements database for internet censorship test results.
#'
#' @param country ISO 3166-1 alpha-2 country code (e.g. "IR", "CN", "RU")
#' @param test_name OONI test name: "web_connectivity", "tor", "signal",
#'   "whatsapp", "facebook_messenger", "telegram", "vanilla_tor",
#'   "http_invalid_request_line", "ndt"
#' @param limit Max results (default 20, max 100)
#' @param input URL or input tested (optional, for web_connectivity)
#' @param since Only measurements after this date (YYYY-MM-DD, optional)
#' @param until Only measurements before this date (YYYY-MM-DD, optional)
#' @return tibble: measurement_uid, report_id, probe_cc, probe_asn,
#'   test_name, input, measurement_start_time, anomaly, confirmed, failure
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
#' Retrieve reported internet censorship incidents from the OONI database.
#'
#' @param limit Max results (default 20)
#' @return tibble: id, title, text, start_time, create_time,
#'   update_time, event_type
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the api.ooni.io package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
ooni_context <- function() {
  .build_context("api.ooni.io", header_lines = c(
    "# api.ooni.io - OONI Internet Censorship Measurement Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Tests: web_connectivity, tor, signal, whatsapp, telegram, ndt",
    "# Country codes: ISO 3166-1 alpha-2 (IR, CN, RU, etc.)",
    "# All functions return tibbles with typed columns."
  ))
}
