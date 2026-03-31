# foia-gov.R
# Self-contained FOIA.gov API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: api_key (DEMO_KEY works for testing). Register at https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 30/hr, registered = 1000/hr

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.foia_base <- "https://api.foia.gov/api"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_agencies <- tibble(
  id = integer(), name = character(), abbreviation = character(),
  description = character()
)

.schema_requests <- tibble(
  agency = character(), year = integer(),
  received = integer(), processed = integer(),
  pending_start = integer(), pending_end = integer()
)

.schema_components <- tibble(
  id = integer(), agency_name = character(), title = character(),
  abbreviation = character(), website = character()
)

# == Agency components =========================================================

#' Fetch FOIA agency components
#'
#' Returns all federal agency components registered with FOIA.gov.
#'
#' @param api_key API key for api.data.gov. DEMO_KEY works for testing.
#'   Register free at https://api.data.gov/signup/
#' @return tibble: id, agency_name, title, abbreviation, website
foia_agencies <- function(api_key = NULL) {
  api_key <- api_key %||% "DEMO_KEY"
  url <- sprintf("%s/agency_components?api_key=%s", .foia_base, api_key)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_components)

  rows <- lapply(d, function(x) {
    tibble(
      id = as.integer(x$id %||% NA),
      agency_name = as.character(x$agency$name %||% NA),
      title = as.character(x$title %||% NA),
      abbreviation = as.character(x$abbreviation %||% NA),
      website = as.character(x$website %||% NA)
    )
  })
  bind_rows(rows)
}

# == FOIA annual report data ===================================================

#' Fetch FOIA annual report request data
#'
#' Retrieves FOIA request statistics from annual reports for a specific
#' agency component.
#'
#' @param agency_abbreviation Agency abbreviation (e.g. "DOJ", "DHS", "EPA")
#' @param year Fiscal year (e.g. 2023)
#' @param api_key API key for api.data.gov. DEMO_KEY works for testing.
#' @return tibble: agency, year, received, processed, pending_start, pending_end
foia_requests <- function(agency_abbreviation, year = 2023, api_key = NULL) {
  api_key <- api_key %||% "DEMO_KEY"
  url <- sprintf(
    "%s/annual_foia_report/request/%s/%d?api_key=%s",
    .foia_base, agency_abbreviation, as.integer(year), api_key
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_requests)

  # The response structure varies; try to extract top-level stats

  tibble(
    agency = as.character(agency_abbreviation),
    year = as.integer(year),
    received = as.integer(raw$request_received %||% NA),
    processed = as.integer(raw$request_processed %||% NA),
    pending_start = as.integer(raw$pending_start_of_year %||% NA),
    pending_end = as.integer(raw$pending_end_of_year %||% NA)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the foia.gov package
#'
#' @return Character string (invisibly), also printed
foia_context <- function() {
  .build_context("foia.gov", header_lines = c(
    "# foia.gov - Freedom of Information Act API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: api_key param (DEMO_KEY works for testing, register at https://api.data.gov/signup/)",
    "# Rate limit: DEMO_KEY 30/hr, registered 1000/hr",
    "# All functions return tibbles with typed columns."
  ))
}
