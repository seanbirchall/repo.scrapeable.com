# foia.gov.R - Self-contained foia.gov client



# foia-gov.R
# Self-contained FOIA.gov API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: api_key (DEMO_KEY works for testing). Register at https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 30/hr, registered = 1000/hr


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.foia_base <- "https://api.foia.gov/api"

`%||%` <- function(a, b) if (is.null(a)) b else a
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
#' Returns all federal agency components (sub-agencies, offices, bureaus)
#' registered with FOIA.gov. Each component can independently receive
#' and process FOIA requests.
#'
#' @param api_key Character. API key for api.data.gov. \code{"DEMO_KEY"}
#'   works for testing (30 req/hour). Register free at
#'   \url{https://api.data.gov/signup/}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Component ID.}
#'     \item{agency_name}{Character. Parent agency name.}
#'     \item{title}{Character. Component title/name.}
#'     \item{abbreviation}{Character. Component abbreviation.}
#'     \item{website}{Character. Component FOIA website URL.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' foia_agencies()
#' }
foia_agencies <- function(api_key = NULL) {
  api_key <- api_key %||% "DEMO_KEY"
  url <- sprintf("%s/agency_components?api_key=%s", .foia_base, api_key)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_components)

  if (is.data.frame(d) && "attributes" %in% names(d)) {
    attrs <- d$attributes
    tibble(
      id = as.integer(d$id),
      agency_name = NA_character_,
      title = as.character(attrs$title %||% NA_character_),
      abbreviation = as.character(attrs$abbreviation %||% NA_character_),
      website = as.character(attrs$website %||% NA_character_)
    )
  } else if (is.list(d) && !is.data.frame(d)) {
    rows <- lapply(d, function(x) {
      tibble(
        id = as.integer(x$id %||% NA),
        agency_name = as.character(x$attributes$title %||% NA),
        title = as.character(x$attributes$title %||% NA),
        abbreviation = as.character(x$attributes$abbreviation %||% NA),
        website = as.character(x$attributes$website %||% NA)
      )
    })
    bind_rows(rows)
  } else {
    .schema_components
  }
}

# == FOIA annual report data ===================================================

#' Fetch FOIA annual report request data
#'
#' Retrieves FOIA request statistics from annual reports for a specific
#' federal agency. Shows how many requests were received, processed,
#' and are pending at the start and end of the fiscal year.
#'
#' @param agency_abbreviation Character. Agency abbreviation (e.g.
#'   \code{"DOJ"}, \code{"DHS"}, \code{"EPA"}, \code{"DOD"}).
#' @param year Integer. Fiscal year (e.g. 2023).
#' @param api_key Character. API key for api.data.gov. \code{"DEMO_KEY"}
#'   works for testing.
#' @return A tibble with columns:
#'   \describe{
#'     \item{agency}{Character. Agency abbreviation.}
#'     \item{year}{Integer. Fiscal year.}
#'     \item{received}{Integer. Number of FOIA requests received.}
#'     \item{processed}{Integer. Number of FOIA requests processed.}
#'     \item{pending_start}{Integer. Requests pending at start of year.}
#'     \item{pending_end}{Integer. Requests pending at end of year.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' foia_requests("DOJ", year = 2023)
#' foia_requests("EPA", year = 2022)
#' }
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

# == Agency listing =============================================================

#' List all FOIA agencies (top-level, not components)
#'
#' Returns a deduplicated list of top-level federal agencies from the
#' FOIA.gov API. Extracted from the agency_components endpoint
#' relationships.
#'
#' @param api_key Character. API key for api.data.gov. \code{"DEMO_KEY"}
#'   works for testing.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. Agency ID.}
#'     \item{name}{Character. Agency name.}
#'     \item{abbreviation}{Character. Agency abbreviation.}
#'     \item{description}{Character. Agency description.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' foia_agency_list()
#' }
foia_agency_list <- function(api_key = NULL) {
  api_key <- api_key %||% "DEMO_KEY"
  url <- sprintf("%s/agency_components?api_key=%s", .foia_base, api_key)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(.schema_agencies)
  d <- raw$data
  if (length(d) == 0) return(.schema_agencies)

  if (is.data.frame(d) && "relationships" %in% names(d)) {
    # JSON:API format - extract agency info from relationships
    rels <- d$relationships
    if (!is.null(rels$agency) && is.data.frame(rels$agency$data)) {
      agency_data <- rels$agency$data
      tibble(
        id = as.integer(agency_data$id),
        name = NA_character_,
        abbreviation = NA_character_,
        description = NA_character_
      ) |> distinct(id, .keep_all = TRUE)
    } else {
      # Fall back to component-level info
      attrs <- d$attributes
      tibble(
        id = as.integer(d$id),
        name = as.character(attrs$title %||% NA_character_),
        abbreviation = as.character(attrs$abbreviation %||% NA_character_),
        description = as.character(attrs$description %||% NA_character_)
      )
    }
  } else {
    .schema_agencies
  }
}

# == Context ===================================================================

#' Get foia.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
foia_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(foia_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/foia.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "foia.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# foia.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# foia.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
