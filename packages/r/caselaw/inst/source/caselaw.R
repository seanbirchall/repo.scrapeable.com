# caselaw.R
# Self-contained Caselaw Access Project API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for basic access
# Rate limits: generous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.caselaw_base <- "https://api.case.law/v1"

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_cases <- tibble(
  id = integer(), name = character(), name_abbreviation = character(),
  decision_date = as.Date(character()), court = character(),
  jurisdiction = character(), citations = character(), url = character()
)

.schema_case <- tibble(
  id = integer(), name = character(), name_abbreviation = character(),
  decision_date = as.Date(character()), court = character(),
  jurisdiction = character(), citations = character(),
  docket_number = character(), url = character()
)

# == Public functions ==========================================================

#' Search cases in the Caselaw Access Project
#'
#' Searches the Harvard Caselaw Access Project for court cases matching
#' a text query. Supports filtering by jurisdiction and date range.
#'
#' @note The Caselaw Access Project API (api.case.law) was deprecated in
#'   2024 and now redirects to documentation. This function may return
#'   errors. See \url{https://case.law} for the current status.
#'
#' @param query Character. Free-text search query (e.g. \code{"first amendment"},
#'   \code{"miranda rights"}, \code{"due process"}).
#' @param jurisdiction Character or \code{NULL}. Jurisdiction slug to filter
#'   results (e.g. \code{"us"} for federal, \code{"cal"} for California,
#'   \code{"ny"} for New York). Default \code{NULL} searches all jurisdictions.
#' @param decision_date_min Character or \code{NULL}. Earliest decision date
#'   in \code{"YYYY-MM-DD"} format.
#' @param decision_date_max Character or \code{NULL}. Latest decision date
#'   in \code{"YYYY-MM-DD"} format.
#' @param page_size Integer. Results per page (default 10, max 100).
#' @return A tibble with one row per case and 8 columns:
#' \describe{
#'   \item{id}{Integer. Case ID.}
#'   \item{name}{Character. Full case name.}
#'   \item{name_abbreviation}{Character. Short case name.}
#'   \item{decision_date}{Date. Date the decision was issued.}
#'   \item{court}{Character. Court name.}
#'   \item{jurisdiction}{Character. Jurisdiction name.}
#'   \item{citations}{Character. Case citations separated by \code{"; "}.}
#'   \item{url}{Character. API URL for the case.}
#' }
#' @examples
#' \dontrun{
#' caselaw_search("first amendment")
#' caselaw_search("miranda rights", jurisdiction = "us")
#' }
#' @export
caselaw_search <- function(query, jurisdiction = NULL,
                           decision_date_min = NULL, decision_date_max = NULL,
                           page_size = 10) {
  params <- list(
    search = utils::URLencode(query, reserved = TRUE),
    page_size = as.integer(page_size)
  )
  if (!is.null(jurisdiction)) params$jurisdiction <- jurisdiction
  if (!is.null(decision_date_min)) params$decision_date_min <- decision_date_min
  if (!is.null(decision_date_max)) params$decision_date_max <- decision_date_max
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.caselaw_base, "/cases/?", qstr)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || length(results) == 0 ||
      (is.data.frame(results) && nrow(results) == 0)) return(.schema_cases)

  tibble(
    id = as.integer(results$id),
    name = as.character(results$name),
    name_abbreviation = as.character(results$name_abbreviation %||% NA_character_),
    decision_date = tryCatch(as.Date(results$decision_date), error = function(e) as.Date(NA)),
    court = as.character(
      if (!is.null(results$court) && !is.null(results$court$name)) results$court$name
      else NA_character_
    ),
    jurisdiction = as.character(
      if (!is.null(results$jurisdiction) && !is.null(results$jurisdiction$name)) results$jurisdiction$name
      else NA_character_
    ),
    citations = vapply(results$citations, function(x) {
      if (is.data.frame(x) && "cite" %in% names(x)) paste(x$cite, collapse = "; ")
      else NA_character_
    }, character(1)),
    url = as.character(results$url %||% NA_character_)
  )
}

#' Get a single case by ID
#'
#' Fetches full metadata for a specific case from the Caselaw Access
#' Project by its numeric ID. Returns a one-row tibble with citation,
#' court, and docket information.
#'
#' @note The Caselaw Access Project API was deprecated in 2024. This
#'   function may return errors. See \url{https://case.law}.
#'
#' @param id Integer. Case ID (obtained from \code{caselaw_search()} results).
#' @return A tibble with 1 row and 9 columns:
#' \describe{
#'   \item{id}{Integer. Case ID.}
#'   \item{name}{Character. Full case name.}
#'   \item{name_abbreviation}{Character. Short case name.}
#'   \item{decision_date}{Date. Date the decision was issued.}
#'   \item{court}{Character. Court name.}
#'   \item{jurisdiction}{Character. Jurisdiction name.}
#'   \item{citations}{Character. Case citations separated by \code{"; "}.}
#'   \item{docket_number}{Character. Court docket number.}
#'   \item{url}{Character. API URL for the case.}
#' }
#' @examples
#' \dontrun{
#' caselaw_case(435800)
#' }
#' @export
caselaw_case <- function(id) {
  url <- sprintf("%s/cases/%s/", .caselaw_base, as.character(id))
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_case)

  tibble(
    id = as.integer(raw$id),
    name = as.character(raw$name),
    name_abbreviation = as.character(raw$name_abbreviation %||% NA_character_),
    decision_date = tryCatch(as.Date(raw$decision_date), error = function(e) as.Date(NA)),
    court = as.character(
      if (!is.null(raw$court) && !is.null(raw$court$name)) raw$court$name
      else NA_character_
    ),
    jurisdiction = as.character(
      if (!is.null(raw$jurisdiction) && !is.null(raw$jurisdiction$name)) raw$jurisdiction$name
      else NA_character_
    ),
    citations = paste(
      if (!is.null(raw$citations) && is.data.frame(raw$citations)) raw$citations$cite
      else NA_character_,
      collapse = "; "
    ),
    docket_number = as.character(raw$docket_number %||% NA_character_),
    url = as.character(raw$url %||% NA_character_)
  )
}

#' Get caselaw client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
caselaw_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(caselaw_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/caselaw.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "caselaw")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# caselaw context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# caselaw", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
