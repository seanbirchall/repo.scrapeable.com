# api-beta-ons-gov-uk.R
# Self-contained ONS (Office for National Statistics) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ons_base <- "https://api.beta.ons.gov.uk/v1"

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

.schema_datasets <- tibble(
  id = character(), title = character(), description = character(),
  release_frequency = character(), state = character(),
  last_updated = character(), unit_of_measure = character()
)

.schema_editions <- tibble(
  edition = character(), state = character(), links_self = character(),
  links_latest_version = character()
)

.schema_observations <- tibble(
  dimension = character(), observation = character()
)

# == Public functions ==========================================================

#' List ONS datasets
#'
#' Returns available datasets from the ONS API.
#'
#' @param limit Maximum number of datasets to return (default 20, max 1000)
#' @param offset Offset for pagination (default 0)
#' @return tibble: id, title, description, release_frequency, state,
#'   last_updated, unit_of_measure
ons_datasets <- function(limit = 20, offset = 0) {
  url <- sprintf("%s/datasets?limit=%d&offset=%d", .ons_base, limit, offset)
  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_datasets)

  as_tibble(items) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      description = as.character(if ("description" %in% names(items)) description else NA_character_),
      release_frequency = as.character(if ("release_frequency" %in% names(items)) release_frequency else NA_character_),
      state = as.character(if ("state" %in% names(items)) state else NA_character_),
      last_updated = as.character(if ("last_updated" %in% names(items)) last_updated else NA_character_),
      unit_of_measure = as.character(if ("unit_of_measure" %in% names(items)) unit_of_measure else NA_character_)
    )
}

#' Get ONS dataset details
#'
#' Returns metadata for a specific dataset including available editions.
#'
#' @param id Dataset ID (e.g. "wellbeing-quarterly", "cpih01")
#' @return tibble: edition, state, links_self, links_latest_version
ons_dataset <- function(id) {
  url <- sprintf("%s/datasets/%s/editions", .ons_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) list(items = NULL))
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_editions)

  as_tibble(items) |>
    transmute(
      edition = as.character(edition),
      state = as.character(if ("state" %in% names(items)) state else NA_character_),
      links_self = as.character(links$self$href),
      links_latest_version = as.character(links$latest_version$href)
    )
}

#' Get ONS dataset observations
#'
#' Returns observation data for a specific dataset version.
#' The version endpoint must be fully specified.
#'
#' @param id Dataset ID (e.g. "cpih01")
#' @param edition Edition name (e.g. "time-series")
#' @param version Version number (integer, e.g. 1)
#' @param limit Maximum observations to return (default 100)
#' @return tibble with dimension and observation columns (varies by dataset)
ons_observations <- function(id, edition, version, limit = 100) {
  url <- sprintf("%s/datasets/%s/editions/%s/versions/%s/observations?limit=%d",
                 .ons_base, id, edition, version, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) list(observations = NULL))
  obs <- raw$observations
  if (is.null(obs) || length(obs) == 0) return(.schema_observations)

  # observations structure varies by dataset, flatten to tibble
  rows <- lapply(obs, function(o) {
    dims <- if (!is.null(o$dimensions)) {
      setNames(
        vapply(o$dimensions, function(d) as.character(d$id %||% d$label %||% NA), character(1)),
        names(o$dimensions)
      )
    } else character(0)
    c(dims, observation = as.character(o$observation %||% NA))
  })
  tryCatch(
    bind_rows(lapply(rows, function(r) as_tibble(as.list(r)))),
    error = function(e) .schema_observations
  )
}

#' ONS API context for LLM use
#'
#' Prints package overview, auth info, and function signatures.
#' @return Invisible string with context info
ons_context <- function() {
  header <- c(
    "# api.beta.ons.gov.uk - Office for National Statistics API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: not documented",
    "#",
    "# Example datasets: 'wellbeing-quarterly', 'cpih01', 'mid-year-pop-est'",
    "# Typical flow: ons_datasets() -> ons_dataset(id) -> ons_observations(id, edition, version)"
  )
  .build_context("api.beta.ons.gov.uk", header_lines = header)
}

# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
