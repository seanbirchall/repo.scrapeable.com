# api.beta.ons.gov.uk.R - Self-contained api.beta.ons.gov.uk client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# api-beta-ons-gov-uk.R
# Self-contained ONS (Office for National Statistics) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ons_base <- "https://api.beta.ons.gov.uk/v1"
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
#' Returns available datasets from the UK Office for National Statistics
#' (ONS) beta API. Each row is one dataset that can be explored further
#' with \code{ons_dataset()} to see editions, then \code{ons_observations()}
#' to retrieve actual data. Covers topics like well-being, weekly deaths,
#' CPI, trade, and population estimates.
#'
#' @param limit Integer. Maximum number of datasets to return. Default
#'   \code{20}, maximum \code{1000}. Use a high value to browse all
#'   available datasets.
#' @param offset Integer. Offset for pagination, default \code{0}. Combine
#'   with \code{limit} to page through large result sets.
#' @return A tibble with one row per dataset:
#'   \describe{
#'     \item{id}{\code{character} -- Dataset identifier (e.g. "cpih01", "wellbeing-quarterly")}
#'     \item{title}{\code{character} -- Human-readable dataset title}
#'     \item{description}{\code{character} -- Detailed description of the dataset}
#'     \item{release_frequency}{\code{character} -- How often updated ("Weekly", "Quarterly", "Annual", etc.)}
#'     \item{state}{\code{character} -- Publication state (typically "published")}
#'     \item{last_updated}{\code{character} -- ISO 8601 timestamp of last update}
#'     \item{unit_of_measure}{\code{character} -- Unit of measure (e.g. "Percentage"), or NA}
#'   }
#' @examples
#' \dontrun{
#' # Browse first 10 datasets
#' ons_datasets(limit = 10)
#'
#' # Find datasets about deaths
#' ons_datasets(limit = 100) |> dplyr::filter(grepl("death", title, ignore.case = TRUE))
#'
#' # Page through results
#' page2 <- ons_datasets(limit = 20, offset = 20)
#' }
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

#' Get editions for a specific ONS dataset
#'
#' Returns the available editions (versions over time) for a given dataset.
#' Use \code{ons_datasets()} to discover dataset IDs, then this function
#' to find edition names needed by \code{ons_observations()}.
#'
#' @param id Character. Dataset identifier, e.g. \code{"cpih01"},
#'   \code{"wellbeing-quarterly"}, \code{"weekly-deaths-region"}.
#'   Obtain valid IDs from \code{ons_datasets()$id}.
#' @return A tibble with one row per edition:
#'   \describe{
#'     \item{edition}{\code{character} -- Edition name (e.g. "time-series")}
#'     \item{state}{\code{character} -- Publication state (e.g. "published")}
#'     \item{links_self}{\code{character} -- API URL for this edition}
#'     \item{links_latest_version}{\code{character} -- API URL for the latest version of this edition}
#'   }
#' @examples
#' \dontrun{
#' # See editions for the CPIH dataset
#' ons_dataset("cpih01")
#'
#' # Explore well-being dataset editions
#' ons_dataset("wellbeing-quarterly")
#' }
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

#' Get observation data from an ONS dataset
#'
#' Returns the actual statistical observations for a specific dataset,
#' edition, and version. This is the data-access layer: use
#' \code{ons_datasets()} to find a dataset, \code{ons_dataset()} to find
#' editions, then this function to retrieve the numbers. Column structure
#' varies by dataset -- dimensions become columns alongside the observation
#' value.
#'
#' @param id Character. Dataset identifier, e.g. \code{"cpih01"},
#'   \code{"wellbeing-quarterly"}. See \code{ons_datasets()$id}.
#' @param edition Character. Edition name, e.g. \code{"time-series"}.
#'   See \code{ons_dataset(id)$edition}.
#' @param version Integer. Version number, e.g. \code{1}. Higher versions
#'   are more recent. See the version number in the latest_version link
#'   from \code{ons_dataset()}.
#' @param limit Integer. Maximum number of observations to return, default
#'   \code{100}. Increase for larger downloads.
#' @return A tibble where columns depend on the dataset's dimensions.
#'   Always includes an \code{observation} column (\code{character}) with
#'   the data value. Additional columns are the dimension names for that
#'   dataset (e.g. "time", "geography", "aggregate"). Returns an empty
#'   tibble with columns \code{dimension} and \code{observation} on error.
#' @examples
#' \dontrun{
#' # Get CPIH observations (Consumer Price Index including Housing)
#' ons_observations("cpih01", "time-series", version = 1, limit = 50)
#'
#' # Get well-being data
#' ons_observations("wellbeing-quarterly", "time-series", version = 1, limit = 20)
#' }
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
# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

# == Context ===================================================================

#' Get api.beta.ons.gov.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ons_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ons_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.beta.ons.gov.uk.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.beta.ons.gov.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.beta.ons.gov.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.beta.ons.gov.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
