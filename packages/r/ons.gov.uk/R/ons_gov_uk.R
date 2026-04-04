# api.beta.ons.gov.uk.R - Self-contained api.beta.ons.gov.uk client



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
#' Browse the catalog of datasets published by the UK Office for National
#' Statistics (ONS) via the Beta API.
#'
#' @details
#' The ONS publishes hundreds of datasets covering topics such as GDP,
#' inflation (CPIH), population, wellbeing, and mortality. Each dataset
#' has one or more editions (e.g. \code{"time-series"}) and versions.
#' Use \code{\link{ons_dataset}} to explore editions and
#' \code{\link{ons_observations}} to retrieve actual data points.
#'
#' Pagination: use \code{offset} and \code{limit} together to page
#' through results. For example, \code{offset = 20, limit = 20} returns
#' datasets 21--40.
#'
#' @param limit Integer. Maximum datasets to return (default 20, max 1000).
#' @param offset Integer. Number of records to skip for pagination (default 0).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. Dataset identifier (e.g. \code{"cpih01"}).}
#'   \item{title}{Character. Dataset title.}
#'   \item{description}{Character. Brief description.}
#'   \item{release_frequency}{Character. How often data are updated.}
#'   \item{state}{Character. Publication state (e.g. \code{"published"}).}
#'   \item{last_updated}{Character. ISO 8601 timestamp of last update.}
#'   \item{unit_of_measure}{Character. Unit of measure, if applicable.}
#' }
#' @export
#' @seealso \code{\link{ons_dataset}}, \code{\link{ons_observations}}
#' @examples
#' ons_datasets(limit = 5)
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

#' Get available editions for an ONS dataset
#'
#' Retrieve the list of editions (data releases) for a specific ONS
#' dataset. Each edition may have multiple versions.
#'
#' @details
#' Most ONS datasets have a single edition called \code{"time-series"}.
#' The \code{links_latest_version} URL points to the most recent version
#' of each edition, which can be parsed to extract the version number
#' for use with \code{\link{ons_observations}}.
#'
#' @param id Character. Dataset identifier, e.g. \code{"wellbeing-quarterly"},
#'   \code{"cpih01"}, \code{"weekly-deaths"}.
#' @return A tibble with columns:
#' \describe{
#'   \item{edition}{Character. Edition name (e.g. \code{"time-series"}).}
#'   \item{state}{Character. Publication state.}
#'   \item{links_self}{Character. API URL for this edition.}
#'   \item{links_latest_version}{Character. API URL for the latest version.}
#' }
#' @export
#' @seealso \code{\link{ons_datasets}}, \code{\link{ons_observations}}
#' @examples
#' ons_dataset("cpih01")
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

#' Get ONS dataset observations (data points)
#'
#' Retrieve actual observation values for a specific dataset, edition,
#' and version. The returned columns depend on the dataset's dimensions
#' (e.g. time period, geography, aggregate).
#'
#' @details
#' Each observation has one or more dimension columns (whose names vary
#' by dataset) and an \code{observation} column containing the numeric
#' value as a character string. The structure mirrors the ONS CMD
#' (Customise My Data) format. Use \code{\link{ons_dataset}} to discover
#' available editions and version numbers.
#'
#' @param id Character. Dataset identifier (e.g. \code{"cpih01"}).
#' @param edition Character. Edition name (e.g. \code{"time-series"}).
#' @param version Integer or character. Version number.
#' @param limit Integer. Maximum observations to return (default 100).
#' @return A tibble whose columns depend on the dataset. Always includes
#'   \code{observation} (character); other columns are dimension identifiers.
#' @export
#' @seealso \code{\link{ons_datasets}}, \code{\link{ons_dataset}}
#' @examples
#' ons_observations("cpih01", "time-series", 1, limit = 10)
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

#' Get ons.gov.uk client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ons.gov.uk.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ons.gov.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ons.gov.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ons.gov.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
