# data.healthcare.gov.R - Self-contained data.healthcare.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# data-healthcare-gov.R
# Self-contained data.healthcare.gov (DKAN/CKAN) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hcgov_base <- "https://data.healthcare.gov/api/1"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url, simplify = TRUE) jsonlite::fromJSON(.fetch(url), simplifyVector = simplify)

`%||%` <- function(a, b) if (is.null(a)) b else a


# == Schemas ===================================================================

.schema_datasets <- tibble(
  identifier = character(), title = character(), description = character(),
  modified = character(), access_level = character(), keyword = character()
)

.schema_dataset_detail <- tibble(
  identifier = character(), title = character(), description = character(),
  modified = character(), access_level = character(),
  publisher_name = character(), contact_name = character(),
  contact_email = character(), distribution_count = integer()
)

# == Datasets ==================================================================

#' List available datasets on data.healthcare.gov
#'
#' Queries the DKAN metastore for all published dataset metadata on
#' data.healthcare.gov. The portal hosts ~337 datasets covering health
#' insurance marketplace plans, rates, network adequacy, agent/broker
#' registration, and related healthcare data.
#'
#' @param page_size Integer. Number of results per page (default 20). Note:
#'   the API may return all results regardless of this value.
#' @param page Integer. Page number, 1-indexed (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{identifier}{Character. Dataset identifier for use with \code{hcgov_dataset()} (e.g. \code{"e4rr-zk4i"}).}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Brief description of the dataset.}
#'     \item{modified}{Character. Last modification date (ISO 8601 format).}
#'     \item{access_level}{Character. Access level, typically \code{"public"}.}
#'     \item{keyword}{Character. Comma-separated keywords.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # List first 10 datasets
#' hcgov_datasets(page_size = 10)
#'
#' # Browse all datasets
#' hcgov_datasets()
#' }
hcgov_datasets <- function(page_size = 20, page = 1) {
  url <- sprintf("%s/metastore/schemas/dataset/items?show-reference-ids&page=%d&page-size=%d",
                 .hcgov_base, page, page_size)

  raw <- tryCatch(.fetch_json(url, simplify = FALSE), error = function(e) {
    warning("Healthcare.gov datasets query failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)

  rows <- lapply(raw, function(d) {
    kw <- d$keyword
    kw_str <- if (is.list(kw)) {
      paste(vapply(kw, function(k) k$data %||% "", character(1)), collapse = ", ")
    } else if (is.character(kw)) {
      paste(kw, collapse = ", ")
    } else {
      NA_character_
    }
    tibble(
      identifier   = as.character(d$identifier %||% NA_character_),
      title        = as.character(d$title %||% NA_character_),
      description  = as.character(d$description %||% NA_character_),
      modified     = as.character(d$modified %||% NA_character_),
      access_level = as.character(d$accessLevel %||% NA_character_),
      keyword      = kw_str
    )
  })
  bind_rows(rows)
}

# == Dataset detail ============================================================

#' Get detailed info about a specific dataset
#'
#' Retrieves full metadata for a single dataset on data.healthcare.gov,
#' including publisher, contact information, and the number of downloadable
#' distributions (files).
#'
#' @param id Character. Dataset identifier as returned by
#'   \code{hcgov_datasets()} (e.g. \code{"e4rr-zk4i"} for Agent Broker
#'   Registration).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{identifier}{Character. Dataset identifier.}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Full description.}
#'     \item{modified}{Character. Last modification date (ISO 8601).}
#'     \item{access_level}{Character. Access level (typically \code{"public"}).}
#'     \item{publisher_name}{Character. Publishing organization.}
#'     \item{contact_name}{Character. Contact person or team.}
#'     \item{contact_email}{Character. Contact email address.}
#'     \item{distribution_count}{Integer. Number of downloadable files/distributions.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hcgov_dataset("e4rr-zk4i")
#' }
hcgov_dataset <- function(id) {
  url <- sprintf("%s/metastore/schemas/dataset/items/%s?show-reference-ids",
                 .hcgov_base, utils::URLencode(id))

  raw <- tryCatch(.fetch_json(url, simplify = FALSE), error = function(e) {
    warning("Healthcare.gov dataset fetch failed for '", id, "': ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_dataset_detail)

  pub <- raw$publisher
  pub_name <- if (is.list(pub) && !is.null(pub$data)) {
    pub$data$name %||% NA_character_
  } else NA_character_

  cp <- raw$contactPoint
  dist <- raw$distribution
  dist_n <- if (is.list(dist)) length(dist) else 0L


  tibble(
    identifier         = as.character(raw$identifier %||% NA_character_),
    title              = as.character(raw$title %||% NA_character_),
    description        = as.character(raw$description %||% NA_character_),
    modified           = as.character(raw$modified %||% NA_character_),
    access_level       = as.character(raw$accessLevel %||% NA_character_),
    publisher_name     = as.character(pub_name),
    contact_name       = as.character(cp$fn %||% NA_character_),
    contact_email      = as.character(cp$hasEmail %||% NA_character_),
    distribution_count = as.integer(dist_n)
  )
}

# == Context (LLM injection) ==================================================

#' Get data.healthcare.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hcgov_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hcgov_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.healthcare.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.healthcare.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.healthcare.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.healthcare.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
