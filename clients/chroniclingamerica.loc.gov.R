# chroniclingamerica.loc.gov.R - Self-contained chroniclingamerica.loc.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# chroniclingamerica-loc-gov.R
# Self-contained Chronicling America (Library of Congress) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none documented
# Note: Uses loc.gov collections API (chroniclingamerica.loc.gov redirects)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.chron_base <- "https://www.loc.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_pages <- tibble(
  title = character(), date = as.Date(character()),
  newspaper = character(), id = character(),
  image_url = character(), url = character()
)

.schema_newspapers <- tibble(
  lccn = character(), title = character(), place = character(),
  start_year = character(), end_year = character(), url = character()
)

# == Public functions ==========================================================


#' Search historic newspaper pages on Chronicling America
#'
#' Full-text search across millions of digitized newspaper pages from the
#' Library of Congress Chronicling America collection (1777--1963). Uses
#' the loc.gov collections API. Returns 40 results per page.
#'
#' @param text Character. Search text (keyword or phrase). Examples:
#'   \code{"baseball"}, \code{"world war"}, \code{"suffrage"},
#'   \code{"gold rush"}.
#' @param page Integer. Page number of results (default 1). Each page
#'   returns up to 40 results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{character -- Page or article title}
#'     \item{date}{Date -- Publication date (may be NA)}
#'     \item{newspaper}{character -- Parent newspaper title(s),
#'       semicolon-separated}
#'     \item{id}{character -- LOC resource identifier URL}
#'     \item{image_url}{character -- URL(s) to page image(s),
#'       semicolon-separated}
#'     \item{url}{character -- LOC resource URL}
#'   }
#' @examples
#' chron_search("baseball")
#' chron_search("suffrage", page = 2)
chron_search <- function(text, page = 1) {
  url <- sprintf(
    "%s/collections/chronicling-america/?q=%s&sp=%d&fo=json",
    .chron_base, utils::URLencode(text, reserved = TRUE), as.integer(page)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_pages)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pages)

  rows <- lapply(results, function(r) {
    tibble(
      title = as.character(r$title %||% NA_character_),
      date = tryCatch(as.Date(r$date %||% NA_character_), error = function(e) as.Date(NA)),
      newspaper = as.character(if (!is.null(r$partof)) paste(sapply(r$partof, function(x) if(is.list(x)) x$title %||% x else x), collapse="; ") else NA_character_),
      id = as.character(r$id %||% NA_character_),
      image_url = as.character(if (!is.null(r$image_url)) paste(r$image_url, collapse="; ") else NA_character_),
      url = as.character(r$url %||% r$id %||% NA_character_)
    )
  })
  bind_rows(rows)
}

#' List newspapers in Chronicling America
#'
#' Returns metadata for digitized newspapers in the Chronicling America
#' collection. Newspapers can be filtered by state. Returns up to 100
#' results per call.
#'
#' @param state Character or NULL. Two-letter state abbreviation to filter.
#'   Examples: \code{"NY"}, \code{"CA"}, \code{"IL"}, \code{"VA"}.
#'   If NULL (default), returns newspapers from all states.
#' @return A tibble with columns:
#'   \describe{
#'     \item{lccn}{character -- Library of Congress Control Number}
#'     \item{title}{character -- Newspaper title}
#'     \item{place}{character -- Publication location(s), semicolon-separated}
#'     \item{start_year}{character -- Earliest year of publication}
#'     \item{end_year}{character -- Latest year of publication}
#'     \item{url}{character -- LOC resource URL}
#'   }
#' @examples
#' chron_newspapers()
#' chron_newspapers(state = "NY")
chron_newspapers <- function(state = NULL) {
  url <- sprintf("%s/collections/chronicling-america/?fo=json&fa=original-format:newspaper", .chron_base)
  if (!is.null(state)) url <- paste0(url, "&fa=location:", utils::URLencode(state))
  url <- paste0(url, "&c=100")

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_newspapers)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_newspapers)

  rows <- lapply(results, function(r) {
    # Extract LCCN from aka list
    lccn <- NA_character_
    if (!is.null(r$aka)) {
      lccn_match <- grep("lccn.loc.gov", unlist(r$aka), value = TRUE)
      if (length(lccn_match) > 0) lccn <- sub(".*/", "", lccn_match[1])
    }

    dates <- r$dates %||% list()
    tibble(
      lccn = as.character(lccn),
      title = as.character(r$title %||% NA_character_),
      place = as.character(if (!is.null(r$location)) paste(unlist(r$location), collapse="; ") else NA_character_),
      start_year = as.character(if (length(dates) > 0) dates[[1]] else NA_character_),
      end_year = as.character(if (length(dates) > 1) dates[[length(dates)]] else NA_character_),
      url = as.character(r$id %||% r$url %||% NA_character_)
    )
  })
  bind_rows(rows)
}

#' Search historic newspapers by date range and state
#'
#' Advanced search that combines full-text search with optional date range
#' and state filters. Both \code{date_from} and \code{date_to} must be
#' provided together for date filtering.
#'
#' @param text Character. Search text (keyword or phrase).
#' @param state Character or NULL. Two-letter state abbreviation (e.g.
#'   \code{"NY"}, \code{"CA"}, \code{"IL"}).
#' @param date_from Character or NULL. Start date as \code{"YYYY-MM-DD"}
#'   (e.g. \code{"1900-01-01"}). Must be paired with \code{date_to}.
#' @param date_to Character or NULL. End date as \code{"YYYY-MM-DD"}
#'   (e.g. \code{"1920-12-31"}). Must be paired with \code{date_from}.
#' @param page Integer. Page number of results (default 1).
#' @return A tibble with columns: title (character), date (Date),
#'   newspaper (character), id (character), image_url (character),
#'   url (character). Same schema as \code{chron_search()}.
#' @examples
#' chron_search_advanced("earthquake", state = "CA",
#'                       date_from = "1906-04-01", date_to = "1906-05-01")
chron_search_advanced <- function(text, state = NULL, date_from = NULL,
                                  date_to = NULL, page = 1) {
  url <- sprintf(
    "%s/collections/chronicling-america/?q=%s&sp=%d&fo=json",
    .chron_base, utils::URLencode(text, reserved = TRUE), as.integer(page)
  )
  if (!is.null(state)) url <- paste0(url, "&fa=state:", utils::URLencode(state))
  if (!is.null(date_from) && !is.null(date_to)) {
    url <- paste0(url, "&dates=", date_from, "/", date_to)
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_pages)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pages)

  rows <- lapply(results, function(r) {
    tibble(
      title = as.character(r$title %||% NA_character_),
      date = tryCatch(as.Date(r$date %||% NA_character_), error = function(e) as.Date(NA)),
      newspaper = as.character(if (!is.null(r$partof)) paste(sapply(r$partof, function(x) if(is.list(x)) x$title %||% x else x), collapse="; ") else NA_character_),
      id = as.character(r$id %||% NA_character_),
      image_url = as.character(if (!is.null(r$image_url)) paste(r$image_url, collapse="; ") else NA_character_),
      url = as.character(r$url %||% r$id %||% NA_character_)
    )
  })
  bind_rows(rows)
}

# == Context ===================================================================

#' Get chroniclingamerica.loc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
chron_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(chron_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/chroniclingamerica.loc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "chroniclingamerica.loc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# chroniclingamerica.loc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# chroniclingamerica.loc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
