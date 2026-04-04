# wayback.archive.org.R - Self-contained wayback.archive.org client

library(httr2)
library(jsonlite)
library(tibble)


# wayback-archive-org.R
# Self-contained Wayback Machine API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_available <- tibble(
  url = character(), available = logical(), snapshot_url = character(),
  timestamp = as.POSIXct(character()), status = character()
)

.schema_cdx <- tibble(
  urlkey = character(), timestamp = as.POSIXct(character()),
  original = character(), mimetype = character(),
  statuscode = integer(), digest = character(), length = integer()
)


# == Availability ==============================================================

#' Check if a URL has snapshots in the Wayback Machine
#'
#' Queries the Internet Archive Availability API to determine whether
#' a given URL has been archived. Optionally checks for the snapshot
#' closest to a specified date.
#'
#' @param url Character string. The URL to check for archived snapshots
#'   (e.g., \code{"https://example.com"}).
#' @param timestamp Optional character string in \code{YYYYMMDD} format.
#'   When provided, returns the snapshot closest to this date
#'   (e.g., \code{"20200101"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{url}{The queried URL}
#'     \item{available}{Logical; whether a snapshot exists}
#'     \item{snapshot_url}{Full URL of the archived snapshot, or \code{NA}}
#'     \item{timestamp}{POSIXct timestamp of the snapshot, or \code{NA}}
#'     \item{status}{HTTP status code of the snapshot, or \code{NA}}
#'   }
#' @export
#' @family Wayback Machine functions
#' @seealso \code{\link{wayback_cdx}} for listing multiple snapshots
#' @examples
#' \dontrun{
#' wayback_available("https://example.com")
#' wayback_available("https://whitehouse.gov", timestamp = "20090120")
#' }
wayback_available <- function(url, timestamp = NULL) {
  api_url <- sprintf("https://archive.org/wayback/available?url=%s",
                     utils::URLencode(url, reserved = TRUE))
  if (!is.null(timestamp)) api_url <- paste0(api_url, "&timestamp=", timestamp)

  raw <- tryCatch(.fetch_json(api_url), error = function(e) {
    message("Wayback availability check failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_available)

  snap <- raw$archived_snapshots$closest
  if (is.null(snap)) {
    return(tibble(
      url = url, available = FALSE, snapshot_url = NA_character_,
      timestamp = as.POSIXct(NA), status = NA_character_
    ))
  }

  tibble(
    url          = url,
    available    = as.logical(snap$available %||% FALSE),
    snapshot_url = as.character(snap$url %||% NA_character_),
    timestamp    = tryCatch(
      as.POSIXct(snap$timestamp %||% NA_character_, format = "%Y%m%d%H%M%S"),
      error = function(e) as.POSIXct(NA)
    ),
    status       = as.character(snap$status %||% NA_character_)
  )
}

# == CDX API ===================================================================

#' Search the Wayback Machine CDX index for archived snapshots
#'
#' Queries the CDX server to enumerate all archived captures of a URL.
#' Returns metadata for each capture including timestamp, HTTP status,
#' MIME type, and content digest. Useful for tracking how a page changed
#' over time or finding specific historical versions.
#'
#' @param url Character string. The URL to search for (e.g.,
#'   \code{"example.com"}, \code{"https://www.nytimes.com"}).
#' @param limit Integer. Maximum number of results to return (default 10).
#' @param from Optional start date filter. Accepts \code{YYYYMMDD} or
#'   \code{YYYY} format (e.g., \code{"2020"} or \code{"20200101"}).
#' @param to Optional end date filter. Same format as \code{from}.
#' @param matchType Character string. URL matching strategy:
#'   \code{"exact"} (default), \code{"prefix"} (all pages under a path),
#'   \code{"host"} (all pages on a host), or \code{"domain"} (host + subdomains).
#' @return A tibble with columns:
#'   \describe{
#'     \item{urlkey}{SURT-form URL key used for indexing}
#'     \item{timestamp}{POSIXct capture timestamp}
#'     \item{original}{Original URL that was captured}
#'     \item{mimetype}{MIME type of the captured content}
#'     \item{statuscode}{Integer HTTP status code at capture time}
#'     \item{digest}{Content digest hash for deduplication}
#'     \item{length}{Integer content length in bytes}
#'   }
#' @export
#' @family Wayback Machine functions
#' @seealso \code{\link{wayback_available}} for a quick existence check
#' @examples
#' \dontrun{
#' # Find recent snapshots of example.com
#' wayback_cdx("example.com", limit = 5, from = "2023")
#'
#' # All pages under a prefix
#' wayback_cdx("nytimes.com/2020/", matchType = "prefix", limit = 20)
#' }
wayback_cdx <- function(url, limit = 10, from = NULL, to = NULL,
                        matchType = "exact") {
  api_url <- sprintf("https://web.archive.org/cdx/search/cdx?url=%s&output=json&limit=%d&matchType=%s",
                     utils::URLencode(url, reserved = TRUE), as.integer(limit), matchType)
  if (!is.null(from)) api_url <- paste0(api_url, "&from=", from)
  if (!is.null(to)) api_url <- paste0(api_url, "&to=", to)

  raw <- tryCatch(.fetch_json(api_url), error = function(e) {
    message("Wayback CDX search failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) < 2) return(.schema_cdx)

  # First row is header, rest are data
  headers <- raw[1, ]
  data <- raw[-1, , drop = FALSE]
  if (nrow(data) == 0) return(.schema_cdx)

  tibble(
    urlkey     = as.character(data[, 1]),
    timestamp  = tryCatch(
      as.POSIXct(as.character(data[, 2]), format = "%Y%m%d%H%M%S"),
      error = function(e) as.POSIXct(rep(NA, nrow(data)))
    ),
    original   = as.character(data[, 3]),
    mimetype   = as.character(data[, 4]),
    statuscode = as.integer(data[, 5]),
    digest     = as.character(data[, 6]),
    length     = as.integer(data[, 7])
  )
}

# == Context ===================================================================

#' Get wayback.archive.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wayback_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wayback_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/wayback.archive.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "wayback.archive.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# wayback.archive.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# wayback.archive.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
