# wayback-archive-org.R
# Self-contained Wayback Machine API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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
#' @param url URL to check (e.g. "https://example.com")
#' @param timestamp Optional timestamp to check closest to (format: YYYYMMDD)
#' @return tibble: url, available, snapshot_url, timestamp, status
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

#' Search the Wayback Machine CDX index
#'
#' Queries the CDX server for archived snapshots of a URL. Returns
#' metadata for each capture including timestamp, status, and MIME type.
#'
#' @param url URL to search for (e.g. "example.com")
#' @param limit Maximum number of results (default 10)
#' @param from Optional start date (format: YYYYMMDD or YYYY)
#' @param to Optional end date (format: YYYYMMDD or YYYY)
#' @param matchType Optional match type: "exact" (default), "prefix",
#'   "host", "domain"
#' @return tibble: urlkey, timestamp, original, mimetype, statuscode,
#'   digest, length
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

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the wayback.archive.org package
#'
#' @return Character string (invisibly), also printed
wayback_context <- function() {
  .build_context("wayback.archive.org", header_lines = c(
    "# wayback.archive.org - Wayback Machine API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Endpoints: availability check and CDX index search",
    "# All functions return tibbles with typed columns."
  ))
}
