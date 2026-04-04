


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
#' @param url URL to check (e.g. "https://example.com")
#' @param timestamp Optional timestamp to check closest to (format: YYYYMMDD)
#' @return tibble: url, available, snapshot_url, timestamp, status
#' @export
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
#' @export
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

#' Generate LLM-friendly context for wayback.archive.org
#'
#' @return Character string with full function signatures and bodies
#' @export
wayback_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/wayback.archive.org.R"
  if (!file.exists(src_file)) {
    cat("# wayback.archive.org context - source not found\n")
    return(invisible("# wayback.archive.org context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

