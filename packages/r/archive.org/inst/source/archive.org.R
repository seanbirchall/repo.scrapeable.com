# archive.org.R
# Self-contained Internet Archive client.
# Covers: Archive.org search/metadata/files and Wayback Machine.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.ia_base <- "https://archive.org"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)


# == Schemas ===================================================================

.schema_search <- tibble(
  identifier = character(), title = character(), description = character(),
  mediatype = character(), date = character(), creator = character(),
  downloads = integer(), collection = character()
)

.schema_metadata <- tibble(
  field = character(), value = character()
)

.schema_available <- tibble(
  url = character(), available = logical(), snapshot_url = character(),
  timestamp = as.POSIXct(character()), status = character()
)

.schema_cdx <- tibble(
  urlkey = character(), timestamp = as.POSIXct(character()),
  original = character(), mimetype = character(),
  statuscode = integer(), digest = character(), length = integer()
)



#' Search Internet Archive items
#'
#' Queries the Internet Archive's advanced search API to find items across
#' its vast collection of texts, videos, audio, software, and images. This
#' is the primary discovery function. Use the returned \code{identifier}
#' values with \code{\link{ia_metadata}} for detailed metadata on individual
#' items. Combine with \code{\link{wayback_available}} or
#' \code{\link{wayback_cdx}} for web archiving features.
#'
#' @param query Character. Search query string supporting the Internet Archive
#'   query syntax. Examples: \code{"nasa"}, \code{"title:hamlet creator:shakespeare"},
#'   \code{"collection:prelinger"}.
#' @param rows Integer. Number of results per page (default 10, max 1000).
#' @param page Integer. Page number for pagination (default 1).
#' @param mediatype Character or \code{NULL}. Filter by media type. Valid
#'   values: \code{"texts"}, \code{"movies"}, \code{"audio"},
#'   \code{"software"}, \code{"image"}, \code{"collection"}, \code{"data"},
#'   \code{"web"}. \code{NULL} (default) returns all types.
#'
#' @return A tibble with one row per item and the following columns:
#' \describe{
#'   \item{identifier}{Character. Unique item identifier for use with \code{ia_metadata()}.}
#'   \item{title}{Character. Item title.}
#'   \item{description}{Character. Item description text.}
#'   \item{mediatype}{Character. Media type (e.g. \code{"texts"}, \code{"movies"}).}
#'   \item{date}{Character. Date associated with the item.}
#'   \item{creator}{Character. Creator/author name(s).}
#'   \item{downloads}{Integer. Download count.}
#'   \item{collection}{Character. Collection name(s) the item belongs to.}
#' }
#'
#' @examples
#' \dontrun{
#' # Search for NASA-related items
#' ia_search("nasa", rows = 10)
#'
#' # Search for public domain movies
#' ia_search("public domain", mediatype = "movies", rows = 20)
#'
#' # Paginate through results
#' page1 <- ia_search("shakespeare", rows = 50, page = 1)
#' page2 <- ia_search("shakespeare", rows = 50, page = 2)
#' }
#' @export
ia_search <- function(query, rows = 10, page = 1, mediatype = NULL) {
  q <- query
  if (!is.null(mediatype)) q <- paste0(q, " AND mediatype:", mediatype)

  url <- sprintf(
    "%s/advancedsearch.php?q=%s&output=json&rows=%d&page=%d&fl[]=identifier&fl[]=title&fl[]=description&fl[]=mediatype&fl[]=date&fl[]=creator&fl[]=downloads&fl[]=collection",
    .ia_base, utils::URLencode(q, reserved = TRUE),
    as.integer(rows), as.integer(page)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_search)

  docs <- raw$response$docs
  if (is.null(docs) || length(docs) == 0) return(.schema_search)

  .collapse <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.list(x)) return(paste(unlist(x), collapse = "; "))
    as.character(x)
  }

  rows <- lapply(docs, function(d) {
    tibble(
      identifier = .collapse(d$identifier),
      title = .collapse(d$title),
      description = .collapse(d$description),
      mediatype = .collapse(d$mediatype),
      date = .collapse(d$date),
      creator = .collapse(d$creator),
      downloads = as.integer(d$downloads %||% NA_integer_),
      collection = .collapse(d$collection)
    )
  })
  bind_rows(rows)
}


#' Get metadata for an Internet Archive item
#'
#' Returns all metadata fields for a specific Internet Archive item in a
#' key-value format. Use identifiers obtained from \code{\link{ia_search}}.
#' This provides richer detail than search results, including subjects,
#' language, file counts, and licensing information.
#'
#' @param identifier Character. The item identifier from
#'   \code{\link{ia_search}}. Examples: \code{"nasa"},
#'   \code{"gov.uscourts.dcd.123456"}, \code{"prelinger-movies"}.
#'
#' @return A tibble with one row per metadata field and the following columns:
#' \describe{
#'   \item{field}{Character. Metadata field name (e.g. \code{"title"},
#'     \code{"creator"}, \code{"subject"}, \code{"mediatype"},
#'     \code{"description"}, \code{"licenseurl"}).}
#'   \item{value}{Character. Field value. Multi-valued fields are
#'     semicolon-separated.}
#' }
#'
#' @examples
#' \dontrun{
#' # Get metadata for a known item
#' ia_metadata("nasa")
#'
#' # Get metadata from a search result
#' results <- ia_search("hamlet", rows = 1)
#' ia_metadata(results$identifier[1])
#' }
#' @export
ia_metadata <- function(identifier) {
  url <- sprintf("%s/metadata/%s", .ia_base, identifier)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_metadata)

  meta <- raw$metadata
  if (is.null(meta)) return(.schema_metadata)

  fields <- names(meta)
  values <- vapply(meta, function(x) {
    if (is.list(x)) paste(unlist(x), collapse = "; ")
    else if (length(x) > 1) paste(x, collapse = "; ")
    else as.character(x)
  }, character(1))

  tibble(field = fields, value = values)
}


#' Check if a URL has snapshots in the Wayback Machine
#'
#' Queries the Wayback Machine availability API to check whether a given URL
#' has been archived, and returns the closest snapshot. For detailed snapshot
#' history, use \code{\link{wayback_cdx}} instead. This function returns a
#' single-row result quickly and is useful for checking availability.
#'
#' @param url Character. URL to check for archived snapshots. Examples:
#'   \code{"https://example.com"}, \code{"https://www.nasa.gov"}.
#' @param timestamp Character or \code{NULL}. Optional timestamp to find the
#'   closest snapshot to, in \code{YYYYMMDD} format. Example: \code{"20200101"}.
#'   Default \code{NULL} returns the most recent snapshot.
#'
#' @return A tibble with one row and the following columns:
#' \describe{
#'   \item{url}{Character. The URL that was checked.}
#'   \item{available}{Logical. \code{TRUE} if a snapshot exists, \code{FALSE} otherwise.}
#'   \item{snapshot_url}{Character. Full URL to the Wayback Machine snapshot, or \code{NA}.}
#'   \item{timestamp}{POSIXct. Timestamp of the closest snapshot, or \code{NA}.}
#'   \item{status}{Character. HTTP status code of the snapshot (e.g. \code{"200"}), or \code{NA}.}
#' }
#'
#' @examples
#' \dontrun{
#' # Check if example.com is archived
#' wayback_available("https://example.com")
#'
#' # Find snapshot closest to a specific date
#' wayback_available("https://www.nasa.gov", timestamp = "20200101")
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


#' Search the Wayback Machine CDX index
#'
#' Queries the CDX server for all archived snapshots of a URL. Returns
#' detailed metadata for each capture, unlike \code{\link{wayback_available}}
#' which returns only the closest match. Use this to build a timeline of how
#' a website changed over time or to find specific historical snapshots.
#'
#' @param url Character. URL to search for. Can include or omit the protocol.
#'   Examples: \code{"example.com"}, \code{"https://www.nasa.gov/index.html"}.
#' @param limit Integer. Maximum number of results to return (default 10).
#'   Use larger values to get more historical snapshots.
#' @param from Character or \code{NULL}. Optional start date filter in
#'   \code{YYYYMMDD} or \code{YYYY} format. Example: \code{"2020"},
#'   \code{"20200101"}.
#' @param to Character or \code{NULL}. Optional end date filter in
#'   \code{YYYYMMDD} or \code{YYYY} format. Example: \code{"2023"},
#'   \code{"20231231"}.
#' @param matchType Character. URL matching strategy:
#'   \code{"exact"} (default), \code{"prefix"} (all URLs starting with the
#'   given URL), \code{"host"} (all URLs on the host), \code{"domain"} (all
#'   URLs on the domain and subdomains).
#'
#' @return A tibble with one row per archived snapshot and the following columns:
#' \describe{
#'   \item{urlkey}{Character. Canonicalized URL key used for sorting.}
#'   \item{timestamp}{POSIXct. Capture timestamp.}
#'   \item{original}{Character. Original URL that was archived.}
#'   \item{mimetype}{Character. MIME type of the response (e.g. \code{"text/html"}).}
#'   \item{statuscode}{Integer. HTTP status code (e.g. \code{200}, \code{301}).}
#'   \item{digest}{Character. Content digest hash for deduplication.}
#'   \item{length}{Integer. Response body length in bytes.}
#' }
#'
#' @examples
#' \dontrun{
#' # Get recent snapshots of example.com
#' wayback_cdx("example.com", limit = 10)
#'
#' # Get snapshots from a specific year
#' wayback_cdx("nasa.gov", limit = 20, from = "2020", to = "2020")
#'
#' # Get all pages under a domain
#' wayback_cdx("example.com", matchType = "domain", limit = 50)
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

#' Get archive.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
archive_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(archive_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/archive.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "archive.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# archive.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# archive.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
