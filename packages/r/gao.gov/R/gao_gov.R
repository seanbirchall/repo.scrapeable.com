# gao.gov.R - Self-contained gao.gov client



# gao-gov.R
# Self-contained GAO Reports client via GovInfo API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: api_key required (free from api.data.gov)
# API: api.govinfo.gov/collections/GAOREPORTS


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.govinfo_base <- "https://api.govinfo.gov"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_reports <- tibble(
  package_id = character(), title = character(),
  date_issued = as.Date(character()), doc_class = character(),
  last_modified = as.POSIXct(character()), summary_url = character()
)

.schema_report_detail <- tibble(
  package_id = character(), title = character(),
  date_issued = as.Date(character()), doc_class = character(),
  government_author = character(), pages = integer(),
  su_doc_class = character(), pdf_url = character(),
  details_url = character()
)

.schema_search <- tibble(
  package_id = character(), title = character(),
  date_issued = as.Date(character()), collection = character(),
  government_author = character(), pdf_url = character()
)

# == Recent reports ============================================================

#' Fetch recent GAO reports
#'
#' Returns GAO reports modified since a given date via the GovInfo
#' collections API. The full GAO archive contains approximately 16,500
#' reports. Results are paginated; use \code{offset} for subsequent pages
#' or \code{\link{gao_recent_all}} for automatic pagination.
#'
#' @param since Start date for modifications (default: 30 days ago).
#'   Accepts ISO 8601 format (e.g. "2024-01-01T00:00:00Z") or a Date object.
#' @param api_key Character. API key from api.data.gov. Register free at
#'   \url{https://api.data.gov/signup/}.
#' @param page_size Integer results per page (default 100, max 1000).
#' @param offset Integer pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{package_id}{GovInfo package ID}
#'     \item{title}{Report title}
#'     \item{date_issued}{Publication date}
#'     \item{doc_class}{Document class code}
#'     \item{last_modified}{Last modification timestamp (POSIXct, UTC)}
#'     \item{summary_url}{URL to the package summary endpoint}
#'   }
#' @examples
#' \dontrun{
#' # Reports from the last 30 days
#' gao_recent(api_key = "your_key")
#'
#' # Reports since a specific date
#' gao_recent(since = "2024-01-01", api_key = "your_key")
#' }
#' @export
gao_recent <- function(since = NULL, api_key = NULL, page_size = 100, offset = 0) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )
  if (is.null(since)) since <- format(Sys.Date() - 30, "%Y-%m-%dT00:00:00Z")
  if (inherits(since, "Date")) since <- format(since, "%Y-%m-%dT00:00:00Z")

  url <- sprintf(
    "%s/collections/GAOREPORTS/%s?pageSize=%d&offset=%d&api_key=%s",
    .govinfo_base, since, page_size, offset, api_key
  )

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovInfo API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(raw)) return(.schema_reports)

  pkgs <- raw$packages
  if (is.null(pkgs) || length(pkgs) == 0) return(.schema_reports)

  tibble(
    package_id    = vapply(pkgs, function(p) p$packageId %||% NA_character_, character(1)),
    title         = vapply(pkgs, function(p) p$title %||% NA_character_, character(1)),
    date_issued   = as.Date(vapply(pkgs, function(p) p$dateIssued %||% NA_character_, character(1))),
    doc_class     = vapply(pkgs, function(p) p$docClass %||% NA_character_, character(1)),
    last_modified = as.POSIXct(vapply(pkgs, function(p) p$lastModified %||% NA_character_, character(1)),
                               format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    summary_url   = vapply(pkgs, function(p) p$packageLink %||% NA_character_, character(1))
  )
}


#' Fetch all GAO reports with automatic pagination
#'
#' Loops through all pages of GAO reports modified since a given date.
#' Progress messages are printed to the console. Respects rate limits
#' with configurable sleep between requests.
#'
#' @param since Start date (default: 90 days ago). Date object or
#'   ISO 8601 string.
#' @param api_key Character. API key from api.data.gov.
#' @param page_size Integer results per page (default 1000).
#' @param max_pages Integer maximum pages to fetch (default 10).
#' @param sleep Numeric seconds to pause between requests (default 0.5).
#' @return A tibble with the same columns as \code{\link{gao_recent}}.
#' @examples
#' \dontrun{
#' # All reports from the last 90 days
#' gao_recent_all(api_key = "your_key")
#' }
#' @export
gao_recent_all <- function(since = NULL, api_key = NULL, page_size = 1000,
                           max_pages = 10, sleep = 0.5) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )
  if (is.null(since)) since <- format(Sys.Date() - 90, "%Y-%m-%dT00:00:00Z")

  results <- list()
  offset <- 0
  for (page in seq_len(max_pages)) {
    if (page > 1) Sys.sleep(sleep)
    message(sprintf("[page %d] offset=%d", page, offset))
    batch <- gao_recent(since = since, api_key = api_key,
                        page_size = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[page]] <- batch
    offset <- offset + page_size
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Report details ============================================================

#' Fetch detailed metadata for a GAO report
#'
#' Returns full metadata for a single report including page count,
#' author information, and PDF download link.
#'
#' @param package_id Character. GovInfo package ID (e.g.
#'   "GAOREPORTS-GAO-24-106130"). Get IDs from \code{\link{gao_recent}}
#'   or \code{\link{gao_search}}.
#' @param api_key Character. API key from api.data.gov.
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{package_id}{GovInfo package ID}
#'     \item{title}{Report title}
#'     \item{date_issued}{Publication date}
#'     \item{doc_class}{Document class}
#'     \item{government_author}{Authoring government entity}
#'     \item{pages}{Page count}
#'     \item{su_doc_class}{SuDoc classification number}
#'     \item{pdf_url}{Direct link to PDF download}
#'     \item{details_url}{Link to full details page}
#'   }
#' @examples
#' \dontrun{
#' gao_report("GAOREPORTS-GAO-24-106130", api_key = "your_key")
#' }
#' @export
gao_report <- function(package_id, api_key = NULL) {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )

  url <- sprintf("%s/packages/%s/summary?api_key=%s",
                 .govinfo_base, package_id, api_key)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovInfo API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(raw)) return(.schema_report_detail)

  tibble(
    package_id        = raw$packageId %||% NA_character_,
    title             = raw$title %||% NA_character_,
    date_issued       = as.Date(raw$dateIssued %||% NA_character_),
    doc_class         = raw$category %||% NA_character_,
    government_author = raw$governmentAuthor1 %||% NA_character_,
    pages             = as.integer(raw$pages %||% NA_integer_),
    su_doc_class      = raw$suDocClassNumber %||% NA_character_,
    pdf_url           = raw$download$pdfLink %||% NA_character_,
    details_url       = raw$detailsLink %||% NA_character_
  )
}


# == Search ====================================================================

#' Search GAO reports by keyword
#'
#' Full-text search across all GAO reports in GovInfo using the search API.
#' Supports Solr-like query syntax. Returns paginated results with a cursor
#' for fetching subsequent pages.
#'
#' @param query Character search term (e.g. "cybersecurity",
#'   "defense spending", "healthcare costs").
#' @param api_key Character. API key from api.data.gov.
#' @param page_size Integer results per page (default 25, max 100).
#' @param offset_mark Character pagination cursor. Use "*" (default) for the
#'   first page. Pass the \code{next_cursor} from a previous call to get the
#'   next page.
#' @param sort_field Character sort field: "score" (relevance, default),
#'   "publishdate", "lastModified", "title".
#' @param sort_order Character "DESC" (default) or "ASC".
#' @return A list with two elements:
#'   \describe{
#'     \item{results}{A tibble with columns: package_id, title, date_issued,
#'       collection, government_author, pdf_url}
#'     \item{next_cursor}{Pagination cursor for the next page (pass to
#'       \code{offset_mark})}
#'   }
#' @examples
#' \dontrun{
#' # Search for cybersecurity reports
#' res <- gao_search("cybersecurity", api_key = "your_key")
#' res$results
#'
#' # Get the next page
#' res2 <- gao_search("cybersecurity", api_key = "your_key",
#'                    offset_mark = res$next_cursor)
#' }
#' @export
gao_search <- function(query, api_key = NULL, page_size = 25,
                       offset_mark = "*", sort_field = "score",
                       sort_order = "DESC") {
  if (is.null(api_key)) stop(
    "api_key is required. Register free at https://api.data.gov/signup/"
  )

  url <- sprintf("%s/search?api_key=%s", .govinfo_base, api_key)

  body <- list(
    query = paste0(query, " collection:GAOREPORTS"),
    pageSize = as.character(page_size),
    offsetMark = offset_mark,
    sorts = list(list(field = sort_field, sortOrder = sort_order))
  )

  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results_list <- raw$results
  if (is.null(results_list) || length(results_list) == 0) {
    return(list(results = .schema_search, next_cursor = NULL))
  }

  results <- tibble(
    package_id        = vapply(results_list, function(r) r$packageId %||% NA_character_, character(1)),
    title             = vapply(results_list, function(r) r$title %||% NA_character_, character(1)),
    date_issued       = as.Date(vapply(results_list, function(r) r$dateIssued %||% NA_character_, character(1))),
    collection        = vapply(results_list, function(r) r$collectionCode %||% NA_character_, character(1)),
    government_author = vapply(results_list, function(r) {
      authors <- r$governmentAuthor
      if (is.null(authors) || length(authors) == 0) NA_character_
      else paste(authors, collapse = "; ")
    }, character(1)),
    pdf_url           = vapply(results_list, function(r) r$download$pdfLink %||% NA_character_, character(1))
  )

  list(results = results, next_cursor = raw$offsetMark)
}


# == Context ===================================================================

#' Get gao.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gao_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gao_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gao.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gao.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gao.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gao.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
