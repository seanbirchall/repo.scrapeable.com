# govinfo.gov.R - Self-contained GovInfo API client
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: uses DEMO_KEY by default; set GOVINFO_API_KEY env var for higher limits
# Rate limits: DEMO_KEY = 40/hr, registered key = 1000/hr


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.govinfo_base <- "https://api.govinfo.gov"

`%||%` <- function(a, b) if (is.null(a)) b else a

.govinfo_key <- function() {
  Sys.getenv("GOVINFO_API_KEY", unset = "DEMO_KEY")
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 2, max_seconds = 10) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_collection <- tibble(
  collectionCode = character(), collectionName = character(),
  packageCount = integer(), granuleCount = integer()
)

.schema_package <- tibble(
  packageId = character(), title = character(),
  collectionCode = character(), dateIssued = as.Date(character()),
  lastModified = character()
)

.schema_search <- tibble(
  title = character(), packageId = character(),
  collectionCode = character(), dateIssued = as.Date(character()),
  governmentAuthor = character(), category = character()
)

# == Collections ===============================================================

#' List all GovInfo collections
#'
#' Returns the full catalog of document collections available through the
#' GovInfo API, including Congressional Bills, Federal Register, Code of
#' Federal Regulations, Congressional Record, and more. Use collection codes
#' with \code{\link{govinfo_packages}} to browse collection contents.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{collectionCode}{Character. Short code (e.g. "FR", "BILLS", "CFR").}
#'     \item{collectionName}{Character. Full collection name.}
#'     \item{packageCount}{Integer. Total number of packages in the collection.}
#'     \item{granuleCount}{Integer. Total number of granules (sub-documents), or NA.}
#'   }
#' @export
#' @family govinfo functions
#' @seealso [govinfo_packages()] to list packages in a collection,
#'   [govinfo_search()] to search across collections
#' @examples
#' \dontrun{
#' govinfo_collections()
#' }
govinfo_collections <- function() {
  url <- paste0(.govinfo_base, "/collections?api_key=", .govinfo_key())
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$collections)) return(.schema_collection)

  d <- res$collections
  tibble(
    collectionCode = as.character(.safe_col(d, "collectionCode")),
    collectionName = as.character(.safe_col(d, "collectionName")),
    packageCount = as.integer(.safe_col(d, "packageCount", NA_integer_)),
    granuleCount = as.integer(.safe_col(d, "granuleCount", NA_integer_))
  )
}

# == Collection Packages =======================================================

#' List packages in a GovInfo collection
#'
#' Browse packages (documents) in a specific GovInfo collection, starting
#' from a given date. Packages are ordered by last modification time.
#' Use \code{\link{govinfo_collections}} to discover valid collection codes.
#'
#' @param collection Character. Collection code (e.g. \code{"FR"} for Federal
#'   Register, \code{"BILLS"} for Congressional Bills, \code{"CFR"} for Code
#'   of Federal Regulations, \code{"CREC"} for Congressional Record).
#' @param start_date Character. ISO date string for the start of the range
#'   (e.g. \code{"2024-01-01"}). Default \code{"2020-01-01"}.
#' @param page_size Integer. Number of packages per page (default 20, max 100).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{packageId}{Character. Unique package identifier (e.g. "FR-2024-01-02").}
#'     \item{title}{Character. Package title.}
#'     \item{collectionCode}{Character. Collection code.}
#'     \item{dateIssued}{Date. Publication date.}
#'     \item{lastModified}{Character. ISO 8601 timestamp of last modification.}
#'   }
#' @export
#' @family govinfo functions
#' @seealso [govinfo_package_summary()] for detailed package metadata
#' @examples
#' \dontrun{
#' govinfo_packages("FR", start_date = "2024-01-01", page_size = 10)
#' }
govinfo_packages <- function(collection, start_date = "2020-01-01",
                             page_size = 20, offset = 0) {
  start_ts <- paste0(start_date, "T00:00:00Z")
  url <- sprintf("%s/collections/%s/%s?offset=%d&pageSize=%d&api_key=%s",
                 .govinfo_base, collection, start_ts, offset,
                 min(page_size, 100), .govinfo_key())
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$packages) || length(res$packages) == 0)
    return(.schema_package)

  d <- res$packages
  tibble(
    packageId = as.character(.safe_col(d, "packageId")),
    title = as.character(.safe_col(d, "title")),
    collectionCode = as.character(.safe_col(d, "docClass")),
    dateIssued = as.Date(.safe_col(d, "dateIssued")),
    lastModified = as.character(.safe_col(d, "lastModified"))
  )
}

# == Package Summary ===========================================================

#' Get summary metadata for a GovInfo package
#'
#' Retrieve detailed metadata for a single GovInfo package, including
#' title, publication date, authoring government branch, publisher,
#' page count, and SuDoc classification number.
#'
#' @param package_id Character. Package identifier
#'   (e.g. \code{"FR-2024-01-02"}).
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{packageId}{Character. The package identifier.}
#'     \item{title}{Character. Full title of the document.}
#'     \item{collectionCode}{Character. Collection code.}
#'     \item{dateIssued}{Date. Publication date.}
#'     \item{category}{Character. Subject category.}
#'     \item{branch}{Character. Government branch (e.g. "legislative").}
#'     \item{governmentAuthor}{Character. Authoring agency, semicolon-separated.}
#'     \item{publisher}{Character. Publishing entity.}
#'     \item{pages}{Integer. Number of pages.}
#'     \item{suDocClassNumber}{Character. SuDoc classification number.}
#'   }
#' @export
#' @family govinfo functions
#' @seealso [govinfo_packages()] to find package IDs
#' @examples
#' \dontrun{
#' govinfo_package_summary("FR-2024-01-02")
#' }
govinfo_package_summary <- function(package_id) {
  url <- sprintf("%s/packages/%s/summary?api_key=%s",
                 .govinfo_base, package_id, .govinfo_key())
  d <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(d)) return(tibble())

  tibble(
    packageId = as.character(d$packageId %||% NA_character_),
    title = as.character(d$title %||% NA_character_),
    collectionCode = as.character(d$collectionCode %||% NA_character_),
    dateIssued = as.Date(d$dateIssued %||% NA_character_),
    category = as.character(d$category %||% NA_character_),
    branch = as.character(d$branch %||% NA_character_),
    governmentAuthor = as.character(
      paste(d$governmentAuthor %||% character(0), collapse = "; ")
    ),
    publisher = as.character(d$publisher %||% NA_character_),
    pages = as.integer(d$pages %||% NA_integer_),
    suDocClassNumber = as.character(d$suDocClassNumber %||% NA_character_)
  )
}

# == Search ====================================================================

#' Search GovInfo documents
#'
#' Full-text search across all GovInfo document collections (or a specific
#' one). Uses the GovInfo Search API with cursor-based pagination.
#' Authentication uses \code{GOVINFO_API_KEY} env var or falls back to
#' \code{DEMO_KEY} (40 requests/hour limit).
#'
#' @param query Character. Full-text search query string.
#' @param collection Character or \code{NULL}. Optional collection code filter
#'   (e.g. \code{"FR"}, \code{"BILLS"}). \code{NULL} searches all collections.
#' @param page_size Integer. Number of results per page (default 20, max 100).
#' @param offset_mark Character. Cursor for pagination. Use \code{"*"} (default)
#'   for the first page; subsequent pages use the cursor from the previous response.
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Document title.}
#'     \item{packageId}{Character. Unique package identifier.}
#'     \item{collectionCode}{Character. Collection the document belongs to.}
#'     \item{dateIssued}{Date. Publication date.}
#'     \item{governmentAuthor}{Character. Authoring entity, semicolon-separated.}
#'     \item{category}{Character. Subject category.}
#'   }
#' @export
#' @family govinfo functions
#' @seealso [govinfo_package_summary()] to get full details for a result
#' @examples
#' \dontrun{
#' govinfo_search("climate change", page_size = 10)
#' govinfo_search("water quality", collection = "FR")
#' }
govinfo_search <- function(query, collection = NULL, page_size = 20,
                           offset_mark = "*") {
  body <- list(
    query = query,
    pageSize = min(page_size, 100),
    offsetMark = offset_mark
  )
  if (!is.null(collection)) body$collection <- collection

  url <- paste0(.govinfo_base, "/search?api_key=", .govinfo_key())
  tmp <- tempfile(fileext = ".json")
  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(30) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = tmp)
  }, error = function(e) NULL)
  if (is.null(resp)) return(.schema_search)
  if (httr2::resp_status(resp) != 200) {
    warning(sprintf("GovInfo search returned HTTP %d", httr2::resp_status(resp)))
    return(.schema_search)
  }
  res <- jsonlite::fromJSON(tmp, simplifyVector = TRUE)

  if (is.null(res$results) || length(res$results) == 0) return(.schema_search)

  d <- res$results
  tibble(
    title = as.character(.safe_col(d, "title")),
    packageId = as.character(.safe_col(d, "packageId")),
    collectionCode = as.character(.safe_col(d, "collectionCode")),
    dateIssued = as.Date(.safe_col(d, "dateIssued")),
    governmentAuthor = vapply(
      seq_len(nrow(d)),
      function(i) {
        ga <- d$governmentAuthor
        if (is.list(ga)) paste(ga[[i]], collapse = "; ") else as.character(ga[i])
      },
      character(1)
    ),
    category = as.character(.safe_col(d, "category"))
  )
}

# == Context ===================================================================

#' Get govinfo.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
govinfo_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(govinfo_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/govinfo.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "govinfo.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# govinfo.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# govinfo.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
