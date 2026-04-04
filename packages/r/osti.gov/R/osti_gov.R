# osti.gov.R - Self-contained osti.gov client


# osti-gov.R
# Self-contained OSTI (Office of Scientific and Technical Information) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osti_base <- "https://www.osti.gov/api/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))


# == Schemas ===================================================================

.schema_records <- tibble(
  osti_id = character(), title = character(), authors = character(),
  publication_date = as.Date(character()), doi = character(),
  product_type = character(), journal_name = character(),
  sponsor_orgs = character(), description = character()
)

# == Public functions ==========================================================

#' Search OSTI scientific/technical records
#'
#' Searches the U.S. Department of Energy's Office of Scientific and
#' Technical Information (OSTI) database, which contains over 3 million
#' records of DOE-funded research outputs including journal articles,
#' technical reports, conference papers, books, patents, and datasets.
#'
#' @param query Character. Search query string. Supports free-text search
#'   across titles, authors, abstracts, and keywords.
#' @param rows Integer. Maximum records to return per page (default 20).
#' @param page Integer. Page number for pagination (1-indexed, default 1).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{osti_id}{Character. Unique OSTI record identifier.}
#'   \item{title}{Character. Publication or record title.}
#'   \item{authors}{Character. Semicolon-delimited author names.}
#'   \item{publication_date}{Date. Publication date.}
#'   \item{doi}{Character. Digital Object Identifier (if available).}
#'   \item{product_type}{Character. Record type (e.g. "Journal Article",
#'     "Technical Report", "Dataset").}
#'   \item{journal_name}{Character. Journal name (for articles).}
#'   \item{sponsor_orgs}{Character. Semicolon-delimited DOE sponsor
#'     organizations.}
#'   \item{description}{Character. Abstract or description text.}
#' }
#'
#' @examples
#' \dontrun{
#' osti_search("solar energy", rows = 10)
#' osti_search("quantum computing", rows = 5, page = 2)
#' }
#'
#' @seealso \code{\link{osti_record}} for fetching a single record by ID,
#'   \code{\link{osti_subject}} for subject-area search.
#' @export
osti_search <- function(query, rows = 20, page = 1) {
  url <- sprintf("%s/records?q=%s&rows=%d&page=%d",
                 .osti_base, utils::URLencode(query), rows, page)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  if (!is.data.frame(raw)) return(.schema_records)

  as_tibble(raw) |>
    transmute(
      osti_id = as.character(osti_id),
      title = as.character(title),
      authors = vapply(authors, function(a) {
        if (is.null(a) || length(a) == 0) return(NA_character_)
        paste(a, collapse = "; ")
      }, character(1)),
      publication_date = tryCatch(as.Date(substr(publication_date, 1, 10)),
                                  error = function(e) as.Date(NA)),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA),
      product_type = as.character(if ("product_type" %in% names(raw)) product_type else NA),
      journal_name = as.character(if ("journal_name" %in% names(raw)) journal_name else NA),
      sponsor_orgs = vapply(sponsor_orgs, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_character_)
        paste(s, collapse = "; ")
      }, character(1)),
      description = as.character(if ("description" %in% names(raw)) description else NA)
    )
}

#' Fetch a single OSTI record by ID
#'
#' Retrieves full metadata for a single OSTI record identified by its
#' numeric OSTI ID. Returns the same columns as \code{\link{osti_search}}
#' but for a single record.
#'
#' @param id Character or numeric. OSTI record identifier
#'   (e.g. \code{"3013277"}).
#'
#' @return A single-row tibble with the same columns as
#'   \code{\link{osti_search}}.
#'
#' @examples
#' \dontrun{
#' osti_record("3013277")
#' }
#'
#' @seealso \code{\link{osti_search}} for finding records by keyword.
#' @export
osti_record <- function(id) {
  url <- sprintf("%s/records/%s", .osti_base, as.character(id))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  if (!is.data.frame(raw)) return(.schema_records)

  as_tibble(raw) |>
    transmute(
      osti_id = as.character(osti_id),
      title = as.character(title),
      authors = vapply(authors, function(a) {
        if (is.null(a) || length(a) == 0) return(NA_character_)
        paste(a, collapse = "; ")
      }, character(1)),
      publication_date = tryCatch(as.Date(substr(publication_date, 1, 10)),
                                  error = function(e) as.Date(NA)),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA),
      product_type = as.character(if ("product_type" %in% names(raw)) product_type else NA),
      journal_name = as.character(if ("journal_name" %in% names(raw)) journal_name else NA),
      sponsor_orgs = vapply(sponsor_orgs, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_character_)
        paste(s, collapse = "; ")
      }, character(1)),
      description = as.character(if ("description" %in% names(raw)) description else NA)
    )
}

#' Search OSTI records by DOE sponsor organization
#'
#' Retrieves OSTI records funded by a specific DOE sponsor organization.
#' Useful for tracking research outputs by funding source.
#'
#' @param sponsor Character. DOE sponsor organization name or partial match
#'   (e.g. \code{"USDOE Office of Science"}, \code{"Office of Energy Efficiency"}).
#' @param rows Integer. Maximum records to return (default 20).
#'
#' @return A tibble with the same columns as \code{\link{osti_search}}.
#'
#' @examples
#' \dontrun{
#' osti_by_sponsor("USDOE Office of Science", rows = 10)
#' }
#'
#' @seealso \code{\link{osti_search}}, \code{\link{osti_subject}}
#' @export
osti_by_sponsor <- function(sponsor, rows = 20) {
  url <- sprintf("%s/records?sponsor_orgs=%s&rows=%d",
                 .osti_base, utils::URLencode(sponsor), rows)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_records)

  as_tibble(raw) |>
    transmute(
      osti_id = as.character(osti_id),
      title = as.character(title),
      authors = vapply(authors, function(a) {
        if (is.null(a) || length(a) == 0) return(NA_character_)
        paste(a, collapse = "; ")
      }, character(1)),
      publication_date = tryCatch(as.Date(substr(publication_date, 1, 10)),
                                  error = function(e) as.Date(NA)),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA),
      product_type = as.character(if ("product_type" %in% names(raw)) product_type else NA),
      journal_name = as.character(if ("journal_name" %in% names(raw)) journal_name else NA),
      sponsor_orgs = vapply(sponsor_orgs, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_character_)
        paste(s, collapse = "; ")
      }, character(1)),
      description = as.character(if ("description" %in% names(raw)) description else NA)
    )
}

#' Search OSTI records by subject area
#'
#' Retrieves OSTI records classified under a specific subject area.
#' Subject areas follow DOE's controlled vocabulary for categorizing
#' scientific and technical information.
#'
#' @param subject Character. Subject area keyword or phrase
#'   (e.g. \code{"nuclear physics"}, \code{"solar energy"},
#'   \code{"climate change"}).
#' @param rows Integer. Maximum records to return (default 20).
#'
#' @return A tibble with the same columns as \code{\link{osti_search}}.
#'
#' @examples
#' \dontrun{
#' osti_subject("nuclear physics", rows = 10)
#' osti_subject("wind energy")
#' }
#'
#' @seealso \code{\link{osti_search}}, \code{\link{osti_by_sponsor}}
#' @export
osti_subject <- function(subject, rows = 20) {
  url <- sprintf("%s/records?subjects=%s&rows=%d",
                 .osti_base, utils::URLencode(subject), rows)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0 || !is.data.frame(raw)) return(.schema_records)

  as_tibble(raw) |>
    transmute(
      osti_id = as.character(osti_id),
      title = as.character(title),
      authors = vapply(authors, function(a) {
        if (is.null(a) || length(a) == 0) return(NA_character_)
        paste(a, collapse = "; ")
      }, character(1)),
      publication_date = tryCatch(as.Date(substr(publication_date, 1, 10)),
                                  error = function(e) as.Date(NA)),
      doi = as.character(if ("doi" %in% names(raw)) doi else NA),
      product_type = as.character(if ("product_type" %in% names(raw)) product_type else NA),
      journal_name = as.character(if ("journal_name" %in% names(raw)) journal_name else NA),
      sponsor_orgs = vapply(sponsor_orgs, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_character_)
        paste(s, collapse = "; ")
      }, character(1)),
      description = as.character(if ("description" %in% names(raw)) description else NA)
    )
}

# == Context ===================================================================

#' Get osti.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
osti_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(osti_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/osti.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "osti.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# osti.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# osti.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
