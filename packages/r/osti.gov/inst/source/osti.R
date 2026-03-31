# osti-gov.R
# Self-contained OSTI (Office of Scientific and Technical Information) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osti_base <- "https://www.osti.gov/api/v1"

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
  cat(out, "\n")
  invisible(out)
}

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
#' @param query Search query string
#' @param rows Maximum records to return (default 20)
#' @param page Page number (1-indexed, default 1)
#' @return tibble: osti_id, title, authors, publication_date, doi,
#'   product_type, journal_name, sponsor_orgs, description
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
#' @param id OSTI record ID
#' @return tibble: single row with record details
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

#' OSTI package context for LLM integration
#'
#' @return Invisibly returns the context string
osti_context <- function() {
  .build_context("osti.gov", header_lines = c(
    "# osti.gov - OSTI DOE Scientific Records Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Product types: Journal Article, Technical Report, Conference, Dataset",
    "# Sponsors: DOE Office of Science, NNSA, ARPA-E, etc."
  ))
}
