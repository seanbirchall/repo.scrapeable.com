# api-crossref-org.R
# Self-contained Crossref API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data, polite pool via email in User-Agent)
# Rate limits: ~50 req/sec with polite pool

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cr_base <- "https://api.crossref.org"

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
    j <- fi - 1
    rox_start <- fi
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
    httr2::req_headers(`User-Agent` = paste0(.ua, " (mailto:", .ua, ")")) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_works <- tibble(
  doi = character(), title = character(), type = character(),
  container_title = character(), published_date = character(),
  authors = character(), publisher = character(),
  reference_count = integer(), is_referenced_by_count = integer(),
  url = character()
)

.schema_journals <- tibble(
  issn = character(), title = character(), publisher = character(),
  subjects = character(), total_dois = integer()
)

# == Private helpers ===========================================================

.parse_works <- function(items) {
  if (is.null(items) || length(items) == 0) return(.schema_works)
  if (!is.data.frame(items)) return(.schema_works)
  if (nrow(items) == 0) return(.schema_works)

  nms <- names(items)

  as_tibble(items) |>
    transmute(
      doi = as.character(if ("DOI" %in% nms) DOI else NA_character_),
      title = as.character(if ("title" %in% nms) sapply(title, function(x) paste(x, collapse = " ")) else NA_character_),
      type = as.character(if ("type" %in% nms) type else NA_character_),
      container_title = as.character(if ("container-title" %in% nms) sapply(`container-title`, function(x) paste(x, collapse = "; ")) else NA_character_),
      published_date = as.character(if ("created" %in% nms) sapply(created, function(x) if (is.list(x)) x$`date-parts`[[1]][1] else NA_character_) else NA_character_),
      authors = as.character(if ("author" %in% nms) sapply(author, function(x) {
        if (is.data.frame(x)) paste(paste(x$given %||% "", x$family %||% ""), collapse = "; ")
        else NA_character_
      }) else NA_character_),
      publisher = as.character(if ("publisher" %in% nms) publisher else NA_character_),
      reference_count = as.integer(if ("reference-count" %in% nms) `reference-count` else NA_integer_),
      is_referenced_by_count = as.integer(if ("is-referenced-by-count" %in% nms) `is-referenced-by-count` else NA_integer_),
      url = as.character(if ("URL" %in% nms) URL else NA_character_)
    )
}

# == Public functions ==========================================================

#' Search Crossref for scholarly works (articles, books, etc.)
#'
#' @param query Search query string
#' @param rows Number of results. Default 10, max 1000.
#' @param offset Starting offset for pagination. Default 0.
#' @param filter Optional filter string (e.g., "type:journal-article")
#' @return tibble: doi, title, type, container_title, published_date,
#'   authors, publisher, reference_count, is_referenced_by_count, url
#' @export
cr_works <- function(query, rows = 10, offset = 0, filter = NULL) {
  url <- sprintf(
    "%s/works?query=%s&rows=%d&offset=%d",
    .cr_base, utils::URLencode(query, reserved = TRUE),
    as.integer(rows), as.integer(offset)
  )
  if (!is.null(filter)) url <- paste0(url, "&filter=", filter)

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_works)

  items <- raw$message$items
  .parse_works(items)
}

#' Search Crossref journals
#'
#' @param query Search query string (journal title)
#' @param rows Number of results. Default 10.
#' @return tibble: issn, title, publisher, subjects, total_dois
#' @export
cr_journals <- function(query, rows = 10) {
  url <- sprintf(
    "%s/journals?query=%s&rows=%d",
    .cr_base, utils::URLencode(query, reserved = TRUE), as.integer(rows)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_journals)

  items <- raw$message$items
  if (is.null(items) || length(items) == 0) return(.schema_journals)
  if (!is.data.frame(items)) return(.schema_journals)

  nms <- names(items)
  as_tibble(items) |>
    transmute(
      issn = as.character(if ("ISSN" %in% nms) sapply(ISSN, paste, collapse = "; ") else NA_character_),
      title = as.character(if ("title" %in% nms) title else NA_character_),
      publisher = as.character(if ("publisher" %in% nms) publisher else NA_character_),
      subjects = as.character(if ("subjects" %in% nms) sapply(subjects, function(x) {
        if (is.data.frame(x)) paste(x$name, collapse = "; ") else NA_character_
      }) else NA_character_),
      total_dois = as.integer(if ("counts" %in% nms) sapply(counts, function(x) x$`total-dois` %||% NA_integer_) else NA_integer_)
    )
}

#' Print Crossref context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
cr_context <- function() {
  .build_context(
    pkg_name = "api.crossref.org",
    header_lines = c(
      "# Package: api.crossref.org",
      "# Crossref REST API - scholarly metadata",
      "# Auth: none (polite pool via email in User-Agent)",
      "# Rate limits: ~50 req/sec in polite pool",
      "#",
      "# Covers: 150M+ works, 100K+ journals, DOI metadata",
      "# Filters: type:journal-article, from-pub-date:2020,",
      "#   has-full-text:true, is-referenced-by-count:>10"
    )
  )
}
