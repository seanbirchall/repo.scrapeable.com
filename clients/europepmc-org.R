# europepmc-org.R
# Self-contained Europe PMC biomedical literature API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented, be polite

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.epmc_base <- "https://www.ebi.ac.uk/europepmc/webservices/rest"

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
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), source = character(), pmid = character(),
  pmcid = character(), title = character(), authors = character(),
  journal = character(), pub_year = integer(), cited_by = integer(),
  is_open_access = character()
)

.schema_citations <- tibble(
  id = character(), source = character(), title = character(),
  authors = character(), journal = character(), pub_year = integer()
)

# == Public functions ==========================================================

#' Search Europe PMC articles
#'
#' @param query Search query (supports Europe PMC advanced syntax)
#' @param page_size Results per page (default 25, max 1000)
#' @param cursor_mark Cursor for pagination (default "*" for first page).
#'   Use the nextCursorMark from previous response for next page.
#' @return tibble: id, source, pmid, pmcid, title, authors, journal,
#'   pub_year, cited_by, is_open_access
epmc_search <- function(query, page_size = 25, cursor_mark = "*") {
  url <- sprintf("%s/search?query=%s&format=json&pageSize=%d&cursorMark=%s",
                 .epmc_base, utils::URLencode(query), page_size,
                 utils::URLencode(cursor_mark))
  raw <- .fetch_json(url)
  results <- raw$resultList$result
  if (is.null(results) || nrow(results) == 0) return(.schema_search)

  out <- as_tibble(results) |>
    transmute(
      id = as.character(id),
      source = as.character(if ("source" %in% names(results)) source else NA),
      pmid = as.character(if ("pmid" %in% names(results)) pmid else NA),
      pmcid = as.character(if ("pmcid" %in% names(results)) pmcid else NA),
      title = as.character(title),
      authors = as.character(if ("authorString" %in% names(results)) authorString else NA),
      journal = as.character(if ("journalTitle" %in% names(results)) journalTitle else NA),
      pub_year = as.integer(if ("pubYear" %in% names(results)) pubYear else NA),
      cited_by = as.integer(if ("citedByCount" %in% names(results)) citedByCount else NA),
      is_open_access = as.character(if ("isOpenAccess" %in% names(results)) isOpenAccess else NA)
    )
  attr(out, "next_cursor") <- raw$nextCursorMark
  attr(out, "hit_count") <- raw$hitCount
  out
}

#' Fetch a single article from Europe PMC
#'
#' @param id Article identifier (PMID, PMC ID, or DOI)
#' @param source Source database: "MED" (PubMed), "PMC", "PAT", etc.
#'   Default "MED".
#' @return tibble: single row with article details
epmc_article <- function(id, source = "MED") {
  url <- sprintf("%s/search?query=EXT_ID:%s AND SRC:%s&format=json&pageSize=1",
                 .epmc_base, utils::URLencode(as.character(id)), source)
  raw <- .fetch_json(url)
  results <- raw$resultList$result
  if (is.null(results) || nrow(results) == 0) return(.schema_search)

  as_tibble(results) |>
    transmute(
      id = as.character(id),
      source = as.character(if ("source" %in% names(results)) source else NA),
      pmid = as.character(if ("pmid" %in% names(results)) pmid else NA),
      pmcid = as.character(if ("pmcid" %in% names(results)) pmcid else NA),
      title = as.character(title),
      authors = as.character(if ("authorString" %in% names(results)) authorString else NA),
      journal = as.character(if ("journalTitle" %in% names(results)) journalTitle else NA),
      pub_year = as.integer(if ("pubYear" %in% names(results)) pubYear else NA),
      cited_by = as.integer(if ("citedByCount" %in% names(results)) citedByCount else NA),
      is_open_access = as.character(if ("isOpenAccess" %in% names(results)) isOpenAccess else NA)
    )
}

#' Fetch citations for an article
#'
#' @param id Article identifier (PMID or PMC ID)
#' @param source Source database (default "MED")
#' @param page Page number (1-indexed, default 1)
#' @param page_size Results per page (default 25)
#' @return tibble: id, source, title, authors, journal, pub_year
epmc_citations <- function(id, source = "MED", page = 1, page_size = 25) {
  url <- sprintf("%s/%s/%s/citations?page=%d&pageSize=%d&format=json",
                 .epmc_base, source, id, page, page_size)
  raw <- .fetch_json(url)
  results <- raw$citationList$citation
  if (is.null(results) || length(results) == 0) return(.schema_citations)
  if (is.data.frame(results)) {
    as_tibble(results) |>
      transmute(
        id = as.character(id),
        source = as.character(if ("source" %in% names(results)) source else NA),
        title = as.character(if ("title" %in% names(results)) title else NA),
        authors = as.character(if ("authorString" %in% names(results)) authorString else NA),
        journal = as.character(if ("journalAbbreviation" %in% names(results)) journalAbbreviation else NA),
        pub_year = as.integer(if ("pubYear" %in% names(results)) pubYear else NA)
      )
  } else {
    .schema_citations
  }
}

#' Europe PMC package context for LLM integration
#'
#' @return Invisibly returns the context string
epmc_context <- function() {
  .build_context("europepmc.org", header_lines = c(
    "# europepmc.org - Europe PMC Biomedical Literature Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Sources: MED (PubMed), PMC, PAT (patents), AGR (Agricola), CBA",
    "# Query syntax: supports AND, OR, field:value (e.g. AUTH:Smith, JOURNAL:Nature)"
  ))
}
