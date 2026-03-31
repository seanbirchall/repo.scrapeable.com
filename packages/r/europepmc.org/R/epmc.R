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
#' @export
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
#' @export
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
#' @export
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
#' @export
epmc_context <- function() {
  .build_context("europepmc.org", header_lines = c(
    "# europepmc.org - Europe PMC Biomedical Literature Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Sources: MED (PubMed), PMC, PAT (patents), AGR (Agricola), CBA",
    "# Query syntax: supports AND, OR, field:value (e.g. AUTH:Smith, JOURNAL:Nature)"
  ))
}
