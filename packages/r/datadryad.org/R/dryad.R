# == Dataset listing ===========================================================

#' List Dryad datasets
#'
#' @param per_page Number of results per page (default 10, max 100)
#' @param page Page number (default 1)
#' @return tibble: id, doi, title, abstract, published_date, storage_size
#' @export
dryad_datasets <- function(per_page = 10, page = 1) {
  url <- paste0(.dryad_base, "/datasets?per_page=", per_page, "&page=", page)
  raw <- .fetch_json(url)
  d <- raw$`_embedded`$`stash:datasets`
  if (is.null(d) || length(d) == 0) return(.schema_datasets)

  as_tibble(d) |>
    transmute(
      id = as.integer(identifier),
      doi = as.character(if ("doi" %in% names(d)) doi else NA_character_),
      title = as.character(title),
      abstract = as.character(if ("abstract" %in% names(d)) abstract else NA_character_),
      published_date = tryCatch(as.Date(if ("publicationDate" %in% names(d)) publicationDate else NA_character_), error = function(e) as.Date(NA)),
      storage_size = as.numeric(if ("storageSize" %in% names(d)) storageSize else NA_real_)
    )
}

# == Dataset detail ============================================================

#' Fetch a single Dryad dataset by DOI
#'
#' @param doi Dataset DOI (e.g. "doi:10.5061/dryad.1234")
#' @return tibble: one row with id, doi, title, abstract, published_date,
#'   storage_size, authors, keywords, license
#' @export
dryad_dataset <- function(doi) {
  url <- paste0(.dryad_base, "/datasets/", utils::URLencode(doi, reserved = TRUE))
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$identifier)) return(.schema_dataset_detail)

  auth_str <- if (!is.null(raw$authors) && is.data.frame(raw$authors)) {
    fn <- if ("firstName" %in% names(raw$authors)) raw$authors$firstName else ""
    ln <- if ("lastName" %in% names(raw$authors)) raw$authors$lastName else ""
    paste(trimws(paste(fn, ln)), collapse = "; ")
  } else NA_character_

  kw_str <- if (!is.null(raw$keywords)) {
    paste(raw$keywords, collapse = "; ")
  } else NA_character_

  tibble(
    id = as.integer(raw$identifier %||% NA_integer_),
    doi = as.character(raw$doi %||% NA_character_),
    title = as.character(raw$title %||% NA_character_),
    abstract = as.character(raw$abstract %||% NA_character_),
    published_date = tryCatch(as.Date(raw$publicationDate), error = function(e) as.Date(NA)),
    storage_size = as.numeric(raw$storageSize %||% NA_real_),
    authors = auth_str,
    keywords = kw_str,
    license = as.character(raw$license %||% NA_character_)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Dryad package
#'
#' @return Character string (invisibly), also printed
#' @export
dryad_context <- function() {
  .build_context("datadryad.org", header_lines = c(
    "# datadryad.org - Dryad Research Data Repository Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Dryad hosts open research data. Datasets identified by DOI.",
    "# Use dryad_datasets() to browse, dryad_dataset() for detail."
  ))
}
