# == Public functions ==========================================================

#' Search Harvard Dataverse
#'
#' @param query Search query (e.g. "climate", "elections")
#' @param type Result type: "dataset", "file", "dataverse" (default "dataset")
#' @param per_page Results per page (default 10)
#' @param start Offset for pagination (default 0)
#' @return tibble: name, type, url, description, published_at, citation
#' @export
dv_search <- function(query, type = "dataset", per_page = 10, start = 0) {
  url <- sprintf(
    "%s/search?q=%s&type=%s&per_page=%d&start=%d",
    .dv_base, utils::URLencode(query, reserved = TRUE),
    type, as.integer(per_page), as.integer(start)
  )
  raw <- .fetch_json(url)
  items <- raw$data$items
  if (is.null(items) || length(items) == 0 ||
      (is.data.frame(items) && nrow(items) == 0)) return(.schema_search)

  tibble(
    name = as.character(items$name),
    type = as.character(items$type),
    url = as.character(items$url %||% NA_character_),
    description = as.character(items$description %||% NA_character_),
    published_at = as.character(items$published_at %||% NA_character_),
    citation = as.character(items$citation %||% NA_character_)
  )
}

#' Get dataset details by DOI
#'
#' @param doi Dataset persistent ID (e.g. "doi:10.7910/DVN/EXAMPLE")
#' @return tibble: name, doi, description, publisher, published_at,
#'   file_name, file_id, file_size, file_type (one row per file)
#' @export
dv_dataset <- function(doi) {
  url <- sprintf("%s/datasets/:persistentId?persistentId=%s",
                 .dv_base, utils::URLencode(doi, reserved = TRUE))
  raw <- .fetch_json(url)
  ds <- raw$data
  if (is.null(ds)) return(.schema_dataset)

  version <- ds$latestVersion
  if (is.null(version)) return(.schema_dataset)

  meta <- version$metadataBlocks$citation$fields
  title <- NA_character_
  desc <- NA_character_
  if (is.data.frame(meta)) {
    title_row <- meta[meta$typeName == "title", ]
    if (nrow(title_row) > 0) title <- as.character(title_row$value[1])
    desc_row <- meta[meta$typeName == "dsDescription", ]
    if (nrow(desc_row) > 0) {
      dval <- desc_row$value[[1]]
      if (is.data.frame(dval) && "dsDescriptionValue" %in% names(dval)) {
        desc <- as.character(dval$dsDescriptionValue$value[1])
      }
    }
  }

  files <- version$files
  if (is.null(files) || length(files) == 0) {
    return(tibble(
      name = title, doi = doi, description = desc,
      publisher = as.character(ds$publisher %||% NA_character_),
      published_at = as.character(ds$publicationDate %||% NA_character_),
      file_name = NA_character_, file_id = NA_integer_,
      file_size = NA_integer_, file_type = NA_character_
    ))
  }

  tibble(
    name = title,
    doi = doi,
    description = desc,
    publisher = as.character(ds$publisher %||% NA_character_),
    published_at = as.character(ds$publicationDate %||% NA_character_),
    file_name = as.character(files$dataFile$filename %||% files$label %||% NA_character_),
    file_id = as.integer(files$dataFile$id %||% NA_integer_),
    file_size = as.integer(files$dataFile$filesize %||% NA_integer_),
    file_type = as.character(files$dataFile$contentType %||% NA_character_)
  )
}

#' Show Dataverse client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
dv_context <- function() {
  .build_context(
    pkg_name = "dataverse.harvard.edu",
    header_lines = c(
      "# dataverse.harvard.edu -- Harvard Dataverse API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for public data",
      "# Search types: dataset, file, dataverse",
      "# Example queries: climate, elections, covid, economics"
    )
  )
}

