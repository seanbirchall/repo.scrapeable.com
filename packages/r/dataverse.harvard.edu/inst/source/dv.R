


# dataverse.R
# Self-contained Harvard Dataverse API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: generous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dv_base <- "https://dataverse.harvard.edu/api"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_search <- tibble(
  name = character(), type = character(), url = character(),
  description = character(), published_at = character(),
  citation = character()
)

.schema_dataset <- tibble(
  name = character(), doi = character(), description = character(),
  publisher = character(), published_at = character(),
  file_name = character(), file_id = integer(), file_size = integer(),
  file_type = character()
)


`%||%` <- function(x, y) if (is.null(x)) y else x

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

# == Context ===================================================================

#' Generate LLM-friendly context for dataverse.harvard.edu
#'
#' @return Character string with full function signatures and bodies
#' @export
dv_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/dataverse.harvard.edu.R"
  if (!file.exists(src_file)) {
    cat("# dataverse.harvard.edu context - source not found\n")
    return(invisible("# dataverse.harvard.edu context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

