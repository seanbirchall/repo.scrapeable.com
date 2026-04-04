

# doe-gov.R
# Self-contained DOE Energy Data eXchange (EDX) CKAN API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown; be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.edx_base <- "https://edx.netl.doe.gov/api/3/action"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyDataFrame = TRUE)


# == Schemas ===================================================================

.schema_datasets <- tibble::tibble(
  id = character(), name = character(), title = character(),
  notes = character(), num_resources = integer(),
  metadata_created = as.Date(character()), metadata_modified = as.Date(character()),
  tags = character()
)

.schema_resources <- tibble::tibble(
  id = character(), name = character(), description = character(),
  format = character(), url = character(), size = numeric(),
  last_modified = character()
)

.safe_date <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as.Date(rep(NA, max(1, length(x)))))
  suppressWarnings(as.Date(sub("T.*", "", as.character(x))))
}

.collapse_tags <- function(tags_list) {
  if (is.null(tags_list)) return(NA_character_)
  vapply(tags_list, function(t) {
    if (is.null(t) || length(t) == 0) return(NA_character_)
    if (is.data.frame(t)) {
      paste(t$name, collapse = "; ")
    } else if (is.character(t)) {
      paste(t, collapse = "; ")
    } else {
      NA_character_
    }
  }, character(1))
}


# == Public functions ==========================================================

#' Search DOE Energy Data eXchange (EDX) datasets
#'
#' @param query Search query string
#' @param rows Maximum results (default 20, max 1000)
#' @param start Offset for pagination (default 0)
#' @return tibble: id, name, title, notes, num_resources,
#'   metadata_created, metadata_modified, tags
#' @export
doe_search <- function(query = "*", rows = 20, start = 0) {
  url <- sprintf("%s/package_search?q=%s&rows=%d&start=%d",
                 .edx_base, utils::URLencode(query), as.integer(rows), as.integer(start))
  raw <- .fetch_json(url)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0 || nrow(results) == 0) {
    return(.schema_datasets)
  }

  tibble::tibble(
    id                = as.character(results$id %||% NA),
    name              = as.character(results$name %||% NA),
    title             = as.character(results$title %||% NA),
    notes             = as.character(results$notes %||% NA),
    num_resources     = as.integer(results$num_resources %||% NA),
    metadata_created  = .safe_date(results$metadata_created),
    metadata_modified = .safe_date(results$metadata_modified),
    tags              = .collapse_tags(results$tags)
  )
}

#' Get full details for a DOE EDX dataset
#'
#' @param id Dataset ID or name slug (e.g., "ree-and-coal-open-geodatabase-collection")
#' @return tibble: single row with dataset details
#' @export
doe_dataset <- function(id) {
  url <- sprintf("%s/package_show?id=%s", .edx_base, utils::URLencode(id))
  raw <- .fetch_json(url)
  r <- raw$result
  if (is.null(r)) return(.schema_datasets)

  tags_str <- if (!is.null(r$tags) && is.data.frame(r$tags) && nrow(r$tags) > 0) {
    paste(r$tags$name, collapse = "; ")
  } else {
    NA_character_
  }

  tibble::tibble(
    id                = as.character(r$id %||% NA),
    name              = as.character(r$name %||% NA),
    title             = as.character(r$title %||% NA),
    notes             = as.character(r$notes %||% NA),
    num_resources     = as.integer(r$num_resources %||% NA),
    metadata_created  = .safe_date(r$metadata_created),
    metadata_modified = .safe_date(r$metadata_modified),
    tags              = tags_str
  )
}

#' List resources (downloadable files) for a DOE EDX dataset
#'
#' @param id Dataset ID or name slug
#' @return tibble: id, name, description, format, url, size, last_modified
#' @export
doe_resources <- function(id) {
  url <- sprintf("%s/package_show?id=%s", .edx_base, utils::URLencode(id))
  raw <- .fetch_json(url)
  res <- raw$result$resources
  if (is.null(res) || nrow(res) == 0) return(.schema_resources)

  tibble::tibble(
    id            = as.character(res$id %||% NA),
    name          = as.character(res$name %||% NA),
    description   = as.character(res$description %||% NA),
    format        = as.character(res$format %||% NA),
    url           = as.character(res$url %||% NA),
    size          = suppressWarnings(as.numeric(res$size)),
    last_modified = as.character(res$last_modified %||% NA)
  )
}

#' List available tags on DOE EDX
#'
#' @param query Optional tag name filter
#' @param limit Maximum tags to return (default 50)
#' @return character vector of tag names
#' @export
doe_tags <- function(query = NULL, limit = 50) {
  url <- sprintf("%s/tag_list?limit=%d", .edx_base, as.integer(limit))
  if (!is.null(query)) url <- paste0(url, "&query=", utils::URLencode(query))
  raw <- .fetch_json(url)
  if (is.null(raw$result) || length(raw$result) == 0) return(character(0))
  as.character(raw$result)
}

#' Get total count of DOE EDX datasets
#'
#' @return integer count of datasets
#' @export
doe_count <- function() {
  url <- sprintf("%s/package_search?q=&rows=0", .edx_base)
  raw <- .fetch_json(url)
  as.integer(raw$result$count %||% 0L)
}


# == Context ===================================================================

#' Generate LLM-friendly context for doe.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
doe_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) {
    src_file <- system.file("source", "doe.R", package = "doe.gov")
  }
  if (is.null(src_file) || !nzchar(src_file) || !file.exists(src_file)) {
    cat("# doe.gov context - source not found\n")
    return(invisible("# doe.gov context - source not found"))
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
