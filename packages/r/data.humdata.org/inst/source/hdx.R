


# humdata.R
# Self-contained OCHA Humanitarian Data Exchange (HDX) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: generous (CKAN API)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hdx_base <- "https://data.humdata.org/api/3/action"

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
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(),
  metadata_created = character(), notes = character()
)

.schema_dataset <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), metadata_created = character(),
  resource_name = character(), resource_url = character(),
  resource_format = character(), resource_size = integer()
)

.schema_organizations <- tibble(
  id = character(), name = character(), title = character(),
  description = character(), package_count = integer()
)


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search HDX datasets
#'
#' @param query Search query (e.g. "refugees", "food security", "cholera")
#' @param rows Number of results (default 10)
#' @param start Offset for pagination (default 0)
#' @return tibble: id, name, title, organization, num_resources,
#'   metadata_created, notes
#' @export
hdx_search <- function(query, rows = 10, start = 0) {
  url <- sprintf(
    "%s/package_search?q=%s&rows=%d&start=%d",
    .hdx_base, utils::URLencode(query, reserved = TRUE),
    as.integer(rows), as.integer(start)
  )
  raw <- .fetch_json(url)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0 ||
      (is.data.frame(results) && nrow(results) == 0)) return(.schema_search)

  tibble(
    id = as.character(results$id),
    name = as.character(results$name),
    title = as.character(results$title),
    organization = as.character(
      if (!is.null(results$organization)) results$organization$title
      else NA_character_
    ),
    num_resources = as.integer(results$num_resources %||% NA_integer_),
    metadata_created = as.character(results$metadata_created %||% NA_character_),
    notes = as.character(substr(results$notes %||% NA_character_, 1, 200))
  )
}

#' Get a specific HDX dataset by ID or name
#'
#' @param id Dataset ID or name slug
#' @return tibble: id, name, title, organization, metadata_created,
#'   resource_name, resource_url, resource_format, resource_size (one row per resource)
#' @export
hdx_dataset <- function(id) {
  url <- sprintf("%s/package_show?id=%s", .hdx_base, id)
  raw <- .fetch_json(url)
  ds <- raw$result
  if (is.null(ds)) return(.schema_dataset)

  resources <- ds$resources
  if (is.null(resources) || length(resources) == 0) {
    return(tibble(
      id = as.character(ds$id), name = as.character(ds$name),
      title = as.character(ds$title),
      organization = as.character(ds$organization$title %||% NA_character_),
      metadata_created = as.character(ds$metadata_created %||% NA_character_),
      resource_name = NA_character_, resource_url = NA_character_,
      resource_format = NA_character_, resource_size = NA_integer_
    ))
  }

  tibble(
    id = as.character(ds$id),
    name = as.character(ds$name),
    title = as.character(ds$title),
    organization = as.character(ds$organization$title %||% NA_character_),
    metadata_created = as.character(ds$metadata_created %||% NA_character_),
    resource_name = as.character(resources$name),
    resource_url = as.character(resources$url),
    resource_format = as.character(resources$format %||% NA_character_),
    resource_size = as.integer(resources$size %||% NA_integer_)
  )
}

#' List HDX organizations
#'
#' @param limit Max results (default 20)
#' @param offset Offset for pagination (default 0)
#' @return tibble: id, name, title, description, package_count
#' @export
hdx_organizations <- function(limit = 20, offset = 0) {
  url <- sprintf(
    "%s/organization_list?all_fields=true&limit=%d&offset=%d",
    .hdx_base, as.integer(limit), as.integer(offset)
  )
  raw <- .fetch_json(url)
  results <- raw$result
  if (is.null(results) || length(results) == 0 ||
      (is.data.frame(results) && nrow(results) == 0)) return(.schema_organizations)

  tibble(
    id = as.character(results$id),
    name = as.character(results$name),
    title = as.character(results$title),
    description = as.character(substr(results$description %||% NA_character_, 1, 200)),
    package_count = as.integer(results$package_count %||% NA_integer_)
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for data.humdata.org
#'
#' @return Character string with full function signatures and bodies
#' @export
hdx_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/data.humdata.org.R"
  if (!file.exists(src_file)) {
    cat("# data.humdata.org context - source not found\n")
    return(invisible("# data.humdata.org context - source not found"))
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

