


# data-ca-gov.R
# Self-contained California Open Data (CKAN) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 at data.ca.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cagov_base <- "https://data.ca.gov/api/3/action"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.ckan_result <- function(url) {
  raw <- .fetch_json(url)
  if (!isTRUE(raw$success)) {
    warning("CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(), notes = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character()
)

.schema_datastore <- tibble()

# == Datasets ==================================================================

#' Search California Open Data datasets
#'
#' Searches the CKAN catalog.
#'
#' @param query Search term
#' @param rows Number of results (default 50)
#' @param start Offset (default 0)
#' @return tibble: id, name, title, organization, num_resources, notes
#' @export
cagov_search <- function(query = NULL, rows = 50, start = 0) {
  params <- sprintf("rows=%d&start=%d", rows, start)
  if (!is.null(query)) {
    params <- paste0(params, "&q=", utils::URLencode(query, reserved = TRUE))
  }
  url <- paste0(.cagov_base, "/package_search?", params)

  result <- .ckan_result(url)
  if (is.null(result)) return(.schema_datasets)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_datasets)

  tibble(
    id            = vapply(datasets, function(d) d$id %||% NA_character_, character(1)),
    name          = vapply(datasets, function(d) d$name %||% NA_character_, character(1)),
    title         = vapply(datasets, function(d) d$title %||% NA_character_, character(1)),
    organization  = vapply(datasets, function(d) d$organization$title %||% NA_character_, character(1)),
    num_resources = vapply(datasets, function(d) as.integer(d$num_resources %||% 0L), integer(1)),
    notes         = vapply(datasets, function(d) {
      n <- d$notes %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


#' Get resources for a California dataset
#'
#' @param dataset_name Dataset name or ID
#' @return tibble: id, name, format, url, description
#' @export
cagov_resources <- function(dataset_name) {
  url <- sprintf("%s/package_show?id=%s", .cagov_base, dataset_name)
  result <- .ckan_result(url)
  if (is.null(result)) return(.schema_resources)

  resources <- result$resources
  if (length(resources) == 0) return(.schema_resources)

  tibble(
    id          = vapply(resources, function(r) r$id %||% NA_character_, character(1)),
    name        = vapply(resources, function(r) r$name %||% NA_character_, character(1)),
    format      = vapply(resources, function(r) r$format %||% NA_character_, character(1)),
    url         = vapply(resources, function(r) r$url %||% NA_character_, character(1)),
    description = vapply(resources, function(r) {
      n <- r$description %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )
}


#' Query California datastore
#'
#' Queries tabular data stored in the CKAN datastore.
#'
#' @param resource_id Resource ID (get from cagov_resources())
#' @param filters Named list of field=value filters
#' @param query Full-text search within the resource
#' @param limit Max rows (default 100)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
cagov_datastore <- function(resource_id, filters = NULL, query = NULL,
                            limit = 100, offset = 0) {
  params <- sprintf("resource_id=%s&limit=%d&offset=%d",
                    resource_id, limit, offset)
  if (!is.null(filters)) {
    params <- paste0(params, "&filters=",
                     utils::URLencode(jsonlite::toJSON(filters, auto_unbox = TRUE)))
  }
  if (!is.null(query)) {
    params <- paste0(params, "&q=", utils::URLencode(query, reserved = TRUE))
  }

  url <- paste0(.cagov_base, "/datastore_search?", params)
  result <- .ckan_result(url)
  if (is.null(result) || is.null(result$records) || length(result$records) == 0) {
    return(.schema_datastore)
  }

  records <- result$records
  # Convert list of lists to tibble
  cols <- unique(unlist(lapply(records, names)))
  cols <- cols[cols != "_id"]

  df_list <- lapply(cols, function(col) {
    vapply(records, function(r) as.character(r[[col]] %||% NA_character_), character(1))
  })
  names(df_list) <- cols
  as_tibble(df_list)
}


# == Context ===================================================================

#' Generate LLM-friendly context for data.ca.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
cagov_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/data.ca.gov.R"
  if (!file.exists(src_file)) {
    cat("# data.ca.gov context - source not found\n")
    return(invisible("# data.ca.gov context - source not found"))
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

