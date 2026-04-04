


# data-ssb-no.R
# Self-contained SSB (Statistics Norway) PxWeb API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ssb_base <- "https://data.ssb.no/api/v0/en/table"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

.post_json <- function(url, body) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# == Schemas ===================================================================

.schema_tables <- tibble(
  id = character(), type = character(), text = character()
)

.schema_metadata <- tibble(
  code = character(), text = character(), n_values = integer(),
  values_sample = character()
)

.schema_data <- tibble(
  dimension = character(), value = character()
)


# == Public functions ==========================================================

#' Browse SSB statistical tables
#'
#' Navigates the hierarchical table tree. Items with type "l" are folders;
#' items ending in digits are usually data tables.
#'
#' @param path Path in the table tree. Empty string for root categories.
#'   Use table IDs directly for specific tables (e.g. "07459").
#' @return tibble: id, type ("l" = folder), text
#' @export
ssb_tables <- function(path = "") {
  url <- if (nchar(path) > 0) {
    sprintf("%s/%s", .ssb_base, path)
  } else {
    .ssb_base
  }
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_tables)

  # If result is a list with 'title' and 'variables', it's table metadata not a listing
  if (!is.null(raw$title)) {
    return(tibble(
      id = as.character(path),
      type = "t",
      text = as.character(raw$title)
    ))
  }

  as_tibble(raw) |>
    transmute(
      id = as.character(id),
      type = as.character(type),
      text = as.character(text)
    )
}

#' Get metadata for an SSB table
#'
#' Returns variable definitions for a specific table, useful for building queries.
#'
#' @param table_id Table ID (e.g. "07459" for population)
#' @return tibble: code, text, n_values, values_sample (first 5 values as comma-separated string)
#' @export
ssb_metadata <- function(table_id) {
  url <- sprintf("%s/%s", .ssb_base, table_id)
  raw <- jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
  vars <- raw$variables
  if (is.null(vars) || length(vars) == 0) return(.schema_metadata)

  rows <- lapply(vars, function(v) {
    vals <- unlist(v$values)
    tibble(
      code = as.character(v$code),
      text = as.character(v$text),
      n_values = as.integer(length(vals)),
      values_sample = paste(head(vals, 5), collapse = ", ")
    )
  })
  bind_rows(rows)
}

#' Query SSB table data
#'
#' Sends a POST query to fetch data from a specific table.
#' Returns JSON-stat2 format parsed into a tibble.
#'
#' @param table_id Table ID (e.g. "07459")
#' @param query List of selection filters. Each element should be a list with
#'   "code", "selection" (list with "filter" and "values").
#'   Use ssb_metadata() to discover valid codes and values.
#'   Example: list(list(code="Region", selection=list(filter="item", values=list("0"))))
#' @param format Response format: "json-stat2" (default) or "csv"
#' @return tibble with dimensions and values from the query result
#' @export
ssb_query <- function(table_id, query = list(), format = "json-stat2") {
  url <- sprintf("%s/%s", .ssb_base, table_id)
  body <- list(query = query, response = list(format = format))

  raw <- .post_json(url, body)
  if (is.null(raw)) return(.schema_data)

  # Parse JSON-stat2 response
  dims <- raw$id
  sizes <- unlist(raw$size)
  if (is.null(dims) || is.null(sizes)) return(.schema_data)

  # Build dimension labels
  dim_labels <- list()
  for (d in dims) {
    dim_info <- raw$dimension[[d]]
    cat_index <- dim_info$category$index
    cat_label <- dim_info$category$label
    if (is.null(cat_label)) {
      # Use index keys as labels
      dim_labels[[d]] <- names(cat_index)
    } else {
      # Order by index
      ordered_keys <- names(cat_index)[order(unlist(cat_index))]
      dim_labels[[d]] <- vapply(ordered_keys, function(k) as.character(cat_label[[k]]), character(1))
    }
  }

  # Build cartesian product of dimension values
  grid <- expand.grid(dim_labels, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  names(grid) <- vapply(dims, function(d) as.character(raw$dimension[[d]]$label), character(1))

  # Extract values
  vals <- unlist(raw$value)
  if (length(vals) == nrow(grid)) {
    grid$value <- as.character(vals)
  }

  as_tibble(grid)
}

# == Context ===================================================================

#' Generate LLM-friendly context for data.ssb.no
#'
#' @return Character string with full function signatures and bodies
#' @export
ssb_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/data.ssb.no.R"
  if (!file.exists(src_file)) {
    cat("# data.ssb.no context - source not found\n")
    return(invisible("# data.ssb.no context - source not found"))
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

