


# api-scb-se.R
# Self-contained SCB (Statistics Sweden) PxWeb API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: 10 queries per 10 seconds


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.scb_base <- "https://api.scb.se/OV0104/v1/doris/en/ssd"
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
  id = character(), type = character(), text = character(),
  updated = character()
)

.schema_metadata <- tibble(
  code = character(), text = character(), n_values = integer(),
  values_sample = character()
)

.schema_data <- tibble(
  key = character(), values = character()
)


# == Public functions ==========================================================

#' Browse SCB statistical tables
#'
#' Navigates the hierarchical table tree. Use empty path for root categories.
#' Items with type "l" are folders; type "t" are data tables.
#'
#' @param path Path in the table tree (e.g. "BE" for Population,
#'   "BE/BE0101" for demographics). Empty string for root.
#' @return tibble: id, type ("l" = folder, "t" = table), text, updated
#' @export
scb_tables <- function(path = "") {
  url <- if (nchar(path) > 0) {
    sprintf("%s/%s", .scb_base, path)
  } else {
    .scb_base
  }
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_tables)

  as_tibble(raw) |>
    transmute(
      id = as.character(id),
      type = as.character(type),
      text = as.character(text),
      updated = as.character(if ("updated" %in% names(raw)) updated else NA_character_)
    )
}

#' Get metadata for an SCB table
#'
#' Returns variable definitions for a specific table, useful for building queries.
#'
#' @param table_path Full path to the table (e.g. "BE/BE0101/BE0101A/BefolkningR1860N")
#' @return tibble: code, text, n_values, values_sample (first 5 values as comma-separated string)
#' @export
scb_metadata <- function(table_path) {
  url <- sprintf("%s/%s", .scb_base, table_path)
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

#' Query SCB table data
#'
#' Sends a POST query to fetch data from a specific table.
#' The query body follows the PxWeb JSON-stat format.
#'
#' @param table_path Full path to the table (e.g. "BE/BE0101/BE0101A/BefolkningR1860N")
#' @param query List of selection filters. Each element should be a list with
#'   "code", "selection" (list with "filter" and "values").
#'   Use scb_metadata() to discover valid codes and values.
#'   Example: list(list(code="Alder", selection=list(filter="item", values=list("0"))))
#' @param format Response format: "json" (default) or "csv"
#' @return tibble with columns from the table (key dimensions + value columns)
#' @export
scb_query <- function(table_path, query = list(), format = "json") {
  url <- sprintf("%s/%s", .scb_base, table_path)
  body <- list(query = query, response = list(format = format))

  raw <- .post_json(url, body)

  if (is.null(raw)) return(.schema_data)

  # Parse PxWeb JSON response: columns + data
  cols <- raw$columns
  if (is.null(cols)) return(.schema_data)

  col_names <- vapply(cols, function(c) as.character(c$text), character(1))
  col_codes <- vapply(cols, function(c) as.character(c$code), character(1))

  data_items <- raw$data
  if (is.null(data_items) || length(data_items) == 0) return(.schema_data)

  rows <- lapply(data_items, function(d) {
    keys <- unlist(d$key)
    vals <- unlist(d$values)
    setNames(as.list(c(keys, vals)), col_codes)
  })

  result <- bind_rows(lapply(rows, as_tibble))
  # Set readable column names
  names(result) <- col_names
  result
}

# == Context ===================================================================

#' Generate LLM-friendly context for api.scb.se
#'
#' @return Character string with full function signatures and bodies
#' @export
scb_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.scb.se.R"
  if (!file.exists(src_file)) {
    cat("# api.scb.se context - source not found\n")
    return(invisible("# api.scb.se context - source not found"))
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

