# data.ssb.no.R - Self-contained data.ssb.no client



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
#' Navigates the hierarchical table tree of Statistics Norway (SSB). The root
#' level contains 23 subject categories (e.g. "Population", "Labour market and
#' earnings"). Items with type "l" are folders that can be browsed further;
#' items ending in digits are data tables.
#'
#' @param path Character. Path in the table tree. Empty string "" for root
#'   categories. Use folder IDs for subcategories (e.g. "be" for Population)
#'   or table IDs directly (e.g. "07459").
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. Folder or table identifier (e.g. "al", "be", "07459").}
#'   \item{type}{Character. "l" for folder/list, "t" for data table.}
#'   \item{text}{Character. Descriptive label (e.g. "Labour market and earnings", "Population").}
#' }
#' @export
#' @examples
#' \dontrun{
#' ssb_tables()            # root categories
#' ssb_tables("be")        # Population subcategories
#' ssb_tables("be/be01")   # deeper browsing
#' }
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
#' Returns variable definitions for a specific table, showing the available
#' dimensions and their valid values. Use this to understand the structure
#' of a table before building queries with \code{ssb_query()}.
#'
#' @param table_id Character. Table ID (e.g. "07459" for population by
#'   region, sex, and age).
#' @return A tibble with one row per variable/dimension and columns:
#' \describe{
#'   \item{code}{Character. Variable code for use in queries (e.g. "Region", "Kjonn", "Alder", "Tid").}
#'   \item{text}{Character. Human-readable label (e.g. "region", "sex", "age", "year").}
#'   \item{n_values}{Integer. Number of valid values for this variable.}
#'   \item{values_sample}{Character. First 5 valid value codes, comma-separated (e.g. "0, 31, 3101, 3103, 3105").}
#' }
#' @export
#' @examples
#' \dontrun{
#' ssb_metadata("07459")
#' }
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
#' Sends a POST query to fetch data from a specific table using the PxWeb
#' API. Returns JSON-stat2 format parsed into a tibble. Use
#' \code{ssb_metadata()} first to discover valid variable codes and values.
#'
#' @param table_id Character. Table ID (e.g. "07459").
#' @param query List. Selection filters. Each element should be a list with
#'   "code" and "selection" (itself a list with "filter" and "values").
#'   Use \code{ssb_metadata()} to discover valid codes and values.
#'   Example: \code{list(list(code="Region", selection=list(filter="item", values=list("0"))))}.
#' @param format Character. Response format: "json-stat2" (default) or "csv".
#' @return A tibble with one column per dimension plus a value column. Column
#'   names come from the table's dimension labels. For table "07459" (population),
#'   typical columns are:
#' \describe{
#'   \item{region}{Character. Geographic area (e.g. "The whole country").}
#'   \item{sex}{Character. "Females" or "Males".}
#'   \item{age}{Character. Age group (e.g. "0 years", "1 year").}
#'   \item{contents}{Character. Measure label (e.g. "Persons").}
#'   \item{year}{Character. Year of observation (e.g. "2023").}
#'   \item{value}{Character. The data value.}
#' }
#' @export
#' @examples
#' \dontrun{
#' ssb_query("07459", query = list(
#'   list(code = "Region", selection = list(filter = "item", values = list("0"))),
#'   list(code = "Kjonn", selection = list(filter = "item", values = list("1", "2"))),
#'   list(code = "Alder", selection = list(filter = "item", values = list("000"))),
#'   list(code = "Tid", selection = list(filter = "item", values = list("2023", "2024")))
#' ))
#' }
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

# == Convenience: Table search ==================================================

#' Search SSB table tree by keyword
#'
#' Searches the SSB table tree for tables and folders matching a keyword.
#' Searches up to 2 levels deep in the hierarchy. For deeper searches, use
#' \code{ssb_tables()} to navigate manually.
#'
#' @param keyword Character. Keyword to search for in table names
#'   (case-insensitive grep). E.g. "population", "income", "employment".
#' @param path Character. Starting path in the tree (default "" for root).
#'   Narrow the search by specifying a subfolder (e.g. "be" for Population).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. Folder or table identifier.}
#'   \item{type}{Character. "l" for folder, "t" for table.}
#'   \item{text}{Character. Descriptive label.}
#'   \item{path}{Character. Full path in the table tree (e.g. "be/be02").}
#' }
#' @export
#' @examples
#' \dontrun{
#' ssb_search("population")
#' ssb_search("income", path = "al")
#' }
ssb_search <- function(keyword, path = "") {
  top <- tryCatch(ssb_tables(path), error = function(e) NULL)
  if (is.null(top) || nrow(top) == 0) return(tibble(id = character(), type = character(),
                                                      text = character(), path = character()))

  matched <- top[grepl(keyword, top$text, ignore.case = TRUE), ]
  matched$path <- if (nchar(path) > 0) paste0(path, "/", matched$id) else matched$id

  folders <- top[top$type == "l", ]
  sub_results <- lapply(seq_len(nrow(folders)), function(i) {
    sub_path <- if (nchar(path) > 0) paste0(path, "/", folders$id[i]) else folders$id[i]
    tryCatch({
      sub <- ssb_tables(sub_path)
      sub_matched <- sub[grepl(keyword, sub$text, ignore.case = TRUE), ]
      if (nrow(sub_matched) > 0) {
        sub_matched$path <- paste0(sub_path, "/", sub_matched$id)
        sub_matched
      } else NULL
    }, error = function(e) NULL)
  })

  bind_rows(c(list(matched), sub_results)) |>
    select(id, type, text, path)
}

# == Context ===================================================================

#' Get data.ssb.no client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ssb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ssb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.ssb.no.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.ssb.no")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.ssb.no context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# data.ssb.no", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
