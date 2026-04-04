# api.scb.se.R - Self-contained api.scb.se client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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
#' Navigates the hierarchical table tree of Statistics Sweden (SCB). The tree
#' is organized by subject area at the root level (e.g. "BE" for Population,
#' "AM" for Labour market). Drill down by appending path segments separated
#' by \code{"/"}. Items with \code{type = "l"} are folders (navigate deeper);
#' items with \code{type = "t"} are data tables (query with
#' \code{\link{scb_query}} or inspect with \code{\link{scb_metadata}}).
#' Use \code{\link{scb_search}} to find tables by keyword instead of browsing.
#'
#' @param path Character. Path in the table tree. Use \code{""} (empty string)
#'   for root-level categories. Examples:
#'   \itemize{
#'     \item \code{""} -- root: shows all 19 subject areas
#'     \item \code{"BE"} -- Population: shows sub-areas
#'     \item \code{"BE/BE0101"} -- Population statistics: shows topics
#'     \item \code{"BE/BE0101/BE0101A"} -- Number of inhabitants: shows tables
#'   }
#'
#' @return A tibble with one row per item and the following columns:
#' \describe{
#'   \item{id}{Character. Item identifier (e.g. \code{"BE"}, \code{"BefolkningR1860N"}).}
#'   \item{type}{Character. \code{"l"} for folder (list), \code{"t"} for data table.}
#'   \item{text}{Character. Human-readable name/description of the item.}
#'   \item{updated}{Character. Last-updated timestamp for tables (\code{NA} for folders).}
#' }
#'
#' @examples
#' \dontrun{
#' # Browse root categories
#' scb_tables()
#'
#' # Browse Population sub-areas
#' scb_tables("BE")
#'
#' # Find actual data tables under Number of inhabitants
#' scb_tables("BE/BE0101/BE0101A")
#' }
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
#' Returns the variable definitions (dimensions and value lists) for a specific
#' data table. Use this to understand what filter codes and values are available
#' before building a query with \code{\link{scb_query}}. Each row represents
#' one dimension (e.g. age, sex, year) with a sample of its valid values.
#'
#' @param table_path Character. Full path to the table in the SCB hierarchy.
#'   Example: \code{"BE/BE0101/BE0101A/BefolkningR1860N"} (Population by age
#'   and sex, 1860-2024). Find valid paths by browsing with
#'   \code{\link{scb_tables}} or searching with \code{\link{scb_search}}.
#'
#' @return A tibble with one row per variable/dimension and the following columns:
#' \describe{
#'   \item{code}{Character. Variable code used in query filters (e.g. \code{"Alder"}, \code{"Kon"}, \code{"Tid"}).}
#'   \item{text}{Character. Human-readable variable name (e.g. \code{"age"}, \code{"sex"}, \code{"year"}).}
#'   \item{n_values}{Integer. Total number of valid values for this variable.}
#'   \item{values_sample}{Character. First 5 valid values, comma-separated (e.g. \code{"0, 1, 2, 3, 4"}).}
#' }
#'
#' @examples
#' \dontrun{
#' # Inspect variables for Population by age/sex table
#' scb_metadata("BE/BE0101/BE0101A/BefolkningR1860N")
#' # Returns: Alder (age, 112 values), Kon (sex, 2 values),
#' #   ContentsCode (1 value), Tid (year, 165 values)
#' }
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
#' Sends a POST request to fetch data from a specific SCB table using the
#' PxWeb JSON-stat format. Use \code{\link{scb_metadata}} first to discover
#' valid variable codes and their values for building the query filters.
#' When \code{query} is an empty list, the API returns data for all
#' combinations of all variables (which may be large).
#'
#' @param table_path Character. Full path to the table in the SCB hierarchy.
#'   Example: \code{"BE/BE0101/BE0101A/BefolkningR1860N"}.
#' @param query List of selection filter specifications. Each element must be
#'   a named list with \code{code} (variable code from \code{scb_metadata()})
#'   and \code{selection} (a list with \code{filter} and \code{values}).
#'   Common filter types: \code{"item"} (exact match), \code{"all"} (wildcard).
#'   Use \code{\link{scb_metadata}} to discover valid codes and values.
#'   Example: \code{list(list(code = "Alder", selection = list(filter = "item",
#'   values = list("0", "1"))))}.
#' @param format Character. Response format: \code{"json"} (default) or
#'   \code{"csv"}.
#'
#' @return A tibble with columns corresponding to the table's dimensions and
#'   value columns. Column names are the human-readable variable texts from the
#'   table metadata. The exact columns depend on the table queried.
#'
#' @examples
#' \dontrun{
#' # Query population for age 0, both sexes, year 2020
#' scb_query(
#'   "BE/BE0101/BE0101A/BefolkningR1860N",
#'   query = list(
#'     list(code = "Alder", selection = list(filter = "item", values = list("0"))),
#'     list(code = "Kon",   selection = list(filter = "item", values = list("1", "2"))),
#'     list(code = "Tid",   selection = list(filter = "item", values = list("2020")))
#'   )
#' )
#'
#' # Query with no filters (returns all data -- may be large)
#' scb_query("BE/BE0101/BE0101A/BefolkningR1860N")
#' }
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

# == Convenience: Table search ==================================================

#' Search SCB table tree by keyword
#'
#' Searches the SCB table tree for tables and folders whose names match a
#' keyword. Searches at the starting level and one level deeper into
#' sub-folders. This is a convenience wrapper around \code{\link{scb_tables}}
#' that avoids manual browsing. Use the returned \code{path} column directly
#' with \code{\link{scb_metadata}} or \code{\link{scb_query}}.
#'
#' @param keyword Character. Keyword to search for in table/folder names.
#'   Matching is case-insensitive. Example: \code{"population"}, \code{"GDP"},
#'   \code{"employment"}.
#' @param path Character. Starting path in the hierarchy to search from
#'   (default \code{""} for root). Example: \code{"NR"} to search only within
#'   National accounts.
#'
#' @return A tibble with one row per matching item and the following columns:
#' \describe{
#'   \item{id}{Character. Item identifier.}
#'   \item{type}{Character. \code{"l"} for folder, \code{"t"} for data table.}
#'   \item{text}{Character. Human-readable name/description.}
#'   \item{path}{Character. Full path to the item, usable with \code{scb_metadata()} or \code{scb_query()}.}
#' }
#'
#' @examples
#' \dontrun{
#' # Search for population-related tables
#' scb_search("population")
#'
#' # Search for GDP data within National accounts
#' scb_search("GDP", path = "NR")
#' }
scb_search <- function(keyword, path = "") {
  top <- tryCatch(scb_tables(path), error = function(e) NULL)
  if (is.null(top) || nrow(top) == 0) return(tibble(id = character(), type = character(),
                                                      text = character(), path = character()))

  # Search at current level
  matched <- top[grepl(keyword, top$text, ignore.case = TRUE), ]
  matched$path <- if (nchar(path) > 0) paste0(path, "/", matched$id) else matched$id

  # Search one level deeper in folders
  folders <- top[top$type == "l", ]
  sub_results <- lapply(seq_len(nrow(folders)), function(i) {
    sub_path <- if (nchar(path) > 0) paste0(path, "/", folders$id[i]) else folders$id[i]
    tryCatch({
      sub <- scb_tables(sub_path)
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

#' Get api.scb.se client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
scb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(scb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.scb.se.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.scb.se")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.scb.se context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.scb.se", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
