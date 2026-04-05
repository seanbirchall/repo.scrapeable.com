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
#' Navigates the hierarchical table tree of Statistics Sweden (SCB) via the
#' PxWeb API. Use an empty path for root categories, then drill down by ID.
#' Items with type \code{"l"} are folders; type \code{"t"} are data tables
#' that can be queried with \code{\link{scb_query}}.
#'
#' @param path Character. Path in the table tree (e.g. \code{"BE"} for
#'   Population, \code{"BE/BE0101"} for demographics). Default \code{""} for
#'   root.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Node identifier for further navigation.}
#'     \item{type}{Character. \code{"l"} = folder (list), \code{"t"} = data table.}
#'     \item{text}{Character. Human-readable label.}
#'     \item{updated}{Character. Last-updated timestamp (tables only; \code{NA} for folders).}
#'   }
#' @examples
#' scb_tables()
#' scb_tables("BE")
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
#' Returns variable definitions for a specific PxWeb table, showing dimension
#' codes, labels, and sample values. Use this to discover valid parameters
#' for \code{\link{scb_query}}.
#'
#' @param table_path Character. Full path to the table
#'   (e.g. \code{"BE/BE0101/BE0101A/BesijR1860Tot2N"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Character. Variable code for use in queries.}
#'     \item{text}{Character. Human-readable variable label.}
#'     \item{n_values}{Integer. Number of possible values.}
#'     \item{values_sample}{Character. First 5 values (comma-separated).}
#'   }
#' @examples
#' scb_metadata("BE/BE0101/BE0101A/BefolkningR1860N")
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
#' Sends a POST query to the PxWeb API to fetch data from a specific SCB
#' table. The response is parsed into a tibble with dimension labels as column
#' names plus value columns. Use \code{\link{scb_metadata}} to discover valid
#' variable codes and their possible values.
#'
#' @param table_path Character. Full path to the table
#'   (e.g. \code{"BE/BE0101/BE0101A/BefolkningR1860N"}).
#' @param query List. Selection filters. Each element should be a list with
#'   \code{"code"} and \code{"selection"} (itself a list with \code{"filter"}
#'   and \code{"values"}). Example:
#'   \code{list(list(code = "Alder", selection = list(filter = "item",
#'   values = list("0"))))}.
#' @param format Character. Response format: \code{"json"} (default) or
#'   \code{"csv"}.
#' @return A tibble whose columns are the table's dimension labels (character)
#'   plus value columns (character). Column names come from the table metadata.
#' @examples
#' scb_query("BE/BE0101/BE0101A/BefolkningR1860N")
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

# == Convenience: Table search ==================================================

#' Search SCB table tree by keyword
#'
#' Recursively searches the SCB table hierarchy for tables whose names match
#' a keyword. Searches up to 2 levels deep from the starting path. Useful for
#' discovering table paths before calling \code{\link{scb_query}}.
#'
#' @param keyword Character. Keyword to search for in table names
#'   (case-insensitive).
#' @param path Character. Starting path in the hierarchy (default \code{""}
#'   for root).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Node identifier.}
#'     \item{type}{Character. \code{"l"} = folder, \code{"t"} = table.}
#'     \item{text}{Character. Node label.}
#'     \item{path}{Character. Full navigation path to the node.}
#'   }
#' @examples
#' scb_search("population")
#' @export
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

#' Get scb.se client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/scb.se.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "scb.se")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# scb.se context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# scb.se", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
