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
#' Navigates the hierarchical table tree of Statistics Norway (SSB) via the
#' PxWeb API. Items with type \code{"l"} are folders; items ending in digits
#' are usually data tables queryable with \code{\link{ssb_query}}.
#'
#' @param path Character. Path in the table tree. Default \code{""} for root
#'   categories. Use folder IDs to drill down (e.g. \code{"be"}) or table IDs
#'   directly (e.g. \code{"07459"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Node identifier for navigation.}
#'     \item{type}{Character. \code{"l"} = folder, \code{"t"} = table.}
#'     \item{text}{Character. Human-readable label.}
#'   }
#' @examples
#' ssb_tables()
#' ssb_tables("be")
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
#' Returns variable definitions for a specific PxWeb table, showing dimension
#' codes, labels, and sample values. Use this to discover valid parameters
#' for \code{\link{ssb_query}}.
#'
#' @param table_id Character. Table identifier (e.g. \code{"07459"} for
#'   population by municipality, sex, and age).
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Character. Variable code for use in queries.}
#'     \item{text}{Character. Human-readable variable label.}
#'     \item{n_values}{Integer. Number of possible values.}
#'     \item{values_sample}{Character. First 5 values (comma-separated).}
#'   }
#' @examples
#' ssb_metadata("07459")
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
#' Sends a POST query to the PxWeb API to fetch data from a specific SSB
#' table. Parses JSON-stat2 responses into a tibble with one column per
#' dimension plus a value column. Use \code{\link{ssb_metadata}} to discover
#' valid variable codes and values.
#'
#' @param table_id Character. Table identifier (e.g. \code{"07459"}).
#' @param query List. Selection filters. Each element should be a list with
#'   \code{"code"} and \code{"selection"} (itself a list with \code{"filter"}
#'   and \code{"values"}). Example:
#'   \code{list(list(code = "Region", selection = list(filter = "item",
#'   values = list("0"))))}.
#' @param format Character. Response format: \code{"json-stat2"} (default)
#'   or \code{"csv"}.
#' @return A tibble whose columns are the table's dimension labels (character)
#'   plus a \code{value} column (character). Column names come from the
#'   JSON-stat2 dimension labels.
#' @examples
#' ssb_query("07459")
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

# == Convenience: Table search ==================================================

#' Search SSB table tree by keyword
#'
#' Recursively searches the SSB table hierarchy for tables whose names match
#' a keyword. Searches up to 2 levels deep from the starting path. Useful for
#' discovering table IDs before calling \code{\link{ssb_query}}.
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
#' ssb_search("population")
#' @export
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

#' Get ssb.no client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ssb.no.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ssb.no")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ssb.no context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ssb.no", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
