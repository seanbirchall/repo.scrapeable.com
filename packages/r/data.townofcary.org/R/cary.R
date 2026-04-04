# == Dataset discovery =========================================================

#' List datasets on Town of Cary open data portal
#'
#' @param limit Number of datasets to return (default 50)
#' @param q Optional search query
#' @return tibble: dataset_id, title, description, theme, records_count, modified
#' @export
cary_datasets <- function(limit = 50, q = NULL) {
  url <- sprintf("%s/catalog/datasets?limit=%d&offset=0", .cary_base, limit)
  if (!is.null(q)) url <- paste0(url, "&where=search(\"", utils::URLencode(q), "\")")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$datasets) || length(raw$datasets) == 0) return(.schema_datasets)
  ds <- raw$datasets
  metas <- ds$dataset$metas$default
  tibble(
    dataset_id    = as.character(ds$dataset$dataset_id),
    title         = as.character(metas$title %||% NA_character_),
    description   = as.character(metas$description %||% NA_character_),
    theme         = vapply(metas$theme %||% list(), function(x) paste(x, collapse = "; "), character(1)),
    records_count = as.integer(ds$dataset$has_records %||% NA),
    modified      = as.character(metas$modified %||% NA_character_)
  )
}

#' Get records from a Town of Cary dataset
#'
#' @param dataset_id Dataset identifier (e.g. "cpd-incidents")
#' @param limit Number of records to return (default 100, max 100)
#' @param offset Offset for pagination (default 0)
#' @param where Optional ODS filter expression
#' @param select Optional comma-separated field names
#' @param order_by Optional field to sort by (e.g. "date_of DESC")
#' @return tibble with dataset fields
#' @export
cary_records <- function(dataset_id, limit = 100, offset = 0,
                         where = NULL, select = NULL, order_by = NULL) {
  url <- sprintf("%s/catalog/datasets/%s/records?limit=%d&offset=%d",
                 .cary_base, dataset_id, limit, offset)
  if (!is.null(where))    url <- paste0(url, "&where=", utils::URLencode(where))
  if (!is.null(select))   url <- paste0(url, "&select=", utils::URLencode(select))
  if (!is.null(order_by)) url <- paste0(url, "&order_by=", utils::URLencode(order_by))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$records) || length(raw$records) == 0) return(tibble())
  recs <- raw$records$record$fields
  if (is.data.frame(recs)) return(as_tibble(recs))
  tibble()
}

#' Get metadata for a specific Town of Cary dataset
#'
#' @param dataset_id Dataset identifier
#' @return tibble: dataset_id, title, description, theme, records_count, fields
#' @export
cary_metadata <- function(dataset_id) {
  url <- sprintf("%s/catalog/datasets/%s", .cary_base, dataset_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$dataset)) return(tibble())
  ds <- raw$dataset
  m <- ds$metas$default
  fields_list <- ds$fields
  field_names <- if (!is.null(fields_list) && is.data.frame(fields_list))
    paste(fields_list$name, collapse = ", ") else NA_character_
  tibble(
    dataset_id    = as.character(ds$dataset_id),
    title         = as.character(m$title %||% NA_character_),
    description   = as.character(m$description %||% NA_character_),
    theme         = paste(m$theme %||% character(), collapse = "; "),
    records_count = as.integer(ds$has_records %||% NA),
    fields        = field_names
  )
}

#' Download a full Town of Cary dataset as CSV
#'
#' @param dataset_id Dataset identifier
#' @param limit Max rows (default 10000)
#' @return tibble with all fields
#' @export
cary_export <- function(dataset_id, limit = 10000) {
  url <- sprintf("%s/catalog/datasets/%s/exports/csv?limit=%d&delimiter=,",
                 .cary_base, dataset_id, limit)
  tmp <- tryCatch(.fetch(url, ext = ".csv"), error = function(e) NULL)
  if (is.null(tmp)) return(tibble())
  tryCatch(as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE)),
           error = function(e) tibble())
}

# == Context ===================================================================

#' Generate LLM-friendly context for townofcary.org
#'
#' @return Character string with full function signatures and bodies
#' @export
cary_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/townofcary.org.R"
  if (!file.exists(src_file)) { cat("# townofcary.org context - source not found\n"); return(invisible(NULL)) }
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
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
