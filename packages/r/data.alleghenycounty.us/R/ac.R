# == Dataset discovery =========================================================

#' Search datasets on WPRDC (Allegheny County open data)
#'
#' @param q Search query (default "" for all)
#' @param rows Number of results (default 50)
#' @param start Offset for pagination (default 0)
#' @return tibble: id, name, title, notes, organization, num_resources, metadata_modified
#' @export
ac_datasets <- function(q = "", rows = 50, start = 0) {
  url <- sprintf("%s/api/3/action/package_search?q=%s&rows=%d&start=%d",
                 .wprdc_base, utils::URLencode(q), rows, start)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(.schema_datasets)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0) return(.schema_datasets)
  tibble(
    id                = as.character(results$id),
    name              = as.character(results$name),
    title             = as.character(results$title),
    notes             = as.character(results$notes %||% NA_character_),
    organization      = vapply(seq_len(nrow(results)), function(i) {
      o <- results$organization
      if (is.data.frame(o)) o$title[i] %||% NA_character_
      else if (is.list(o)) (o[[i]]$title %||% NA_character_)
      else NA_character_
    }, character(1)),
    num_resources     = as.integer(results$num_resources),
    metadata_modified = as.character(results$metadata_modified)
  )
}

#' Get metadata for a WPRDC dataset
#'
#' @param id_or_name Dataset ID or name slug
#' @return tibble with dataset metadata including resource URLs
#' @export
ac_metadata <- function(id_or_name) {
  url <- sprintf("%s/api/3/action/package_show?id=%s", .wprdc_base, id_or_name)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  r <- raw$result
  resources <- r$resources
  res_df <- if (!is.null(resources) && is.data.frame(resources)) {
    tibble(
      resource_id = as.character(resources$id),
      name        = as.character(resources$name %||% NA_character_),
      format      = as.character(resources$format %||% NA_character_),
      url         = as.character(resources$url %||% NA_character_)
    )
  } else tibble(resource_id = character(), name = character(), format = character(), url = character())
  attr(res_df, "dataset_title") <- r$title
  attr(res_df, "dataset_notes") <- r$notes
  res_df
}

#' Query a WPRDC datastore resource
#'
#' Uses the CKAN DataStore SQL API to query tabular data.
#'
#' @param resource_id CKAN resource ID
#' @param sql Optional SQL query (if NULL, fetches raw records)
#' @param limit Number of records (default 1000, max 50000)
#' @param offset Offset for pagination (default 0)
#' @param filters Optional named list of field=value filters
#' @return tibble with resource data
#' @export
ac_query <- function(resource_id, sql = NULL, limit = 1000, offset = 0, filters = NULL) {
  if (!is.null(sql)) {
    url <- sprintf("%s/api/3/action/datastore_search_sql?sql=%s",
                   .wprdc_base, utils::URLencode(sql))
  } else {
    url <- sprintf("%s/api/3/action/datastore_search?resource_id=%s&limit=%d&offset=%d",
                   .wprdc_base, resource_id, limit, offset)
    if (!is.null(filters)) {
      url <- paste0(url, "&filters=", utils::URLencode(jsonlite::toJSON(filters, auto_unbox = TRUE)))
    }
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || length(records) == 0) return(tibble())
  df <- as_tibble(records)
  # Remove internal _id column if present
  if ("_id" %in% names(df)) df <- df |> select(-`_id`)
  df
}

#' Get the total record count for a WPRDC resource
#'
#' @param resource_id CKAN resource ID
#' @return integer record count
#' @export
ac_count <- function(resource_id) {
  url <- sprintf("%s/api/3/action/datastore_search?resource_id=%s&limit=0",
                 .wprdc_base, resource_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(NA_integer_)
  as.integer(raw$result$total %||% NA)
}

# == Context ===================================================================

#' Generate LLM-friendly context for alleghenycounty.us
#'
#' @return Character string with full function signatures and bodies
#' @export
ac_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/alleghenycounty.us.R"
  if (!file.exists(src_file)) { cat("# alleghenycounty.us context - source not found\n"); return(invisible(NULL)) }
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
