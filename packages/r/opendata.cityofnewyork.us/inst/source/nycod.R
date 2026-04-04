


# opendata-cityofnewyork-us.R
# Self-contained NYC Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (app token optional for higher rate limits)
# API: Socrata SODA at data.cityofnewyork.us

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyc_base <- "https://data.cityofnewyork.us"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), rows_updated_at = as.POSIXct(character())
)

.schema_records <- tibble()

# == Datasets ==================================================================

#' List NYC Open Data datasets
#'
#' Returns metadata for datasets available on the NYC Open Data portal.
#'
#' @param limit Number of datasets to return (default 50)
#' @param query Optional search filter on dataset name
#' @return tibble: id, name, category, description, rows_updated_at
#' @export
nycod_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .nyc_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NYC Open Data API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id               = as.character(raw$id),
    name             = as.character(raw$name),
    category         = as.character(raw$category %||% NA_character_),
    description      = ifelse(nchar(raw$description %||% "") > 200,
                              paste0(substr(raw$description, 1, 200), "..."),
                              as.character(raw$description %||% "")),
    rows_updated_at  = as.POSIXct(as.numeric(raw$rowsUpdatedAt), origin = "1970-01-01")
  )

  if (!is.null(query)) {
    result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  }
  result
}


#' Fetch records from an NYC Open Data dataset
#'
#' Queries a specific dataset using the Socrata SODA API.
#' Supports SoQL filtering, ordering, and pagination.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "erm2-nwe9" for 311).
#'   Find IDs via nycod_datasets().
#' @param where SoQL where clause (e.g. "borough='MANHATTAN'")
#' @param select SoQL select clause (e.g. "borough,count(*)")
#' @param group SoQL group by clause
#' @param order SoQL order clause (e.g. "created_date DESC")
#' @param limit Max rows to return (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
#' @export
nycod_query <- function(dataset_id, where = NULL, select = NULL,
                        group = NULL, order = NULL,
                        limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .nyc_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("NYC Open Data query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)

  as_tibble(raw)
}


#' Fetch all records from an NYC dataset with auto-pagination
#'
#' @param dataset_id Socrata dataset identifier
#' @param where Optional SoQL where clause
#' @param max_rows Maximum total rows to fetch (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble of all matching records
#' @export
nycod_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                            page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- nycod_query(dataset_id, where = where,
                         limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}


# == Context ===================================================================

#' Generate LLM-friendly context for opendata.cityofnewyork.us
#'
#' @return Character string with full function signatures and bodies
#' @export
nycod_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/opendata.cityofnewyork.us.R"
  if (!file.exists(src_file)) {
    cat("# opendata.cityofnewyork.us context - source not found\n")
    return(invisible("# opendata.cityofnewyork.us context - source not found"))
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

