# opendata.cityofnewyork.us.R - Self-contained opendata.cityofnewyork.us client



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
#' Returns metadata for datasets available on the NYC Open Data portal
#' (data.cityofnewyork.us). Optionally filters by name substring.
#'
#' @param limit Number of datasets to return (default 50)
#' @param query Optional search filter on dataset name (case-insensitive
#'   substring match, e.g. "311", "taxi", "crime")
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Socrata dataset identifier (character)}
#'     \item{name}{Dataset display name (character)}
#'     \item{category}{Dataset category, e.g. "Public Safety" (character)}
#'     \item{description}{Truncated description, max 200 chars (character)}
#'     \item{rows_updated_at}{Last data update timestamp (POSIXct)}
#'   }
#' @examples
#' nycod_datasets(limit = 10)
#' nycod_datasets(query = "311")
#' @seealso [nycod_query()], [nycod_fetch_all()], [nycod_context()]
#' @source <https://data.cityofnewyork.us>
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
#' Queries a specific dataset using the Socrata SODA API with full SoQL
#' (Socrata Query Language) support for filtering, aggregation, and sorting.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "erm2-nwe9" for 311
#'   complaints). Find IDs via [nycod_datasets()].
#' @param where SoQL where clause (e.g. "borough='MANHATTAN'",
#'   "created_date > '2025-01-01'")
#' @param select SoQL select clause (e.g. "borough,count(*)")
#' @param group SoQL group by clause (e.g. "borough")
#' @param order SoQL order clause (e.g. "created_date DESC")
#' @param limit Max rows to return (default 1000, Socrata max 50000)
#' @param offset Pagination offset (default 0)
#' @return A tibble of query results. Columns vary by dataset.
#' @examples
#' nycod_query("erm2-nwe9", limit = 5)
#' nycod_query("erm2-nwe9", where = "borough='MANHATTAN'", limit = 10)
#' @seealso [nycod_datasets()], [nycod_fetch_all()], [nycod_context()]
#' @source <https://data.cityofnewyork.us>
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
#' Repeatedly calls the SODA API, paginating through results until
#' all matching records are retrieved or max_rows is reached.
#'
#' @param dataset_id Socrata dataset identifier (e.g. "erm2-nwe9")
#' @param where Optional SoQL where clause for filtering
#' @param max_rows Maximum total rows to fetch (default 10000). Use this
#'   to cap large datasets.
#' @param page_size Rows per API request (default 1000)
#' @return A tibble of all matching records. Columns vary by dataset.
#' @examples
#' nycod_fetch_all("erm2-nwe9", where = "borough='BRONX'", max_rows = 500)
#' @seealso [nycod_query()], [nycod_datasets()], [nycod_context()]
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

#' Get opendata.cityofnewyork.us client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nycod_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nycod_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/opendata.cityofnewyork.us.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "opendata.cityofnewyork.us")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# opendata.cityofnewyork.us context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# opendata.cityofnewyork.us", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
