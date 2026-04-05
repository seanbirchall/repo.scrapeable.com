# wprdc.org.R - Western PA Regional Data Center (CKAN API)
# Self-contained client for data.wprdc.org
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Base: https://data.wprdc.org/api/3


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wprdc_base <- "https://data.wprdc.org/api/3"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_packages <- tibble(
  name = character(), title = character(), notes = character(),
  organization = character(), num_resources = integer(),
  metadata_modified = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character()
)

# == Dataset discovery =========================================================

#' Search WPRDC datasets by keyword
#'
#' Searches the Western PA Regional Data Center (WPRDC) CKAN catalog.
#' The WPRDC hosts 300+ open datasets for the Pittsburgh/Allegheny County
#' region covering public safety, property, transit, health, and more.
#'
#' @param q Optional character search query. When \code{NULL}, returns
#'   all datasets (sorted by relevance).
#' @param rows Integer maximum results (default 20, max 1000).
#' @param start Integer pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Dataset slug/identifier}
#'     \item{title}{Human-readable dataset title}
#'     \item{notes}{Description text (truncated to 300 chars)}
#'     \item{organization}{Publishing organization name}
#'     \item{num_resources}{Integer count of data resources in the dataset}
#'     \item{metadata_modified}{Last modification timestamp}
#'   }
#' @export
#' @family WPRDC functions
#' @seealso \code{\link{wprdc_package}} to get resources for a dataset,
#'   \code{\link{wprdc_data}} to fetch actual data records
#' @examples
#' \dontrun{
#' wprdc_search("police")
#' wprdc_search("property assessment", rows = 10)
#' }
wprdc_search <- function(q = NULL, rows = 20, start = 0) {
  url <- sprintf("%s/action/package_search?rows=%d&start=%d",
                 .wprdc_base, as.integer(rows), as.integer(start))
  if (!is.null(q)) url <- paste0(url, "&q=", utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(.schema_packages)
  results <- raw$result$results
  if (is.null(results) || length(results) == 0) return(.schema_packages)
  tibble(
    name              = as.character(results$name %||% NA),
    title             = as.character(results$title %||% NA),
    notes             = substr(as.character(results$notes %||% ""), 1, 300),
    organization      = as.character(results$organization$title %||% NA),
    num_resources     = as.integer(results$num_resources %||% NA),
    metadata_modified = as.character(results$metadata_modified %||% NA)
  )
}

#' List all WPRDC dataset names
#'
#' Returns a character vector of all dataset slugs in the WPRDC catalog.
#' Use these slugs with \code{\link{wprdc_package}} to get resource IDs.
#'
#' @param limit Integer maximum results (default 500).
#' @return Character vector of dataset name slugs.
#' @export
#' @family WPRDC functions
#' @seealso \code{\link{wprdc_search}} for keyword searching,
#'   \code{\link{wprdc_package}} to inspect a dataset
#' @examples
#' \dontrun{
#' names <- wprdc_list()
#' head(names)
#' }
wprdc_list <- function(limit = 500) {
  url <- sprintf("%s/action/package_list?limit=%d", .wprdc_base, as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(character())
  as.character(raw$result)
}

#' Get metadata and resources for a WPRDC dataset
#'
#' Returns the list of data resources (files/APIs) within a dataset.
#' Use the resource ID with \code{\link{wprdc_data}} to fetch records.
#'
#' @param name Character dataset slug (e.g., \code{"police-incident-blotter"}).
#'   Get slugs from \code{\link{wprdc_list}} or \code{\link{wprdc_search}}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Resource UUID (use with \code{wprdc_data})}
#'     \item{name}{Resource name}
#'     \item{format}{File format (e.g., "CSV", "GeoJSON")}
#'     \item{url}{Direct download URL}
#'     \item{description}{Resource description}
#'   }
#' @export
#' @family WPRDC functions
#' @seealso \code{\link{wprdc_data}} to query records from a resource
#' @examples
#' \dontrun{
#' wprdc_package("police-incident-blotter")
#' }
wprdc_package <- function(name) {
  url <- sprintf("%s/action/package_show?id=%s", .wprdc_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  r <- raw$result
  resources <- r$resources
  if (is.null(resources) || !is.data.frame(resources)) return(tibble())
  tibble(
    id          = as.character(resources$id %||% NA),
    name        = as.character(resources$name %||% NA),
    format      = as.character(resources$format %||% NA),
    url         = as.character(resources$url %||% NA),
    description = as.character(resources$description %||% NA)
  )
}

# == Data access ===============================================================

#' Query WPRDC datastore records
#'
#' Fetches actual data rows from a WPRDC resource via the CKAN
#' datastore API. Supports full-text search, field filters, sorting,
#' and pagination. The \code{_id} column is automatically removed.
#'
#' @param resource_id Character resource UUID. Get from
#'   \code{\link{wprdc_package}}.
#' @param q Optional character full-text search query.
#' @param filters Optional named list of field:value pairs for exact
#'   filtering (e.g., \code{list(INCIDENTNEIGHBORHOOD = "Shadyside")}).
#' @param limit Integer maximum records (default 100, max 32000).
#' @param offset Integer pagination offset (default 0).
#' @param sort Optional character sort specification
#'   (e.g., \code{"INCIDENTTIME desc"}).
#' @return A tibble of data records with columns matching the resource schema.
#' @export
#' @family WPRDC functions
#' @seealso \code{\link{wprdc_sql}} for SQL-based queries,
#'   \code{\link{wprdc_package}} to find resource IDs
#' @examples
#' \dontrun{
#' # Get recent police incidents
#' wprdc_data("1797ead8-8262-41cc-9099-cbc8a161924b",
#'            limit = 50, sort = "INCIDENTTIME desc")
#' }
wprdc_data <- function(resource_id, q = NULL, filters = NULL,
                       limit = 100, offset = 0, sort = NULL) {
  url <- sprintf("%s/action/datastore_search?resource_id=%s&limit=%d&offset=%d",
                 .wprdc_base, resource_id, as.integer(limit), as.integer(offset))
  if (!is.null(q)) url <- paste0(url, "&q=", utils::URLencode(q))
  if (!is.null(filters)) {
    fj <- jsonlite::toJSON(filters, auto_unbox = TRUE)
    url <- paste0(url, "&filters=", utils::URLencode(as.character(fj)))
  }
  if (!is.null(sort)) url <- paste0(url, "&sort=", utils::URLencode(sort))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || !is.data.frame(records) || nrow(records) == 0) return(tibble())
  as_tibble(records) |> select(-any_of("_id"))
}

#' Run a SQL query against the WPRDC datastore
#'
#' Executes a PostgreSQL-dialect SQL query against the CKAN datastore.
#' Table names are resource UUIDs quoted in double quotes. Supports
#' JOINs, GROUP BY, aggregations, and all standard SQL operations.
#'
#' @param sql Character SQL query string. Table names must be resource
#'   UUIDs in double quotes (e.g.,
#'   \code{'SELECT * FROM "1797ead8-8262-41cc-9099-cbc8a161924b" LIMIT 10'}).
#' @return A tibble of query results.
#' @export
#' @family WPRDC functions
#' @seealso \code{\link{wprdc_data}} for simpler filter-based queries
#' @examples
#' \dontrun{
#' wprdc_sql('SELECT "INCIDENTNEIGHBORHOOD", COUNT(*) as n
#'            FROM "1797ead8-8262-41cc-9099-cbc8a161924b"
#'            GROUP BY "INCIDENTNEIGHBORHOOD" ORDER BY n DESC LIMIT 10')
#' }
wprdc_sql <- function(sql) {
  url <- paste0(.wprdc_base, "/action/datastore_search_sql?sql=", utils::URLencode(sql))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || !isTRUE(raw$success)) return(tibble())
  records <- raw$result$records
  if (is.null(records) || !is.data.frame(records) || nrow(records) == 0) return(tibble())
  as_tibble(records)
}

# == Context ===================================================================

#' Get wprdc.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
wprdc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(wprdc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/wprdc.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "wprdc.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# wprdc.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# wprdc.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
