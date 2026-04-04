# phila.gov.R - City of Philadelphia open data client (CARTO SQL API)
#
# Data source: phl.carto.com (CARTO SQL API)
# Datasets: ~267 (crime, permits, violations, licenses, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.phl_carto <- "https://phl.carto.com/api/v2/sql"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.phl_sql <- function(sql) {
  url <- paste0(.phl_carto, "?q=", utils::URLencode(sql))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$rows) || length(raw$rows) == 0) return(tibble())
  df <- as_tibble(raw$rows)
  # Remove CARTO internal geom columns
  df <- df |> select(-any_of(c("the_geom", "the_geom_webmercator")))
  df
}

# == Known tables ==============================================================

.phl_tables <- c(
  "incidents_part1_part2",
  "li_permits",
  "li_violations",
  "li_complaints",
  "li_case_inspections",
  "li_trade_licenses",
  "li_commercial_activity_licenses",
  "li_business_licenses",
  "complaints_against_police",
  "shootings"
)

# == Data access ===============================================================

#' List known Philadelphia CARTO data tables
#'
#' Returns a tibble of the ten most commonly used datasets available through
#' the City of Philadelphia CARTO SQL API at phl.carto.com. Use these names
#' with \code{phl_query()} to build custom SQL queries, or with the
#' convenience functions \code{phl_crimes()}, \code{phl_permits()}, and
#' \code{phl_violations()}.
#'
#' @return A tibble with one column:
#'   \describe{
#'     \item{table_name}{Character. CARTO table identifier (e.g.
#'       \code{"incidents_part1_part2"}, \code{"li_permits"}).}
#'   }
#'
#' @examples
#' phl_tables()
#'
#' @seealso \code{\link{phl_query}}, \code{\link{phl_count}}
#' @export
phl_tables <- function() {
  tibble(table_name = .phl_tables)
}

#' Run a SQL query against Philadelphia open data
#'
#' Execute arbitrary SQL against any Philadelphia dataset hosted on the
#' CARTO platform. The API supports full PostgreSQL/PostGIS syntax including
#' joins, window functions, and spatial queries. Internal geometry columns
#' (\code{the_geom}, \code{the_geom_webmercator}) are automatically removed
#' from the result.
#'
#' @param sql Character. A valid SQL query string. Table names correspond to
#'   CARTO dataset identifiers (see \code{phl_tables()} for common ones).
#'
#' @return A tibble of query results with column types inferred by the API.
#'   Returns an empty tibble if the query returns no rows or fails.
#'
#' @details The SQL is URL-encoded and sent to
#'   \code{https://phl.carto.com/api/v2/sql}. No authentication is required.
#'   Large result sets should include a \code{LIMIT} clause.
#'
#' @examples
#' # Count shootings in 2024
#' phl_query("SELECT count(*) AS n FROM shootings WHERE year = 2024")
#'
#' # Top 5 complaint types
#' phl_query("SELECT complainttypename, count(*) AS n FROM li_complaints
#'            GROUP BY complainttypename ORDER BY n DESC LIMIT 5")
#'
#' @seealso \code{\link{phl_tables}}, \code{\link{phl_crimes}},
#'   \code{\link{phl_permits}}, \code{\link{phl_violations}}
#' @export
phl_query <- function(sql) {
  .phl_sql(sql)
}

#' Get Philadelphia crime incidents
#'
#' Query Part I and Part II crime incidents reported to the Philadelphia
#' Police Department. Records are sourced from the \code{incidents_part1_part2}
#' CARTO table and ordered by dispatch date/time descending (most recent first).
#'
#' @param limit Integer. Maximum number of rows to return (default 1000).
#' @param where Character or \code{NULL}. Optional SQL WHERE clause
#'   (without the keyword \code{WHERE}). Combined with \code{year} via
#'   \code{AND} when both are supplied.
#' @param year Integer or \code{NULL}. Convenience filter for a single
#'   calendar year (e.g. \code{2024}). Translates to a date range filter
#'   on \code{dispatch_date}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{dc_dist}{Character. Police district number.}
#'     \item{psa}{Character. Police service area.}
#'     \item{dispatch_date_time}{Character (ISO 8601). Dispatch timestamp.}
#'     \item{dispatch_date}{Character. Date portion (YYYY-MM-DD).}
#'     \item{dispatch_time}{Character. Time portion (HH:MM:SS).}
#'     \item{hour}{Integer. Hour of dispatch (0--23).}
#'     \item{dc_key}{Numeric. Unique record key.}
#'     \item{location_block}{Character. Generalized block-level address.}
#'     \item{ucr_general}{Character. UCR crime code.}
#'     \item{text_general_code}{Character. Human-readable crime category.}
#'     \item{point_x}{Numeric. Longitude (WGS 84).}
#'     \item{point_y}{Numeric. Latitude (WGS 84).}
#'   }
#'
#' @examples
#' # Most recent 10 crimes
#' phl_crimes(limit = 10)
#'
#' # All thefts in 2024
#' phl_crimes(limit = 500, year = 2024,
#'            where = "text_general_code = 'Thefts'")
#'
#' @seealso \code{\link{phl_query}}, \code{\link{phl_count}}
#' @export
phl_crimes <- function(limit = 1000, where = NULL, year = NULL) {
  w <- character()
  if (!is.null(year)) w <- c(w, sprintf("dispatch_date >= '%d-01-01' AND dispatch_date < '%d-01-01'", year, year + 1))
  if (!is.null(where)) w <- c(w, where)
  where_clause <- if (length(w) > 0) paste("WHERE", paste(w, collapse = " AND ")) else ""
  sql <- sprintf("SELECT dc_dist, psa, dispatch_date_time, dispatch_date, dispatch_time, hour, dc_key, location_block, ucr_general, text_general_code, point_x, point_y FROM incidents_part1_part2 %s ORDER BY dispatch_date_time DESC LIMIT %d",
                 where_clause, limit)
  .phl_sql(sql)
}

#' Get Philadelphia building and zoning permits
#'
#' Query Licenses & Inspections permit records from the \code{li_permits}
#' CARTO table. Results are ordered by issue date descending (most recent
#' first) and include owner, contractor, permit type, and geocoded location.
#'
#' @param limit Integer. Maximum number of rows to return (default 1000).
#' @param where Character or \code{NULL}. Optional SQL WHERE clause
#'   (without the keyword \code{WHERE}). Example:
#'   \code{"permittype = 'RESIDENTIAL'"}.
#'
#' @return A tibble with ~30 columns including \code{permitnumber},
#'   \code{permittype}, \code{permitdescription}, \code{typeofwork},
#'   \code{descriptionofwork}, \code{permitissuedate}, \code{status},
#'   \code{ownername}, \code{contractorname}, \code{geocode_x},
#'   \code{geocode_y}, and \code{council_district}.
#'
#' @examples
#' phl_permits(limit = 5)
#'
#' # Permits issued for demolition
#' phl_permits(limit = 50, where = "typeofwork = 'DEMOLITION'")
#'
#' @seealso \code{\link{phl_violations}}, \code{\link{phl_query}}
#' @export
phl_permits <- function(limit = 1000, where = NULL) {
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""
  sql <- sprintf("SELECT * FROM li_permits %s ORDER BY permitissuedate DESC LIMIT %d",
                 where_clause, limit)
  .phl_sql(sql)
}

#' Get Philadelphia code violations
#'
#' Query Licenses & Inspections code violation records from the
#' \code{li_violations} CARTO table. Each row represents a single
#' violation tied to a case, with dates, descriptions, and geocoded
#' addresses.
#'
#' @param limit Integer. Maximum number of rows to return (default 1000).
#' @param where Character or \code{NULL}. Optional SQL WHERE clause
#'   (without the keyword \code{WHERE}). Example:
#'   \code{"violationtype = 'PM-302.1'"}.
#'
#' @return A tibble with ~28 columns including \code{casenumber},
#'   \code{address}, \code{violationdate}, \code{violationtype},
#'   \code{violationdescription}, \code{casestatus}, \code{casegroup},
#'   \code{ownername}, \code{geocode_x}, and \code{geocode_y}.
#'
#' @examples
#' phl_violations(limit = 5)
#'
#' @seealso \code{\link{phl_permits}}, \code{\link{phl_query}}
#' @export
phl_violations <- function(limit = 1000, where = NULL) {
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""
  sql <- sprintf("SELECT * FROM li_violations %s LIMIT %d", where_clause, limit)
  .phl_sql(sql)
}

#' Get row count for a Philadelphia CARTO table
#'
#' Returns the total number of rows in a given Philadelphia open data table,
#' optionally filtered by a WHERE clause. Useful for understanding dataset
#' size before downloading large results.
#'
#' @param table_name Character. CARTO table name (see \code{phl_tables()}).
#' @param where Character or \code{NULL}. Optional SQL WHERE clause
#'   (without the keyword \code{WHERE}).
#'
#' @return A single integer representing the row count, or \code{NA_integer_}
#'   if the query fails.
#'
#' @examples
#' phl_count("incidents_part1_part2")
#' phl_count("shootings", where = "year = 2024")
#'
#' @seealso \code{\link{phl_tables}}, \code{\link{phl_query}}
#' @export
phl_count <- function(table_name, where = NULL) {
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""
  sql <- sprintf("SELECT count(*) as n FROM %s %s", table_name, where_clause)
  raw <- .phl_sql(sql)
  if (nrow(raw) == 0) return(NA_integer_)
  as.integer(raw$n[1])
}

# == Context ===================================================================

#' Get phila.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
phl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(phl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/phila.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "phila.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# phila.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# phila.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
