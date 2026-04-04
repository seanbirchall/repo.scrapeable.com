# == Data access ===============================================================

#' List known Philadelphia CARTO data tables
#'
#' @return tibble: table_name
#' @export
phl_tables <- function() {
  tibble(table_name = .phl_tables)
}

#' Run a SQL query against Philadelphia open data
#'
#' Uses the CARTO SQL API to query any Philadelphia dataset.
#'
#' @param sql SQL query string
#' @return tibble with query results
#' @export
phl_query <- function(sql) {
  .phl_sql(sql)
}

#' Get Philadelphia crime incidents
#'
#' @param limit Number of rows (default 1000)
#' @param where Optional SQL WHERE clause (without 'WHERE')
#' @param year Optional year filter (e.g. 2024)
#' @return tibble: dc_dist, dispatch_date_time, dispatch_date, location_block,
#'   text_general_code, point_x, point_y
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

#' Get Philadelphia permits
#'
#' @param limit Number of rows (default 1000)
#' @param where Optional SQL WHERE clause
#' @return tibble with permit data
#' @export
phl_permits <- function(limit = 1000, where = NULL) {
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""
  sql <- sprintf("SELECT * FROM li_permits %s ORDER BY permitissuedate DESC LIMIT %d",
                 where_clause, limit)
  .phl_sql(sql)
}

#' Get Philadelphia code violations
#'
#' @param limit Number of rows (default 1000)
#' @param where Optional SQL WHERE clause
#' @return tibble with violation data
#' @export
phl_violations <- function(limit = 1000, where = NULL) {
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""
  sql <- sprintf("SELECT * FROM li_violations %s LIMIT %d", where_clause, limit)
  .phl_sql(sql)
}

#' Get row count for a Philadelphia table
#'
#' @param table_name CARTO table name
#' @param where Optional WHERE clause
#' @return integer count
#' @export
phl_count <- function(table_name, where = NULL) {
  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""
  sql <- sprintf("SELECT count(*) as n FROM %s %s", table_name, where_clause)
  raw <- .phl_sql(sql)
  if (nrow(raw) == 0) return(NA_integer_)
  as.integer(raw$n[1])
}

# == Context ===================================================================

#' Generate LLM-friendly context for phila.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
phl_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/phila.gov.R"
  if (!file.exists(src_file)) { cat("# phila.gov context - source not found\n"); return(invisible(NULL)) }
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
